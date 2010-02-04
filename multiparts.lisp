;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2010, Fred Gibson <fred@streamfocus.com>.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage :com.streamfocus.mel.mime
  (:use :cl)
  (:export
   "SAVE-ATTACHMENT-FILE"
   "EML-MESSAGE->VIEWABLE-PART"))

(in-package :com.streamfocus.mel.mime)

(defun save-attachment-file (part filename)
 (let* ((attach (mel.mime:part-body-string part))
        (length (length
                 (string-trim '(#\newline #\return #\linefeed)
                              (read-line (make-string-input-stream attach)))))
        (in (make-string-input-stream attach)))
   (with-open-file (out filename :direction :output :element-type '(unsigned-byte 8))
     (loop with buffer = (make-array length :element-type 'character)
        for count = (read-sequence buffer in)
        while (> count 0)
        do (write-sequence (mel.mime::decode-base64 buffer) out)
          (flet ((peek ()(peek-char nil in nil :eof))
                 (consume () (read-char in nil :eof)))
            (tagbody
             start (let ((c (peek)))
                     (when (member c '(#\return #\linefeed #\newline))
                       (progn (consume) (go start))))))))))

(defun eml-message->viewable-part (obj)
 "takes a part or eml message"
 (flet ((eml-content-subtype (o)
          (multiple-value-bind (a b c)(mel.mime:content-type o)
            (declare (ignore a c))
            b))
        (eml-content-type (o)
          (multiple-value-bind (a b c)(mel.mime:content-type o)
            (declare (ignore b c))
            a)))
   (if (eq (eml-content-subtype obj) :plain)
       obj
       (let ((parts (mel.mime:parts obj)))
         (when parts
           (or
            (find :plain parts :key #'eml-content-subtype)
            (let ((mpart (find :multipart parts :key #'eml-content-type)))
              (when mpart (eml-message->viewable-part mpart)))))))))
