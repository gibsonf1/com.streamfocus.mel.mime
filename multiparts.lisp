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
   "EML-MESSAGE->VIEWABLE-PART"
   "EML->REPLY-TO"
   "EML->FROM"
   "EML->TO"
   "EML->CC"
   "EML-ADDRESS->NAME"
   "EML-ADDRESS->EMAIL"
   "EML-FIND-ATTACHMENTS"
   "EML-ATTACHMENT-NAME"
   "EML-PART->PARENT"
   "PART-HTML-PAGE?"
   "PART-BODY-HTML?"
   "PART-BODY-HTML"
   "PART-BODY-TEXT?"
   "PART-BODY-TEXT"
   )
  (:nicknames :sf.mel))

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

;;TODO -> find/save/view alternative/related files see 10569-mail.eml for format

(defun eml-content-type (obj)
          (multiple-value-bind (a b c)(mel.mime:content-type obj)
            (declare (ignore b c))
            a))

(defun eml-message->viewable-part (obj)
 "takes a part or eml message"
 (flet ((eml-content-subtype (o)
          (multiple-value-bind (a b c)(mel.mime:content-type o)
            (declare (ignore a c))
            b))
        )
   (if (eq (eml-content-subtype obj) :plain)
       obj
       (let ((parts (mel.mime:parts obj)))
         (when parts
           (or
            (find :plain parts :key #'eml-content-subtype)
            (let ((mpart (find :multipart parts :key #'eml-content-type)))
              (when mpart (eml-message->viewable-part mpart)))))))))

(defun eml-address->email (eml-address)
  (when (slot-boundp eml-address 'mel:address-spec)
    (mel:address-spec eml-address)))

(defun eml-address->name (eml-address)
  (when (slot-boundp eml-address 'mel:display-name)
    (mel:display-name eml-address)))

(defun eml-part-base64? (part)
  (when (eq (mel.mime:content-transfer-encoding part) :base64)
    t))

(defun eml-find-attachments (eml)
  (flet ((eml-mixed? ()
           (multiple-value-bind (a b c) (mel.mime:content-type eml)
             (declare (ignore a c))
             (when (eq b :mixed) t))))
    (when (eml-mixed?)
      (let (attach)
        (dolist (part (mel:parts eml))
          (when (eml-part-base64? part)
            (push part attach)))
        attach))))

(defun eml-attachment-name (part)
  "Returns attachment filename string"
  (when (eml-part-base64? part)
    (getf (mel.mime::content-parameters part) :name)))
    
(defun eml-part->parent (part)
  (if (typep part 'mel.public:mime-message)
      part
      (let ((parent (mel.mime::parent part)))
        (if parent
            (eml-part->parent parent)
            part))))

(defun eml-part-text? (part)
  (eq (eml-content-subtype part) :plain))

(defun eml-part-html? (part)
  (eq (eml-content-subtype part) :html))

(defun eml-content-subtype (obj)
  (multiple-value-bind (a b c)(mel.mime:content-type obj)
             (declare (ignore a c))
             b))

(defun eml-message->text-part (obj)
  "takes a part or eml message"
  (if (and (typep obj 'mel.mime:part)
           (eml-part-text? obj))
      obj
      (let ((parts (mel.mime:parts obj)))
        (when parts
          (or
           (find :plain parts :key #'eml-content-subtype)
           (let ((mpart (find :multipart parts :key #'eml-content-type)))
             (when mpart (eml-message->text-part mpart))))))))

(defun eml-message->html-part (obj)
  "takes a part or eml message"
  (if (and (typep obj 'mel.mime:part)
           (eml-part-html? obj))
      obj
      (let ((parts (mel.mime:parts obj)))
        (when parts
          (or
           (find :html parts :key #'eml-content-subtype)
           (let ((mpart (find :multipart parts :key #'eml-content-type)))
             (when mpart (eml-message->html-part mpart))))))))

(defun part-html-page? (msg)
  (let* ((msg-html? (eml-part-html? msg))
         (part (unless msg-html?
                 (eml-message->html-part msg)))
         (stream (cond (part
                        (mel.mime:part-body-stream part))
                       (msg-html?
                        (mel.public:message-body-stream msg)))))
    (when stream
      (when msg-html?
        (mel.mime:skip-rfc2822-header stream))
      (let ((line (read-line stream nil :eof)))
        (close stream)
        (when (> (length line) 10)
          (if (string= (subseq line 2 9) "DOCTYPE")
            t
            nil))))))

(defun part-body-html? (msg)
  (let* ((msg-html? (eml-part-html? msg))
         (part (unless msg-html?
                 (eml-message->html-part msg))))
    (part-body-probe :part part :msg (when msg-html? msg))))

(defun part-body-text? (msg)
  (let* ((msg-text? (eml-part-text? msg))
         (part (unless msg-text?
                 (eml-message->text-part msg))))
    (part-body-probe :part part :msg (when msg-text? msg))))
         
(defun part-body-probe (&key part msg)
  (let ((stream (cond (part
                       (mel.mime:part-body-stream part))
                      (msg
                       (mel.public:message-body-stream msg)))))
    (when stream
      (when (read-char stream nil nil)
        (close stream)
        t))))

(defun part-body-decode (&key part msg string-fn)
  (let ((string (cond (part
                        (mel.mime:part-body-string part))
                       (msg
                        (message-body-string msg)))))
    (when string
      (eml-decode-rfc2822 string :string-fn string-fn))))

(defun part-body-html (msg &key string-fn)
  (let* ((msg-html? (eml-part-html? msg))
         (part (unless msg-html?
                 (eml-message->html-part msg))))
    (part-body-decode :part part :msg (when msg-html? msg) :string-fn string-fn)))
    
(defun part-body-text (msg &key string-fn)
  (let* ((msg-text? (eml-part-text? msg))
         (part (unless msg-text?
                 (eml-message->text-part msg))))
    (part-body-decode :part part :msg (when msg-text? msg) :string-fn string-fn)))

(defun message-body-string (msg)
  (with-output-to-string (out)
    (with-open-stream (s (mel.public:message-body-stream msg))
      (do ((c (read-char s nil :eof)(read-char s nil :eof)))
          ((eq c :eof) out)
        (case c
          (otherwise (write-char c out)))))))

(defun eml-decode-rfc2822 (string &key string-fn);utf-8
  (declare (type string string) (optimize (speed 2)))
  (let ((in (make-string-input-stream string))
        (out (make-string-output-stream))
        (match 0)
        buffer)
    (labels ((peek ()
               (peek-char nil in nil :eof))
             (consume ()
               (let ((char (read-char in nil :eof)))
                 (output (string char))))
             (output (string)
               (if string-fn
                   (funcall string-fn string)
                   (princ string out)))
             (ignore ()
               (read-char in nil :eof))
             (try ()
               (setf buffer (concatenate 'string buffer (string (read-char in nil :eof))))
               (incf match))
             (reset ()
               (setf match 0 buffer nil)
               (start))
             (match? (code html)
               (if (= match (length code))
                   (progn
                     (output html)
                     (reset))
                   (let ((c (peek)))
                     (cond ((char= c (char code match))
                            (try)(match? code html))
                           ((eq c :eof) (return-result))
                           (t
                              (output buffer)
                              (reset))))))
             (code? ()
               (try)
               (let ((c (peek)))
                 (case c
                   ((#\Newline #\return #\linefeed)
                    (ignore)
                    (let ((c (peek)))
                      (case c
                        ((#\Newline #\return #\linefeed)(ignore)(reset))
                        (t (reset)))))
                   (#\C (match? "=C2=A0" "&nbsp;"))
                   (#\A (match? "=A0" "&nbsp;"))
                   (#\2 (match? "=20" ""))
                   (#\3 (match? "=3D" "="))
                   (:eof (return-result))
                   (t (output buffer)
                      (consume)
                      (reset)))))
             (start ()
               (let ((c (peek)))
                 (case c
                   (#\= (code?))
                   (:eof (return-result))
                   (t (consume)
                      (start)))))
             (return-result ()
               (when buffer
                 (output buffer))))
      (start)
      (unless string-fn
        (get-output-stream-string out)))))




