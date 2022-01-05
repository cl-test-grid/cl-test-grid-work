(ql:quickload :test-grid-storage)

(defpackage #:tg-replicas
  (:use :cl)
  (:shadow #:get)
  (:export #:get))
(in-package #:tg-replicas)

(defun snapshot-file (file-name)
  (asdf:system-relative-pathname :test-grid-storage
                                 file-name))

(defvar *replicas* (make-hash-table :test #'equal))

(defun get (storage-name)
  (or (gethash storage-name *replicas*)
      (let ((r (tg-storage:make-replica storage-name
                                        (snapshot-file (format nil "db-~A.lisp" storage-name)))))
        (setf (gethash storage-name *replicas*)
              r))))
