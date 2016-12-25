(ql:quickload :test-grid-storage)
(ql:quickload :test-grid-reporting)

(defpackage #:asdf-reports
  (:use :cl)
  (:import-from #:test-grid-reporting
                #:list-results
                #:subset                
                #:print-compiler-diff
                #:print-pivot
                #:lisp
                #:libname
                #:system-name
                #:lib-world
                #:contact-email
                #:results-cell-printer
                #:print-quicklisp-diff-report
                #:print-quicklisp-diff-report2
                #:failure-p
                #:result-spec))
(in-package #:asdf-reports)

(defparameter *r* (tg-storage:sync (tg-storage:get-replica "asdf")))
;(defparameter *ra* (tg-storage:sync (tg-storage:get-replica "abcl")))
(defparameter *rm* (tg-storage:sync (tg-storage:get-replica "main")))

(defparameter *db* (tg-data:join-dbs (tg-storage:data *r*)
                                     (tg-storage:data *rm*)
                                     ; (tg-storage:data *ra*)
                                     ))
(defparameter *all-results* (list-results *db*))

(defparameter *ascii-warns*
  (tg-rep::quicklisp-diff-items *all-results*
                                "quicklisp 2013-01-28 + asdf.2.29.5-no-warn + asdf-sys-conn-fix"
                                "quicklisp 2013-01-28 + asdf.2.29.5 + asdf-sys-conn-fix"))
(defparameter *utf-warns*
  (tg-rep::quicklisp-diff-items *all-results*
                                "quicklisp 2013-01-28 + asdf.2.29.5-utf8-no-warn + asdf-sys-conn-fix"
                                "quicklisp 2013-01-28 + asdf.2.29.5-utf8 + asdf-sys-conn-fix"))

(defparameter *utf-only-warns*
  (set-difference *utf-warns* *ascii-warns*
                  :test (lambda (r1 r2)
                          (equalp (list (lisp r1) (system-name r1))
                                  (list (lisp r2) (system-name r2))))))

(defun lisp-systems (results)
  (remove-duplicates (mapcar (lambda (r) (list (lisp r) (system-name r)))
                             results)
                     :test #'equal))

(length (lisp-systems *utf-warns*))
(length (lisp-systems *ascii-warns*))
(length (lisp-systems *utf-only-warns*))

(length (set-difference (lisp-systems *ascii-warns*)
                        (lisp-systems *utf-warns*)
                        :test #'equal))

(print-quicklisp-diff-report "asdf/asdf-diff-15-utf-only.html"
                             *utf-only-warns*
                             "quicklisp 2013-01-28 + asdf.2.29.5-utf8-no-warn + asdf-sys-conn-fix"
                             "quicklisp 2013-01-28 + asdf.2.29.5-utf8 + asdf-sys-conn-fix")

(print-quicklisp-diff-report "asdf/asdf-diff-15.html"
                             *all-results*
                             "quicklisp 2013-01-28 + asdf.2.29.5-utf8-no-warn + asdf-sys-conn-fix"
                             "quicklisp 2013-01-28 + asdf.2.29.5-utf8 + asdf-sys-conn-fix")

(tg-rep::print-load-failures "asdf/sbcl-warning-failures-utf-only.html"
                             *utf-only-warns*
                             "sbcl-1.1.1-linux-x86"
                             "quicklisp 2013-01-28 + asdf.2.29.5-utf8 + asdf-sys-conn-fix")

(print-quicklisp-diff-report "asdf/asdf-diff-21.html"
                             *all-results*
                             "quicklisp 2013-02-17" 
                             "quicklisp 2013-02-17 + asdf.2.31.8")

(print-quicklisp-diff-report "asdf/asdf-diff-22-cmucl.enc.default.html"
                             (subset *all-results* (lambda (r)
                                                     (search "cmu" (lisp r))))
                             "quicklisp 2013-02-17.no-require-asdf" 
                             ;"quicklisp 2013-02-17 + asdf.2.32"
                             "quicklisp 2013-02-17 + asdf.2.32.enc.default"
                             )

(print-quicklisp-diff-report "asdf/asdf-diff-23.html"
                             *all-results*
                             "quicklisp 2013-03-12" 
                             "quicklisp 2013-03-12 + asdf.2.32.35")

(print-quicklisp-diff-report "asdf/asdf-diff-24.html"
                             *all-results*
                             "quicklisp 2013-12-13" 
                             "quicklisp 2013-12-13 + asdf.38337a5")

(print-quicklisp-diff-report "asdf/asdf-diff-25.html"
                             *all-results*
                             "quicklisp 2013-12-13" 
                             "quicklisp 2013-12-13 + asdf.28a5c93") ; aka ASDF 3.1.0.46

(print-quicklisp-diff-report "asdf/asdf-diff-26.html"
                             *all-results*
                             "quicklisp 2013-12-13" 
                             "quicklisp 2013-12-13 + asdf.28a5c93.no-upgrade") ; aka ASDF 3.1.0.46


(print-quicklisp-diff-report "asdf/asdf-diff-27.html"
                             *all-results*                             
                             "quicklisp 2013-12-13 + asdf.28a5c93" ; aka ASDF 3.1.0.46
                             "quicklisp 2013-12-13 + asdf.3.1.0.53" 
                             )

(print-quicklisp-diff-report "asdf/asdf-diff-28.html"
                             *all-results*                             
                             "quicklisp 2013-12-13"
                             "quicklisp 2013-12-13 + asdf.3.1.0.63" 
                             )

(print-quicklisp-diff-report "asdf/asdf-diff-29.html"
                             *all-results*                             
                             "quicklisp 2013-12-13 + asdf.2.32.35"
                             "quicklisp 2013-12-13 + asdf.3.1.0.63")

(print-quicklisp-diff-report "asdf/asdf-diff-30.html"
                             *all-results*                             
                             "quicklisp 2014-01-13"
                             "quicklisp 2014-01-13 + asdf.3.1.0.64.warn-check")

(print-quicklisp-diff-report2 "asdf/asdf-diff-31.html"
                              *all-results*                             
                              "quicklisp 2014-01-13"
                              "quicklisp 2014-01-13 + asdf.3.1.0.64.warn-check")

(print-quicklisp-diff-report2 "asdf/asdf-diff-32.html"
                              *all-results*                             
                              "quicklisp 2013-12-13"
                              "quicklisp 2013-12-13 + asdf.3.1.0.66")

(print-quicklisp-diff-report "asdf/asdf-diff-33.html"
                             *all-results*                             
                             "quicklisp 2013-12-13"
                             "quicklisp 2013-12-13 + asdf.3.1.0.66")

(print-quicklisp-diff-report "asdf/asdf-diff-34.html"
                             *all-results*                             
                             "quicklisp 2013-12-13 + asdf.2.32.35"
                             "quicklisp 2013-12-13 + asdf.3.1.0.66")

(print-quicklisp-diff-report "asdf/asdf-diff-35.html"
                             *all-results*                             
                             "quicklisp 2014-02-11"
                             "quicklisp 2014-02-11 + asdf.3.1.0.67")

(print-quicklisp-diff-report "asdf/asdf-diff-36.html"
                             *all-results*                             
                             "quicklisp 2014-02-11 + asdf.3.1.0.67"
                             "quicklisp 2014-02-11 + asdf.3.1.0.70.use-uiop")

(print-quicklisp-diff-report "asdf/asdf-diff-37.html"
                             *all-results*                             
                             "quicklisp 2014-02-11"
                             "quicklisp 2014-02-11 + asdf.3.1.0.94")

(print-quicklisp-diff-report "asdf/asdf-diff-38.html"
                             *all-results*                             
                             "quicklisp 2014-02-11 + asdf.3.1.0.94"
                             "quicklisp 2014-02-11 + asdf.3.1.0.94.synt-patch")

(print-quicklisp-diff-report "asdf/asdf-diff-39.html"
                             *all-results*                             
                             "quicklisp 2014-02-11"
                             "quicklisp 2014-02-11 + asdf.synt-control.7a28dbf0")

(print-quicklisp-diff-report "asdf/asdf-diff-40.html"
                             *all-results*                             
                             "quicklisp 2014-02-11"
                             "quicklisp 2014-02-11 + asdf.synt-control.d32afa0c")

(tg-rep::print-load-failures "asdf/asdf-load-failures-3.1.0.94.synt-patch.html"
                             (tg-rep::quicklisp-diff-items *all-results*
                                                           "quicklisp 2014-02-11 + asdf.3.1.0.94"
                                                           "quicklisp 2014-02-11 + asdf.3.1.0.94.synt-patch")
                             "sbcl-1.1.16-linux-x86"
                             "quicklisp 2014-02-11 + asdf.3.1.0.94.synt-patch")

(print-quicklisp-diff-report "asdf/asdf-diff-41.html"
                             *all-results*                             
                             "quicklisp 2014-03-17"
                             "quicklisp 2014-03-17 + asdf.3.1.0.116")

(print-quicklisp-diff-report "asdf/asdf-diff-42.html"
                             *all-results*                             
                             "quicklisp 2014-04-25"
                             "quicklisp 2014-04-25 + asdf.synt-control.e4229d8")

(print-quicklisp-diff-report "asdf/asdf-diff-43.html"
                             *all-results*                             
                             "quicklisp 2014-04-25"
                             "quicklisp 2014-04-25 + asfd.3.1.0.120")

(print-quicklisp-diff-report "asdf/asdf-diff-44.html"
                             *all-results*                             
                             "quicklisp 2014-06-16"
                             "quicklisp 2014-06-16 + asdf.3.1.2.9")

(print-quicklisp-diff-report "asdf/asdf-diff-45.html"
                             *all-results*                     
                             "quicklisp 2015-06-08"
                             "quicklisp 2015-06-08 + asdf.d70a8f8")

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2015-06-08"
                                                       "quicklisp 2015-06-08 + asdf.d70a8f8")))
  (print-quicklisp-diff-report "asdf/asdf-diff-46.html"
                               results-to-compare
                               "quicklisp 2015-06-08"
                               "quicklisp 2015-06-08 + asdf.d70a8f8"))

(print-quicklisp-diff-report "asdf/asdf-diff-47.html"
                             *all-results*                     
                             "quicklisp 2015-06-08"
                             "quicklisp 2015-06-08 + asdf.c3f7c73")

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2015-06-08"
                                                       "quicklisp 2015-06-08 + asdf.c3f7c73")))
  (print-quicklisp-diff-report "asdf/asdf-diff-48.html"
                               results-to-compare
                               "quicklisp 2015-06-08"
                               "quicklisp 2015-06-08 + asdf.c3f7c73"))

(print-quicklisp-diff-report "asdf/asdf-diff-49.html"
                             *all-results*
                             "quicklisp 2015-09-24"
                             "quicklisp 2015-09-24 + asdf.3.1.5.20")

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2015-09-24"
                                                       "quicklisp 2015-09-24 + asdf.3.1.5.20")))
  (print-quicklisp-diff-report "asdf/asdf-diff-50.html"
                               results-to-compare
                               "quicklisp 2015-09-24"
                               "quicklisp 2015-09-24 + asdf.3.1.5.20"))

(print-quicklisp-diff-report "asdf/asdf-diff-51.html"
                             *all-results*                     
                             "quicklisp 2016-12-08"
                             "quicklisp 2016-12-08 + asdf.3.1.7.43")

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2016-12-08"
                                                       "quicklisp 2016-12-08 + asdf.3.1.7.43")))
  (print-quicklisp-diff-report "asdf/asdf-diff-52.html"
                               (tg-rep::subset results-to-compare
                                               (lambda (r)
                                                 (not (and (string= "quicklisp 2016-12-08 + asdf.3.1.7.43"
                                                                    (tg-rep::lib-world r))
                                                           (tg-rep::failure-p r)
                                                           (let ((err-text (tg-rep::fail-condition-text r)))
                                                             (some (lambda (txt) (search txt err-text))
                                                                   '("undefined function ASDF/INTERFACE::OPERATION-FORCED"
                                                                     "OPERATION instances must only be created through MAKE-OPERATION."
                                                                     "function ASDF/INTERFACE::OPERATION-FORCED is undefined."
                                                                     "Undefined function ASDF/INTERFACE::OPERATION-FORCED")))))))
                               "quicklisp 2016-12-08"
                               "quicklisp 2016-12-08 + asdf.3.1.7.43"))



(sptm:repli-exec *r* 'tg-data:update-run-descr '((:lib-world "quicklisp 2014-04-25 + asfd.3.1.0.120")
                                                 (:lib-world "quicklisp 2014-04-25 + asdf.3.1.0.120")))
(sptm:save-snapshot)
(length (subset *all-results*
                (lambda (r) (string= "quicklisp 2016-12-08 + asdf.3.1.7.43" (lib-world r)))))

(format t "~%~{~A~%~}"
        (sort (alexandria:flatten
               (tg-rep::distinct (subset *all-results*
                                         (lambda (r) (string= "quicklisp 2016-12-08 + asdf.3.1.7.43"
                                                              (lib-world r))))
                                 '(lisp)))
              #'string<))

(format t "~%~{~S~%~}"
        (sort (tg-rep::distinct *all-results* '(lib-world lisp tg-rep::contact-email))
              (lambda (l1 l2)
                (tg-utils::list< (list #'string< #'string< #'string<)
                                 l1 l2))))

