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
                #:result-spec
                #:print-new-failures))
(in-package #:asdf-reports)

(handler-bind ((serious-condition (lambda (c)
                                    (declare (ignore c))
                                    (invoke-restart 'sptm:use-partially-updated))))
  (defparameter *r* (tg-storage:sync (tg-storage:get-replica "asdf"))))

;(defparameter *ra* (tg-storage:sync (tg-storage:get-replica "abcl")))
(handler-bind ((serious-condition (lambda (c)
                                    (declare (ignore c))
                                    (invoke-restart 'sptm:use-partially-updated))))
    (defparameter *rm* (tg-storage:sync (tg-storage:get-replica "main"))))

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


(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2016-12-08"
                                                       "quicklisp 2016-12-08 + asdf.3.1.7.43")))
  (print-quicklisp-diff-report "asdf/asdf-diff-53.html"
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
                                                                     "Undefined function ASDF/INTERFACE::OPERATION-FORCED"
                                                                     "There exists no package with name \"CFFI-TOOLCHAIN\"")))))))
                               "quicklisp 2016-12-08"
                               "quicklisp 2016-12-08 + asdf.3.1.7.43"))


(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2016-12-08"
                                                       "quicklisp 2016-12-08 + asdf.3.1.7.43 + slime.786c032 + cffi.ff0a22e + asdf-system-connections.9f08524")))
  (print-quicklisp-diff-report "asdf/asdf-diff-54.html"
                               results-to-compare
                               "quicklisp 2016-12-08"
                               "quicklisp 2016-12-08 + asdf.3.1.7.43 + slime.786c032 + cffi.ff0a22e + asdf-system-connections.9f08524"))


(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2016-12-08"
                                                       "quicklisp 2016-12-08 + asdf.3.1.7.43 + slime.786c032 + cffi.ff0a22e + asdf-system-connections.9f08524")))
  (print-quicklisp-diff-report "asdf/asdf-diff-55.html"
                               (tg-rep::subset results-to-compare
                                               (lambda (r)
                                                 (not (and (string= "quicklisp 2016-12-08 + asdf.3.1.7.43 + slime.786c032 + cffi.ff0a22e + asdf-system-connections.9f08524"
                                                                    (tg-rep::lib-world r))
                                                           (tg-rep::failure-p r)
                                                           (let ((err-text (tg-rep::fail-condition-text r)))
                                                             (some (lambda (txt) (search txt err-text))
                                                                   '("OPERATION instances must only be created through MAKE-OPERATION.")))))))
                               "quicklisp 2016-12-08"
                               "quicklisp 2016-12-08 + asdf.3.1.7.43 + slime.786c032 + cffi.ff0a22e + asdf-system-connections.9f08524"))

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2016-12-08"
                                                       "quicklisp 2016-12-08 + asdf.dee8b12 + slime.786c032 + cffi.ff0a22e.no-uffi-compat + asdf-system-connections.9f08524 + iolib.57c8079 + prove.127da52 + cl-protobufs.bb5c018 + uiop.dee8b12")))
  (print-quicklisp-diff-report "asdf/asdf-diff-56.html"
                               (tg-rep::subset results-to-compare
                                               (lambda (r)
                                                 (not (and (string= "quicklisp 2016-12-08 + asdf.dee8b12 + slime.786c032 + cffi.ff0a22e.no-uffi-compat + asdf-system-connections.9f08524 + iolib.57c8079 + prove.127da52 + cl-protobufs.bb5c018 + uiop.dee8b12"
                                                                    (tg-rep::lib-world r))
                                                           (tg-rep::failure-p r)
                                                           (let ((err-text (tg-rep::fail-condition-text r)))
                                                             (some (lambda (txt) (search txt err-text))
                                                                   '("OPERATION instances must only be created through MAKE-OPERATION.")))))))
                               "quicklisp 2016-12-08"
                               "quicklisp 2016-12-08 + asdf.dee8b12 + slime.786c032 + cffi.ff0a22e.no-uffi-compat + asdf-system-connections.9f08524 + iolib.57c8079 + prove.127da52 + cl-protobufs.bb5c018 + uiop.dee8b12"))


(print-quicklisp-diff-report "asdf/asdf-diff-57.html"
                             *all-results*
                             "quicklisp 2017-02-27"
                             "quicklisp 2017-02-27 + asdf.3.2.1-rc1.4d66692")

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2017-02-27"
                                                       "quicklisp 2017-02-27 + asdf.3.2.1-rc1.4d66692")))
  (print-quicklisp-diff-report "asdf/asdf-diff-58.html"
                               results-to-compare
                               "quicklisp 2017-02-27"
                               "quicklisp 2017-02-27 + asdf.3.2.1-rc1.4d66692"))

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2017-02-27"
                                                       "quicklisp 2017-02-27 + asdf.3.2.1-rc1.4d66692")))
  (print-quicklisp-diff-report "asdf/asdf-diff-59.html"
                               (tg-rep::subset results-to-compare
                                               (lambda (r)
                                                 (not (and (string= "quicklisp 2017-02-27 + asdf.3.2.1-rc1.4d66692"
                                                                    (tg-rep::lib-world r))
                                                           (tg-rep::failure-p r)
                                                           (let ((err-text (tg-rep::fail-condition-text r)))
                                                             (some (lambda (txt) (search txt err-text))
                                                                   '("OPERATION instances must only be created through MAKE-OPERATION.")))))))
                               "quicklisp 2017-02-27"
                               "quicklisp 2017-02-27 + asdf.3.2.1-rc1.4d66692"))

(print-quicklisp-diff-report "asdf/asdf-diff-60.html"
                             *all-results*
                             "quicklisp 2017-02-27"
                             "quicklisp 2017-02-27 + asdf.3.3.0-rc1.a6bd7c6 + iolib.57c8079 + cl-protobufs.57c8079")

(print-quicklisp-diff-report "asdf/asdf-diff-61.html"
                             *all-results*
                             "quicklisp 2017-02-27"
                             "quicklisp 2017-02-27 + asdf.3.3.0-rc1.a6bd7c6 + iolib.pr42.66ea927 + cl-protobufs.57c8079")

(let ((results-to-compare (tg-rep::tests-failed-on-new *all-results*
                                                       "quicklisp 2017-02-27"
                                                       "quicklisp 2017-02-27 + asdf.3.3.0-rc1.a6bd7c6 + iolib.pr42.66ea927 + cl-protobufs.57c8079")))
  (print-quicklisp-diff-report "asdf/asdf-diff-62.html"
                               results-to-compare
                               "quicklisp 2017-02-27"
                               "quicklisp 2017-02-27 + asdf.3.3.0-rc1.a6bd7c6 + iolib.pr42.66ea927 + cl-protobufs.57c8079"))

(defun print-new-failures (report-file all-results old-lib-world new-lib-world)
  (let ((results-to-compare (tg-rep::tests-failed-on-new all-results
                                                         old-lib-world
                                                         new-lib-world)))
    (print-quicklisp-diff-report report-file
                                 results-to-compare
                                 old-lib-world
                                 new-lib-world)))


(print-new-failures "asdf/asdf-diff-63.html"
                    *all-results*
                    "quicklisp 2017-05-16"
                    "quicklisp 2017-05-16 + asdf.3.1.7")

(print-new-failures "asdf/asdf-diff-64.html"
                    *all-results*
                    "quicklisp 2017-05-16"
                    "quicklisp 2017-05-16 + asdf.3.2.1")

(print-new-failures "asdf/asdf-diff-65.html"
                    *all-results*
                    "quicklisp 2017-05-16"
                    "quicklisp 2017-05-16 + asdf.3.3.0-rc2.e0d5157")
  
(print-new-failures "asdf/asdf-diff-66.html"
                    *all-results*
                    "quicklisp 2017-06-30"
                    "quicklisp 2017-06-30 + asdf.3.3.0-rc3.9fc397e")
  
(print-new-failures "asdf/asdf-diff-67.html"
                    *all-results*
                    "quicklisp 2017-08-30"
                    "quicklisp 2017-08-30 + asdf.3.2.1-14-g5eca374")

(print-new-failures "asdf/asdf-diff-68.html"
                    *all-results*
                    "quicklisp 2017-08-30"
                    "quicklisp 2017-08-30 + asdf.3.2.1-373cafd")

(print-new-failures "asdf/asdf-diff-69.html"
                    *all-results*
                    "quicklisp 2017-08-30"
                    "quicklisp 2017-08-30 + asdf.3.2.1-373cafd"
                    :report-function 'print-quicklisp-diff-report2)

(print-new-failures "asdf/asdf-diff-70.html"
                    *all-results*
                    "quicklisp 2017-08-30"
                    "quicklisp 2017-08-30 + asdf-3.3.1-rc-ffb922b")

(print-new-failures "asdf/asdf-diff-71.html"
                    *all-results*
                    "quicklisp 2017-08-30"
                    "quicklisp 2017-08-30 + asdf-3.3.1-rc3-1a3d333")

(print-new-failures "asdf/asdf-diff-72.html"
                    *all-results*
                    "quicklisp 2018-01-31"
                    "quicklisp 2018-01-31 + asdf-3.3.1.3")

(print-new-failures "asdf/asdf-diff-73.html"
                    *all-results*
                    "quicklisp 2018-02-28"
                    "quicklisp 2018-02-28 + asdf-3.3.1.7")

(print-new-failures "asdf/asdf-diff-74.html"
                    (subset *all-results*
                            (lambda (r)
                              (not (string= (lisp r)
                                            "acl-10.0-linux-x86"))))
                    "quicklisp 2018-03-28"
                    "quicklisp 2018-03-28 + asdf.syntax-ctrl.8dc0778")

;; Some bizzare crashes had happened when testing non-SMP Allegro -
;; after quicklisp compiled fasl file for adsf.lisp first time on one invocation,
;; next invocatinos of the same command chash allegro with the following
;; message:
;; 
;;   Allegro CL(pid 6858): System Error (gsgc) Unknown object type at (0xbf85406a)
;;   The internal data structures in the running Lisp image have been
;;   corrupted and execution cannot continue.  Check all foreign functions
;;   and any Lisp code that was compiled with high speed and/or low safety,
;;   as these are two common sources of this failure.  If you cannot find
;;   anything incorrect in your code you should contact technical support
;;   for Allegro Common Lisp, and we will try to help determine whether
;;   this is a coding error or an internal bug.
;;   A core file can be produced at this time.  Core files may or may not
;;   be useful in diagnosing an error of this type and are often not useful.
;;   See doc/introduction.htm#core-dumps-2 for more details.
;;
;; However, since the goal of https://github.com/cl-test-grid/cl-test-grid/issues/41
;; was to compare ASDF 3.3.5.7 with ASDF branch asdf-uiop-docstring,
;; and the crashes happen for both of them, let's ignore. The SMP versions
;; do not have this issue.

(defparameter *results-without-acl-nonsmp*
  (subset *all-results*
          (lambda (r) (not (member (lisp r)
                                   '("acl-10.1-linux-x86" "acl-10.1m-linux-x86")
                                   :test #'string=)))))

(print-new-failures "asdf/asdf-diff-75.html"
                    ;; *all-results*
                    *results-without-acl-nonsmp*
                    "quicklisp 2021-12-30"
                    "quicklisp 2021-12-30 + asdf.3.3.5.7")

(print-new-failures "asdf/asdf-diff-76.html"
                    ;; *all-results*
                    *results-without-acl-nonsmp*
                    "quicklisp 2021-12-30 + asdf.3.3.5.7"
                    "quicklisp 2021-12-30 + asdf-uiop-docstring.6ba0224")

(print-new-failures "asdf/asdf-diff-77.html" 
                    ;; *all-results*
                    *results-without-acl-nonsmp*
                    "quicklisp 2021-12-30"
                    "quicklisp 2021-12-30 + asdf-uiop-docstring.6ba0224")



(format t "~%~{~A~%~}"
        (sort (alexandria:flatten
               (tg-rep::distinct (subset *all-results*
                                         (lambda (r) (string= "quicklisp 2021-12-30 + asdf-uiop-docstring.6ba0224"
                                                              (lib-world r))))
                                 '(lisp)))
              #'string<))

(ql:quickload :cl-store)
(time
 (cl-store:store *db* "/home/anton/prj/cl-test-grid/work/db-cl-store.data"))
(time
 (cl-store:store (tg-storage:data *rm*) "/home/anton/prj/cl-test-grid/work/db-cl-store.data"))
(time (tg-storage:sync))

(defparameter *rt* (tg-storage:make-replica "vector-test" #P"/home/anton/prj/cl-test-grid/work/db-vector-test.lisp" ))
(and (setf (sptm:vdata *rt*)  (sptm:vdata *rm*))
     nil)

(tg-storage:sync *rt*)
(sptm:save-local-snapshot *rt*)
(time
 (sptm::read-local-snapshot *rt*))

(time
 (sptm::read-local-snapshot *rm*))



;; todo: delete these as I had iolib and something else in ~/quicklisp-asdf3/local-storage/

  ("quicklisp 2017-08-30 + asdf.3.2.1-14-g5eca374"
   "cmu-snapshot-2016-12__21b_unicode_-linux-x86")
  ("quicklisp 2017-08-30 + asdf.3.2.1-14-g5eca374"
   "ccl-1.11-r16635-f96-linux-x86")
  ("quicklisp 2017-08-30 + asdf.3.2.1-14-g5eca374"
   "sbcl-1.3.21-linux-x86")
  

;; "quicklisp 2017-02-27 + asdf.3.3.0-rc1.a6bd7c6 + iolib.pr42.66ea927 + cl-protobufs.57c8079"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(and
 (sptm:repli-exec *r* 'tg-data:update-run-descr '((:lib-world "quicklisp 2018-02-28 + asdf.syntax-ctrl.8dc0778")
                                                  (:lib-world "quicklisp 2018-03-28 + asdf.syntax-ctrl.8dc0778")))
 nil)


(sptm:save-local-snapshot *r*)

(length (subset *all-results*
                (lambda (r) (string= "quicklisp 2016-12-08 + asdf.3.1.7.43" (lib-world r)))))


(format t "~%~{~S~%~}"
        (sort (tg-rep::distinct *all-results* '(lib-world lisp tg-rep::contact-email))
              (lambda (l1 l2)
                (tg-utils::list< (list #'string< #'string< #'string<)
                                 l1 l2))))

