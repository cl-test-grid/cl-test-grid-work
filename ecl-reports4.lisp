(ql:quickload :test-grid-storage)
(ql:quickload :test-grid-reporting)

(defpackage #:ecl-reports
  (:use :cl)
  (:import-from #:test-grid-reporting
                #:list-results
                #:subset                
                #:print-compiler-diff
                #:print-pivot
                #:lisp
                #:libname
                #:lib-world
                #:results-cell-printer))
(in-package #:ecl-reports)

;; Fetch results data. May take several minutes,
;; especially when you do it first time. Observer progress log
;; in *standard-output*.
(defparameter *rm* (tg-storage:sync (tg-storage:get-replica "main")))
(defparameter *r* (tg-storage:sync (tg-storage:get-replica "ecl")))

(defparameter *all-results* (list-results (tg-data:join-dbs (tg-storage:data *r*)
                                                            (tg-storage:data *rm*))))

;; Generate some. The resulting HTML file will be placed into
;; <test-grid-repo>/reports-generated/

(print-compiler-diff "ecl/ecl-diff-2-bytecode.html"
                     *all-results*
                     "quicklisp 2013-10-03"
                     "ecl-13.5.1-unknown-linux-i686-bytecode"
                     "ecl-13.5.1-c8a4d98d-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-2-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2013-10-03"
                     "ecl-13.5.1-unknown-linux-i686-lisp-to-c"
                     "ecl-13.5.1-c8a4d98d-linux-i686-lisp-to-c")

(print-compiler-diff "ecl/ecl-diff-3-bytecode.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-13.5.1-unknown-linux-i686-bytecode"
                     "ecl-15.2.21-ee989b97-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-3-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-13.5.1-unknown-linux-i686-lisp-to-c"
                     "ecl-15.2.21-ee989b97-linux-i686-lisp-to-c")

(print-compiler-diff "ecl/ecl-diff-4-bytecode.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-15.2.21-ee989b97-linux-i686-bytecode"
                     "ecl-15.3.7-9db31fd7-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-4-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-15.2.21-ee989b97-linux-i686-lisp-to-c"
                     "ecl-15.3.7-9db31fd7-linux-i686-lisp-to-c")

(print-compiler-diff "ecl/ecl-diff-5-bytecode.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-13.5.1-unknown-linux-i686-bytecode"
                     "ecl-15.3.7-9db31fd7-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-5-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-13.5.1-unknown-linux-i686-lisp-to-c"
                     "ecl-15.3.7-9db31fd7-linux-i686-lisp-to-c")

(print-compiler-diff "ecl/ecl-diff-6-bytecode.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-15.2.21-ee989b97-linux-i686-bytecode"
                     "ecl-15.3.7-rc1-5d9e58f6-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-6-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-15.2.21-ee989b97-linux-i686-lisp-to-c"
                     "ecl-15.3.7-rc1-5d9e58f6-linux-i686-lisp-to-c")

(print-compiler-diff "ecl/ecl-diff-7-bytecode.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-13.5.1-unknown-linux-i686-bytecode"
                     "ecl-15.3.7-rc1-5d9e58f6-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-7-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-13.5.1-unknown-linux-i686-lisp-to-c"
                     "ecl-15.3.7-rc1-5d9e58f6-linux-i686-lisp-to-c")

(print-compiler-diff "ecl/ecl-diff-8-bytecode.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-15.3.7-9db31fd7-linux-i686-bytecode"
                     "ecl-15.3.7-rc1-5d9e58f6-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-8-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2015-01-13"
                     "ecl-15.3.7-9db31fd7-linux-i686-lisp-to-c"
                     "ecl-15.3.7-rc1-5d9e58f6-linux-i686-lisp-to-c")

(print-compiler-diff "ecl/ecl-diff-9-bytecode.html"
                     *all-results*
                     "quicklisp 2015-03-02"
                     "ecl-15.3.7-rc1-5d9e58f6-linux-i686-bytecode"
                     "ecl-15.3.7-a014bd2c-linux-i686-bytecode")

(print-compiler-diff "ecl/ecl-diff-10-lisp-to-c.html"
                     *all-results*
                     "quicklisp 2015-05-05"
                     "ecl-15.3.7-827d3035-linux-x64-lisp-to-c"
                     "ecl-15.3.7-a014bd2c-linux-x64-lisp-to-c")

(tg-rep::print-pivot "ecl/ecl-results-10-lisp-to-c.html"
                     (subset *all-results* (lambda (r)
                                             (and (string= (lib-world r) "quicklisp 2015-05-05")
                                                  (member (lisp r)
                                                          '("ecl-15.3.7-827d3035-linux-x64-lisp-to-c"
                                                            "ecl-15.3.7-a014bd2c-linux-x64-lisp-to-c")
                                                          :test #'string=)
                                                  (member (libname r)
                                                          '(:asdf-contrib :asdf-encodings :buffalo :checkl
                                                            :cl-store :clack-errors :cxml-stp :hu.dwim.computed-class
                                                            :iterate :lambda-reader :weblocks)))))
                     :rows '((libname string<))
                     :cols `((lib-world string<)
                             (lisp ,(tg-utils::ordering-comparator (list "ecl-15.3.7-827d3035-linux-x64-lisp-to-c"
                                                                         "ecl-15.3.7-a014bd2c-linux-x64-lisp-to-c")
                                                                   #'string=)))
                     :cell-printer #'results-cell-printer)


