(load "~/prj/cl-test-grid/work/replicas.lisp")
(ql:quickload :test-grid-reporting)

(defpackage #:abcl-reports
  (:use :cl)
  (:import-from #:test-grid-reporting
                #:list-results
                #:subset                
                #:print-compiler-diff
                #:print-pivot
                #:lisp
                #:libname
                #:lib-world
                #:contact-email
                #:results-cell-printer))
(in-package #:abcl-reports)

;;; Retrieving test results from online storage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *r* (tg-replicas:get "abcl"))
(defparameter *rm* (tg-replicas:get "main"))

(tg-storage:sync *r*)
(tg-storage:sync *rm*)

(defparameter *db* (tg-data:join-dbs (tg-storage:data *r*)
                                     (tg-storage:data *rm*)))
;; Now *DB* stores a data structure documented here:
;; https://github.com/cl-test-grid/cl-test-grid/tree/master/data

;;; Generating reports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The API is explained here:
;;; https://github.com/cl-test-grid/cl-test-grid/tree/master/reporting

(defparameter *all-results* (list-results *db*))

(print-compiler-diff "abcl/abcl-diff3.html"
                     *all-results*
                     "quicklisp 2013-03-12"
                     "abcl-1.1.1-fasl39-linux-x86"
                     ;"abcl-1.2.0-dev-svn-14300-fasl39-linux-x86"
                     "abcl-1.2.0-dev-svn-14436-fasl40-linux-x86"
                     ;"quicklisp 2013-03-12"
                     )

(print-compiler-diff "abcl/abcl-diff4.html"
                     *all-results*
                     "quicklisp 2013-02-17"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "quicklisp 2013-03-12"
                     )

(print-compiler-diff "abcl/abcl-diff5.html"
                     *all-results*
                     "quicklisp 2013-03-12"
                     "abcl-1.2.0-dev-svn-14436-fasl40-linux-x86"
                     "abcl-1.2.0-dev-svn-14445-fasl40-linux-x86")

(print-compiler-diff "abcl/abcl-diff6.html"
                     *all-results*
                     "quicklisp 2013-03-12"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.2.0-dev-svn-14445-fasl40-linux-x86")

(print-compiler-diff "abcl/abcl-diff7.html"
                     *all-results*
                     "quicklisp 2013-03-12"
                     "abcl-1.2.0-dev-svn-14445-fasl40-linux-x86"
                     "abcl-1.2.0-dev-svn-14451-fasl40-linux-x86")

(print-compiler-diff "abcl/abcl-diff8.html"
                     *all-results*
                     "quicklisp 2013-03-12"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.2.0-dev-svn-14451-fasl40-linux-x86")


(print-compiler-diff "abcl/abcl-diff8.html"
                     *all-results*
                     "quicklisp 2013-03-12"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.2.0-dev-svn-14451-fasl40-linux-x86")

(print-compiler-diff "abcl/abcl-diff9.html"
                     *all-results*
                     "quicklisp 2013-03-12"
                     "abcl-1.2.0-dev-svn-14451-fasl40-linux-x86"
                     "abcl-1.2.0-dev-svn-14452-fasl40-linux-x86")

(print-compiler-diff "abcl/abcl-diff10.html"
                     *all-results*
                     "quicklisp 2013-04-20"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.2.0-fasl42-linux-x86")

(print-compiler-diff "abcl/abcl-diff11.html"
                     *all-results*
                     "quicklisp 2013-04-20"
                     "abcl-1.2.0-fasl42-linux-x86"
                     "abcl-1.2.0-dev-svn-14528-fasl42-linux-x86")

(print-compiler-diff "abcl/abcl-diff12.html"
                     *all-results*
                     "quicklisp 2013-04-20"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.2.0-dev-svn-14529-fasl42-linux-x86")

(print-compiler-diff "abcl/abcl-diff13.html"
                     *all-results*
                     "quicklisp 2013-04-20"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.3.0-dev-svn-14545-fasl42-linux-x86")

(print-compiler-diff "abcl/abcl-diff14.html"
                     *all-results*
                     "quicklisp 2013-06-15"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.3.0-dev-svn-14556-fasl42-linux-x86")

(print-compiler-diff "abcl/abcl-diff15.html"
                     *all-results*
                     "quicklisp 2013-06-15"
                     "abcl-1.1.1-fasl39-linux-x86"
                     "abcl-1.3.0-dev-svn-14558-fasl42-linux-x86")

(print-compiler-diff "abcl/abcl-diff16.html"
                     *all-results*
                     "quicklisp 2013-08-13"
                     "abcl-1.2.0-fasl42-linux-x86"
                     "abcl-1.3.0-dev-svn-14580-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff17.html"
                     *all-results*
                     "quicklisp 2014-03-17"
                     "abcl-1.2.0-fasl42-linux-x86"
                     "abcl-1.3.0-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff18.html"
                     *all-results*
                     "quicklisp 2014-03-17"
                     "abcl-1.2.1-fasl42-linux-x86"
                     "abcl-1.3.0-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff19.html"
                     *all-results*
                     "quicklisp 2014-03-17"
                     "abcl-1.2.1-fasl42-linux-x86"
                     "abcl-1.4.0-dev-svn-14662-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff20.html"
                     *all-results*
                     "quicklisp 2014-03-17"
                     "abcl-1.2.1-fasl42-linux-x86"
                     "abcl-1.3.0-svn-14683-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff21.html"
                     *all-results*
                     "quicklisp 2014-03-17"
                     "abcl-1.2.1-fasl42-linux-x86"
                     "abcl-1.3.0-svn-14688-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff22.html"
                     *all-results*
                     "quicklisp 2014-03-17"
                     "abcl-1.3.0-fasl42-linux-x86"
                     "abcl-1.3.0-svn-14688-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff23.html"
                     *all-results*
                     "quicklisp 2014-04-25"
                     "abcl-1.2.1-fasl42-linux-x86"
                     "abcl-1.3.1-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff24.html"
                     *all-results*
                     "quicklisp 2014-04-25"
                     "abcl-1.3.0-fasl42-linux-x86"
                     "abcl-1.3.1-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff25.html"
                     *all-results*
                     "quicklisp 2015-03-02"
                     "abcl-1.3.1-fasl42-linux-x86"
                     "abcl-1.4.0-dev-svn-14755-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff26.html"
                     *all-results*
                     "quicklisp 2015-04-07"
                     "abcl-1.3.1-fasl42-linux-x86"
                     "abcl-1.4.0-dev-svn-14756-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff27.html"
                     *all-results*
                     "quicklisp 2015-04-07"
                     "abcl-1.3.1-fasl42-linux-x86"
                     "abcl-1.3.2-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff28.html"
                     *all-results*
                     "quicklisp 2016-09-29"
                     "abcl-1.3.2-fasl42-linux-x86"
                     "abcl-1.4.0-fasl42-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff29.html"
                     *all-results*
                     "quicklisp 2017-06-30"
                     "abcl-1.4.0-fasl42-linux-x86"
                     "abcl-1.5.0-fasl43-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff30.html"
                     *all-results*
                     "quicklisp 2019-10-08"
                     "abcl-1.5.0-fasl43-linux-x86"
                     "abcl-1.6.0-fasl43-linux-x86"
                     )

(print-compiler-diff "abcl/abcl-diff31.html"
                     *all-results*
                     "quicklisp 2019-10-08"
                     "abcl-1.4.0-fasl42-linux-x86"
                     "abcl-1.6.0-fasl43-linux-x86"
                     )

;; On ABCL 1.5.0 a log of tests :CRASH which :FAILed on ABCL 1.4.0
;; https://mailman.common-lisp.net/pipermail/armedbear-devel/2017-July/003867.html
;;
;; Let's filter out all such cases to see what else is different

(let ((results-by-test
       ;; A hashtable from (<lisp> <libname> <test>) to <result>
       ;; for this quicklisp and two ABCL versions.
       ;; Example entry:
       ;; ("abcl-1.4.0-fasl42-linux-x86" :COMMON-DOC (:LOAD "common-doc-graphviz")) -> (#<RESULT ... #x30203855098D>)
       (tg-rep::group-by (subset *all-results*
                                 (lambda (r)
                                   (and (string= (lib-world r) "quicklisp 2017-06-30")
                                        (member (lisp r) '("abcl-1.4.0-fasl42-linux-x86"
                                                           "abcl-1.5.0-fasl43-linux-x86")
                                                :test #'string=))))
                         '(lisp libname tg-rep::result-spec-test))))
  (labels ((outcome (libname test lisp)
             (let ((result (first (gethash `(,lisp ,libname ,test)
                                           results-by-test))))
               (and result
                    (tg-rep::result-spec-outcome result))))
           (bad-test-p (libname test)
             (and (eq :fail (outcome libname test "abcl-1.4.0-fasl42-linux-x86"))
                  (eq :crash (outcome libname test "abcl-1.5.0-fasl43-linux-x86")))))
    (print-compiler-diff "abcl/abcl-diff30.html"
                         (subset *all-results*
                                 (lambda (r)
                                   (not (bad-test-p (libname r)
                                                    (tg-rep::result-spec-test r)))))
                         "quicklisp 2017-06-30"
                         "abcl-1.4.0-fasl42-linux-x86"
                         "abcl-1.5.0-fasl43-linux-x86")))

(

 ("abcl-1.1.1-fasl39-linux-x86" "quicklisp 2013-07-22")
 ("abcl-1.2.0-fasl42-linux-x86" "quicklisp 2013-07-22")
 ("abcl-1.3.0-dev-svn-14558-fasl42-linux-x86" "quicklisp 2013-07-22")
 ("abcl-1.3.0-dev-svn-14580-fasl42-linux-x86" "quicklisp 2013-07-22")
 
 ("abcl-1.1.1-fasl39-linux-x86" "quicklisp 2013-08-13")
 ("abcl-1.2.0-fasl42-linux-x86" "quicklisp 2013-08-13")
 ("abcl-1.3.0-dev-svn-14580-fasl42-linux-x86" "quicklisp 2013-08-13")

)

(format t "~{~A~%~}"
        (sort (tg-rep::distinct (subset *all-results* 
                                        (lambda (r)
                                          (and (search "abcl" (tg-rep::lisp r))
                                               (string= "avodonosov@yandex.ru" (tg-rep::contact-email r))
                                               (member (tg-rep::lib-world r) '("quicklisp 2013-08-13" "quicklisp 2013-07-22")
                                                       :test #'string=)
                                               (not (tg-rep::load-result r))
                                               (> (tg-rep::duration r) 100))))
                                (list 'tg-rep::lisp 'tg-rep::lib-world (lambda (r) (float (/ (tg-rep::test-run-duration r) 60 60)))))
              (lambda (rowa rowb)
                (tg-utils::list< '(string< string< <) rowa rowb))))

(format t "~{~A~%~}"
        (mapcar (lambda (r)
                  (list (tg-rep::lisp r)
                        (tg-rep::libname r)
                        (float (/ (tg-rep::duration r) 60 60))))
                (subset *all-results*
                        (lambda (r)
                          (and (search "abcl" (tg-rep::lisp r))
                               (string= "avodonosov@yandex.ru" (tg-rep::contact-email r))
                               (member (tg-rep::lib-world r) '("quicklisp 2013-08-13" "quicklisp 2013-07-22")
                                       :test #'string=)
                               (not (tg-rep::load-result r))
                               (and (> (tg-rep::duration r) 200)))))))

(("abcl-1.1.1-fasl39-linux-x86" "quicklisp 2013-07-22" 84076)
 ("abcl-1.1.1-fasl39-linux-x86" "quicklisp 2013-08-13" 81150)
 ("abcl-1.2.0-fasl42-linux-x86" "quicklisp 2013-07-22" 67463)
 ("abcl-1.2.0-fasl42-linux-x86" "quicklisp 2013-08-13" 64447)
 ("abcl-1.3.0-dev-svn-14558-fasl42-linux-x86" "quicklisp 2013-07-22" 65047)
 ("abcl-1.3.0-dev-svn-14580-fasl42-linux-x86" "quicklisp 2013-07-22" 64010)
 ("abcl-1.3.0-dev-svn-14580-fasl42-linux-x86" "quicklisp 2013-08-13" 64418))

;; load failures report
(tg-rep::print-load-failures "abcl/abcl-1-2-0-load-failures.html"
                             *all-results*
                             "abcl-1.2.0-dev-svn-14451-fasl40-linux-x86"
                             "quicklisp 2013-03-12")

(tg-rep::print-load-failures "abcl/abcl-1-2-1-load-failures.html"
                             *all-results*
                             "abcl-1.2.0-dev-svn-14529-fasl42-linux-x86"
                             "quicklisp 2013-04-20")


(handler-case (ql:quickload :cells) (serious-condition (e) (format nil "!!! Serious condition occured: ~A !!!" (type-of e))))

(handler-bind ((serious-condition (lambda (e) (format nil "!!! Serious condition occured: ~A !!!" (type-of e))))))

java -jar ~/lisps/abcl-1.2.0/dist/abcl.jar --noinit --nosystem --batch --load ~/cl-test-grid/work-dir/agent/quicklisp/setup.lisp --eval '(handler-case (ql:quickload :cells) (serious-condition (e) (format t "!!! Serious condition occured: ~A !!!~%" (type-of e))))'

java -jar ~/lisps/abcl/dist/abcl.jar --noinit --nosystem --batch --load ~/cl-test-grid/work-dir/agent/quicklisp/setup.lisp --eval '(handler-case (ql:quickload :cells) (serious-condition (e) (format t "!!! Serious condition occured: ~A !!!~%" (type-of e))))'


