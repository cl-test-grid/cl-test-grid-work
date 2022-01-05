;; Executed at 2021-01-05.

(load "/home/anton/prj/cl-test-grid/work/replicas.lisp")
(ql:quickload :test-grid-storage)
(ql:quickload :test-grid-reporting)

(defpackage #:db-maintenance
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
(in-package #:db-maintenance)

(defparameter *rm* (tg-replicas:get "main"))
(tg-storage:sync *rm*)

;;; 0. contributors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tg-rep::print-contributors (tg-storage:data *rm*) "quicklisp 2021-12-09")

;;; 1. archive results ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plist-equal (plist-1 plist-2)
  (and (= (length plist-1) (length plist-2))
       (alexandria:doplist (key val plist-1 t)
         (when (not (equal val (getf plist-2 key)))
           (return nil)))))

(assert (plist-equal '(:a 1 :b "2")
                     '(:a 1 :b "2")))

(assert (not (plist-equal '(:a 1 :b "2" :c '3)
                          '(:a 1 :b "2"))))

(assert (not (plist-equal '(:a 1)
                          '(:a 1 :b "2"))))


(assert (plist-equal '(:LISP "abcl-1.2.1-fasl42-linux-x86"
                       :LIB-WORLD "quicklisp 2019-02-02"
                       :TIME 3759462153
                       :RUN-DURATION 54071
                       :CONTACT-EMAIL "avodonosov@yandex.ru")
                     '(:lisp "abcl-1.2.1-fasl42-linux-x86"
                       :lib-world "quicklisp 2019-02-02"
                       :time 3759462153
                       :run-duration 54071
                       :contact-email "avodonosov@yandex.ru")))

(defun test-run-equal (run-1 run-2)
  (plist-equal (tg-data::run-descr run-1)
               (tg-data::run-descr run-2)))

(defun sort-runs (test-runs)
  "Destructive, same as SORT."
  (sort test-runs
        (tg-utils::plist-comparator :lib-world #'string> :time #'>)
        :key #'tg-data::run-descr))

(defun archive-file (file-name)
  (merge-pathnames file-name (tg-data::standard-archive-dir)))

#|

# Shell scripts to extract all archive test run descriptions.
# It is much faster than reading the whole archive - lisp reader
# implementations are quite slow.

cd ~/prj/cl-test-grid/cl-test-grid-results/

# all lines with test run descriptions
grep ':descr' db*.lisp  > all-descrs

# Remove the file names, and the preceeding '(:descr' and possible ':runs (`,
# then trim the leading and trailing spaces - leave only the description plists
cat all-descrs | sed 's/db[[:digit:]]*\.lisp\://;s/:runs (//;s/(:descr//' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//' > all-descrs-stripped

|#

(defparameter *arch-run-descrs*
  (with-open-file (in "~/prj/cl-test-grid/cl-test-grid-results/all-descrs-stripped"
                      :direction :input
                      :element-type 'character
                      :external-format tg-utils:*utf-8-external-format*)
    (tg-utils::with-safe-io-syntax
      (read-delimited-list #\)
                           (make-concatenated-stream in
                                                     (make-string-input-stream ")"))
                           t))))

(defparameter *new-runs*
  (sort-runs
   (copy-list
    (set-difference (getf (tg-storage:data *rm*)
                          :runs)
                    *arch-run-descrs*
                    :test (lambda (run arch-run-descr)
                            (plist-equal (tg-data::run-descr run)
                                         arch-run-descr))))))


(defparameter *arch-subdb26* (tg-data:read-db (archive-file "db26.lisp")))

(defparameter *new-and-last-arch-runs*
  (append (getf *arch-subdb26* :runs) *new-runs*))

(length *arch-run-descrs*)
(length (getf (tg-storage:data *rm*) :runs))
(length *new-runs*)
(length *new-and-last-arch-runs*)
(length (getf *arch-subdb26* :runs))

(tg-data::run-descr (car (last *new-runs*)))
(tg-data::run-descr (first *new-runs*))

(format t "~{~S~%~}" (mapcar 'tg-data::run-descr *new-runs*))

;; Manually chose the number of runs to save in each
;; file so that the files are under 100 MB.
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 430))
                  (archive-file "db26.lisp"))
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 375 430))
                  (archive-file "db27.lisp"))
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 324 375))
                  (archive-file "db28.lisp"))
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 273 324))
                  (archive-file "db29.lisp"))
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 184 273))
                  (archive-file "db30.lisp"))
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 95 184))
                  (archive-file "db31.lisp"))
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 12 95))
                  (archive-file "db32.lisp"))
(tg-data::save-db (tg-data:make-db (subseq *new-and-last-arch-runs* 0 12))
                  (archive-file "db33.lisp"))

;;; 2. remove elder than the last 3 quicklisps from the "main" storage  ;;;;;;;;

(sort (alexandria:flatten
       (tg-rep::distinct (getf (tg-storage:data *rm*) :runs)
                         (list (alexandria:compose (tg-utils::plist-getter :lib-world)
                                                   (tg-utils::plist-getter :descr)))))
      #'string>)

(let ((to-delete '(;"quicklisp 2021-12-09"
                   ;"quicklisp 2021-10-21"
                   ;"quicklisp 2021-08-07"
                   "quicklisp 2021-06-30"
                   "quicklisp 2021-05-31"
                   "quicklisp 2021-02-28"
                   "quicklisp 2020-12-20"
                   "quicklisp 2020-10-16"
                   "quicklisp 2020-09-25"
                   "quicklisp 2020-07-15"
                   "quicklisp 2020-06-10"
                   "quicklisp 2020-02-18"
                   "quicklisp 2019-12-27"
                   "quicklisp 2019-11-30"
                   "quicklisp 2019-10-08"
                   "quicklisp 2019-10-07"
                   "quicklisp 2019-08-13"
                   "quicklisp 2019-07-11"
                   "quicklisp 2019-05-21"
                   "quicklisp 2019-03-07"
                   "quicklisp 2019-02-02")))
  (dolist (lib-world to-delete)
    (sptm::repli-exec *rm* 'tg-data:remove-test-runs (list :lib-world lib-world))))

(sptm::save-local-snapshot *rm*)

(getf (first (getf (tg-storage:data *rm*) :runs)) :descr)
(getf (car (last (getf (tg-storage:data *rm*) :runs))) :descr)
(length (getf (tg-storage:data *rm*) :runs))

(sptm::save-snapshot (sptm::transaction-log *rm*)
                     (sptm::vdata *rm*))
(sptm::snapshot-version (sptm::transaction-log *rm*))

(sptm::version *rm*)
