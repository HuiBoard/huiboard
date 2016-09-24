(in-package #:huim)

;; Five fingered from Rosetta Code, and changed the name.

;; see https://rosettacode.org/wiki/Rosetta_Code:Copyrights

;; Below is released by Rosetta Code under the GNU Free Documentation License, version 1.2. Rosetta code is not affiliated with us, and does not endorse us in any way.


(defun lcs-list (list-1 list-2 &key (test #'eql))
  "Find the longest common subsequence of LIST-1 and LIST-2 using TEST."
  (cond
   ((null list-1) nil)
   ((null list-2) nil)
   ((funcall test (first list-1) (first list-2))
    (cons (first list-1) (lcs-list (rest list-1) (rest list-2) :test test)))
   (t (let ((lcs-1 (lcs-list list-1 (rest list-2) :test test))
            (lcs-2 (lcs-list (rest list-1) list-2 :test test)))
        (if (> (length lcs-1) (length lcs-2))
            lcs-1
          lcs-2)))))
