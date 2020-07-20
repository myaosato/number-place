(defpackage number-place/number-place
  (:use :cl :number-place/board)
  (:export))
(in-package :number-place/number-place)

(defun dfs (board)
  (let* ((xy (find-empty board))
         (x (car xy))
         (y (cdr xy)))
    (cond ((null xy) ; not found => fullfiled
           (return-from dfs board))
          (t
           (loop :for num :from 1 :to 9
                 :when (can-set-number board x y num)
                 :do (progn
                       (set-number board x y num)
                       (when (dfs board)
                         (return-from dfs board))
                       (unset-number board x y)))
           nil))))

(defun main ()
  (let ((board (make-board)))
    (read-to-board board *standard-input*)
    (dfs board)
    (write-from-board board *standard-output*)))
