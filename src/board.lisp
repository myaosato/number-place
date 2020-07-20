(defpackage number-place/board
  (:use :cl)
  (:export :board
           :make-board
           :is-empty
           :find-empty
           :fullfilled-p
           :set-number
           :unset-number
           :can-set-number
           :read-to-board
           :write-from-board))
(in-package :number-place/board)

(defstruct board
  (field (make-array (list 9 9) :element-type '(integer 0 9) :initial-element 0)
         :type (simple-array (unsigned-byte 4) (9 9))))

(defun get-number (board x y)
  (aref (board-field board) (1- x) (1- y)))

(defun (setf get-number) (number board x y)
  (setf (aref (board-field board) (1- x) (1- y)) number))

(defun is-empty (board x y)
  (= (get-number board x y) 0))

(defun find-empty (board)
  (loop :for x :from 1 :to 9
        :do (loop :for y :from 1 :to 9
                  :if (is-empty board x y)
                  :do (return-from find-empty (cons x y))))
  nil)

(defun fullfilled-p (board)
  (not (find-empty board)))

(defun set-number (board x y v)
  (setf (get-number board x y) v))

(defun unset-number (board x y)
  (set-number board x y 0))

(defun can-set-number (board x y v)
  (loop :for _ :from 1 :to 9
        :if (and (/= _ x) (= (get-number board _ y) v))
        :do (return-from can-set-number nil)
        :if (and (/= y _) (= (get-number board x _) v))
        :do (return-from can-set-number nil))
  (let ((bx (* (floor (1- x) 3) 3))
        (by (* (floor (1- y) 3) 3)))
    (loop :for a :from 1 :to 3
          :do (loop :for b :from 1 :to 3
                    :for xx := (+ bx a)
                    :for yy := (+ by b)
                    :if (and (or (/= xx x) (/= yy y)) (= (get-number board xx yy) v))
                    :do (return-from can-set-number nil)))
    t))

;; TODO
(defun read-to-board (board in)
  (loop :for y :from 1 :to 9
        :for line := (read-line in)
        :do (with-input-from-string (cs line)
              (loop :for x :from 1 :to 9
                    :for c := (read-char cs)
                    :do (set-number board x y (if (char= c #\*) 0 (- (char-code c) 48)))))))

(defun write-from-board (board out)
  (loop :for y :from 1 :to 9
        :do (loop :for x :from 1 :to 9
                  :for num := (get-number board x y)
                  :do (format out "~A" (if (= num 0) #\* num)))
        :do (format out "~%")))
