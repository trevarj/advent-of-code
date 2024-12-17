;;; day17.el --- AoC 2024 Day 17                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'aoc-2024)

(defvar sample-input "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(defun parse (input)
  (-let ((parsed
          (--map
           (--> it
                (s-split ": " it)
                (cadr it)
                (funcall
                 (if (s-contains-p "," it)
                     (-compose
                      (-partial #'-map #'string-to-number)
                      #'s-split-words)
                   #'string-to-number)
                 it))
           input)))
    (list (seq-into (seq-take parsed 3) 'vector)
          (seq-into (seq-elt parsed 3) 'vector))))

(defun interpret (regs op operand)
  (pcase op
    (0 (setf (aref regs 0)
             (/ (aref regs 0) (expt 2 (combo-operand regs operand)))))
    (1 (setf (aref regs 1)
             (logxor (aref regs 1) operand)))
    (2 (setf (aref regs 1)
             (mod (combo-operand regs operand) 8)))
    ((and 3 (guard (> (aref regs 0) 0)))
     (setf (aref regs 3) (- operand 2)))
    (4 (setf (aref regs 1)
             (logxor (aref regs 1) (aref regs 2))))
    (5 (setf (aref regs 4) (mod (combo-operand regs operand) 8)))
    (6 (setf (aref regs 1)
             (/ (aref regs 0) (expt 2 (combo-operand regs operand)))))
    (7 (setf (aref regs 2)
             (/ (aref regs 0) (expt 2 (combo-operand regs operand)))))))

(defun combo-operand (regs operand)
  (pcase operand
    ((pred (<= 0 _ 3)) operand)
    (4 (aref regs 0))
    (5 (aref regs 1))
    (6 (aref regs 2))))

(defun run-program (regs program)
  (named-let run ((regs (vconcat regs [0 nil])) ; a, b, c, ip, out
                  (out '()))
    (if-let* ((ip (aref regs 3))
              (_ (< ip (length program))))
        (progn
          (interpret regs (aref program ip) (aref program (1+ ip)))
          (when-let ((val (aref regs 4))) (push val out))
          (setf (aref regs 4) nil) ; clear out register
          (setf (aref regs 3) (+ 2 (aref regs 3))) ; inc IP
          (run regs out))
      (message "regs: %s" regs)
      (nreverse (s-join "," (-map #'number-to-string out))))))

(defun solve (input)
  (-let (((regs program) (parse input)))
    (run-program regs program)))

(defun solve-1 (input)
  (solve input))

(defun solve-2 (input))

(solve-1 (read-sample-input-lines)) ; 4,6,3,5,6,3,5,2,1,0
(run-program [10 0 0] [5 0 5 1 5 4]) ; 0,1,2
(run-program [2024 0 0] [0 1 5 4 3 0]) ; 4,2,5,6,7,7,7,7,3,1,0
(run-program [0 0 9] [2 6])  ; reg B = 1
(run-program [0 29 0] [1 7]) ; reg B = 26
(solve-1 (read-input-lines))

(provide 'day17)
;;; day17.el ends here
