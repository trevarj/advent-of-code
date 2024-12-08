;;; day7.el --- AoC 2024 Day 7                       -*- lexical-binding: t; -*-

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
(require 'dash)
(require 's)
(require 'memoize)

(defvar sample-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defun concat-nums (a b)
  (string-to-number
   (s-concat (number-to-string a) (number-to-string b))))

(memoize 'traverse)
(defun traverse (ops acc nums)
  (if (not nums)
      acc
    (let ((next (car nums))
          (rest (cdr nums)))
      (-flatten (--map
                 (traverse ops (funcall it acc next) rest)
                 ops)))))

(defun solve-equation (ops target nums)
  (->> (traverse ops (car nums) (cdr nums))
       (--find (eq target it))))

(defun solve (input ops)
  (->> (--map (solve-equation ops (car it) (cdr it)) input)
       (-flatten)
       (-sum)))

(defun solve-1 (input)
  (solve input '(+ *)))

(defun solve-2 (input)
  (solve input '(+ * concat-nums)))

(solve-1 (sample-input-lines-ints)) ; 3749
(solve-1 (input-lines-ints))

(solve-2 (sample-input-lines-ints)) ; 11387
(solve-2 (input-lines-ints)) ; long time!

(provide 'day7)
;;; day7.el ends here
