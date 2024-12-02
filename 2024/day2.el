;;; day2.el --- AoC Day 2                            -*- lexical-binding: t; -*-

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

(defvar sample-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defun parse (lines)
  (->> lines
       (--map (-partition-in-steps 2 1 it))))

(defun within-range-p (a b)
  (< 0 (abs (- a b)) 4))

(defun is-safe-p (lst)
  (-some--> lst
    (-partition-in-steps 2 1 it)
    (and (-all-p (-applify #'within-range-p) it)
         (or
          (-all-p (-applify #'>) it)
          (-all-p (-applify #'<) it)))))

(defun count-t (lst)
  (--count it lst))

(defun solve-1 (input)
  (count-t (-map #'is-safe-p input)))

(defun solve-2 (input)
  (count-t
   (--map
    (-let ((min-len (1- (length it))))
      (->> it
           (-powerset)
           (--filter (>= (length it) min-len))
           (-any-p #'is-safe-p)
           ))
    input)))

(solve-1 (sample-input-lines-ints)) ; 2
(solve-1 (input-lines-ints))

(solve-2 (sample-input-lines-ints)) ; 4
(solve-2 (input-lines-ints))

(provide 'day2)
;;; day2.el ends here
