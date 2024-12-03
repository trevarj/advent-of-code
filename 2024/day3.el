;;; day3.el --- AoC Day 3                            -*- lexical-binding: t; -*-

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

(defvar sample-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defvar sample-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defvar regex-1 "mul(\\([1-9][0-9]\\{0,2\\}\\),\\([1-9][0-9]\\{0,2\\}\\))")
(defvar regex-2 "mul(\\([1-9][0-9]\\{0,2\\}\\),\\([1-9][0-9]\\{0,2\\}\\))\\|don't\\|do")

(defun extract-factors (lst)
  (-map (-compose
         (-partial #'-map #'string-to-number)
         #'cdr)
        lst))

(defun parse (lines)
  (->> (s-join "" lines)
       (s-match-strings-all regex-1)
       (extract-factors)))

(defun parse-2 (lines)
  (->> (s-join "" lines)
       (s-match-strings-all regex-2)
       (-partition-before-pred (lambda (it) (or (equal it '("don't"))
                                                (equal it '("do")))))
       (--filter (not (equal (car it) '("don't"))))
       (--map-when (equal (car it) '("do")) (cdr it))
       (-flatten-n 1)
       (extract-factors)))

(parse-2 (read-sample-input-lines :override sample-input-2))

(defun solve (lst)
  (->> lst
       (-map (-applify #'*))
       (-sum)))

(defun solve-1 (input)
  (->> (parse input)
       (solve)))

(defun solve-2 (input)
  (->> (parse-2 input)
       (solve)))

(solve-1 (read-sample-input-lines)) ; 161
(solve-1 (read-input-lines))

(solve-2 (read-sample-input-lines :override sample-input-2)) ; 48
(solve-2 (read-input-lines))

(provide 'day3)
;;; day3.el ends here
