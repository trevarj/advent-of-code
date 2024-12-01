;;; day1.el --- AoC Day 1                            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>

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

;;; Code:

(require 'aoc-2024)
(require 'dash)

(defvar sample-input "3   4
4   3
2   5
1   3
3   9
3   3")

(defun parse (lines)
  (-unzip-lists lines))

(defun solve-1 (input)
  (->> (parse input)
       (-map #'sort)
       (funcall (-applify #'-zip-lists))
       (-map (-applify (-compose #'abs #'-)))
       (-sum)))

(defun solve-2 (input)
  (let* ((lists (parse input))
         (freq (-frequencies (cadr lists))))
    (->> (car lists)
         (--map
          (* it (or (alist-get it freq) 0)))
         (-sum))))

(solve-1 (sample-input-lines-ints)) ; 11
(solve-1 (input-lines-ints))

(solve-2 (sample-input-lines-ints)) ; 31
(solve-2 (input-lines-ints))

(provide 'day1)
;;; day1.el ends here
