;;; day25.el --- AoC 2024 Day 25                     -*- lexical-binding: t; -*-

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

(defvar sample-input "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")

(defun key-or-lock (lines)
  (-let (((type ch)
          (if (--all-p (eq it ?#) (nth 0 lines)) '(:lock ?.) '(:key ?#))))
    (cons type
          (--map (1- (--count (eq it ch) it)) (apply #'-zip lines)))))

(defun lock-key-match-p (lock key)
  (-all-p #'identity (-zip-with #'>= (cdr lock) (cdr key))))

(defun parse (input)
  (->> (s-split "\n\n" input t)
       (--map
        (-map #'string-to-list
              (s-split "\n" it)))
       (-map #'key-or-lock)))

(defun solve-1 (input)
  (-let* (((&alist :key keys :lock locks) (-group-by #'car (parse input))))
    (-count (-applify #'lock-key-match-p) (-table-flat #'list locks keys))))

(defun solve-2 (input))

(solve-1 sample-input) ; 3
(solve-1 (read-input))

(provide 'day25)
;;; day25.el ends here
