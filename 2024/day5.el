;;; day5.el --- AoC 2024 Day 5                       -*- lexical-binding: t; -*-

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

(defvar sample-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defun parse (input)
  (--map (-map #'split-words-to-ints it)
         (-split-on "" (s-lines input))))

(defun split-words-to-ints (str)
  (->> str
       (s-split-words)
       (-map #'string-to-number)))

(defun rules-alist (parsed-input)
  (--map (-map-when #'listp #'cadr it)
         (-group-by #'car (car parsed-input))))

(defun fix-update-p (rules update)
  "Returns cons of whether the update was fixed with the final update."
  (--> update
       (--sort (memq other (alist-get it rules)) it)
       (cons (not (equal update it)) it)))

(defun middle-value (lst)
  (nth (/ (length lst) 2) lst))

(defun solve (input &optional fix)
  (--> (parse input)
       (-map
        (-partial #'fix-update-p (rules-alist it))
        (cadr it))
       (--filter (eq (car it) fix) it)
       (-map #'middle-value it)
       (-sum it)))

(solve sample-input) ; 143
(solve (read-input)) ;

(solve sample-input t) ; 123
(solve (read-input) t) ;

(provide 'day5)
;;; day5.el ends here
