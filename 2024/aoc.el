;;; aoc.el --- Advent of Code 2024 in Elisp          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((dash "2.19.1) (f "0.21.0") (s "1.13.0"))
;; Keywords: lisp, games

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

;; Package file for my Advent of Code 2024 solutions

;;; Code:
(require 'f)
(require 's)

(defun aoc--split-lines (input &optional modifiers)
  "Split lines with modifiers applied"
  (-as-> (s-split "\n" input t) line
         (if (memq 'words modifiers)
             (-map #'s-split-words line)
           line)
         (if (memq 'nums modifiers)
             (--map (pcase it
                      ((cl-type list)
                       (-map #'string-to-number it))
                      (n (string-to-number n)))
                    line)
           line)))

(defun read-input ()
  "Reads input for day as string."
  (let* ((day (s-chop-prefix "day" (f-base (buffer-file-name))))
         (input-file (s-append ".txt" day)))
    (f-read (f-join "inputs" input-file))))

(cl-defun read-input-lines (&key modifiers)
  "Read lines of input file for corresponding lisp file that called this
function."
  (aoc--split-lines (read-input) modifiers))

(cl-defun read-sample-input-lines (&key modifiers override)
  "Read lines of locally defined sample-input lines"
  (when override
    (setq sample-input override))
  (aoc--split-lines sample-input modifiers))

(defun sample-input-lines-ints ()
  (read-sample-input-lines :modifiers '(words nums)))

(defun table (lines &optional char-mapper)
  (->> (-map #'string-to-list lines)
       (--map (-map (or char-mapper #'identity) it))))

(defun int-table (lines)
  (table lines (-partial #'+ -48)))

(defun sample-input-int-table ()
  (int-table (read-sample-input-lines)))

(defun input-int-table ()
  (int-table (read-input-lines)))

(defun grid (table &optional char-mapper)
  (->> (--map (apply #'vector it) table)
       (apply #'vector)))

(defun int-grid (lines)
  (grid lines (-partial #'+ -48)))

(defun sample-input-int-grid ()
  (int-grid (read-sample-input-lines)))

(defun input-int-grid ()
  (int-grid (read-input-lines)))

(defun input-lines-ints ()
  (read-input-lines :modifiers '(words nums)))

(cl-defun sample-lines (&key override)
  (read-sample-input-lines :modifiers '(words) :override override))

(defun input-lines ()
  (read-sample-input-lines 'words))

(defun aoc-2024-generate-day (day)
  (interactive (list (read-number "Day: " (car aoc-day-level))))
  (with-current-buffer (find-file (format "day%d.el" day))
    (goto-line 26)
    (insert "
(require 'aoc-2024)
(require 'dash)

(defvar sample-input \"\")

(defun parse (lines))

(defun solve-1 (input))

(defun solve-2 (input))")))

(defun table-replace (elem x y table)
  (-replace-at y (-replace-at x elem (nth y table)) table))

(provide 'aoc-2024)
;;; aoc.el ends here
