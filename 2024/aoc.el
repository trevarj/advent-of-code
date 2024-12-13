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
  (aoc--split-lines (or override sample-input) modifiers))

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

(defun grid (table)
  (->> (--map (apply #'vector it) table)
       (apply #'vector)))

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
    (forward-line 26)
    (insert "
(require 'aoc-2024)
(require 'dash)

(defvar sample-input \"\")

(defun parse (lines))

(defun solve-1 (input))

(defun solve-2 (input))")))

(defun table-replace (elem x y table)
  (-replace-at y (-replace-at x elem (nth y table)) table))

;; Graphs
(defun nodes (table)
  "Returns list of nodes, each being ((x y) val)"
  (->> table
       (-map-indexed
        (lambda (y row)
          (-map-indexed
           (lambda (x val)
             `((,x ,y) ,val))
           row)))
       (-flatten-n 1)))

(defun neighbors (graph node &optional pred)
  "Return a list of nodes that are the neighbors to given node."
  (-let ((((x y) val) node))
    (->> (-zip-with #'add-pairs
                    (-repeat 4 `(,x ,y))
                    '((-1 0) (1 0) (0 -1) (0 1)))
         (--map (assoc it graph))
         (--filter (and it (funcall (or pred #'identity) it))))))

(defun add-pairs (a b)
  "(+ (ax ay) (bx by) => ((+ ax bx) (+ ay by))"
  (-zip-with #'+ a b))

(defun adjacent (a b)
  "True if two coordinates are adjacent on a grid"
  (-let (((x1 y1) a)
         ((x2 y2) b))
    (or (and (eq x1 x2) (eq 1 (abs (- y1 y2))))
        (and (eq y1 y2) (eq 1 (abs (- x1 x2)))))))

(provide 'aoc-2024)
;;; aoc.el ends here
