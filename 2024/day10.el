;;; day10.el --- AoC 2024 Day 10                     -*- lexical-binding: t; -*-

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

(defvar sample-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defun nodes (input)
  (->> input
       (-map-indexed
        (lambda (y row)
          (-map-indexed
           (lambda (x val)
             `((,x ,y) ,val))
           row)))
       (-flatten-n 1)))

(defun add-pairs (a b)
  (-zip-with #'+ a b))

(defun neighbors (node)
  (-let ((((x y) val) node))
    (-zip-with #'add-pairs
               (-repeat 4 `(,x ,y))
               '((-1 0) (1 0) (0 -1) (0 1)))))

(defun reachable-peaks (node map)
  (->> (neighbors node)
       (--map (assoc it map))
       (--filter (and it
                      (eq (cadr it) (1+ (cadr node)))))))

(defun trailheads (map)
  (--filter (eq 0 (cadr it)) map))

(defun traverse-score (node map)
  (-let (((pos val) node)
         (reachable (reachable-peaks node map)))
    (cond
     ((eq val 9) `(,pos))
     (reachable (--reduce-from (-union acc (traverse-score it map)) nil reachable))
     (t nil))))

(defun traverse-rating (node map)
  (-let (((pos val) node)
         (reachable (reachable-peaks node map)))
    (cond
     ((eq val 9) 1)
     (reachable (--reduce-from (+ acc (traverse-rating it map)) 0 reachable))
     (t 0))))

(defun solve-1 (input)
  (let* ((map (nodes input))
         (heads (trailheads map)))
    (->> heads
         (--reduce-from (-concat acc (traverse-score it map)) '())
         (length))))

(defun solve-2 (input)
  (let* ((map (nodes input))
         (heads (trailheads map)))
    (->> heads
         (--reduce-from (+ acc (traverse-rating it map)) 0))))

(solve-1 (sample-input-int-table)) ; 36
(solve-1 (input-int-table))

(solve-2 (sample-input-int-table)) ; 81
(solve-2 (input-int-table))

(provide 'day10)
;;; day10.el ends here
