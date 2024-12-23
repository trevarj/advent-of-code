;;; day20.el --- AoC 2024 Day 20                     -*- lexical-binding: t; -*-

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

(defvar sample-input "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(defun node-map (input)
  (ht<-plist
   (-flatten-n
    1 (--remove (eq (cdr it) ?#)
                (nodes (table input))))))

(defun adjs (pos)
  (-zip-with #'add-pairs (-repeat 4 pos)
             '((1 0) (-1 0) (0 1) (0 -1))))

(let* ((nodes (node-map (read-input-lines)))
       (S (car (ht-find (lambda (_ v) (eq v ?S)) nodes)))
       (dist (ht (S 0)))
       (todo (list S)))
  (while-let ((todo)
              (pos (pop todo)))
    (dolist (new (adjs pos))
      (when (and (ht-contains-p nodes new)
                 (not (eq (ht-get nodes new) ?#))
                 (not (ht-contains-p dist new)))
        (ht-set dist new (1+ (ht-get dist pos)))
        (push new todo))))

  (let ((a 0) (b 0))
    (-each (combinations (ht-items dist) 2)
      (-lambda ((((x1 y1) i) ((x2 y2) j)))
        (let ((d (+ (abs (- x1 x2))
                    (abs (- y1 y2)))))
          (when (and (eq d 2) (>= (- (abs (- j i)) d) 100))
            (setf a (1+ a)))
          (when (and (< d 21) (>= (- (abs (- j i)) d) 100))
            (setf b (1+ b))))))
    (list a b)))

(provide 'day20)
;;; day20.el ends here
