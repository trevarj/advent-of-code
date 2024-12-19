;;; day16.el --- AoC 2024 Day 16                     -*- lexical-binding: t; -*-

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
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(defvar sample-input-2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(defun dijkstra (nodes source target)
  (let ((dists (ht-create))
        (prev (ht-create))
        (dir (ht-create))
        (U '()))
    (-each nodes
      (lambda (n)
        (ht-set dists n (if (equal (car n) source) 0 most-positive-fixnum))
        (ht-set prev (car n) nil)
        (ht-set dir n :right)
        (push n U)))

    (catch 'done
      (while-let (((not (seq-empty-p U)))
                  (curr (-min-by (-partial #'node-dist-cmp dists) U)))

        (when (or (equal (car curr) target)
                  (eq most-positive-fixnum (ht-get dists curr)))
          (throw 'done nil))

        ;; TODO: make the lambda pred a parameter?
        (-each (neighbors-directed nodes curr (lambda (x) (not (eq (cadar x) ?#))))
          (-lambda ((n . d))
            (when-let* (((member n U))
                        (curr-dir (ht-get dir curr))
                        (dir-cost (if (eq curr-dir d) 1 1001))
                        (new-dist (+ (ht-get dists curr) dir-cost))
                        ((< new-dist (ht-get dists n))))
              (ht-set dists n new-dist)
              (ht-set prev (car n) (car curr))
              (ht-set dir n d))))

        (setq U (-remove-item curr U))))

    (named-let reconstruct ((path '()) (curr target))
      (if curr
          (reconstruct (cons curr path) (ht-get prev curr))
        (if (equal source (car path))
            (list path (ht-get dists (list target ?E)))
          nil)))))

(defun solve-1 (input)
  (let* ((table (table input))
         (nodes (nodes table))
         (S (car (find-node ?S nodes)))
         (E (car (find-node ?E nodes)))
         (path (dijkstra nodes S E)))
    (cadr path)))

(defun solve-2 (input))

(solve-1 (read-sample-input-lines)) ; 7036
(solve-1 (read-input-lines))

(provide 'day16)
;;; day16.el ends here
