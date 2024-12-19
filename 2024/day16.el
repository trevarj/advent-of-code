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

(defun dfs (nodes start target)
  (named-let rec ((visited '())
                  (score 0)
                  (curr start)
                  (dir :right)
                  )
    (unless (member curr visited)
      (push curr visited)
      (if (equal curr target)
          score
        (when-let*
            ((ss (->> (neighbors-directed nodes curr (lambda (x) (not (eq (cadar x) ?#))))
                      (-map
                       (-lambda ((n . d))
                         (when-let* ((s (rec visited (+ score (if (eq dir d) 1 1000)) n d)))
                           s)))
                      (-non-nil))))
          (-min ss))))))

(let* ((table (table (read-sample-input-lines)))
       (nodes (nodes table))
       (S (find-node ?S nodes))
       (E (find-node ?E nodes))
       (score (dfs nodes S E))
       )
  score
  )
(cada)
(car (cdr (car '(((0 13) 35) . :left))))
(defun solve-1 (input))

(defun solve-2 (input))

(provide 'day16)
;;; day16.el ends here
