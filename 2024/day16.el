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
(require 'heap)

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

(defun min-heap-cmp (a b)
  (< (car a) (car b)))

(defun adjs (curr)
  (-let* ((((x y) . dir) curr)
          ((dx dy) (dir->vec dir)))
    (list (list 1000 `(,x ,y) (vec->dir `(,(- dy) ,dx)))
          (list 1000 `(,x ,y) (vec->dir `(,dy ,(- dx))))
          (list 1 `(,(+ x dx) ,(+ y dy)) dir))))

(defun dijkstra (nodes source target &optional part2)
  (let* ((node-map (ht<-plist (-flatten-n 1 nodes)))
         (dists (ht-create))
         (prev (ht-create))
         (Q (make-heap #'min-heap-cmp)))
    (ht-set dists (cons source :right) 0)
    (heap-add Q (cons 0 (cons source :right)))

    (catch 'done
      (while (not (heap-empty Q))
        (-let* (((curr-weight . curr) (heap-delete-root Q))
                ((curr-pos . curr-dir) curr))
          (when (and (not part2) (equal curr-pos target))
            (throw 'done (ht-get dists curr)))

          (-each (adjs curr)
            (-lambda ((cost npos dir))
              (unless (eq (ht-get node-map npos) ?#)
                (-let* ((n (cons npos dir))
                        (new-dist (+ curr-weight cost)))
                  (when (not (ht-get prev n))
                    (ht-set prev n (ht-create)))
                  (cond
                   ((< new-dist (ht-get dists n most-positive-fixnum))
                    (ht-set dists n new-dist)
                    (ht-set prev n (ht (curr t)))
                    (heap-add Q (cons new-dist n)))
                   ((<= new-dist (ht-get dists n most-positive-fixnum))
                    (setf (ht-get* prev n curr) t)))))))))

      (let* ((s (list (cons target :right)
                      (cons target :left)
                      (cons target :up)
                      (cons target :down)))
             (routes (ht-create))
             (good-nodes (ht-create)))
        (while-let ((s)
                    (n (pop s)))
          (when-let* ((n-links (ht-get prev n)))
            (--each (ht-keys n-links)
              (when (not (ht-contains-p routes it))
                (ht-set routes it t)
                (ht-set good-nodes (car it) t)
                (setf s (cons it s))))))
        (ht-keys good-nodes)))))

(defun solve (input &optional part2)
  (let* ((table (table input))
         (nodes (nodes table))
         (S (car (find-node ?S nodes)))
         (E (car (find-node ?E nodes))))
    (dijkstra nodes S E part2)))

(defun solve-1 (input)
  (solve input))

(defun solve-2 (input)
  (solve input t))

(solve-1 (read-sample-input-lines)) ; 7036
(solve-1 (read-input-lines))

(solve-2 (read-sample-input-lines)) ; 45
(solve-2 (read-input-lines))

(provide 'day16)
;;; day16.el ends here
