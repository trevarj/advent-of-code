;;; day18.el --- AoC 2024 Day 18                     -*- lexical-binding: t; -*-

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
(require 'ht)

(defvar sample-input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(defun drop-bytes (grid bytes w h n)
  (-reduce-from
   (-lambda (g (x y))
     (-update-at y (-partial #'-update-at x (-const ?#)) g))
   grid (-take n bytes)))

(defun point-distance (a b)
  (-let (((x1 y1) a)
         ((x2 y2) b))
    (sqrt (+ (expt (- x2 x1) 2)
             (expt (- y2 y1) 2)))))

(defun node-dist-cmp (dists a b)
  (> (ht-get dists a)
     (ht-get dists b)))

(defun dijkstra (grid source target)
  (let ((dists (ht-create))
        (prev (ht-create))
        (U '()))
    (-each grid
      (lambda (n)
        (ht-set dists n (if (equal (car n) source) 0 most-positive-fixnum))
        (ht-set prev (car n) nil)
        (push n U)))

    (catch 'done
      (while-let (((not (seq-empty-p U)))
                  (curr (-min-by (-partial #'node-dist-cmp dists) U)))

        (when (or (equal (car curr) target)
                  (eq most-positive-fixnum (ht-get dists curr)))
          (throw 'done nil))

        (-each (neighbors grid curr (lambda (x) (not (eq (cadr x) ?#))))
          (lambda (n)
            (when-let* (((member n U))
                        (new-dist (1+ (ht-get dists curr)))
                        ((< new-dist (ht-get dists n))))
              (ht-set dists n new-dist)
              (ht-set prev (car n) (car curr)))))

        (setq U (-remove-item curr U))))

    (named-let reconstruct ((path '()) (curr target))
      (if curr
          (reconstruct (cons curr path) (ht-get prev curr))
        (if (equal source (car path))
            path
          nil)))))

(defun solve-1 (input w h n target)
  (let* ((grid (make-list h (make-list w ?.)))
         (graph (drop-bytes grid input w h n)))
    (1- (length (dijkstra (nodes graph) '(0 0) target)))))

(defun solve-2 (input w h n target)
  (let* ((len (length input))
         (high len)
         (low n))
    (while-let (((< low high))
                (mid (/ (+ high low) 2))
                (grid (make-list h (make-list w ?.)))
                (nodes (nodes (drop-bytes grid input w h mid))))
      (let ((res (dijkstra nodes '(0 0) target)))
        ;; (message "low %s high %s mid %s" low high mid)
        (if res
            (setq low (1+ mid))
          (setq high mid))))
    (nth (1- low) input)))

(solve-1 (sample-input-lines-ints) 7 7 12 '(6 6)) ; 22
(solve-1 (input-lines-ints) 71 71 1024 '(70 70))

(solve-2 (sample-input-lines-ints) 7 7 12 '(6 6)) ; (6 1)
(solve-2 (input-lines-ints) 71 71 1024 '(70 70))

(provide 'day18)
;;; day18.el ends here
