;;; day8.el --- AoC 2024 Day 8                            -*- lexical-binding: t; -*-

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

(defvar sample-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defun outside-map-p (pos max)
  (-let* (((x y) pos))
    (not (and (<= 0 y (1- max))
              (<= 0 x (1- max))))))

(defun antenna-pairs (map)
  (->> map
       (-map-indexed
        (lambda (y row)
          (-map-indexed (lambda (x val)
                          (list val x y))
                        row)))
       (-flatten-n 1)
       (-group-by #'car)
       (--remove (eq ?. (car it)))
       (--map
        (->> (cdr it)
             (-map #'cdr)
             (-powerset)
             (--filter (eq (length it) 2))))
       (-flatten-n 1)))

(defun add-vec (a b)
  (-zip-with #'+ a b))

(defun scale-vec (mag vec)
  (-map (-partial #'* mag) vec))

(defun inverse-vec (p)
  (-map #'- p))

(defun antenna-distance (a1 a2)
  (pcase (cons a1 a2)
    (`((,x ,y1) . (,x ,y2)) (list 0 (- y1 y2)))
    (`((,x1 ,y) . (,x2 ,y)) (list 0 (- x1 x2)))
    (`((,x1 ,y1) . (,x2 ,y2)) (list (- x1 x2)
                                    (- y1 y2)))))

(cl-loop for n = (add-vec '(1 1) n)
         with n = '(1 1)
         while (not (outside-map-p n 12))
         collect n)

(defun proliferate-antinode (node d map-dim &optional limit)
  (cl-loop for n = (add-vec d n)
           for i from 0
           while (and (not (outside-map-p n map-dim))
                      (if limit (< i limit) t))
           collect n
           with n = node))

(defun find-antinodes (pair map-dim &optional incl-pair limit)
  (-let* (((a1 a2) pair)
          (d (antenna-distance a1 a2))
          (n1 (proliferate-antinode a1 d map-dim limit))
          (n2 (proliferate-antinode a2 (inverse-vec d) map-dim limit)))
    (append n1 n2 (if incl-pair pair))))

(defun parse (lines)
  (-map #'string-to-list lines))

(defun solve (input &optional incl-pair limit)
  (let* ((map (parse input))
         (dim (length map)))
    (->> (antenna-pairs map)
         (-map (lambda (pair)
                 (find-antinodes pair dim incl-pair limit)))
         (-flatten-n 1)
         (-distinct)
         (length)
         )))

(defun solve-1 (input)
  (solve input nil 1))

(defun solve-2 (input)
  (solve input t))

(solve-1 (read-sample-input-lines)) ; 14
(solve-1 (read-input-lines))

(solve-2 (read-sample-input-lines)) ; 34
(solve-2 (read-input-lines))

(provide 'day8)
;;; day8.el ends here
