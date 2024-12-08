;;; day6.el --- AoC 2024 Day 6                       -*- lexical-binding: t; -*-

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
(require 's)

(defvar sample-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defun find-index-table (pred table)
  "Finds an element in a table when pred is t. Returns alist of elem and
its coordinate."
  (->> table
       (--map-indexed
        (when-let* ((x (-find-index pred it))
                    (elem (nth x it)))
          (list elem x it-index)))
       (-non-nil)))

(defun table-get (x y table)
  (nth x (nth y table)))

(defun table-replace (elem x y table)
  (-replace-at y (-replace-at x elem (nth y table)) table))

(defun obstacle-p (elem)
  (eq elem ?#))

(defun find-guard (table)
  "Returns guard stance (^ v < >) with coordinate."
  (-first-item (find-index-table
                (-rpartial #'eq ?^) table)))

(defun outside-map-p (x y table)
  (let ((len (length table)))
    (not (and (<= 0 y (1- len))
              (<= 0 x (1- len))))))

(defun hit-obstacle-p (x y table)
  (let* ((ch (table-get x y table)))
    (obstacle-p ch)))

(defun turn-right (dir x y)
  (pcase dir
    (?^ ?>)
    (?v ?<)
    (?> ?v)
    (?< ?^)))

(defun go-straight (dir x y)
  (pcase dir
    (?^ `(?^ ,x ,(1- y)))
    (?v `(?v ,x ,(1+ y)))
    (?> `(?> ,(1+ x) ,y))
    (?< `(?< ,(1- x) ,y))))

(defun find-next-position (guard table)
  (-let* (((dir x y) guard)
          ((straight &as _ sx sy) (go-straight dir x y))
          (rdir (turn-right dir x y)))
    (cond
     ((outside-map-p sx sy table) nil)
     ((hit-obstacle-p sx sy table)
      (find-next-position `(,rdir ,x ,y) table))
     (t straight))))

(defun guard-patrol (guard table)
  (cl-loop for next = (find-next-position next table)
           while next
           if (member next path) return :loop
           else collect next into path
           with next = guard
           with path = (list guard)
           finally return path))

(defun parse (lines)
  (-map #'string-to-list lines))

(defun solve-1 (input)
  (let* ((map (parse input))
         (guard (find-guard map)))
    (length (-distinct (-map #'cdr (guard-patrol guard map))))))

(defun solve-2 (input)
  (let* ((map (parse input))
         (guard (find-guard map))
         (path (-distinct (-map #'cdr (guard-patrol guard map)))))
    (cl-loop for idx from 1
             for (x y) in (cdr path)
             for new-map = (table-replace ?# x y map)
             for patrol = (guard-patrol guard new-map)
             do (message "%s/%s" idx (length path))
             ;; do (when (eq :loop patrol) (message "%s\n%s\n" patrol (s-join "\n" new-map)))
             count (eq :loop patrol))))

(solve-1 (read-sample-input-lines)) ; 41
(solve-1 (read-input-lines)) ; 5080

(solve-2 (read-sample-input-lines)) ; 6
(solve-2 (read-input-lines))

(provide 'day6)
;;; day6.el ends here
