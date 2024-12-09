;;; day9.el --- AoC 2024 Day 9                       -*- lexical-binding: t; -*-

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

(defvar sample-input-small "12345")
(defvar sample-input "2333133121414131402")

(defun read-disk-map (map)
  (->> map
       (--map-indexed
        (if (cl-evenp it-index)
            (list (1+ (ceiling (/ it-index 2))) it)
          (list 0 it)))
       (apply #'vector)))

(defun defragment (map &optional part-2)
  (cl-loop for i downfrom (1- (length map)) to 0
           do (cl-loop for j from 0 to i
                       for (i-data i-size) = (aref map i)
                       for (j-data j-size) = (aref map j)
                       when (and (> i-data 0) (eq 0 j-data))
                       do
                       (cond
                        ((<= i-size j-size)
                         (setf (aref map i) (list 0 i-size)
                               (aref map j) (list 0 (- j-size i-size))
                               map (vconcat (seq-subseq map 0 j)
                                            (vector (list i-data i-size))
                                            (seq-subseq map j))))
                        ((and (not part-2) (> i-size j-size))
                         (setf (aref map i) (list i-data (- i-size j-size))
                               (aref map j) (list i-data j-size))))))
  map)

(defun checksum (map)
  (cl-loop for (data size) across map
           append (-repeat size data) into v
           with v = (vector)
           finally
           return (cl-loop
                   for n in v
                   for i from 0
                   when (> n 0)
                   sum (* i (1- n)))))

(defun parse (line)
  (--map (- it 48) (string-to-list (s-trim line))))

(defun solve (input &optional part-2)
  (->> (parse input)
       (read-disk-map)
       (funcall (-flip #'defragment) part-2)
       (checksum)))

(defun solve-1 (input)
  (solve input))

(defun solve-2 (input)
  (solve input t))

(solve-1 sample-input) ; 1928
(solve-1 (read-input))

(solve-2 sample-input) ; 2858
(solve-2 (read-input))

(provide 'day9)
;;; day9.el ends here
