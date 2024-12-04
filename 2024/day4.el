;;; day4.el --- AoC 2024 Day 4                       -*- lexical-binding: t; -*-

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

(defvar sample-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defun chars-to-strings (char-list)
  (-map #'char-to-string char-list))

(defun parse (lines)
  (->> lines
       (-map #'string-to-list)
       (-map #'chars-to-strings)))

(defun apply-overlays (overlays table)
  "Select items from table using overlays that designates which indices
to choose."
  (-reduce #'list
           (--map
            (->> table
                 (-zip-with #'-select-by-indices it)
                 (-flatten)
                 (-reduce #'s-concat)) overlays)))

(defun windows (width height table)
  "Generate an overlapping sliding window over a matrix, which start
from (0,0), slides downward by one towards the bottom and then shifts
over by one column."
  (->> table
       ;; partition columns
       (-map (-partial #'-partition-all-in-steps width 1))
       ;; transpose
       (funcall (-applify #'-zip-lists))
       ;; partition rows
       (-map (-partial #'-partition-all-in-steps height 1))))

(defun solve-1 (input)
  (->> (windows 4 4 input)
       (-flatten-n 1)
       (-map (-partial #'apply-overlays
                       '(((0) (1) (2) (3)) ; l2r diag
                         ((3) (2) (1) (0)) ; r2l diag
                         ((0 1 2 3) () () ()) ; top row
                         ((0) (0) (0) (0))))) ; left row
       (-flatten)
       (--count (or (s-equals-p it "XMAS")
                    (s-equals-p it "SAMX")))))

(defun solve-2 (input)
  (->> (windows 3 3 input)
       (-flatten-n 1)
       (--map (apply-overlays '(((0 2) (1) (0 2))) it))
       (--count (or (s-equals-p it "MSAMS")
                    (s-equals-p it "MMASS")
                    (s-equals-p it "SSAMM")
                    (s-equals-p it "SMASM")))))

(solve-1 (parse (read-sample-input-lines))) ; 18
(solve-1 (parse (read-input-lines)))

(solve-2 (parse (read-sample-input-lines))) ; 9
(solve-2 (parse (read-input-lines)))

(provide 'day4)
;;; day4.el ends here
