;;; day12.el --- AoC 2024 Day 12                     -*- lexical-binding: t; -*-

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

(defvar sample-input "AAAA
BBCD
BBCC
EEEC")

(defvar sample-input-2 "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")

(defvar sample-input-large "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")


(defun solve-1 (input))

(defun solve-2 (input))

(solve-1 (read-sample-input-lines)) ; 140
(solve-1 (read-sample-input-lines :override sample-input-2)) ; 772
(solve-1 (read-sample-input-lines :override sample-input-large)) ; 1930
(solve-1 (read-input-lines))

(provide 'day12)
;;; day12.el ends here
