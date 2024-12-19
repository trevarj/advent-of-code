;;; day19.el --- AoC 2024 Day 19                     -*- lexical-binding: t; -*-

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

(defvar sample-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")


(defun parse (input)
  (-let (((subs patterns) (s-split "\n\n" input)))
    (list (s-split-words subs) (s-split "\n" patterns t))))

(defun construct-counts (subs target)
  (let* ((n (length target))
         (dp (make-vector (1+ n) 0)))
    (setf (aref dp 0) 1)
    (dotimes (i n)
      (-each subs
        (lambda (pat)
          (when-let* ((i (1+ i))
                      (plen (length pat))
                      (d (- i plen))
                      ((and (>= i plen)
                            (aref dp d)
                            (equal pat (substring target d i)))))
            (setf (aref dp i) (+ (aref dp i) (aref dp d)))))))
    (aref dp n)))

(-let (((subs pats) (parse sample-input)))
  (-map (-partial #'construct-counts subs) pats))

(defun solve (input)
  (-let (((subs pats) (parse input)))
    (-map (-partial #'construct-counts subs) pats)))

(defun solve-1 (input)
  (-count (-compose #'not #'zerop) (solve input)))

(defun solve-2 (input)
  (-sum (solve input)))

(solve-1 sample-input) ; 6
(solve-1 (read-input))

(solve-2 sample-input) ; 16
(solve-2 (read-input))

(provide 'day19)
;;; day19.el ends here
