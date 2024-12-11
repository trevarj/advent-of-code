;;; day11.el --- AoC 2024 Day 11                     -*- lexical-binding: t; -*-

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

(defvar sample-input "125 17")

(defun digits (n)
  (1+ (truncate (log n 10))))

(defun even-digits-p (n)
  (cl-evenp (digits n)))

(defvar stone-counts)

(defun increment-or-insert (key inc)
  (if-let* ((val (gethash key stone-counts)))
      (puthash key (+ val inc) stone-counts)
    (puthash key inc stone-counts)))

(defun blink (s)
  (-let (((stone . count) s))
    (pcase stone
      ((guard (zerop count)))
      (0
       (increment-or-insert 0 (- count))
       (increment-or-insert 1 count))
      ((pred even-digits-p)
       (let* ((digs (digits stone))
              (p (expt 10 (/ digs 2))))
         (increment-or-insert (/ stone p) count)
         (increment-or-insert (mod stone p) count)
         (increment-or-insert stone (- count))))
      (_
       (increment-or-insert (* stone 2024) count)
       (increment-or-insert stone (- count))))))

(defun do-blink ()
  (-each (-zip-pair (hash-table-keys stone-counts)
                    (hash-table-values stone-counts))
    #'blink))

(defun solve (input n)
  (setq stone-counts (make-hash-table))
  (--each (-frequencies (car input))
    (puthash (car it) (cdr it) stone-counts))
  (dotimes (_ n) (do-blink))
  (-sum (hash-table-values stone-counts)))

(solve (sample-input-lines-ints) 25) ; 55312
(solve (input-lines-ints) 25)
(solve (input-lines-ints) 75)

(provide 'day11)
;;; day11.el ends here
