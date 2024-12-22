;;; day22.el --- AoC 2024 Day 22                     -*- lexical-binding: t; -*-

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

(defvar sample-input "1
10
100
2024")

(defvar sample-input-2 "1
2
3
2024")


(defun mix (secret val)
  (logxor val secret))

(defun prune (secret)
  (mod secret 16777216))

(defun next-secret (secret)
  (let* ((s1 (prune (mix secret (* secret 64))))
         (s2 (prune (mix s1 (/ s1 32)))))
    (prune (mix s2 (* s2 2048)))))

(defun first-digit (secret)
  (mod secret 10))

(defun secrets (seed)
  (-iterate #'next-secret seed 2001))

(defun solve-1 (input)
  (->> input
       (--map (-last-item (secrets it)))
       (-sum)))

(defun solve-2 (input)
  (let ((counts (ht-create)))
    (->> input
         (-map
          (lambda (secret)
            (let* ((secrets (-map #'first-digit (secrets secret)))
                   (diffs (->> secrets
                               (-partition-in-steps 2 1)
                               (-map (-lambda ((a b)) (- b a)))))
                   (seqs (-interleave (-partition-in-steps 4 1 diffs)
                                      (-slice secrets 4))))
              (-each (ht-items (ht<-plist seqs))
                (-lambda ((seq amount))
                  (ht-update-with! counts seq (lambda (v) (+ v amount)) 0)))
              ))))
    (-max (ht-values counts))
    ))

(solve-1 (read-sample-input-lines :modifiers '(nums))) ; 37327623
(solve-1 (read-input-lines :modifiers '(nums)))

(solve-2 (read-sample-input-lines :modifiers '(nums) :override sample-input-2)) ; 23
(solve-2 (read-input-lines :modifiers '(nums)))

(provide 'day22)
;;; day22.el ends here
