;;; day13.el --- AoC 2024 Day 13                     -*- lexical-binding: t; -*-

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
(require 's)
(require 'dash)

(defvar sample-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defvar parse-re "Button A: X\\+\\([[:digit:]]*\\), Y\\+\\([[:digit:]]*\\)\nButton B: X\\+\\([[:digit:]]*\\), Y\\+\\([[:digit:]]*\\)\nPrize: X=\\([[:digit:]]*\\), Y=\\([[:digit:]]*\\)")

(defun parse (lines)
  (->> (s-split "\n\n" lines t)
       (-map
        (-compose
         (-partial #'-map #'string-to-number)
         #'cdr
         (-partial #'s-match parse-re)))))

(defun b (ax ay bx by tx ty)
  (/ (- (* ty ax) (* ay tx))
     (- (* by ax) (* ay bx))))

(defun a (ax bx tx b)
  (/ (- tx (* bx b))
     ax))

(defun solve-equation (inputs)
  (-let* (((ax ay bx by tx ty) inputs)
          (b-press (apply #'b inputs))
          (a-press (a ax bx tx b-press)))
    (when (and (eq tx (+ (* a-press ax) (* b-press bx)))
               (eq ty (+ (* a-press ay) (* b-press by))))
      (+ (* 3 a-press) b-press))))

(defun solve (inputs)
  (-reduce-from
   (lambda (acc input)
     (if-let* ((tokens (solve-equation input)))
         (+ acc tokens)
       acc))
   0 inputs))

(defun solve-1 (input)
  (->> (parse input)
       (solve)))

(defun solve-2 (input)
  (->> (parse input)
       (--map
        (--map-indexed
         (pcase it-index
           ((or 4 5) (+ 10000000000000 it))
           (_ it))
         it))
       (solve)))

(solve-1 sample-input) ; 480
(solve-1 (read-input))

(solve-2 (read-input))

(provide 'day13)
;;; day13.el ends here
