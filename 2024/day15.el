;;; day15.el --- AoC 2024 Day 15                     -*- lexical-binding: t; -*-

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

(defvar sample-input "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(defvar map)

(defun parse (input)
  (-let (((g d) (s-split "\n\n" input)))
    (cons (grid (s-split "\n" g))
          (string-to-list (s-concat d)))))

(defun command-to-dir (cmd)
  (pcase cmd
    (?^ '(0 -1))
    (?v '(0 1))
    (?< '(-1 0))
    (?> '(1 0))))

(defun find-robot (map)
  (cl-block loop
    (cl-loop for row across map
             for y from 0
             do (cl-loop for col across row
                         for x from 0
                         when (eq ?@ col)
                         do (cl-return-from loop `(,x ,y))))))

(defun try-move (obj dir)
  (-let* (((x y) obj)
          (type (aref-grid map x y)))
    (pcase type
      (?# nil)
      ((or ?@ ?O)
       (-when-let* (((next &as nx ny) (add-pairs obj dir))
                    (moved (try-move next dir)))
         (setf-grid map x y ?.)
         (setf-grid map nx ny type)
         next))
      (?. t))))

(defun solve-1 (input)
  (-let* (((m . cmds) (parse input))
          (robot (find-robot m)))
    (setq map m)
    (--each cmds
      (when-let* ((move (try-move robot (command-to-dir it))))
        (setq robot move)))
    (cl-loop for row across map
             for y from 0
             sum (cl-loop for val across row
                          for x from 0
                          when (eq val ?O)
                          sum (+ x (* 100 y))))))

(defun transform-input (input)
  (s-replace-all '(("#" . "##")
                   ("O" . "[]")
                   ("." . "..")
                   ("@" . "@."))
                 input))

(transform-input sample-input)

(solve-1 sample-input)
(solve-1 (read-input))

(defun solve-2 (input))

(provide 'day15)
;;; day15.el ends here
