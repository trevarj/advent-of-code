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

(defvar sample-input-2 "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

(defvar sample-input-large "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(defun parse (input)
  (-let (((g d) (s-split "\n\n" input)))
    (cons (grid (s-split "\n" g))
          (string-to-list (s-concat d)))))

(defun dir-to-vec (dir)
  (pcase dir
    (:up '(0 -1))
    (:down '(0 1))
    (:left '(-1 0))
    (:right '(1 0))))

(defun cmd-to-dir (cmd)
  (pcase cmd
    (?^ :up)
    (?v :down)
    (?< :left)
    (?> :right)))

(defun find-robot (map)
  (cl-block loop
    (cl-loop for row across map
             for y from 0
             do (cl-loop for col across row
                         for x from 0
                         when (eq ?@ col)
                         do (cl-return-from loop `(,x ,y))))))

(defun try-move (map obj dir)
  (-let* (((x y) obj)
          (type (aref-grid map x y))
          (d (dir-to-vec dir))
          ((next &as nx ny) (add-pairs obj d)))
    (pcase type
      (?# nil)
      ((or (or ?@ ?O) (guard (and (memq type '(?\[ ?\])) (memq dir '(:left :right)))))
       (when (try-move map next dir)
         (setf-grid map x y ?.)
         (setf-grid map nx ny type)
         next))
      ((or ?\[ ?\])
       (-when-let* ((moved (try-move map next dir))
                    ((side-type side-dir) (if (eq type ?\[) '(?\] :right) '(?\[ :left)))
                    ((side &as sx sy) (add-pairs obj (dir-to-vec side-dir)))
                    ((side-next &as snx sny) (add-pairs next (dir-to-vec side-dir)))
                    (side-moved (try-move map side-next dir)))
         (setf-grid map x y ?.)
         (setf-grid map nx ny type)
         (setf-grid map sx sy ?.)
         (setf-grid map snx sny side-type)
         t))
      (?. t))))

(defun grid-clone (g)
  (grid (seq-map (-partial #'seq-map #'identity) g)))

(defun solve (input)
  (-let* (((map . cmds) (parse input))
          (robot (find-robot map)))
    ;; (message "start:\n%s" (display-grid map))
    (--each cmds
      ;; (message "cmd: %s (%s)" (cmd-to-dir it) it)
      (when-let* ((cmd (cmd-to-dir it))
                  (map2 (grid-clone map))
                  (move (try-move map2 robot cmd)))
        ;; (message "%s" (display-grid map))
        (setq map map2)
        (setq robot move)))
    (cl-loop for row across map
             for y from 0
             sum (cl-loop for val across row
                          for x from 0
                          when (or (eq val ?O) (eq val ?\[))
                          sum (+ x (* 100 y))))))

(defun transform-input (input)
  (s-replace-all '(("#" . "##")
                   ("O" . "[]")
                   ("." . "..")
                   ("@" . "@."))
                 input))

(defun solve-1 (input)
  (solve input))

(defun solve-2 (input)
  (solve (transform-input input)))

(solve-1 sample-input) ; 2028
(solve-1 (read-input)) ; 1436690

(solve-2 sample-input-2)
(solve-2 sample-input-large) ; 9021
(solve-2 (read-input)) ; 1482350

(provide 'day15)
;;; day15.el ends here
