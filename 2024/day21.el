;;; day21.el --- AoC 2024 Day 21                     -*- lexical-binding: t; -*-

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

;; Thanks to this guy, cause I am too dumb.
;; https://gist.github.com/nickponline/67a8452c4d3e6187018b541c2655438d

;; Keymap generation done by me at least!

;;

;;; Code:

(require 'aoc-2024)
(require 'memoize)

(defvar sample-input "029A
980A
179A
456A
379A")

(defvar numpad '((?7 . (-2 -3)) (?8 . (-1 -3)) (?9 . (0 -3))
                 (?4 . (-2 -2)) (?5 . (-1 -2)) (?6 . (0 -2))
                 (?1 . (-2 -1)) (?2 . (-1 -1)) (?3 . (0 -1))
                 (?0 . (-1 0))  (?A . (0 0))))

(defvar dirpad '((?^ . (-1 0)) (?A . (0 0))
                 (?< . (-2 1)) (?v . (-1 1)) (?> . (0 1))))

(defun all-moves (keypad)
  (let ((keys (-map #'car keypad)))
    (-non-nil
     (-table-flat
      (lambda (a b) (unless (eq a b) (list a b)))
      keys keys))))

(defun vec-to-dirs (v)
  (-let (((x y) v)
         (dirs '()))
    (setf dirs (append dirs (-repeat (abs x) (cond ((< x 0) ?<) ((> x 0) ?>)))))
    (setf dirs (append dirs (-repeat (abs y) (cond ((< y 0) ?^) ((> y 0) ?v)))))
    (-flatten dirs)))

(defun remove-deadzone (nodes from paths)
  (-filter
   (lambda (path)
     (named-let walk ((s from) (p path))
       (let ((next (add-pairs s (cmd->vec (car p)))))
         (cond
          ((eq p nil) t)
          ((ht-get nodes next)
           (walk next (cdr p)))
          (t nil)))))
   paths))

(defun move-map (keypad)
  (let* ((moves (all-moves keypad))
         (map (ht<-alist keypad))
         (node-map (ht<-alist (--map (cons (cdr it) (car it)) keypad))))
    (-map
     (-lambda ((move &as from to))
       (->> (sub-pairs (ht-get map to) (ht-get map from))
            (vec-to-dirs)
            (-permutations)
            (remove-deadzone node-map (ht-get map from))
            (cons move)))
     moves)))

(defvar NUMPAD-MAP (ht<-alist (move-map numpad)))
(defvar DIRPAD-MAP (ht<-alist (move-map dirpad)))

(defmemoize minimum-rewrite (level text nbots)
  (if (eq level (1+ nbots))
      (length text)
    (let ((PM (if (zerop level) NUMPAD-MAP DIRPAD-MAP))
          (k-total 0))
      (-each (-zip-lists (cons ?A text) text)
        (-lambda ((move &as start end))
          (setf k-total
                (+ k-total
                   (if-let* ((min-moves
                              (-non-nil
                               (--map (minimum-rewrite
                                       (1+ level)
                                       (append it '(?A))
                                       nbots)
                                      (ht-get PM move)))))
                       (-min min-moves)
                     1)))))
      k-total)))

(memoize-restore 'minimum-rewrite)

(defun solve (codes bots)
  (-reduce-from
   (lambda (acc code)
     (let* ((code (string-to-list code))
            (ordinal (string-to-number (apply #'string (-filter #'cl-digit-char-p code))))
            (min-len (minimum-rewrite 0 code bots)))
       (+ acc (* ordinal min-len))))
   0 codes))

(solve (read-sample-input-lines) 2)
(solve (read-input-lines) 2)

(solve (read-input-lines) 25)

(provide 'day21)
;;; day21.el ends here
