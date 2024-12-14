;;; day14.el --- AoC 2024 Day 14                     -*- lexical-binding: t; -*-

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

(defvar sample-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defun parse (input)
  (->> input
       (--map (s-match-strings-all "[p\|v]=\\(-?[[:digit:]]*\\),\\(-?[[:digit:]]*\\)" it))
       (--map (-map (-compose (-partial #'-map #'string-to-number) #'cdr) it) )))

(defun shift-robot (robot secs w h)
  (-let (((pos vel) robot))
    (list (-reduce-from
           (-lambda ((x y) (vx vy))
             (list (mod (+ x vx) w)
                   (mod (+ y vy) h)))
           pos
           (-repeat secs vel))
          vel)))

(defun robot-quadrant (pos w h)
  (-let* (((x y) pos)
          (mid-x (/ w 2))
          (mid-y (/ h 2)))
    (cond
     ((and (< x mid-x) (< y mid-y)) 1)
     ((and (> x mid-x) (< y mid-y)) 2)
     ((and (< x mid-x) (> y mid-y)) 3)
     ((and (> x mid-x) (> y mid-y)) 4)
     (t nil))))

(defun display-robots (robots width height)
  (let ((map (grid (-repeat height (-repeat width ?.)))))
    (-each robots
      (-lambda (((x y) _))
        (aref-grid map x y ?#)))
    (s-join "\n" (-map #'s-concat map))))

(defun solve-1 (input secs width height)
  (->> (parse input)
       (--map
        (-> it
            (shift-robot secs width height)
            (car)
            (robot-quadrant width height)))
       (-non-nil)
       (-frequencies)
       (-map #'cdr)
       (-product)))

(defun solve-2 (input width height)
  (let ((robots (parse input)))
    (named-let loop ((s 1)
                     (rs (--map
                          (shift-robot it 1 width height) robots)))
      (cond
       ((eq s 10000) s)
       (t
        (f-append (format "%s\n%s\n"
                          s (display-robots rs width height))
                  'utf-8
                  (format "/tmp/aoc_2024_14.txt" s))
        (loop (1+ s) (--map (shift-robot it 1 width height) rs)))))))

(solve-1 (read-sample-input-lines) 100 11 7) ; 12
(solve-1 (read-input-lines) 100 101 103)

(solve-2 (read-input-lines) 101 103) ; check the file!

(provide 'day14)
;;; day14.el ends here
