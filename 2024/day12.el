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
(require 'ht)

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

;; grid = {i+j*1j: c for i,r in enumerate(open('./inputs/12.txt'))
;;                   for j,c in enumerate(r.strip())}

;; sets = {p: {p} for p in grid}

;; for p in grid:
;;     for n in p+1, p-1, p+1j, p-1j:
;;         if n in grid and grid[p] == grid[n]:
;;             sets[p] |= sets[n]
;;             for x in sets[p]:
;;                 sets[x] = sets[p]

;; sets = {tuple(s) for s in sets.values()}

;; def edge(ps):
;;     P = {(p,d) for d in (+1,-1,+1j,-1j) for p in ps if p+d not in ps}
;;     return P, P - {(p+d*1j, d) for p,d in P}

;; for part in 0,1: print(sum(len(s) * len(edge(s)[part]) for s in sets))

(defvar sets)
(defvar regions)

(defun edge (grid ps)
  (let* ((P (-distinct
             (-non-nil
              (-mapcat
               (lambda (p)
                 (-map
                  (lambda (d)
                    (if (not (member (list (add-pairs (car p) d) (cadr p)) ps))
                        (cons (car p) d)
                      nil))
                  '((0 1) (1 0) (0 -1) (-1 0))))
               ps))))
         (EDGES (-difference
                 P
                 (-distinct
                  (-map
                   (-lambda (((x y) . d))
                     (cons (add-pairs `(,x ,y) (-zip-with #'* d '(0 1)))
                           d))
                   P)))))
    (list P EDGES)))

(defun solve (input &optional part1)
  (setq sets (ht-create #'equal))
  (setq regions nil)
  (let ((grid (nodes (table input))))
    (-each grid
      (lambda (p)
        (ht-set sets p (ht (p nil)))))
    (-each grid
      (lambda (p)
        (-each (neighbors grid p (lambda (x) (eq (cadr p) (cadr x))))
          (lambda (n)
            (let ((new-p-set (ht-merge (ht-get sets p) (ht-get sets n))))
              (ht-set sets p new-p-set)
              (-each (ht-keys new-p-set)
                (lambda (x) (ht-set sets x new-p-set))))))))

    (-reduce-from (lambda (acc s)
                    (let ((E (edge grid s)))
                      (+ acc (* (length s) (length (if part1 (car E) (cadr E))))))) 0
                      (-map (lambda (ps)
                              (let ((pss (cons (car ps) (-flatten-n 1 (cdr ps)))))
                                pss) )
                            (ht-items (ht<-alist (-map #'ht-keys (ht-values sets))))))))

(solve (read-sample-input-lines) t) ; 140
(solve (read-sample-input-lines :override sample-input-2) t) ; 772
(solve (read-sample-input-lines :override sample-input-large) t) ; 1930
(solve (read-input-lines) t)


;; Doesn't work!
(solve (read-sample-input-lines)) ; 80
(solve (read-sample-input-lines :override sample-input-2)) ; 436
(solve (read-input-lines))

(provide 'day12)
;;; day12.el ends here
