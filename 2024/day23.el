;;; day23.el --- AoC 2024 Day 23                     -*- lexical-binding: t; -*-

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
(require 'memoize)

(defvar sample-input "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defun node-map (links)
  (let ((map (ht-create)))
    (-each links
      (-lambda ((a b))
        (ht-update-with! map a (lambda (s) (ht-set s b t) s) (ht-create))
        (ht-update-with! map b (lambda (s) (ht-set s a t) s) (ht-create))))
    map))

(defun list-test (a b)
  (not (-difference a b)))

(defun hash-fn-list (a)
  (sxhash-equal (sort a)))

(define-hash-table-test 'same-elements #'list-test #'hash-fn-list)

(defun find-trio (map target node path)
  (cond
   ((and (equal node target)
         (eq (length path) 3))
    path)
   ((eq (length path) 3) nil)
   (t
    (-reduce-from
     (lambda (found peer)
       (if-let* ((f (find-trio map target peer (cons node path))))
           (cons f found)
         found))
     '()
     (ht-keys (ht-get map node))))))

(defun clique-p (map subset)
  (catch 'break
    (-each (combinations subset 2)
      (-lambda ((u v))
        (when (not (ht-contains-p (ht-get map u) v))
          (throw 'break nil))))
    t))

(defun greedy-largest-clique (map start)
  (let* ((nodes (ht-keys map))
         (curr-clique (list start)))
    (-each nodes
      (lambda (node)
        (when (--all-p (ht-contains-p (ht-get map it) node) curr-clique)
          (push node curr-clique))))
    curr-clique))

(defun solve-1 (input)
  (let ((map (node-map input))
        (triples (ht-create 'same-elements)))
    (-each (ht-keys map)
      (-lambda (node)
        (-each (-flatten-n 2 (find-trio map node node '()))
          (lambda (triple)
            (ht-set triples triple t)))))
    (length
     (-filter (lambda (trip) (-any (-partial #'s-starts-with-p "t") trip)) (ht-keys triples)))))

(defun solve-2 (input)
  (let* ((map (node-map input))
         (largest '()))
    (-each (ht-keys map)
      (lambda (n)
        (when-let* ((l (greedy-largest-clique map n))
                    ((> (length l) (length largest))))
          (setf largest l))))
    (s-join "," (sort largest))))

(solve-1 (read-sample-input-lines :modifiers '(words))) ; 7
(solve-1 (read-input-lines :modifiers '(words)))

(solve-2 (read-sample-input-lines :modifiers '(words))) ; co,de,ka,ta
(solve-2 (read-input-lines :modifiers '(words)))

(provide 'day23)
;;; day23.el ends here
