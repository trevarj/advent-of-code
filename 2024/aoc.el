;;; aoc.el --- Advent of Code 2024 in Elisp          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((dash "2.19.1) (f "0.21.0") (s "1.13.0") (ht "2.3")
;; Keywords: lisp, games

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

;; Package file for my Advent of Code 2024 solutions

;;; Code:
(require 'f)
(require 's)
(require 'dash)
(require 'ht)

(defun aoc--split-lines (input &optional modifiers)
  "Split lines with modifiers applied"
  (-as-> (s-split "\n" input t) line
         (if (memq 'words modifiers)
             (-map #'s-split-words line)
           line)
         (if (memq 'nums modifiers)
             (--map (pcase it
                      ((cl-type list)
                       (-map #'string-to-number it))
                      (n (string-to-number n)))
                    line)
           line)))

(defun read-input ()
  "Reads input for day as string."
  (let* ((day (s-chop-prefix "day" (f-base (buffer-file-name))))
         (input-file (s-append ".txt" day)))
    (f-read (f-join "inputs" input-file))))

(cl-defun read-input-lines (&key modifiers)
  "Read lines of input file for corresponding lisp file that called this
function."
  (aoc--split-lines (read-input) modifiers))

(cl-defun read-sample-input-lines (&key modifiers override)
  "Read lines of locally defined sample-input lines"
  (aoc--split-lines (or override sample-input) modifiers))

(defun sample-input-lines-ints ()
  (read-sample-input-lines :modifiers '(words nums)))

(defun table (lines &optional char-mapper)
  (->> (-map #'string-to-list lines)
       (--map (-map (or char-mapper #'identity) it))))

(defun int-table (lines)
  (table lines (-partial #'+ -48)))

(defun sample-input-int-table ()
  (int-table (read-sample-input-lines)))

(defun input-int-table ()
  (int-table (read-input-lines)))

(defun grid (lines &optional char-mapper)
  (->> (table lines char-mapper)
       (--map (apply #'vector it))
       (apply #'vector)))

(defun setf-grid (grid x y elt)
  (setf (aref (aref grid y) x) elt))

(defun aref-grid (grid x y)
  (aref (aref grid y) x))

(defun display-grid (grid)
  (s-join "\n" (seq-map #'concat grid)))

(defun draw-points-on-table (table points ch &optional n)
  (-reduce-from
   (-lambda (g (x y))
     (-update-at y (-partial #'-update-at x (-const ch)) g))
   table (-take (or n (length points)) points)))

(defun sample-input-int-grid ()
  (int-grid (read-sample-input-lines)))

(defun input-int-grid ()
  (int-grid (read-input-lines)))

(defun input-lines-ints ()
  (read-input-lines :modifiers '(words nums)))

(cl-defun sample-lines (&key override)
  (read-sample-input-lines :modifiers '(words) :override override))

(defun input-lines ()
  (read-sample-input-lines 'words))

(defun aoc-2024-generate-day (day)
  (interactive (list (read-number "Day: " (car aoc-day-level))))
  (with-current-buffer (find-file (format "day%d.el" day))
    (goto-line 26)
    (insert "
(require 'aoc-2024)

(defvar sample-input \"\")

(defun parse (lines))

(defun solve-1 (input))

(defun solve-2 (input))")))

(defun table-replace (elem x y table)
  (-replace-at y (-replace-at x elem (nth y table)) table))

;; Graphs
(defun nodes (table)
  "Returns list of nodes, each being ((x y) val)"
  (->> table
       (-map-indexed
        (lambda (y row)
          (-map-indexed
           (lambda (x val)
             `((,x ,y) ,val))
           row)))
       (-flatten-n 1)))

(defun find-node (val nodes)
  (-find (lambda (n) (eq (cadr n) val)) nodes))

(defun neighbors (graph node &optional pred)
  "Return a list of nodes that are the neighbors to given node."
  (-let ((((x y) val) node))
    (->> (-zip-with #'add-pairs
                    (-repeat 4 `(,x ,y))
                    '((-1 0) (1 0) (0 -1) (0 1)))
         (--map (assoc it graph))
         (--filter (and it (funcall (or pred #'identity) it))))))

(defun neighbors-directed (graph node &optional pred)
  "Return a list of nodes that are the neighbors to given node."
  (-let ((((x y) val) node))
    (->> (-zip-with #'add-pairs
                    (-repeat 4 `(,x ,y))
                    '((-1 0) (1 0) (0 -1) (0 1)))
         (--map (assoc it graph))
         (funcall (-flip #'-zip) '(:left :right :up :down))
         (--filter (and it (car it) (funcall (or pred #'identity) it))))))

(defun node-dist-cmp (dists a b)
  (> (ht-get dists a)
     (ht-get dists b)))

(defun dijkstra (nodes source target)
  (let ((dists (ht-create))
        (prev (ht-create))
        (U '()))
    (-each nodes
      (lambda (n)
        (ht-set dists n (if (equal (car n) source) 0 most-positive-fixnum))
        (ht-set prev (car n) nil)
        (push n U)))

    (catch 'done
      (while-let (((not (seq-empty-p U)))
                  (curr (-min-by (-partial #'node-dist-cmp dists) U)))

        (when (or (equal (car curr) target)
                  (eq most-positive-fixnum (ht-get dists curr)))
          (throw 'done nil))

        ;; TODO: make the lambda pred a parameter?
        (-each (neighbors nodes curr (lambda (x) (not (eq (cadr x) ?#))))
          (lambda (n)
            (when-let* (((member n U))
                        (new-dist (1+ (ht-get dists curr)))
                        ((< new-dist (ht-get dists n))))
              (ht-set dists n new-dist)
              (ht-set prev (car n) (car curr)))))

        (setq U (-remove-item curr U))))

    (named-let reconstruct ((path '()) (curr target))
      (if curr
          (reconstruct (cons curr path) (ht-get prev curr))
        (if (equal source (car path))
            path
          nil)))))

(defun add-pairs (a b)
  "(+ (ax ay) (bx by) => ((+ ax bx) (+ ay by))"
  (-zip-with #'+ a b))

(defun sub-pairs (a b)
  "(- (ax ay) (bx by) => ((- ax bx) (- ay by))"
  (-zip-with #'- a b))

(defun dir->vec (dir)
  (pcase dir
    (:up '(0 -1))
    (:down '(0 1))
    (:left '(-1 0))
    (:right '(1 0))))

(defun vec->dir (dir)
  (pcase dir
    ('(0 -1) :up)
    ('(0 1) :down)
    ('(-1 0) :left)
    ('(1 0) :right)))

(defun cmd->dir (cmd)
  (pcase cmd
    (?^ :up)
    (?v :down)
    (?< :left)
    (?> :right)))

(defun adjacent (a b)
  "True if two coordinates are adjacent on a grid"
  (-let (((x1 y1) a)
         ((x2 y2) b))
    (or (and (eq x1 x2) (eq 1 (abs (- y1 y2))))
        (and (eq y1 y2) (eq 1 (abs (- x1 x2)))))))

(defun combinations (lst r)
  "Python itertools combinations(lst, r)"
  (let ((indices (seq-into (number-sequence 0 (1- r)) 'vector))
        (lst (seq-into lst 'vector))
        (n (length lst))
        (combos '()))
    (when (<= r (length lst))
      (push (mapcar (lambda (i) (aref lst i)) indices) combos)
      (catch 'done
        (while-let ((t)
                    (i (1- r)))

          (named-let rev ()
            (cond
             ((not (eq (aref indices i) (- (+ i n) r))))
             ((<= i 0) (throw 'done combos))
             (t (setf i (1- i)) (rev))))

          (setf (aref indices i) (1+ (aref indices i)))

          (named-let inc ((j (1+ i)))
            (when (< j r)
              (setf (aref indices j) (1+ (aref indices (1- j))))
              (inc (1+ j))))
          (push (mapcar (lambda (i) (aref lst i)) indices) combos))))))

(provide 'aoc-2024)
;;; aoc.el ends here
