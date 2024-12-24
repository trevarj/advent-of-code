;;; day24.el --- AoC 2024 Day 24                     -*- lexical-binding: t; -*-

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

(defvar sample-input "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")

(defun parse-op (op)
  (pcase op
    ("AND" #'logand)
    ("OR" #'logior)
    ("XOR" #'logxor)))

(defun parse (input)
  (-let (((top bot) (s-split "\n\n" input)))
    (list
     (->> (-map #'s-split-words (s-split "\n" top t))
          (-map (-lambda ((l r)) `(,l . ,(string-to-number r))))
          (ht<-alist))
     (->> (-map #'s-split-words (s-split "\n" bot t))
          (-map (-lambda ((a op b out)) `(,(parse-op op) ,a ,b ,out)))))))

(defun z-bits (map gates)
  (while gates
    (-if-let* (((gate &as op a b out) (pop gates))
               (a (ht-get map a))
               (b (ht-get map b)))
        (ht-set map out (funcall op a b))
      (setf gates (append gates `(,gate)))))
  (--> (ht->alist map)
       (--filter (s-starts-with-p "z" (car it)) it)
       (sort it :reverse t)
       (-map #'cdr it)))

(defun bits-to-string (bits)
  (mapconcat #'number-to-string bits))

(defun solve-1 (input)
  (-let* (((map gates) (parse input)))
    (--> (z-bits map gates)
         (bits-to-string it)
         (string-to-number it 2))))

(defun solve-2 (input)
  (->> (parse input)
       (cadr)
       (detect-mismatches)
       (-map #'-last-item)
       (sort)
       (s-join ","))
  )

(defun input-wire-p (wire)
  (or (s-starts-with-p "x" wire)
      (s-starts-with-p "y" wire)))

(defun detect-mismatches (gates)
  (let ((mismatches '()))
    (-each gates
      (-lambda ((g &as op a b out))
        (when (or (and (s-starts-with-p "z" out)
                       (not (equal out "z45"))
                       (not (equal op #'logxor)))
                  (and (not (s-starts-with-p "z" out))
                       (equal op #'logxor)
                       (not (input-wire-p a))
                       (not (input-wire-p b)))
                  (and (input-wire-p a)
                       (input-wire-p b)
                       (not (s-ends-with-p "00" a))
                       (not (s-ends-with-p "00" b))
                       (not (-any (-lambda ((g2 &as op1 a1 b1 _))
                                    (and
                                     (not (equal g g2))
                                     (or (equal out a1)
                                         (equal out b1))
                                     (equal op1
                                            (if (equal op #'logxor) #'logxor #'logior))))
                                  gates)
                            )))
          (push g mismatches))))
    mismatches))

(solve-1 sample-input) ; 2024
(solve-1 (read-input))

(solve-2 (read-input))

(provide 'day24)
;;; day24.el ends here
