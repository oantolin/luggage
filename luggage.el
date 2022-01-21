;;; luggage.el --- Largely Undesired Gadget: Generative Art Gallery for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: games, hypermedia

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

;; Commands to generate pretty SVGs with some element of randomness.

;;; Code:

(require 'svg)
(require 'cl-lib)

(defgroup luggage nil
  "Largely Undesired Gadget: Generative Art Gallery for Emacs"
  :group 'games)

(defcustom luggage-mondrian-colors
  '(("white" . 3)
    ("red" . 1)
    ("blue" . 1)
    ("yellow" . 1))
  "Weighted list of colors to use in `luggage-mondrian'."
  :type '(choice
          (const :tag "Classic"
                 (("white" . 3)
                  ("red" . 1)
                  ("blue" . 1)
                  ("yellow" . 1)))
          (const :tag "Stained glass"
                 (("#dc147b" . 1)
                  ("#f683c4" . 1)
                  ("#ae0464" . 1)
                  ("#6094a0" . 1)
                  ("#9ecbca" . 1)
                  ("#484269" . 1)))
          (alist :key-type string :value-type integer)))

(defun luggage--sample (choices)
  "Return a random selection from the weighted CHOICES.
The argument should be a list of pairs of the form (choice . weight)."
  (cl-loop with total = (cl-reduce #'+ choices :key #'cdr)
           with rnd = (random total)
           for (choice . weight) in choices
           if (< rnd weight)
           return choice
           else do (cl-decf rnd weight)))

(defun luggage--show (name svg)
  "Display SVG in a buffer called NAME."
  (let ((buf (get-buffer-create (format "*%s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (svg-print svg))
      (setq revert-buffer-function
            (let ((cmd (intern
                        (format "luggage-%s"
                                (downcase (string-replace " " "-" name))))))
              (lambda (_ignore-auto _confirm) (funcall cmd))))
      (image-mode))
    (pop-to-buffer buf)))

(defun luggage-mondrian ()
  "Produce a random image similar to a Mondrian composition."
  (interactive)
  (let* ((w 400) (h 400) (mw 70) (mh 70) (sw (/ w 2)) (sh (/ h 2))
         (svg (svg-create w h)))
    (cl-labels ((split (u v)
                  (let ((p (/ (+ 33 (random 35)) 100.0)))
                    (floor (+ (* p u) (* (- 1 p) v)))))
                (canvas (a b c d)
                  (let ((w (- c a)) (h (- d b)))
                    (cond
                      ((or (and (< w mw) (< h mh))
                           (not (or (> w sw) (> h sh) (= (random 2) 0))))
                       (svg-rectangle
                        svg a b w h
                        :stroke-width 5 :stroke "black"
                        :fill-color (luggage--sample luggage-mondrian-colors)))
                      ((< w mw)
                       (let ((y (split b d)))
                         (canvas a b c y)
                         (canvas a y c d)))
                      ((< h mh)
                       (let ((x (split a c)))
                         (canvas a b x d)
                         (canvas x b c d)))
                      (t
                       (let ((x (split a c))
                             (y (split b d)))
                         (canvas a b x y)
                         (canvas x b c y)
                         (canvas a y x d)
                         (canvas x y c d)))))))
      (canvas 0 0 w h)
      (luggage--show "Mondrian" svg))))

(defun luggage-mondrian-new-york-city ()
  "Generate a random image similar to Mondrian's New York City I."
  (interactive)
  (let* ((s 400) (l 10) (n (+ 5 (random 4)))
         (xs (list 0 s)) (ys (list 0 s))
         (svg (svg-create s s :stroke-width l))
         (colors (cl-remove "white" luggage-mondrian-colors
                            :key #'car :test #'equal)))
    (dotimes (_ n)
      (cl-flet
          ((rnd (ns)
             (cl-loop for x = (/ (expt (/ (random 1001) 1000.0) 2) 2)
                   for r = (floor (* s (if (= (random 2) 0) x (- 1 x))))
                   until (> (or (cl-loop for n in ns minimize (abs (- n r)))
                                (1+ l))
                            l)
                   finally (return (cons r ns)))))
        (let ((c (luggage--sample colors)))
          (setq xs (rnd xs) ys (rnd ys))
          (let ((x (car xs)) (y (car ys)))
            (svg-line svg x 0 x s :stroke c)
            (svg-line svg 0 y s y :stroke c)))))
    (luggage--show "Mondrian New York City" svg)))

(defun luggage-10-print ()
  "10 PRINT CHR$(205.5+RND(1)); : GOTO 10"
  (interactive)
  (let ((svg (svg-create 400 400
                         :stroke-width 4
                         :stroke-linecap "round"
                         :stroke-color "black")))
    (dotimes (x 40)
      (dotimes (y 40)
        (if (= (random 2) 0)
            (svg-line svg (* 10 x) (* 10 y) (* 10 (1+ x)) (* 10 (1+ y)))
          (svg-line svg (* 10 x) (* 10 (1+ y)) (* 10 (1+ x)) (* 10 y)))))
    (luggage--show "10-PRINT" svg)))

(provide 'luggage)
;;; luggage.el ends here
