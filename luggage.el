;;; luggage.el --- Largely Undesired Gadget: Generative Art Gallery for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Omar Antolín Camarena

;; Author: Omar Antolín Camarena;; -*- lexical-binding: t; -*- <omar@matem.unam.mx>
;; Keywords: games, hypermedia, hypermedia, hypermedia

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

(defun lugagge-mondrian ()
  "Produce a random Mondrian-like image."
  (interactive)
  (let* ((w 400) (h 400) (mw 70) (mh 70) (sw (/ w 2)) (sh (/ h 2))
         (svg (svg-create w h)))
    (cl-labels ((split (u v)
                  (let ((p (/ (+ 33 (random 35)) 100.0)))
                    (floor (+ (* p u) (* (- 1 p) v)))))
                (color ()
                  (pcase (random 6)
                    ((or 0 1 2) "white")
                    (3 "yellow")
                    (4 "red")
                    (5 "blue")))
                (canvas (a b c d)
                  (let ((w (- c a)) (h (- d b)))
                    (cond
                      ((or (and (< w mw) (< h mh))
                           (not (or (> w sw) (> h sh) (= (random 2) 0))))
                       (svg-rectangle svg a b w h
                                      :stroke-width 5 :stroke "black"
                                      :fill-color (color)))
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
      (let ((buf (get-buffer-create "*Mondrian*")))
        (with-current-buffer buf (svg-insert-image svg))
        (pop-to-buffer buf)))))

(provide 'luggage)
;;; luggage.el ends here

