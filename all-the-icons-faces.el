;;; all-the-icons-faces.el --- A module of faces for all-the-icons

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/domtronn/all-the-icons.el
;; Keywords: convenient, lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains all of the faces used by the package for
;; colouring icons

;;; Code:

(defgroup all-the-icons-faces nil
  "Manage how All The Icons icons are coloured and themed."
  :prefix "all-the-icons-"
  :group 'tools
  :group 'all-the-icons)


;; red
(defface all-the-icons-red
  '((((background dark)) :foreground "#AC4142")
    (((background light)) :foreground "#AC4142"))
  "Face for red icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lred
  '((((background dark)) :foreground "#EB595A")
    (((background light)) :foreground "#EB595A"))
  "Face for lred icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dred
  '((((background dark)) :foreground "#843031")
    (((background light)) :foreground "#843031"))
  "Face for dred icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-red-alt
  '((((background dark)) :foreground "#ce5643")
    (((background light)) :foreground "#843031"))
  "Face for dred icons"
  :group 'all-the-icons-faces)

;; green
(defface all-the-icons-green
  '((((background dark)) :foreground "#90A959")
    (((background light)) :foreground "#90A959"))
  "Face for green icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lgreen
  '((((background dark)) :foreground "#C6E87A")
    (((background light)) :foreground "#3D6837"))
  "Face for lgreen icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dgreen
  '((((background dark)) :foreground "#6D8143")
    (((background light)) :foreground "#6D8143"))
  "Face for dgreen icons"
  :group 'all-the-icons-faces)

;; yellow
(defface all-the-icons-yellow
  '((((background dark)) :foreground "#FFD446")
    (((background light)) :foreground "#FFCC0E"))
  "Face for yellow icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lyellow
  '((((background dark)) :foreground "#FFC16D")
    (((background light)) :foreground "#FF9300"))
  "Face for lyellow icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dyellow
  '((((background dark)) :foreground "#B48D56")
    (((background light)) :foreground "#B48D56"))
  "Face for dyellow icons"
  :group 'all-the-icons-faces)

;; blue
(defface all-the-icons-blue
  '((((background dark)) :foreground "#6A9FB5")
    (((background light)) :foreground "#6A9FB5"))
  "Face for blue icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-blue-alt
  '((((background dark)) :foreground "#2188b6")
    (((background light)) :foreground "#2188b6"))
  "Face for blue icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lblue
  '((((background dark)) :foreground "#8FD7F4")
    (((background light)) :foreground "#677174"))
  "Face for lblue icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dblue
  '((((background dark)) :foreground "#446674")
    (((background light)) :foreground "#446674"))
  "Face for dblue icons"
  :group 'all-the-icons-faces)

;; maroon
(defface all-the-icons-maroon
  '((((background dark)) :foreground "#8F5536")
    (((background light)) :foreground "#8F5536"))
  "Face for maroon icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lmaroon
  '((((background dark)) :foreground "#CE7A4E")
    (((background light)) :foreground "#CE7A4E"))
  "Face for lmaroon icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dmaroon
  '((((background dark)) :foreground "#72584B")
    (((background light)) :foreground "#72584B"))
  "Face for dmaroon icons"
  :group 'all-the-icons-faces)

;; purple
(defface all-the-icons-purple
  '((((background dark)) :foreground "#AA759F")
    (((background light)) :foreground "#68295B"))
  "Face for purple icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lpurple
  '((((background dark)) :foreground "#E69DD6")
    (((background light)) :foreground "#E69DD6"))
  "Face for lpurple icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dpurple
  '((((background dark)) :foreground "#694863")
    (((background light)) :foreground "#694863"))
  "Face for dpurple icons"
  :group 'all-the-icons-faces)

;; orange
(defface all-the-icons-orange
  '((((background dark)) :foreground "#D4843E")
    (((background light)) :foreground "#D4843E"))
  "Face for orange icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lorange
  '((((background dark)) :foreground "#FFA500")
    (((background light)) :foreground "#FFA500"))
  "Face for lorange icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dorange
  '((((background dark)) :foreground "#915B2D")
    (((background light)) :foreground "#915B2D"))
  "Face for dorange icons"
  :group 'all-the-icons-faces)

;; cyan
(defface all-the-icons-cyan
  '((((background dark)) :foreground "#75B5AA")
    (((background light)) :foreground "#75B5AA"))
  "Face for cyan icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-cyan-alt
  '((((background dark)) :foreground "#61dafb")
    (((background light)) :foreground "#0595bd"))
  "Face for cyan icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lcyan
  '((((background dark)) :foreground "#A5FDEC")
    (((background light)) :foreground "#2C7D6E"))
  "Face for lcyan icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dcyan
  '((((background dark)) :foreground "#48746D")
    (((background light)) :foreground "#48746D"))
  "Face for dcyan icons"
  :group 'all-the-icons-faces)

;; pink
(defface all-the-icons-pink
  '((((background dark)) :foreground "#F2B4B8")
    (((background light)) :foreground "#FC505B"))
  "Face for pink icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lpink
  '((((background dark)) :foreground "#FFBDC1")
    (((background light)) :foreground "#FF505B"))
  "Face for lpink icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dpink
  '((((background dark)) :foreground "#B18286")
    (((background light)) :foreground "#7E5D5F"))
  "Face for dpink icons"
  :group 'all-the-icons-faces)

;; silver
(defface all-the-icons-silver
  '((((background dark)) :foreground "#716E68")
    (((background light)) :foreground "#716E68"))
  "Face for silver icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-lsilver
  '((((background dark)) :foreground "#B9B6AA")
    (((background light)) :foreground "#7F7869"))
  "Face for lsilver icons"
  :group 'all-the-icons-faces)
(defface all-the-icons-dsilver
  '((((background dark)) :foreground "#838484")
    (((background light)) :foreground "#838484"))
  "Face for dsilver icons"
  :group 'all-the-icons-faces)


(provide 'all-the-icons-faces)
;;; all-the-icons-faces.el ends here
