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
  '((t :foreground "#AC4142"))
  "Face for medium red icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lred
  '((t :foreground "#C97071"))
  "Face for light red icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dred
  '((t :foreground "#BE2F31"))
  "Face for dark red icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-red
  '((((background dark)) :inherit all-the-icons-dred)
    (((background light)) :inherit all-the-icons-red))
  "Face for background-color-dependent red icons."
  :group 'all-the-icons-faces)

;; green
(defface all-the-icons-green
  '((t :foreground "#90A959"))
  "Face for medium green icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lgreen
  '((t :foreground "#B2C38B"))
  "Face for light green icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dgreen
  '((t :foreground "#66783E"))
  "Face for dark green icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-green
  '((((background dark)) :inherit all-the-icons-dgreen)
    (((background light)) :inherit all-the-icons-green))
  "Face for background-color-dependent green icons."
  :group 'all-the-icons-faces)

;; yellow
(defface all-the-icons-yellow
  '((t :foreground "#F4BF75"))
  "Face for medium yellow icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lyellow
  '((t :foreground "#FAE0BC"))
  "Face for light yellow icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dyellow
  '((t :foreground "#EE9E2E"))
  "Face for dark yellow icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-yellow
  '((((background dark)) :inherit all-the-icons-dyellow)
    (((background light)) :inherit all-the-icons-yellow))
  "Face for background-color-dependent yellow icons."
  :group 'all-the-icons-faces)

;; blue
(defface all-the-icons-blue
  '((t :foreground "#6A9FB5"))
  "Face for medium blue icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lblue
  '((t :foreground "#9DC0CE"))
  "Face for light blue icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dblue
  '((t :foreground "#46788d"))
  "Face for dark blue icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-blue
  '((((background dark)) :inherit all-the-icons-dblue)
    (((background light)) :inherit all-the-icons-blue))
  "Face for background-color-dependent blue icons."
  :group 'all-the-icons-faces)

;; maroon
(defface all-the-icons-maroon
  '((t :foreground "#8F5536"))
  "Face for medium maroon icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lmaroon
  '((t :foreground "#BE7953"))
  "Face for light maroon icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dmaroon
  '((t :foreground "#7C4426"))
  "Face for dark maroon icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-maroon
  '((((background dark)) :inherit all-the-icons-dmaroon)
    (((background light)) :inherit all-the-icons-maroon))
  "Face for background-color-dependent maroon icons."
  :group 'all-the-icons-faces)

;; purple
(defface all-the-icons-purple
  '((t :foreground "#AA759F"))
  "Face for medium purple icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lpurple
  '((t :foreground "#C7A4C0"))
  "Face for light purple icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dpurple
  '((t :foreground "#825078"))
  "Face for dark purple icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-purple
  '((((background dark)) :inherit all-the-icons-dpurple)
    (((background light)) :inherit all-the-icons-purple))
  "Face for background-color-dependent purple icons."
  :group 'all-the-icons-faces)

;; orange
(defface all-the-icons-orange
  '((t :foreground "#D28445"))
  "Face for medium orange icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lorange
  '((t :foreground "#E1AD83"))
  "Face for light orange icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dorange
  '((t :foreground "#A35F27"))
  "Face for dark orange icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-orange
  '((((background dark)) :inherit all-the-icons-dorange)
    (((background light)) :inherit all-the-icons-orange))
  "Face for background-color-dependent orange icons."
  :group 'all-the-icons-faces)

;; cyan
(defface all-the-icons-cyan
  '((t :foreground "#75B5AA"))
  "Face for medium cyan icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lcyan
  '((t :foreground "#A7D0C9"))
  "Face for light cyan icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dcyan
  '((t :foreground "#4D9085"))
  "Face for dark cyan icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-cyan
  '((((background dark)) :inherit all-the-icons-dcyan)
    (((background light)) :inherit all-the-icons-cyan))
  "Face for background-color-dependent cyan icons."
  :group 'all-the-icons-faces)

;; pink
(defface all-the-icons-pink
  '((t :foreground "#FF00CC"))
  "Face for medium pink icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lpink
  '((t :foreground "#FF4DDB"))
  "Face for light pink icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dpink
  '((t :foreground "#CC00A3"))
  "Face for dark pink icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-pink
  '((((background dark)) :inherit all-the-icons-dpink)
    (((background light)) :inherit all-the-icons-pink))
  "Face for background-color-dependent pink icons."
  :group 'all-the-icons-faces)

;; grey
(defface all-the-icons-grey
  '((t :foreground "#A5A5A5"))
  "Face for medium grey icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lgrey
  '((t :foreground "#7F7F7F"))
  "Face for light grey icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dgrey
  '((t :foreground "#7F7F7F"))
  "Face for dark grey icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-grey
  '((((background dark)) :inherit all-the-icons-dgrey)
    (((background light)) :inherit all-the-icons-grey))
  "Face for background-color-dependent grey icons."
  :group 'all-the-icons-faces)

(define-obsolete-face-alias 'all-the-icons-silver 'all-the-icons-grey "6.0.0")
(define-obsolete-face-alias 'all-the-icons-lsilver 'all-the-icons-lgrey "6.0.0")
(define-obsolete-face-alias 'all-the-icons-dsilver 'all-the-icons-dgrey "6.0.0")
(define-obsolete-face-alias 'all-the-icons-red-alt 'all-the-icons-auto-red "6.0.0")
(define-obsolete-face-alias 'all-the-icons-blue-alt 'all-the-icons-auto-blue "6.0.0")
(define-obsolete-face-alias 'all-the-icons-purple-alt 'all-the-icons-auto-purple "6.0.0")
(define-obsolete-face-alias 'all-the-icons-cyan-alt 'all-the-icons-auto-cyan "6.0.0")

(provide 'all-the-icons-faces)
;;; all-the-icons-faces.el ends here
