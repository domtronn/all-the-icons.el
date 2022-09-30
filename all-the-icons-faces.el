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
  "Face for red icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lred
  '((((background dark)) :foreground "#C97071")
    (((background light)) :foreground "#C97071"))
  "Face for lred icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dred
  '((((background dark)) :foreground "#BE2F31")
    (((background light)) :foreground "#BE2F31"))
  "Face for dred icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-red
  '((((background dark)) :foreground "#BE2F31")
    (((background light)) :foreground "#AC4142"))
  "Face for auto-red icons."
  :group 'all-the-icons-faces)

;; green
(defface all-the-icons-green
  '((((background dark)) :foreground "#90A959")
    (((background light)) :foreground "#90A959"))
  "Face for green icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lgreen
  '((((background dark)) :foreground "#B2C38B")
    (((background light)) :foreground "#B2C38B"))
  "Face for lgreen icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dgreen
  '((((background dark)) :foreground "#66783E")
    (((background light)) :foreground "#66783E"))
  "Face for dgreen icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-green
  '((((background dark)) :foreground "#66783E")
    (((background light)) :foreground "#90A959"))
  "Face for auto-green icons."
  :group 'all-the-icons-faces)

;; yellow
(defface all-the-icons-yellow
  '((((background dark)) :foreground "#F4BF75")
    (((background light)) :foreground "#F4BF75"))
  "Face for yellow icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lyellow
  '((((background dark)) :foreground "#FAE0BC")
    (((background light)) :foreground "#FAE0BC"))
  "Face for lyellow icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dyellow
  '((((background dark)) :foreground "#EE9E2E")
    (((background light)) :foreground "#EE9E2E"))
  "Face for dyellow icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-yellow
  '((((background dark)) :foreground "#EE9E2E")
    (((background light)) :foreground "#F4BF75"))
  "Face for auto-yellow icons."
  :group 'all-the-icons-faces)

;; blue
(defface all-the-icons-blue
  '((((background dark)) :foreground "#6A9FB5")
    (((background light)) :foreground "#6A9FB5"))
  "Face for blue icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lblue
  '((((background dark)) :foreground "#9DC0CE")
    (((background light)) :foreground "#9DC0CE"))
  "Face for lblue icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dblue
  '((((background dark)) :foreground "#46788d")
    (((background light)) :foreground "#46788d"))
  "Face for dblue icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-blue
  '((((background dark)) :foreground "#46788d")
    (((background light)) :foreground "#6A9FB5"))
  "Face for auto-blue icons."
  :group 'all-the-icons-faces)

;; maroon
(defface all-the-icons-maroon
  '((((background dark)) :foreground "#8F5536")
    (((background light)) :foreground "#8F5536"))
  "Face for maroon icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lmaroon
  '((((background dark)) :foreground "#BE7953")
    (((background light)) :foreground "#BE7953"))
  "Face for lmaroon icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dmaroon
  '((((background dark)) :foreground "#7C4426")
    (((background light)) :foreground "#7C4426"))
  "Face for dmaroon icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-maroon
  '((((background dark)) :foreground "#7C4426")
    (((background light)) :foreground "#8F5536"))
  "Face for auto-maroon icons."
  :group 'all-the-icons-faces)

;; purple
(defface all-the-icons-purple
  '((((background dark)) :foreground "#AA759F")
    (((background light)) :foreground "#AA759F"))
  "Face for purple icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lpurple
  '((((background dark)) :foreground "#C7A4C0")
    (((background light)) :foreground "#C7A4C0"))
  "Face for lpurple icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dpurple
  '((((background dark)) :foreground "#825078")
    (((background light)) :foreground "#825078"))
  "Face for dpurple icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-purple
  '((((background dark)) :foreground "#825078")
    (((background light)) :foreground "#AA759F"))
  "Face for auto-purple icons."
  :group 'all-the-icons-faces)

;; orange
(defface all-the-icons-orange
  '((((background dark)) :foreground "#D28445")
    (((background light)) :foreground "#D28445"))
  "Face for orange icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lorange
  '((((background dark)) :foreground "#E1AD83")
    (((background light)) :foreground "#E1AD83"))
  "Face for lorange icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dorange
  '((((background dark)) :foreground "#A35F27")
    (((background light)) :foreground "#A35F27"))
  "Face for dorange icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-orange
  '((((background dark)) :foreground "#A35F27")
    (((background light)) :foreground "#D28445"))
  "Face for auto-orange icons."
  :group 'all-the-icons-faces)

;; cyan
(defface all-the-icons-cyan
  '((((background dark)) :foreground "#75B5AA")
    (((background light)) :foreground "#75B5AA"))
  "Face for cyan icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lcyan
  '((((background dark)) :foreground "#A7D0C9")
    (((background light)) :foreground "#A7D0C9"))
  "Face for lcyan icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dcyan
  '((((background dark)) :foreground "#4D9085")
    (((background light)) :foreground "#4D9085"))
  "Face for dcyan icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-cyan
  '((((background dark)) :foreground "#4D9085")
    (((background light)) :foreground "#75B5AA"))
  "Face for auto-cyan icons."
  :group 'all-the-icons-faces)

;; pink
(defface all-the-icons-pink
  '((((background dark)) :foreground "#FF00CC")
    (((background light)) :foreground "#FF00CC"))
  "Face for pink icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lpink
  '((((background dark)) :foreground "#FF4DDB")
    (((background light)) :foreground "#FF4DDB"))
  "Face for lpink icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dpink
  '((((background dark)) :foreground "#CC00A3")
    (((background light)) :foreground "#CC00A3"))
  "Face for dpink icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-pink
  '((((background dark)) :foreground "#CC00A3")
    (((background light)) :foreground "#FF00CC"))
  "Face for auto-pink icons."
  :group 'all-the-icons-faces)

;; silver
(defface all-the-icons-silver
  '((((background dark)) :foreground "#A5A5A5")
    (((background light)) :foreground "#A5A5A5"))
  "Face for silver icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-lsilver
  '((((background dark)) :foreground "#7F7F7F")
    (((background light)) :foreground "#7F7F7F"))
  "Face for lsilver icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-dsilver
  '((((background dark)) :foreground "#7F7F7F")
    (((background light)) :foreground "#7F7F7F"))
  "Face for dsilver icons."
  :group 'all-the-icons-faces)
(defface all-the-icons-auto-silver
  '((((background dark)) :foreground "#7F7F7F")
    (((background light)) :foreground "#7F7F7F"))
  "Face for auto-silver icons."
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
