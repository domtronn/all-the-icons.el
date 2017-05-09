;;; all-the-icons-test.el --- Tests for `all-the-icons'

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: test
;; Created: 09 May 2017

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

;; Test cases for `all-the-icons'

;; Define all tests for mode icons
(require 'f)

(defvar all-the-icons--root-test (f-dirname (f-this-file)))
(defvar all-the-icons--root-code (f-parent all-the-icons--root-test))

(require 'all-the-icons (expand-file-name "all-the-icons.el" all-the-icons--root-code))

(cl-loop
 for alist in (apropos-internal "^all-the-icons-[a-z\\-]*icon-alist$")
 do (cl-loop
     for config in (symbol-value alist)
     do (cl-destructuring-bind (mode f &optional icon &rest args) config
          (when icon
            (eval
             `(ert-deftest ,(intern (format "all-the-icons--validate:%s:%s" alist mode)) ()
                ,(format "A test to validate that the config for %s in `%s' is correct" icon alist)
                (should (funcall ',f ,icon))))))))

(ert "all-the-icons--validate:")

(provide 'all-the-icons-test)
;;; all-the-icons-test.el ends here
