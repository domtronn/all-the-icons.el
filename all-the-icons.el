;;; all-the-icons.el --- A library for inserting Developer icons

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((dash "20160306.1222") (flycheck "20160307.739") (emacs "24.3"))
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

;;; Code:

(require 'dash)

(require 'data-alltheicons  "./data/data-alltheicons.el")
(require 'data-devicons     "./data/data-devicons.el")
(require 'data-faicons      "./data/data-faicons.el")
(require 'data-fileicons    "./data/data-fileicons.el")
(require 'data-octicons     "./data/data-octicons.el")
(require 'data-weathericons "./data/data-weathericons.el")

;; red
(defvar ati/red "#AC4142")
(defvar ati/lred "#EB595A")
(defvar ati/dred "#843031")
;; green
(defvar ati/green "#90A959")
(defvar ati/lgreen "#C6E87A")
(defvar ati/dgreen "#6D8143")

;; yellow
(defvar ati/yellow "#FFD446")
(defvar ati/lyellow "#FFC16D")
(defvar ati/dyellow "#B48D56")

;; blue
(defvar ati/blue "#6A9FB5")
(defvar ati/lblue "#8FD7F4")
(defvar ati/dblue "#446674")

;; maroon
(defvar ati/maroon "#8F5536")
(defvar ati/lmaroon "#CE7A4E")
(defvar ati/dmaroon "#72584B")

;; purple
(defvar ati/purple "#AA759F")
(defvar ati/lpurple "#E69DD6")
(defvar ati/dpurple "#694863")

;; orange
(defvar ati/orange "#D28445")
(defvar ati/lorange "#FFA151")
(defvar ati/dorange "#915B2D")

;; cyan
(defvar ati/cyan "#75B5AA")
(defvar ati/lcyan "#A5FDEC")
(defvar ati/dcyan "#48746D")

;; pink
(defvar ati/pink "#F2B4B8")
(defvar ati/lpink "#FFBDC1")
(defvar ati/dpink "#B18286")

;; silver
(defvar ati/silver "#716E68")
(defvar ati/lsilver "#B9B6AA")
(defvar ati/dsilver "#838484")

(defvar ati/scale-factor 1.2)
(defvar ati/default-adjust -0.2)
(defvar ati/color-icons t)
(defvar ati/icon-alist
  '(
    ;; Meta
    ("\\.tags"          ati/octicon "tag" 1.0 0.0 ati/blue)
    ("^TAGS$"           ati/octicon "tag" 1.0 0.0 ati/blue)
    ("\\.log"           ati/octicon "bug" 1.0 0.0 ati/maroon)

    ;;
    ("\\.key$"          ati/octicon "key" nil 0.0 ati/lblue)
    ("\\.pem$"          ati/octicon "key" nil 0.0 ati/orange)
    ("\\.p12$"          ati/octicon "key" nil 0.0 ati/dorange)
    ("\\.crt$"          ati/octicon "key" nil 0.0 ati/lblue)
    ("\\.pub$"          ati/octicon "key" nil 0.0 ati/blue)

    ("^TODO$"           ati/octicon "checklist" nil 0.0 ati/lyellow)
    ("^LICENSE$"        ati/octicon "book" 1.0 0.0 ati/blue)
    ("^readme"          ati/octicon "book" 1.0 0.0 ati/lcyan)

    ("\\.fish"          ati/devicon "terminal" nil nil ati/lpink)
    ("\\.zsh"           ati/devicon "terminal" nil nil ati/lcyan)

    ;; Config
    ("\\.node"          ati/devicon "nodejs-small" 1.0 nil ati/green)
    ("\\.babelrc$"      ati/fileicon "babel" nil nil ati/yellow)
    ("\\.bashrc$"       ati/alltheicon "script" 0.9 nil ati/dpink)
    ("\\.bowerrc$"      ati/devicon "bower" 1.2 nil ati/silver)
    ("^bower.json$"     ati/devicon "bower" 1.2 nil ati/lorange)
    ("\\.ini$"          ati/octicon "settings" nil 0.0 ati/yellow)
    ("\\.eslintignore"  ati/fileicon "eslint" 0.8 nil ati/purple)
    ("\\.eslint"        ati/fileicon "eslint" 0.8 nil ati/lpurple)
    ("\\.git"           ati/alltheicon "git" 1.0 nil ati/lred)
    ("nginx"            ati/fileicon "nginx" 0.9 nil ati/dgreen)
    ("apache"           ati/alltheicon "apache" 0.9 nil ati/dgreen)

    ("\\.dockerignore$" ati/fileicon "dockerfile" 1.2 nil ati/dblue)
    ("^\\.?Dockerfile"      ati/fileicon "dockerfile" nil nil ati/blue)
    ("^Brewfile$"       ati/faicon "beer" nil nil ati/lsilver)
    ("\\.npmignore"     ati/fileicon "npm" nil nil ati/dred)
    ("^package.json$"   ati/fileicon "npm" nil nil ati/red)

    ;; ;; AWS
    ("^stack.*.json$" ati/alltheicon "aws" nil nil ati/orange)
    
    
    ("\\.[jc]son$"      ati/octicon "settings" nil 0.0 ati/yellow)
    ("\\.yml$"          ati/octicon "settings" nil 0.0 ati/dyellow)

    ("\\.pkg$"          ati/octicon "package" nil 0.0 ati/dsilver)
    ("\\.rpm$"          ati/octicon "package" nil 0.0 ati/dsilver)

    ("\\.elc$"          ati/octicon "file-binary" nil 0.0 ati/dsilver)

    ("\\.gz$"          ati/octicon "file-binary" nil 0.0 ati/lmaroon)
    ("\\.zip$"          ati/octicon "file-zip" nil 0.0 ati/lmaroon)
    ("\\.7z$"           ati/octicon "file-zip" nil 0.0 ati/lmaroon)


    ;; lock files
    ("~$"               ati/octicon "lock" nil 0.0 ati/maroon)

    ("\\.dmg$"          ati/octicon "tools" nil 0.0 ati/lsilver)
    ("\\.dll$"          ati/faicon "cogs" nil nil ati/silver)
    ("\\.DS_STORE$"     ati/faicon "cogs" nil nil ati/silver)

    ;; Source Codes
    ("\\.scpt$"         ati/fileicon "apple" nil nil ati/pink)
    ("\\.aup$"          ati/fileicon "audacity" nil nil ati/yellow)

    ("\\.java$"         ati/devicon "java" 1.0 nil ati/purple)
    
    ("\\.mp3$"          ati/faicon "volume-up" nil nil ati/dred)
    ("\\.wav$"          ati/faicon "volume-up" nil nil ati/dred)
    ("\\.m4a$"          ati/faicon "volume-up" nil nil ati/dred)

    ("\\.jl$"           ati/fileicon "julia" nil 0.0 ati/purple)
    ("\\.matlab$"       ati/fileicon "matlab" nil nil ati/orange)

    ("\\.pl$"           ati/alltheicon "perl" nil nil ati/lorange)
    ("\\.pl6$"          ati/fileicon "perl6" nil nil ati/cyan)
    ("\\.pod$"          ati/devicon "perl" 1.2 nil ati/lgreen)

    ("\\.php$"          ati/fileicon "php" nil nil ati/lsilver)
    ("\\.pony$"         ati/fileicon "pony" nil nil ati/maroon)
    ("\\.prol?o?g?$"    ati/devicon "prolog" 1.1 nil ati/lmaroon)
    ("\\.py$"           ati/devicon "python" 1.0 nil ati/dblue)

    ("\\.gem$"          ati/devicon "ruby-rough" nil nil ati/red)
    ("\\.rb$"           ati/octicon "ruby" nil 0.0 ati/lred)
    ("\\.rs$"           ati/devicon "rust" 1.2 nil ati/maroon)
    ("\\.rlib$"         ati/devicon "rust" 1.2 nil ati/dmaroon)
    ("\\.r[ds]?x?$"     ati/fileicon "R" nil nil ati/lblue)

    ("\\.scala$"        ati/alltheicon "scala" nil nil ati/red)

    ("\\.swift$"        ati/devicon "swift" 1.0 -0.1 ati/green)
    
    ("-?spec\\.js$"     ati/alltheicon "jasmine" 0.9 -0.1 ati/lpurple)
    ("-?test\\.js$"     ati/alltheicon "jasmine" 0.9 -0.1 ati/lpurple)
    ("-?spec\\."        ati/faicon "flask" 1.0 0.0 ati/dgreen)
    ("-?test\\."        ati/faicon "flask" 1.0 0.0 ati/dgreen)

    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("\\.c$"            ati/alltheicon "c-line" nil nil ati/blue)
    ("\\.h$"            ati/alltheicon "c-line" nil nil ati/purple)

    ("\\.cpp$"          ati/alltheicon "cplusplus-line" nil -0.2 ati/blue)
    ("\\.hpp$"          ati/alltheicon "cplusplus-line" nil -0.2 ati/purple)

    ("\\.csx?$"         ati/alltheicon "csharp-line" nil nil ati/dblue)

    ("\\.clj$"          ati/devicon "clojure" 1.0 nil ati/blue)

    ("\\.coffee$"       ati/alltheicon "coffeescript" 1.0 nil ati/maroon)
    ("\\.iced$"         ati/alltheicon "coffeescript" 1.0 nil ati/lmaroon)

    ;; Git
    ("^MERGE_"          ati/octicon "git-merge" nil 0.0 ati/red)
    ("^COMMIT_EDITMSG"  ati/octicon "git-commit" nil 0.0 ati/red)

    ;; Lisps
    ("\\.cl$"           ati/fileicon "clisp" nil nil ati/lorange)
    ("\\.l$"            ati/fileicon "lisp" nil nil ati/orange)
    ("\\.el$"           ati/fileicon "elisp" 1.0 -0.2 ati/purple)

    ;; Stylesheeting
    ("\\.css$"          ati/devicon "css3-full" nil nil ati/yellow)
    ("\\.scss$"         ati/devicon "sass" nil nil ati/pink)
    ("\\.sass$"         ati/devicon "sass" nil nil ati/dpink)
    ("\\.less$"         ati/alltheicon "less" 0.8 nil ati/dyellow)
    ("\\.postcss$"      ati/fileicon "postcss" nil nil ati/dred)
    ("\\.sss$"          ati/fileicon "postcss" nil nil ati/dred)
    ("\\.styl$"         ati/devicon "stylus" nil nil ati/lgreen)
    ("stylelint"        ati/fileicon "stylelint" nil nil ati/lyellow)
    ("\\.csv$"          ati/octicon "graph" nil 0.0 ati/dblue)

    ("\\.hs$"           ati/devicon "haskell" 1.0 nil ati/red)

    ;; Web modes
    ("\\.haml$"         ati/fileicon "haml" nil nil ati/lyellow)
    ("\\.html?$"        ati/devicon "html5" nil nil ati/orange)
    ("\\.erb$"          ati/devicon "html5" nil nil ati/lred)
    ("\\.slim$"         ati/octicon "dashboard" nil 0.0 ati/yellow)
    ("\\.jade$"         ati/fileicon "jade" nil nil ati/red)
    ("\\.pug$"          ati/fileicon "pug" nil nil ati/red)

    ;; JavaScript
    ("^gulpfile"        ati/devicon "gulp" 1.0 nil ati/lred)
    ("^gruntfile"       ati/devicon "grunt" 1.0 -0.1 ati/lyellow)

    ("\\.d3\\.?js"      ati/alltheicon "d3" 0.8 nil ati/lgreen)
    
    ("\\.react"         ati/devicon "react" 1.1 nil ati/lblue)
    ("\\.js$"           ati/alltheicon "javascript" 0.9 nil ati/yellow)
    ("\\.es[0-9]$"      ati/alltheicon "javascript" 0.9 nil ati/yellow)
    ("\\.jsx$"          ati/fileicon "jsx" 0.8 nil ati/dyellow)
    ("\\.njs$"          ati/devicon "nodejs-small" 1.2 nil ati/lgreen)

    ;; File Types
    ("\\.ico$"          ati/octicon "file-media" nil 0.0 ati/orange)
    ("\\.png$"          ati/octicon "file-media" nil 0.0 ati/blue)
    ("\\.gif$"          ati/octicon "file-media" nil 0.0 ati/green)
    ("\\.jpe?g$"        ati/octicon "file-media" nil 0.0 ati/dblue)
    ("\\.svg$"          ati/alltheicon "svg" 0.9 nil ati/lgreen)

    ;; Video
    ("\\.mov"           ati/faicon "film" nil nil ati/blue)
    ("\\.mp4"           ati/faicon "film" nil nil ati/blue)
    ("\\.ogv"           ati/faicon "film" nil nil ati/dblue)

    ;; Fonts
    ("\\.ttf$"          ati/octicon "pencil" nil 0.0 ati/dcyan)
    ("\\.woff2?$"       ati/octicon "pencil" nil 0.0 ati/cyan)

    ;; Doc
    ("\\.pdf"           ati/octicon "file-pdf" nil 0.0 ati/dred)
    ("\\.te?xt"         ati/octicon "file-text" nil 0.0 ati/cyan)
    ("\\.doc[xm]?$"     ati/fileicon "word" nil nil ati/blue)
    ("\\.texi?$"        ati/fileicon "tex" nil nil ati/lred)
    ("\\.md$"           ati/octicon "markdown" nil 0.0 ati/lblue)
    ("\\.bib$"          ati/fileicon "bib" nil nil ati/maroon)
    ("\\.org$"          ati/fileicon "org" nil nil ati/lgreen)

    ("\\.pp[st]$"       ati/fileicon "ppt" nil nil ati/orange)
    ("\\.pp[st]x$"      ati/fileicon "ppt" nil nil ati/red)
    ("\\.knt$"          ati/fileicon "presentation" nil nil ati/cyan)

    ("bookmark"         ati/octicon "bookmark" 1.1 0.0 ati/lpink)
    ("\\.cache$"        ati/octicon "database" 1.0 0.0 ati/green)

    ("^\\."             ati/octicon "gear" nil 0.0)
    ("."                ati/faicon "file-o" 0.8 0.0 ati/dsilver)))

(defvar ati/dir-icon-alist
  '(
    ("trash"            ati/faicon "trash-o" 1.2 -0.1)
    ("dropbox"          ati/faicon "dropbox" 1.2 -0.1)
    ("google[ _-]drive" ati/devicon "google-drive" 1.3 -0.1)
    ("atom"             ati/devicon "atom" 1.2 -0.1)
    ("documents"        ati/faicon "book" 1.2 -0.1)
    ("download"         ati/octicon "cloud-download" 1.2 -0.1)
    ("desktop"          ati/faicon "desktop" 1.2 -0.1)
    ("pictures"         ati/faicon "picture-o" 1.2 -0.1)
    ("photos"           ati/faicon "camera-retro" 1.2 -0.1)
    ("music"            ati/faicon "headphones" 1.2 -0.1)
    ("movies"           ati/faicon "video-camera" 1.2 -0.1)
    ("code"             ati/octicon "code" 1.2 -0.1)
    ("workspace"        ati/octicon "code" 1.2 -0.1)
    ("."                ati/octicon "file-directory" 1.2)
    ))

(defvar ati/weather-icon-alist
  '(
    ("tornado" ati/wicon "tornado")
    ("hurricane" ati/wicon "hurricane")
    ("thunderstorms" ati/wicon "thunderstorm")
    ("sunny" ati/wicon "day-sunny")
    ("rain.*snow" ati/wicon "rain-mix")
    ("rain.*hail" ati/wicon "rain-mix")
    ("sleet" ati/wicon "sleet")
    ("hail" ati/wicon "hail")
    ("drizzle" ati/wicon "sprinkle")
    ("rain" ati/wicon "showers" 1.1 0.0)
    ("showers" ati/wicon "showers")
    ("blowing.*snow" ati/wicon "snow-wind")
    ("snow" ati/wicon "snow")
    ("dust" ati/wicon "dust")
    ("fog" ati/wicon "fog")
    ("haze" ati/wicon "day-haze")
    ("smoky" ati/wicon "smoke")
    ("blustery" ati/wicon "cloudy-windy")
    ("windy" ati/wicon "cloudy-gusts")
    ("cold" ati/wicon "snowflake-cold")
    ("partly.*cloudy.*night" ati/wicon "night-alt-partly-cloudy")
    ("partly.*cloudy" ati/wicon "day-cloudy-high")
    ("cloudy.*night" ati/wicon "night-alt-cloudy")
    ("cxloudy.*day" ati/wicon "day-cloudy")
    ("cloudy" ati/wicon "cloudy")
    ("clear.*night" ati/wicon "night-clear")
    ("fair.*night" ati/wicon "stars")
    ("fair.*day" ati/wicon "horizon")
    ("hot" ati/wicon "hot")
    ("not.*available" ati/wicon "na")
    ))

(defvar ati/mode-icon-alist
  '(
    (emacs-lisp-mode          ati/fileicon "elisp" nil -0.1)
    (dired-mode               ati/octicon "file-directory" nil 0.0)
    (lisp-interaction-mode    ati/fileicon "lisp" nil -0.1)
    (js2-mode                 ati/devicon "javascript-badge" nil -0.1)
    (term-mode                ati/octicon "terminal" nil 0.0)
    (eshell-mode              ati/octicon "terminal" nil 0.0)
    (magit-status-mode        ati/alltheicon "git")
    (magit-refs-mode          ati/octicon "git-branch" nil 0.0)
    (magit-process-mode       ati/octicon "mark-github" nil 0.0)
    (magit-popup-mode         ati/alltheicon "git")
    (magit-diff-mode          ati/octicon "git-compare" nil 0.0)
    (ediff-mode               ati/octicon "git-compare" nil 0.0)
    (comint-mode              ati/faicon "terminal" nil 0.0)
    (eww-mode                 ati/faicon "firefox" nil -0.1)
    (org-agenda-mode          ati/octicon "checklist" nil 0.0)
    (cfw:calendar-mode        ati/octicon "calendar" nil 0.0)
    (jenkins-mode             ati/fileicon "jenkins")
    (ibuffer-mode             ati/faicon "files-o" nil 0.0)
    (messages-buffer-mode     ati/faicon "stack-overflow" nil -0.1)
    (help-mode                ati/faicon "info" nil -0.1)
    (benchmark-init/tree-mode ati/octicon "dashboard" nil 0.0)
    ))

;; ====================
;;   Functions Start
;; ====================

(defun ati/auto-mode-match? (&optional file)
  "Whether or not FILE's `major-mode' match against its `auto-mode-alist'."
  (let* ((file (or file (buffer-file-name) (buffer-name)))
         (auto-mode (ati/match-to-alist file auto-mode-alist)))
    (eq major-mode auto-mode)))

(defun ati/match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (cdr (--first (string-match (car it) file) alist)))

;; Icon functions
(defun ati-icon-for-dir (dir &optional chevron)
  "Format an icon for DIR with CHEVRON similar to tree based directories.

Produces different symbols by inspeting DIR to distinguish
symlinks and git repositories which do not depend on the
directory contents"
  (let* ((matcher (ati/match-to-alist (file-name-base dir) ati/dir-icon-alist))
         (path (expand-file-name dir))
         (chevron (or (ati/octicon (format "chevron-%s" chevron) 0.8 -0.1) ""))
         (icon (cond
                ((file-symlink-p path)
                 (ati/octicon "file-symlink-directory" 1.2))
                ((file-exists-p (format "%s/.git" path))
                 (ati/octicon "repo" 1.2))
                (t (apply (car matcher) (cdr matcher))))))
    (format "\t%s\t%s " chevron icon)))

(defun ati-icon-for-buffer ()
  "Get the formatted icon for the current buffer.

This function priotises the use of the buffers file extension to
discern the icon when its `major-mode' matches its auto mode,
otherwise it will use the buffers `major-mode' to decide its
icon."
  (ati//icon-info-for-buffer))

(defun ati-icon-family-for-buffer ()
  "Get the icon font family for the current buffer."
  (ati//icon-info-for-buffer "family"))

;; Icon Functions

(defun ati-icon-for-file (file)
  "Get the formatted icon for FILE."
  (let ((icon (ati/match-to-alist file ati/icon-alist)))
    (apply (car icon) (cdr icon))))

(defun ati-icon-for-mode (mode)
  "Get the formatted icon for MODE."
  (let ((icon (cdr (assoc mode ati/mode-icon-alist))))
    (if icon (apply (car icon) (cdr icon)) mode)))

;; Family Face Functions
(defun ati-icon-family-for-file (file)
  "Get the icons font family for FILE."
  (let ((icon (ati/match-to-alist file ati/icon-alist)))
    (funcall (intern (format "%s-family" (car icon))))))

(defun ati-icon-family-for-mode (mode)
  "Get the icons font family for MODE."
  (let ((icon (cdr (assoc mode ati/mode-icon-alist))))
    (if icon (funcall (intern (format "%s-family" (car icon)))) nil)))

(defun ati//icon-info-for-buffer (&optional f)
  "Get icon info for the current buffer.

When F is provided, the info function is calculated with the format
`ati-icon-%s-for-file' or `ati-icon-%s-for-mode'."
  (let* ((base-f (concat "ati-icon" (when f (format "-%s" f))))
         (file-f (intern (concat base-f "-for-file")))
         (mode-f (intern (concat base-f "-for-mode"))))
    (if (and (buffer-file-name)
             (ati/auto-mode-match?))
      (funcall file-f (file-name-nondirectory (buffer-file-name)))
      (funcall mode-f major-mode))))

;; Weather icons
(defun ati-icon-for-weather (weather)
  "Get an icon for a WEATHER status."
  (let ((icon (ati/match-to-alist weather ati/weather-icon-alist)))
    (if icon (apply (car icon) (cdr icon)) weather)))

;; Definitions

(defun ati//function-name (name)
  "Get the symbol for an icon function name for icon set NAME."
  (intern (concat "ati/" (downcase (symbol-name name)))))

(defun ati//family-name (name)
  "Get the symbol for an icon family function for icon set NAME."
  (intern (concat "ati/" (downcase (symbol-name name)) "-family")))

(defmacro deficon (name alist family)
  "Macro to geneate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `ati/NAME'.

ALIST is the alist containing maps between icon names and the
UniCode for the character.  All of these can be found in the data
directory of this package.

FAMILY is the font family to use for the icons."
  `(prog1
       (defun ,(ati//family-name name) () ,family);
       (defun ,(ati//function-name name) (icon-name &optional height v-adjust col)
         (let ((icon (cdr (assoc icon-name ,alist)))
               (col (or (and ati/color-icons (symbol-value col))
                        (face-attribute 'default :foreground)))
               (height  (* ati/scale-factor (or height 1.0)))
               (v-adjust (* ati/scale-factor (or v-adjust ati/default-adjust)))
               (family ,family))
           (propertize icon
                       'face `(:family ,family :height ,height :foreground ,col)
                       'display `(raise ,v-adjust))))))

(deficon alltheicon ati-data/alltheicons-alist "dev-icons")
(deficon octicon ati-data/octicons-alist       "github-octicons")
(deficon devicon ati-data/devicons-alist       "icomoon")
(deficon fileicon ati-data/file-icon-alist     "file-icons")
(deficon faicon ati-data/fa-icon-alist         "FontAwesome")
(deficon wicon ati-data/weather-icons-alist    "Weather Icons")

(provide 'all-the-icons)

;;; all-the-icons.el ends here
