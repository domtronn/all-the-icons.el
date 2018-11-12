;;; all-the-icons.el --- A library for inserting Developer icons

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Version: 3.1.4
;; Package-Requires: ((emacs "24.3") (memoize "1.0.1"))
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

;; This package is a utility for using and formatting various Icon
;; fonts within Emacs.  Icon Fonts allow you to propertize and format
;; icons the same way you would normal text.  This enables things such
;; as better scaling of and anti aliasing of the icons.

;; This package was inspired by

;; - `mode-icons' for Emacs, found at https://github.com/ryuslash/mode-icons
;; - `file-icons' for Atom, found at https://atom.io/packages/file-icons

;; Currently, this package provides an interface to the following Icon Fonts

;; - Atom File Icons,       found at https://atom.io/packages/file-icons
;; - FontAwesome Icons,     found at http://fontawesome.io/
;; - GitHub Octicons,       found at http://octicons.github.com
;; - Material Design Icons, found at http://google.github.io/material-design-icons/
;; - Weather Icons,         found at https://erikflowers.github.io/weather-icons/
;; - AllTheIcons,           a custom Icon Font maintained as part of this package

;; Requests for new icons will be accepted and added to the AllTheIcons Icon Font

;;; Usage:

;; The simplest usage for this package is to use the following functions;

;;   `all-the-icons-icon-for-buffer'
;;   `all-the-icons-icon-for-file'
;;   `all-the-icons-icon-for-mode'

;; Which can be used to get a formatted icon for the current buffer, a
;; file name or a major mode respectively.  e.g.

;;   (insert (all-the-icons-icon-for-file "foo.js"))

;; Inserts a JavaScript icon formatted like this

;;   #("some-icon" 0 1 (display (raise -0.24)
;;              face (:family "dev-icons" :height 1.08 :foreground "#FFD446")))

;; You can also insert icons directly using the individual icon family
;; functions

;;   `all-the-icons-alltheicon'     // Custom font with fewest icons
;;   `all-the-icons-devicon'        // Developer Icons
;;   `all-the-icons-faicon'         // Font Awesome Icons
;;   `all-the-icons-fileicon'       // File Icons from the Atom File Icons package
;;   `all-the-icons-octicon'        // GitHub Octicons
;;   `all-the-icons-material'       // Material Design Icons
;;   `all-the-icons-wicon'          // Weather Icons

;; You can call these functions with the icon name you want to insert, e.g.

;;   (all-the-icons-octicon "file-binary")  // GitHub Octicon for Binary File
;;   (all-the-icons-faicon  "cogs")         // FontAwesome icon for cogs
;;   (all-the-icons-wicon   "tornado")      // Weather Icon for tornado

;; A list of all the icon names for the different font families can be
;; found in the data directory, or by inspecting the alist variables.
;; All the alist variables are prefixed with `all-the-icons-data/'

;;; Code:
(require 'memoize)
(require 'cl-lib)

(require 'data-alltheicons  "./data/data-alltheicons.el")
(require 'data-faicons      "./data/data-faicons.el")
(require 'data-fileicons    "./data/data-fileicons.el")
(require 'data-octicons     "./data/data-octicons.el")
(require 'data-weathericons "./data/data-weathericons.el")
(require 'data-material     "./data/data-material.el")

(require 'all-the-icons-faces)

;;; Custom Variables
(defgroup all-the-icons nil
  "Manage how All The Icons formats icons."
  :prefix "all-the-icons-"
  :group 'appearance
  :group 'convenience)

(defcustom all-the-icons-color-icons t
  "Whether or not to include a foreground colour when formatting the icon."
  :group 'all-the-icons
  :type 'boolean)

(defcustom all-the-icons-scale-factor 1.2
  "The base Scale Factor for the `height' face property of an icon."
  :group 'all-the-icons
  :type 'number)

(defcustom all-the-icons-default-adjust -0.2
  "The default adjustment to be made to the `raise' display property of an icon."
  :group 'all-the-icons
  :type 'number)


(defcustom all-the-icons-mode-repositories
  '(".git"        ; Git VCS root dir
    ".hg"         ; Mercurial VCS root dir
    ".fslckout"   ; Fossil VCS root dir
    "_FOSSIL_"    ; Fossil VCS root DB on Windows
    ".bzr"        ; Bazaar VCS root dir
    "_darcs"      ; Darcs VCS root dir
    ".svn" ; Svn VCS root dir
    "CVS"  ; Csv VCS root dir
    )
  "A list of files considered to mark the root of a repository."
  :group 'all-the-icons
  :type '(repeat string))



(defvar all-the-icons-font-families '() "List of defined icon font families.")
(defvar all-the-icons-font-names '() "List of defined font file names this package was built with.")

(defvar all-the-icons-icon-alist
  '(
    ;; Meta
    ("\\.tags"          all-the-icons-octicon "tag"                     :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("^TAGS$"           all-the-icons-octicon "tag"                     :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("\\.log"           all-the-icons-octicon "bug"                     :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon)

    ;;
    ("\\.key$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lblue)
    ("\\.pem$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-orange)
    ("\\.p12$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-dorange)
    ("\\.crt$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lblue)
    ("\\.pub$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-blue)
    ("\\.gpg$"          all-the-icons-octicon "key"                     :v-adjust 0.0 :face all-the-icons-lblue)

    ("^TODO$"           all-the-icons-octicon "checklist"               :v-adjust 0.0 :face all-the-icons-lyellow)
    ("^LICENSE$"        all-the-icons-octicon "book"                    :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("^readme"          all-the-icons-octicon "book"                    :height 1.0 :v-adjust 0.0 :face all-the-icons-lcyan)

    ("\\.fish"          all-the-icons-alltheicon "terminal"             :face all-the-icons-lpink)
    ("\\.zsh"           all-the-icons-alltheicon "terminal"             :face all-the-icons-lcyan)
    ("\\.sh"            all-the-icons-alltheicon "terminal"             :face all-the-icons-purple)

    ;; Config
    ("\\.node"          all-the-icons-alltheicon "nodejs"               :height 1.0  :face all-the-icons-green)
    ("\\.babelrc$"      all-the-icons-fileicon "babel"                  :face all-the-icons-yellow)
    ("\\.bashrc$"       all-the-icons-alltheicon "script"               :height 0.9  :face all-the-icons-dpink)
    ("\\.bowerrc$"      all-the-icons-alltheicon "bower"                :height 1.0 :v-adjust 0.0 :face all-the-icons-silver)
    ("^bower.json$"     all-the-icons-alltheicon "bower"                :height 1.0 :v-adjust 0.0 :face all-the-icons-lorange)
    ("\\.ini$"          all-the-icons-octicon "settings"                :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.eslintignore"  all-the-icons-fileicon "eslint"                 :height 0.9  :face all-the-icons-purple)
    ("\\.eslint"        all-the-icons-fileicon "eslint"                 :height 0.9  :face all-the-icons-lpurple)
    ("\\.git"           all-the-icons-alltheicon "git"                  :height 1.0  :face all-the-icons-lred)
    ("nginx"            all-the-icons-fileicon "nginx"                  :height 0.9  :face all-the-icons-dgreen)
    ("apache"           all-the-icons-alltheicon "apache"               :height 0.9  :face all-the-icons-dgreen)
    ("^Makefile$"       all-the-icons-fileicon "gnu"                    :face all-the-icons-dorange)
    ("\\.mk$"           all-the-icons-fileicon "gnu"                    :face all-the-icons-dorange)

    ("\\.dockerignore$" all-the-icons-fileicon "dockerfile"             :height 1.2  :face all-the-icons-dblue)
    ("^\\.?Dockerfile"  all-the-icons-fileicon "dockerfile"             :face all-the-icons-blue)
    ("^Brewfile$"       all-the-icons-faicon "beer"                     :face all-the-icons-lsilver)
    ("\\.npmignore"     all-the-icons-fileicon "npm"                    :face all-the-icons-dred)
    ("^package.json$"   all-the-icons-fileicon "npm"                    :face all-the-icons-red)
    ("^package.lock.json$" all-the-icons-fileicon "npm"                 :face all-the-icons-dred)
    ("^yarn\.lock"      all-the-icons-fileicon "yarn"                   :face all-the-icons-blue-alt)

    ("\.xml$"           all-the-icons-faicon "file-code-o"              :height 0.95 :face all-the-icons-lorange)

    ;; ;; AWS
    ("^stack.*.json$"   all-the-icons-alltheicon "aws"                  :face all-the-icons-orange)

    
    ("^serverless\\.yml$" all-the-icons-faicon "bolt"                   :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.[jc]son$"      all-the-icons-octicon "settings"                :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.ya?ml$"        all-the-icons-octicon "settings"                :v-adjust 0.0 :face all-the-icons-dyellow)

    ("\\.pkg$"          all-the-icons-octicon "package"                 :v-adjust 0.0 :face all-the-icons-dsilver)
    ("\\.rpm$"          all-the-icons-octicon "package"                 :v-adjust 0.0 :face all-the-icons-dsilver)

    ("\\.elc$"          all-the-icons-octicon "file-binary"             :v-adjust 0.0 :face all-the-icons-dsilver)

    ("\\.gz$"           all-the-icons-octicon "file-binary"             :v-adjust 0.0 :face all-the-icons-lmaroon)
    ("\\.zip$"          all-the-icons-octicon "file-zip"                :v-adjust 0.0 :face all-the-icons-lmaroon)
    ("\\.7z$"           all-the-icons-octicon "file-zip"                :v-adjust 0.0 :face all-the-icons-lmaroon)

    ("\\.dat$"          all-the-icons-faicon "bar-chart"                :face all-the-icons-cyan :height 0.9)
    ;; lock files
    ("~$"               all-the-icons-octicon "lock"                    :v-adjust 0.0 :face all-the-icons-maroon)

    ("\\.dmg$"          all-the-icons-octicon "tools"                   :v-adjust 0.0 :face all-the-icons-lsilver)
    ("\\.dll$"          all-the-icons-faicon "cogs"                     :face all-the-icons-silver)
    ("\\.DS_STORE$"     all-the-icons-faicon "cogs"                     :face all-the-icons-silver)

    ;; Source Codes
    ("\\.scpt$"         all-the-icons-fileicon "apple"                  :face all-the-icons-pink)
    ("\\.aup$"          all-the-icons-fileicon "audacity"               :face all-the-icons-yellow)

    ("\\.elm"           all-the-icons-fileicon "elm"                    :face all-the-icons-blue)

    ("\\.erl$"          all-the-icons-alltheicon "erlang"               :face all-the-icons-red :v-adjust -0.1 :height 0.9)
    ("\\.hrl$"          all-the-icons-alltheicon "erlang"               :face all-the-icons-dred :v-adjust -0.1 :height 0.9)

    ("\\.eex$"          all-the-icons-alltheicon "elixir"               :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
    ("\\.ex$"           all-the-icons-alltheicon "elixir"               :face all-the-icons-lpurple :v-adjust -0.1 :height 0.9)
    ("\\.exs$"          all-the-icons-alltheicon "elixir"               :face all-the-icons-lred :v-adjust -0.1 :height 0.9)
    ("^mix.lock$"       all-the-icons-alltheicon "elixir"               :face all-the-icons-lyellow :v-adjust -0.1 :height 0.9)

    ("\\.java$"         all-the-icons-alltheicon "java"                 :height 1.0  :face all-the-icons-purple)

    ("\\.go$"           all-the-icons-alltheicon "go"                   :height 1.0  :face all-the-icons-blue)

    ("\\.mp3$"          all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.wav$"          all-the-icons-faicon "volume-up"                :face all-the-icons-dred)
    ("\\.m4a$"          all-the-icons-faicon "volume-up"                :face all-the-icons-dred)

    ("\\.jl$"           all-the-icons-fileicon "julia"                  :v-adjust 0.0 :face all-the-icons-purple)
    ("\\.matlab$"       all-the-icons-fileicon "matlab"                 :face all-the-icons-orange)

    ("\\.p[ml]$"        all-the-icons-alltheicon "perl"                 :face all-the-icons-lorange)
    ("\\.pl6$"          all-the-icons-fileicon "perl6"                  :face all-the-icons-cyan)
    ("\\.pod$"          all-the-icons-alltheicon "perldocs"             :height 1.2  :face all-the-icons-lgreen)

    ("\\.php$"          all-the-icons-fileicon "php"                    :face all-the-icons-lsilver)
    ("\\.pony$"         all-the-icons-fileicon "pony"                   :face all-the-icons-maroon)
    ("\\.prol?o?g?$"    all-the-icons-alltheicon "prolog"               :height 1.1  :face all-the-icons-lmaroon)
    ("\\.py$"           all-the-icons-alltheicon "python"               :height 1.0  :face all-the-icons-dblue)

    ("\\.rkt$"          all-the-icons-fileicon "racket"                 :height 1.2 :face all-the-icons-red)
    ("\\.gem$"          all-the-icons-alltheicon "ruby-alt"             :face all-the-icons-red)
    ("_?test\\.rb$"        all-the-icons-fileicon "test-ruby"            :height 1.0 :v-adjust 0.0 :face all-the-icons-red)
    ("_?test_helper\\.rb$" all-the-icons-fileicon "test-ruby"            :height 1.0 :v-adjust 0.0 :face all-the-icons-dred)
    ("\\.rb$"           all-the-icons-octicon "ruby"                    :v-adjust 0.0 :face all-the-icons-lred)
    ("\\.rs$"           all-the-icons-alltheicon "rust"                 :height 1.2  :face all-the-icons-maroon)
    ("\\.rlib$"         all-the-icons-alltheicon "rust"                 :height 1.2  :face all-the-icons-dmaroon)
    ("\\.r[ds]?x?$"     all-the-icons-fileicon "R"                      :face all-the-icons-lblue)

    ("\\.scala$"        all-the-icons-alltheicon "scala"                :face all-the-icons-red)
    ("\\.scm$"          all-the-icons-fileicon   "scheme"               :height 1.2 :face all-the-icons-red)
    ("\\.swift$"        all-the-icons-alltheicon "swift"                :height 1.0 :v-adjust -0.1 :face all-the-icons-green)

    ("-?spec\\.ts$"     all-the-icons-fileicon "test-typescript"        :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("-?test\\.ts$"     all-the-icons-fileicon "test-typescript"        :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("-?spec\\.js$"     all-the-icons-fileicon "test-js"                :height 1.0 :v-adjust 0.0 :face all-the-icons-lpurple)
    ("-?test\\.js$"     all-the-icons-fileicon "test-js"                :height 1.0 :v-adjust 0.0 :face all-the-icons-lpurple)
    ("-?spec\\.jsx$"    all-the-icons-fileicon "test-react"             :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt)
    ("-?test\\.jsx$"    all-the-icons-fileicon "test-react"             :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt)

    ("-?spec\\."        all-the-icons-fileicon "test-generic"           :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen)
    ("-?test\\."        all-the-icons-fileicon "test-generic"           :height 1.0 :v-adjust 0.0 :face all-the-icons-dgreen)

    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("\\.c$"            all-the-icons-alltheicon "c-line"               :face all-the-icons-blue)
    ("\\.h$"            all-the-icons-alltheicon "c-line"               :face all-the-icons-purple)
    ("\\.m$"            all-the-icons-fileicon "apple"                  :v-adjust 0.0 :height 1.0)
    ("\\.mm$"           all-the-icons-fileicon "apple"                  :v-adjust 0.0 :height 1.0)

    ("\\.c\\(c\\|pp\\|xx\\)$"   all-the-icons-alltheicon "cplusplus-line"       :v-adjust -0.2 :face all-the-icons-blue)
    ("\\.h\\(h\\|pp\\|xx\\)$"   all-the-icons-alltheicon "cplusplus-line"       :v-adjust -0.2 :face all-the-icons-purple)

    ("\\.csx?$"         all-the-icons-alltheicon "csharp-line"          :face all-the-icons-dblue)

    ("\\.cljc?$"        all-the-icons-alltheicon "clojure-line"         :height 1.0 :face all-the-icons-blue :v-adjust 0.0)
    ("\\.cljs$"         all-the-icons-fileicon "cljs"                   :height 1.0 :face all-the-icons-dblue :v-adjust 0.0)

    ("\\.coffee$"       all-the-icons-alltheicon "coffeescript"         :height 1.0  :face all-the-icons-maroon)
    ("\\.iced$"         all-the-icons-alltheicon "coffeescript"         :height 1.0  :face all-the-icons-lmaroon)

    ;; Git
    ("^MERGE_"          all-the-icons-octicon "git-merge"               :v-adjust 0.0 :face all-the-icons-red)
    ("^COMMIT_EDITMSG"  all-the-icons-octicon "git-commit"              :v-adjust 0.0 :face all-the-icons-red)

    ;; Lisps
    ("\\.cl$"           all-the-icons-fileicon "clisp"                  :face all-the-icons-lorange)
    ("\\.l\\(isp\\)?$"  all-the-icons-fileicon "lisp"                   :face all-the-icons-orange)
    ("\\.el$"           all-the-icons-fileicon "elisp"                  :height 1.0 :v-adjust -0.2 :face all-the-icons-purple)

    ;; Stylesheeting
    ("\\.css$"          all-the-icons-alltheicon "css3"                 :face all-the-icons-yellow)
    ("\\.scss$"         all-the-icons-alltheicon "sass"                 :face all-the-icons-pink)
    ("\\.sass$"         all-the-icons-alltheicon "sass"                 :face all-the-icons-dpink)
    ("\\.less$"         all-the-icons-alltheicon "less"                 :height 0.8  :face all-the-icons-dyellow)
    ("\\.postcss$"      all-the-icons-fileicon "postcss"                :face all-the-icons-dred)
    ("\\.sss$"          all-the-icons-fileicon "postcss"                :face all-the-icons-dred)
    ("\\.styl$"         all-the-icons-alltheicon "stylus"               :face all-the-icons-lgreen)
    ("stylelint"        all-the-icons-fileicon "stylelint"              :face all-the-icons-lyellow)
    ("\\.csv$"          all-the-icons-octicon "graph"                   :v-adjust 0.0 :face all-the-icons-dblue)

    ("\\.hs$"           all-the-icons-alltheicon "haskell"              :height 1.0  :face all-the-icons-red)

    ;; Web modes
    ("\\.inky-haml$"    all-the-icons-fileicon "haml"                   :face all-the-icons-lyellow)
    ("\\.haml$"         all-the-icons-fileicon "haml"                   :face all-the-icons-lyellow)
    ("\\.html?$"        all-the-icons-alltheicon "html5"                :face all-the-icons-orange)
    ("\\.inky-erb?$"    all-the-icons-alltheicon "html5"                :face all-the-icons-lred)
    ("\\.erb$"          all-the-icons-alltheicon "html5"                :face all-the-icons-lred)
    ("\\.hbs$"          all-the-icons-fileicon "moustache"              :face all-the-icons-green)
    ("\\.inky-slim$"    all-the-icons-octicon "dashboard"               :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.slim$"         all-the-icons-octicon "dashboard"               :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.jade$"         all-the-icons-fileicon "jade"                   :face all-the-icons-red)
    ("\\.pug$"          all-the-icons-fileicon "pug-alt"                :face all-the-icons-red)

    ;; JavaScript
    ("^gulpfile"        all-the-icons-alltheicon "gulp"                 :height 1.0  :face all-the-icons-lred)
    ("^gruntfile"       all-the-icons-alltheicon "grunt"                :height 1.0 :v-adjust -0.1 :face all-the-icons-lyellow)
    ("^webpack"         all-the-icons-fileicon "webpack"                :face all-the-icons-lblue)

    ("\\.d3\\.?js"      all-the-icons-alltheicon "d3"                   :height 0.8  :face all-the-icons-lgreen)

    ("\\.re$"            all-the-icons-fileicon "reason"                :height 1.0  :face all-the-icons-red-alt)
    ("\\.rei$"           all-the-icons-fileicon "reason"                :height 1.0  :face all-the-icons-dred)
    ("\\.ml$"            all-the-icons-fileicon "ocaml"                 :height 1.0  :face all-the-icons-lpink)
    ("\\.mli$"           all-the-icons-fileicon "ocaml"                 :height 1.0  :face all-the-icons-dpink)

    ("\\.react"         all-the-icons-alltheicon "react"                :height 1.1  :face all-the-icons-lblue)
    ("\\.d\\.ts$"       all-the-icons-fileicon "typescript"             :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
    ("\\.ts$"           all-the-icons-fileicon "typescript"             :height 1.0 :v-adjust -0.1 :face all-the-icons-blue-alt)
    ("\\.js$"           all-the-icons-alltheicon "javascript"           :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.es[0-9]$"      all-the-icons-alltheicon "javascript"           :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow)
    ("\\.jsx$"          all-the-icons-fileicon "jsx-2"                  :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
    ("\\.njs$"          all-the-icons-alltheicon "nodejs"               :height 1.2  :face all-the-icons-lgreen)
    ("\\.vue$"          all-the-icons-fileicon "vue"                    :face all-the-icons-lgreen)

    ;; File Types
    ("\\.ico$"          all-the-icons-octicon "file-media"              :v-adjust 0.0 :face all-the-icons-blue)
    ("\\.png$"          all-the-icons-octicon "file-media"              :v-adjust 0.0 :face all-the-icons-orange)
    ("\\.gif$"          all-the-icons-octicon "file-media"              :v-adjust 0.0 :face all-the-icons-green)
    ("\\.jpe?g$"        all-the-icons-octicon "file-media"              :v-adjust 0.0 :face all-the-icons-dblue)
    ("\\.svg$"          all-the-icons-alltheicon "svg"                  :height 0.9  :face all-the-icons-lgreen)

    ;; Video
    ("\\.mov"           all-the-icons-faicon "film"                     :face all-the-icons-blue)
    ("\\.mp4"           all-the-icons-faicon "film"                     :face all-the-icons-blue)
    ("\\.ogv"           all-the-icons-faicon "film"                     :face all-the-icons-dblue)

    ;; Fonts
    ("\\.ttf$"          all-the-icons-fileicon "font"                   :v-adjust 0.0 :face all-the-icons-dcyan)
    ("\\.woff2?$"       all-the-icons-fileicon "font"                   :v-adjust 0.0 :face all-the-icons-cyan)

    ;; Doc
    ("\\.pdf"           all-the-icons-octicon "file-pdf"                :v-adjust 0.0 :face all-the-icons-dred)
    ("\\.te?xt"         all-the-icons-octicon "file-text"               :v-adjust 0.0 :face all-the-icons-cyan)
    ("\\.doc[xm]?$"     all-the-icons-fileicon "word"                   :face all-the-icons-blue)
    ("\\.texi?$"        all-the-icons-fileicon "tex"                    :face all-the-icons-lred)
    ("\\.md$"           all-the-icons-octicon "markdown"                :v-adjust 0.0 :face all-the-icons-lblue)
    ("\\.bib$"          all-the-icons-fileicon "bib"                    :face all-the-icons-maroon)
    ("\\.org$"          all-the-icons-fileicon "org"                    :face all-the-icons-lgreen)

    ("\\.pp[st]$"       all-the-icons-fileicon "powerpoint"             :face all-the-icons-orange)
    ("\\.pp[st]x$"      all-the-icons-fileicon "powerpoint"             :face all-the-icons-red)
    ("\\.knt$"          all-the-icons-fileicon "powerpoint"             :face all-the-icons-cyan)

    ("bookmark"         all-the-icons-octicon "bookmark"                :height 1.1 :v-adjust 0.0 :face all-the-icons-lpink)
    ("\\.cache$"        all-the-icons-octicon "database"                :height 1.0 :v-adjust 0.0 :face all-the-icons-green)

    ("^\\*scratch\\*$"  all-the-icons-faicon "sticky-note"              :face all-the-icons-lyellow)
    ("^\\*scratch.*"    all-the-icons-faicon "sticky-note"              :face all-the-icons-yellow)
    ("^\\*new-tab\\*$"  all-the-icons-material "star"                     :face all-the-icons-cyan)

    ("^\\."             all-the-icons-octicon "gear"                    :v-adjust 0.0)
    ("."                all-the-icons-faicon "file-o"                   :height 0.8 :v-adjust 0.0 :face all-the-icons-dsilver)))

(defvar all-the-icons-dir-icon-alist
  '(
    ("trash"            all-the-icons-faicon "trash-o"          :height 1.2 :v-adjust -0.1)
    ("dropbox"          all-the-icons-faicon "dropbox"          :height 1.0 :v-adjust -0.1)
    ("google[ _-]drive" all-the-icons-alltheicon "google-drive" :height 1.3 :v-adjust -0.1)
    ("^atom$"           all-the-icons-alltheicon "atom"         :height 1.2 :v-adjust -0.1)
    ("documents"        all-the-icons-faicon "book"             :height 1.0 :v-adjust -0.1)
    ("download"         all-the-icons-faicon "cloud-download"   :height 0.9 :v-adjust -0.2)
    ("desktop"          all-the-icons-octicon "device-desktop"  :height 1.0 :v-adjust -0.1)
    ("pictures"         all-the-icons-faicon "picture-o"        :height 0.9 :v-adjust -0.2)
    ("photos"           all-the-icons-faicon "camera-retro"     :height 1.0 :v-adjust -0.1)
    ("music"            all-the-icons-faicon "music"            :height 1.0 :v-adjust -0.1)
    ("movies"           all-the-icons-faicon "film"             :height 0.9 :v-adjust -0.1)
    ("code"             all-the-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("workspace"        all-the-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("test"             all-the-icons-fileicon "test-dir"       :height 0.9)
    ("\\.git"           all-the-icons-alltheicon "git"          :height 1.0)
    ("."                all-the-icons-octicon "file-directory"  :height 1.0 :v-adjust -0.1)
    ))

(defvar all-the-icons-weather-icon-alist
  '(
    ("tornado"               all-the-icons-wicon "tornado")
    ("hurricane"             all-the-icons-wicon "hurricane")
    ("thunderstorms"         all-the-icons-wicon "thunderstorm")
    ("sunny"                 all-the-icons-wicon "day-sunny")
    ("rain.*snow"            all-the-icons-wicon "rain-mix")
    ("rain.*hail"            all-the-icons-wicon "rain-mix")
    ("sleet"                 all-the-icons-wicon "sleet")
    ("hail"                  all-the-icons-wicon "hail")
    ("drizzle"               all-the-icons-wicon "sprinkle")
    ("rain"                  all-the-icons-wicon "showers" :height 1.1 :v-adjust 0.0)
    ("showers"               all-the-icons-wicon "showers")
    ("blowing.*snow"         all-the-icons-wicon "snow-wind")
    ("snow"                  all-the-icons-wicon "snow")
    ("dust"                  all-the-icons-wicon "dust")
    ("fog"                   all-the-icons-wicon "fog")
    ("haze"                  all-the-icons-wicon "day-haze")
    ("smoky"                 all-the-icons-wicon "smoke")
    ("blustery"              all-the-icons-wicon "cloudy-windy")
    ("windy"                 all-the-icons-wicon "cloudy-gusts")
    ("cold"                  all-the-icons-wicon "snowflake-cold")
    ("partly.*cloudy.*night" all-the-icons-wicon "night-alt-partly-cloudy")
    ("partly.*cloudy"        all-the-icons-wicon "day-cloudy-high")
    ("cloudy.*night"         all-the-icons-wicon "night-alt-cloudy")
    ("cxloudy.*day"          all-the-icons-wicon "day-cloudy")
    ("cloudy"                all-the-icons-wicon "cloudy")
    ("clear.*night"          all-the-icons-wicon "night-clear")
    ("fair.*night"           all-the-icons-wicon "stars")
    ("fair.*day"             all-the-icons-wicon "horizon")
    ("hot"                   all-the-icons-wicon "hot")
    ("not.*available"        all-the-icons-wicon "na")
    ))

(defvar all-the-icons-mode-icon-alist
  '(
    (emacs-lisp-mode           all-the-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.2 :face all-the-icons-purple)
    (inferior-emacs-lisp-mode  all-the-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.2 :face all-the-icons-lblue)
    (dired-mode                all-the-icons-octicon "file-directory"      :v-adjust 0.0)
    (lisp-interaction-mode     all-the-icons-fileicon "lisp"               :v-adjust -0.1 :face all-the-icons-orange)
    (org-mode                  all-the-icons-fileicon "org"                :v-adjust 0.0 :face all-the-icons-lgreen)
    (typescript-mode           all-the-icons-fileicon "typescript"         :v-adjust -0.1 :face all-the-icons-blue-alt)
    (js-mode                   all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js-jsx-mode               all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js2-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js3-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (rjsx-mode                 all-the-icons-fileicon "jsx-2"              :v-adjust -0.1 :face all-the-icons-cyan-alt)
    (term-mode                 all-the-icons-octicon "terminal"            :v-adjust 0.2)
    (eshell-mode               all-the-icons-octicon "terminal"            :v-adjust 0.0 :face all-the-icons-purple)
    (magit-refs-mode           all-the-icons-octicon "git-branch"          :v-adjust 0.0 :face all-the-icons-red)
    (magit-process-mode        all-the-icons-octicon "mark-github"         :v-adjust 0.0)
    (magit-diff-mode           all-the-icons-octicon "git-compare"         :v-adjust 0.0 :face all-the-icons-lblue)
    (ediff-mode                all-the-icons-octicon "git-compare"         :v-adjust 0.0 :Face all-the-icons-red)
    (comint-mode               all-the-icons-faicon "terminal"             :v-adjust 0.0 :face all-the-icons-lblue)
    (eww-mode                  all-the-icons-faicon "firefox"              :v-adjust -0.1 :face all-the-icons-red)
    (org-agenda-mode           all-the-icons-octicon "checklist"           :v-adjust 0.0 :face all-the-icons-lgreen)
    (cfw:calendar-mode         all-the-icons-octicon "calendar"            :v-adjust 0.0)
    (ibuffer-mode              all-the-icons-faicon "files-o"              :v-adjust 0.0 :face all-the-icons-dsilver)
    (messages-buffer-mode      all-the-icons-faicon "stack-overflow"       :v-adjust -0.1)
    (help-mode                 all-the-icons-faicon "info"                 :v-adjust -0.1 :face all-the-icons-purple)
    (benchmark-init/tree-mode  all-the-icons-octicon "dashboard"           :v-adjust 0.0)
    (jenkins-mode              all-the-icons-fileicon "jenkins"            :face all-the-icons-blue)
    (magit-popup-mode          all-the-icons-alltheicon "git"              :face all-the-icons-red)
    (magit-status-mode         all-the-icons-alltheicon "git"              :face all-the-icons-lred)
    (magit-log-mode            all-the-icons-alltheicon "git"              :face all-the-icons-green)
    (Custom-mode               all-the-icons-octicon "settings")

    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode             all-the-icons--web-mode-icon)

    (fundamental-mode                   all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.2 :face all-the-icons-dsilver)
    (special-mode                       all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.2 :face all-the-icons-yellow)
    (text-mode                          all-the-icons-octicon "file-text"         :v-adjust 0.0 :face all-the-icons-cyan)
    (ruby-mode                          all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
    (inf-ruby-mode                      all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (projectile-rails-compilation-mode  all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (rspec-compilation-mode             all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (rake-compilation-mode              all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (shell-mode                         all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
    (fish-mode                          all-the-icons-alltheicon "terminal"       :face all-the-icons-lpink)
    (nginx-mode                         all-the-icons-fileicon "nginx"            :height 0.9  :face all-the-icons-dgreen)
    (apache-mode                        all-the-icons-alltheicon "apache"         :height 0.9  :face all-the-icons-dgreen)
    (makefile-mode                      all-the-icons-fileicon "gnu"              :face all-the-icons-dorange)
    (dockerfile-mode                    all-the-icons-fileicon "dockerfile"       :face all-the-icons-blue)
    (docker-compose-mode                all-the-icons-fileicon "dockerfile"       :face all-the-icons-lblue)
    (xml-mode                           all-the-icons-faicon "file-code-o"        :height 0.95 :face all-the-icons-lorange)
    (json-mode                          all-the-icons-octicon "settings"          :face all-the-icons-yellow)
    (yaml-mode                          all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
    (elisp-byte-code-mode               all-the-icons-octicon "file-binary"       :v-adjust 0.0 :face all-the-icons-dsilver)
    (archive-mode                       all-the-icons-octicon "file-zip"          :v-adjust 0.0 :face all-the-icons-lmaroon)
    (elm-mode                           all-the-icons-fileicon "elm"              :face all-the-icons-blue)
    (erlang-mode                        all-the-icons-alltheicon "erlang"         :face all-the-icons-red :v-adjust -0.1 :height 0.9)
    (elixir-mode                        all-the-icons-alltheicon "elixir"         :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
    (java-mode                          all-the-icons-alltheicon "java"           :height 1.0  :face all-the-icons-purple)
    (go-mode                            all-the-icons-alltheicon "go"             :height 1.0  :face all-the-icons-blue)
    (matlab-mode                        all-the-icons-fileicon "matlab"           :face all-the-icons-orange)
    (perl-mode                          all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
    (cperl-mode                         all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
    (php-mode                           all-the-icons-fileicon "php"              :face all-the-icons-lsilver)
    (prolog-mode                        all-the-icons-alltheicon "prolog"         :height 1.1  :face all-the-icons-lmaroon)
    (python-mode                        all-the-icons-alltheicon "python"         :height 1.0  :face all-the-icons-dblue)
    (racket-mode                        all-the-icons-fileicon "racket"           :height 1.2 :face all-the-icons-red)
    (rust-mode                          all-the-icons-alltheicon "rust"           :height 1.2  :face all-the-icons-maroon)
    (scala-mode                         all-the-icons-alltheicon "scala"          :face all-the-icons-red)
    (scheme-mode                        all-the-icons-fileicon   "scheme"         :height 1.2 :face all-the-icons-red)
    (swift-mode                         all-the-icons-alltheicon "swift"          :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
    (c-mode                             all-the-icons-alltheicon "c-line"         :face all-the-icons-blue)
    (c++-mode                           all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-blue)
    (csharp-mode                        all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
    (clojure-mode                       all-the-icons-alltheicon "clojure-line"   :height 1.0  :face all-the-icons-blue)
    (cider-repl-mode                    all-the-icons-alltheicon "clojure-line"   :height 1.0  :face all-the-icons-dblue)
    (clojurescript-mode                 all-the-icons-fileicon "cljs"             :height 1.0  :face all-the-icons-dblue)
    (coffee-mode                        all-the-icons-alltheicon "coffeescript"   :height 1.0  :face all-the-icons-maroon)
    (lisp-mode                          all-the-icons-fileicon "lisp"             :face all-the-icons-orange)
    (css-mode                           all-the-icons-alltheicon "css3"           :face all-the-icons-yellow)
    (scss-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-pink)
    (sass-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-dpink)
    (less-css-mode                      all-the-icons-alltheicon "less"           :height 0.8  :face all-the-icons-dyellow)
    (stylus-mode                        all-the-icons-alltheicon "stylus"         :face all-the-icons-lgreen)
    (csv-mode                           all-the-icons-octicon "graph"             :v-adjust 0.0 :face all-the-icons-dblue)
    (haskell-mode                       all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    (haml-mode                          all-the-icons-fileicon "haml"             :face all-the-icons-lyellow)
    (html-mode                          all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
    (rhtml-mode                         all-the-icons-alltheicon "html5"          :face all-the-icons-lred)
    (mustache-mode                      all-the-icons-fileicon "moustache"        :face all-the-icons-green)
    (slim-mode                          all-the-icons-octicon "dashboard"         :v-adjust 0.0 :face all-the-icons-yellow)
    (jade-mode                          all-the-icons-fileicon "jade"             :face all-the-icons-red)
    (pug-mode                           all-the-icons-fileicon "pug"              :face all-the-icons-red)
    (react-mode                         all-the-icons-alltheicon "react"          :height 1.1  :face all-the-icons-lblue)
    (image-mode                         all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-blue)
    (texinfo-mode                       all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    (markdown-mode                      all-the-icons-octicon "markdown"          :v-adjust 0.0 :face all-the-icons-lblue)
    (bibtex-mode                        all-the-icons-fileicon "bib"              :face all-the-icons-maroon)
    (org-mode                           all-the-icons-fileicon "org"              :face all-the-icons-lgreen)
    (compilation-mode                   all-the-icons-faicon "cogs"               :v-adjust 0.0 :height 1.0)
    (objc-mode                          all-the-icons-faicon "apple"              :v-adjust 0.0 :height 1.0)
    ))

;; ====================
;;   Functions Start
;; ====================

(defun* all-the-icons-is-repo (path)
  "Check if PATH is a repository."
  (dolist (repo all-the-icons-mode-repositories)
    (when (file-exists-p (format "%s/%s" path repo))
      (return-from all-the-icons-is-repo t))))

(defun all-the-icons-auto-mode-match? (&optional file)
  "Whether or not FILE's `major-mode' match against its `auto-mode-alist'."
  (let* ((file (or file (buffer-file-name) (buffer-name)))
         (auto-mode (all-the-icons-match-to-alist file auto-mode-alist)))
    (eq major-mode auto-mode)))

(defun all-the-icons-match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (cdr (cl-find-if (lambda (it) (string-match (car it) file)) alist)))

(defun all-the-icons-dir-is-submodule (dir)
  "Checker whether or not DIR is a git submodule."
  (let* ((gitmodule-dir (locate-dominating-file dir ".gitmodules"))
         (modules-file  (expand-file-name (format "%s.gitmodules" gitmodule-dir)))
         (module-search (format "submodule \".*?%s\"" (file-name-base dir))))

    (when (and gitmodule-dir (file-exists-p (format "%s/.git" dir)))
      (with-temp-buffer
        (insert-file-contents modules-file)
        (search-forward-regexp module-search (point-max) t)))))

;; Icon functions
(defun all-the-icons-icon-for-dir (dir &optional chevron padding)
  "Format an icon for DIR with CHEVRON similar to tree based directories.

If PADDING is provided, it will prepend and separate the chevron
and directory with PADDING.

Produces different symbols by inspecting DIR to distinguish
symlinks and git repositories which do not depend on the
directory contents"
  (let* ((matcher (all-the-icons-match-to-alist (file-name-base dir) all-the-icons-dir-icon-alist))
         (path (expand-file-name dir))
         (chevron (if chevron (all-the-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
         (padding (or padding "\t"))
         (icon (cond
                ((file-symlink-p path)
                 (all-the-icons-octicon "file-symlink-directory" :height 1.0))
                ((all-the-icons-dir-is-submodule path)
                 (all-the-icons-octicon "file-submodule" :height 1.0))
                ((all-the-icons-is-repo path)
                 (format "%s" (all-the-icons-octicon "repo" :height 1.1)))
                (t (apply (car matcher) (cdr matcher))))))
    (format "%s%s%s%s%s" padding chevron padding icon padding)))

(defun all-the-icons-icon-for-buffer ()
  "Get the formatted icon for the current buffer.

This function prioritises the use of the buffers file extension to
discern the icon when its `major-mode' matches its auto mode,
otherwise it will use the buffers `major-mode' to decide its
icon."
  (all-the-icons--icon-info-for-buffer))

(defun all-the-icons-icon-family-for-buffer ()
  "Get the icon font family for the current buffer."
  (all-the-icons--icon-info-for-buffer "family"))

(defun all-the-icons--web-mode-icon (&rest arg-overrides) "Get icon for a `web-mode' buffer with ARG-OVERRIDES." (all-the-icons--web-mode nil arg-overrides))
(defun all-the-icons--web-mode-icon-family () "Get icon family for a `web-mode' buffer." (all-the-icons--web-mode t))
(defun all-the-icons--web-mode (&optional family arg-overrides)
  "Return icon or FAMILY for `web-mode' based on `web-mode-content-type'.
Providing ARG-OVERRIDES will modify the creation of the icon."
  (let ((non-nil-args (cl-reduce (lambda (acc it) (if it (append acc (list it)) acc)) arg-overrides :initial-value '())))
    (cond
     ((equal web-mode-content-type "jsx")
      (if family (all-the-icons-fileicon-family) (apply 'all-the-icons-fileicon (append '("jsx-2") non-nil-args))))
     ((equal web-mode-content-type "javascript")
      (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("javascript") non-nil-args))))
     ((equal web-mode-content-type "json")
      (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("less") non-nil-args))))
     ((equal web-mode-content-type "xml")
      (if family (all-the-icons-faicon-family) (apply 'all-the-icons-faicon (append '("file-code-o") non-nil-args))))
     ((equal web-mode-content-type "css")
      (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("css3") non-nil-args))))
     (t
      (if family (all-the-icons-alltheicon-family) (apply 'all-the-icons-alltheicon (append '("html5") non-nil-args)))))))

;; Icon Functions

;;;###autoload
(defun all-the-icons-icon-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (all-the-icons-match-to-alist file all-the-icons-icon-alist))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;;###autoload
(defun all-the-icons-icon-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (cdr (assoc mode all-the-icons-mode-icon-alist)))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if icon (apply (car icon) args) mode)))

(memoize 'all-the-icons-icon-for-file)
(memoize 'all-the-icons-icon-for-mode)

;; Family Face Functions
(defun all-the-icons-icon-family-for-file (file)
  "Get the icons font family for FILE."
  (let ((icon (all-the-icons-match-to-alist file all-the-icons-icon-alist)))
    (funcall (intern (format "%s-family" (car icon))))))

(defun all-the-icons-icon-family-for-mode (mode)
  "Get the icons font family for MODE."
  (let ((icon (cdr (assoc mode all-the-icons-mode-icon-alist))))
    (if icon (funcall (intern (format "%s-family" (car icon)))) nil)))

(defun all-the-icons-icon-family (icon)
  "Get a propertized ICON family programatically."
  (plist-get (get-text-property 0 'face icon) :family))

(memoize 'all-the-icons-icon-family-for-file)
(memoize 'all-the-icons-icon-family-for-mode)
(memoize 'all-the-icons-icon-family)

;;;###autoload
(defun all-the-icons--icon-info-for-buffer (&optional f)
  "Get icon info for the current buffer.

When F is provided, the info function is calculated with the format
`all-the-icons-icon-%s-for-file' or `all-the-icons-icon-%s-for-mode'."
  (let* ((base-f (concat "all-the-icons-icon" (when f (format "-%s" f))))
         (file-f (intern (concat base-f "-for-file")))
         (mode-f (intern (concat base-f "-for-mode"))))
    (if (and (buffer-file-name)
             (all-the-icons-auto-mode-match?))
        (funcall file-f (file-name-nondirectory (buffer-file-name)))
      (funcall mode-f major-mode))))

;; Weather icons
(defun all-the-icons-icon-for-weather (weather)
  "Get an icon for a WEATHER status."
  (let ((icon (all-the-icons-match-to-alist weather all-the-icons-weather-icon-alist)))
    (if icon (apply (car icon) (cdr icon)) weather)))

;; Definitions

(eval-and-compile
  (defun all-the-icons--function-name (name)
    "Get the symbol for an icon function name for icon set NAME."
    (intern (concat "all-the-icons-" (downcase (symbol-name name)))))

  (defun all-the-icons--family-name (name)
    "Get the symbol for an icon family function for icon set NAME."
    (intern (concat "all-the-icons-" (downcase (symbol-name name)) "-family")))

  (defun all-the-icons--data-name (name)
    "Get the symbol for an icon family function for icon set NAME."
    (intern (concat "all-the-icons-" (downcase (symbol-name name)) "-data")))

  (defun all-the-icons--insert-function-name (name)
    "Get the symbol for an icon insert function for icon set NAME."
    (intern (concat "all-the-icons-insert-" (downcase (symbol-name name))))))

;; Icon insertion functions

(defun all-the-icons--read-candidates ()
  "Helper to build a list of candidates for all families."
  (cl-reduce 'append (mapcar (lambda (it) (all-the-icons--read-candidates-for-family it t)) all-the-icons-font-families)))

(defun all-the-icons--read-candidates-for-family (family &optional show-family)
  "Helper to build read candidates for FAMILY.
If SHOW-FAMILY is non-nil, displays the icons family in the candidate string."
  (let ((data   (funcall (all-the-icons--data-name family)))
        (icon-f (all-the-icons--function-name family)))
    (mapcar
     (lambda (it)
       (let* ((icon-name (car it))
              (icon-name-head (substring icon-name 0 1))
              (icon-name-tail (substring icon-name 1))

              (icon-display (propertize icon-name-head 'display (format "%s\t%s" (funcall icon-f icon-name) icon-name-head)))
              (icon-family (if show-family (format "\t[%s]" family) ""))

              (candidate-name (format "%s%s%s" icon-display icon-name-tail icon-family))
              (candidate-icon (funcall (all-the-icons--function-name family) icon-name)))

         (cons candidate-name candidate-icon)))
     data)))

;;;###autoload
(defun all-the-icons-install-fonts (&optional pfx)
  "Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install"
  (interactive "P")
  (when (or pfx (yes-or-no-p "This will download and install fonts, are you sure you want to do this?"))
    (let* ((url-format "https://github.com/domtronn/all-the-icons.el/blob/master/fonts/%s?raw=true")
           (font-dest (cl-case window-system
                        (x  (concat (or (getenv "XDG_DATA_HOME")            ;; Default Linux install directories
                                        (concat (getenv "HOME") "/.local/share"))
                                    "/fonts/"))
                        (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                        (ns (concat (getenv "HOME") "/Library/Fonts/" ))))  ;; Default MacOS install directory
           (known-dest? (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

      (unless (file-directory-p font-dest) (mkdir font-dest t))

      (mapc (lambda (font)
              (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
            all-the-icons-font-names)
      (when known-dest?
        (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
        (shell-command-to-string (format "fc-cache -f -v")))
      (message "%s Successfully %s `all-the-icons' fonts to `%s'!"
               (all-the-icons-wicon "stars" :v-adjust 0.0)
               (if known-dest? "installed" "downloaded")
               font-dest))))

;;;###autoload
(defun all-the-icons-insert (&optional arg family)
  "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it."
  (interactive "P")
  (let* ((standard-output (current-buffer))
         (candidates (if family
                         (all-the-icons--read-candidates-for-family family)
                       (all-the-icons--read-candidates)))
         (prompt     (if family
                         (format "%s Icon: " (funcall (all-the-icons--family-name family)))
                       "Icon : "))

         (selection (completing-read prompt candidates nil t))
         (result    (cdr (assoc selection candidates))))

    (if arg (prin1 result) (insert result))))

;; Debug Helpers

(defun all-the-icons-insert-icons-for (family &optional height duration)
  "Insert all of the available icons associated with FAMILY.
If a HEIGHT is provided it will render the icons at this height.
This is useful both to see the icons more clearly and to test
different height rendering.  If DURATION is provided, it will
pause for DURATION seconds between printing each character."
  (let* ((data-f    (all-the-icons--data-name family))
         (insert-f  (all-the-icons--function-name family))

         (height (or height 2.0))
         (data (funcall data-f)))
    (mapc
     (lambda (it)
       (insert (format "%s - %s\n" (funcall insert-f (car it) :height height) (car it)))
       (when duration (sit-for duration 0)))
     data)))

(defmacro define-icon (name alist family &optional font-name)
  "Macro to generate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `all-the-icons-NAME'.

ALIST is the alist containing maps between icon names and the
UniCode for the character.  All of these can be found in the data
directory of this package.

FAMILY is the font family to use for the icons.
FONT-NAME is the name of the .ttf file providing the font, defaults to FAMILY."
  `(progn
     (add-to-list 'all-the-icons-font-families (quote ,name))
     (add-to-list 'all-the-icons-font-names (quote ,(downcase (format "%s.ttf" (or font-name family)))))

     (defun ,(all-the-icons--family-name name) () ,family)
     (defun ,(all-the-icons--data-name name) () ,alist)
     (defun ,(all-the-icons--function-name name) (icon-name &rest args)
       (let ((icon (cdr (assoc icon-name ,alist)))
             (other-face (when all-the-icons-color-icons (plist-get args :face)))
             (height  (* all-the-icons-scale-factor (or (plist-get args :height) 1.0)))
             (v-adjust (* all-the-icons-scale-factor (or (plist-get args :v-adjust) all-the-icons-default-adjust)))
             (family ,family))
         (unless icon
           (error (format "Unable to find icon with name `%s' in icon set `%s'" icon-name (quote ,name))))
         (propertize icon
                     'face (if other-face
                               `(:family ,family :height ,height :inherit ,other-face)
                             `(:family ,family :height ,height))
                     'display `(raise ,v-adjust)
                     'rear-nonsticky t
                     'font-lock-ignore t)))
     (defun ,(all-the-icons--insert-function-name name) (&optional arg)
       ,(format "Insert a %s icon at point." family)
       (interactive "P")
       (all-the-icons-insert arg (quote ,name)))))

(define-icon alltheicon all-the-icons-data/alltheicons-alist    "all-the-icons")
(define-icon fileicon   all-the-icons-data/file-icon-alist      "file-icons")
(define-icon faicon     all-the-icons-data/fa-icon-alist        "FontAwesome")
(define-icon octicon    all-the-icons-data/octicons-alist       "github-octicons" "octicons")
(define-icon wicon      all-the-icons-data/weather-icons-alist  "Weather Icons"   "weathericons")
(define-icon material   all-the-icons-data/material-icons-alist "Material Icons"  "material-design-icons")

(provide 'all-the-icons)

;;; all-the-icons.el ends here
