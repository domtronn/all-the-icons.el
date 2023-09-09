;;; all-the-icons.el --- A library for inserting Developer icons -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Dominic Charlesworth <dgc336@gmail.com>
;; Copyright (C) 2019-2023  Jimmy Yuen Ho Wong <wyuenho@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 6.0.0
;; Package-Requires: ((emacs "27.1"))
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

;; This package adds various SVG icon sets to Emacs.

;; This package was inspired by

;; - `mode-icons' for Emacs, found at https://github.com/ryuslash/mode-icons
;; - `file-icons' for Atom, found at https://atom.io/packages/file-icons

;; Currently, this package provides an interface to the following icon sets:

;; - Atom File Icons,       found at https://atom.io/packages/file-icons
;; - GitHub Octicons,       found at http://octicons.github.com
;; - Weather Icons,         found at https://erikflowers.github.io/weather-icons
;; - VSCode Icons,          found at https://github.com/microsoft/vscode-codicons
;; - Optimized Devicons,    found at https://github.com/file-icons/DevOpicons
;; - Optimized MFizz,       found at https://github.com/file-icons/MFixx
;; - FluentUI System Icons, found at https://github.com/microsoft/fluentui-system-icons
;; - Material Design Icons, found at https://github.com/google/material-design-icons
;; - Font Awesome 4,        found at https://fontawesome.com/v4/icons/


;;; Usage:

;; The simplest usage for this package is to use the following functions;

;;   `all-the-icons-icon-for-buffer'
;;   `all-the-icons-icon-for-dir'
;;   `all-the-icons-icon-for-file'
;;   `all-the-icons-icon-for-mode'
;;   `all-the-icons-icon-for-weather'

;; Which can be used to get a formatted icon for the current buffer, a file
;; name, a major mode or a weather status respectively.  e.g.

;;   (insert (all-the-icons-icon-for-file "foo.js"))

;; which will insert a JavaScript SVG icon.

;; You can also insert icons directly using the individual icon set functions:

;;   `all-the-icons-file-icons'            // File Icons from the Atom File Icons package
;;   `all-the-icons-octicons'              // GitHub Octicons
;;   `all-the-icons-vscode-codicons'       // Visual Studio Code Icons
;;   `all-the-icons-weather-icons'         // Weather Icons
;;   `all-the-icons-devopicons'            // DevopIcons
;;   `all-the-icons-mfixx'                 // MFixx Icons
;;   `all-the-icons-fontawesome-4'         // Font Awesome 4
;;   `all-the-icons-fluentui-system-icons' // FluentUI System Icons
;;   `all-the-icons-material-icons'        // Material Design Icons

;; You can call these functions with the icon name you want to insert, e.g.

;;   (all-the-icons-octicons        "file-binary")  // GitHub Octicon for Binary File
;;   (all-the-icons-weather-icons   "tornado")      // Weather Icon for tornado

;; A list of all the icon names for the different font families can be found in
;; the data files, or by inspecting the alist variables.  All the alist
;; variables are prefixed with `all-the-icons-data-'

;;; Code:
(require 'cl-lib)
(require 'map)
(require 'svg)

(when load-in-progress
  (add-to-list 'load-path (file-name-directory load-file-name)))

(require 'all-the-icons-data-alltheicons)
(require 'all-the-icons-data-devopicons)
(require 'all-the-icons-data-file-icons)
(require 'all-the-icons-data-mfixx)
(require 'all-the-icons-data-octicons)
(require 'all-the-icons-data-vscode-codicons)
(require 'all-the-icons-data-weather-icons)
(require 'all-the-icons-data-fontawesome-4)
(require 'all-the-icons-data-fluentui-system-icons)
(require 'all-the-icons-data-material-icons)

(require 'all-the-icons-faces)

(defvar web-mode-content-type) ;silence byte-compiler warning

;; Obsolete stuff

(define-obsolete-variable-alias 'all-the-icons-font-families 'all-the-icons-sets "6.0.0")

(define-obsolete-function-alias 'all-the-icons-fileicon-data 'all-the-icons-file-icons-data "6.0.0")
(define-obsolete-function-alias 'all-the-icons-fileicon 'all-the-icons-file-icons "6.0.0")
(define-obsolete-function-alias 'all-the-icons-insert-fileicon 'all-the-icons-insert-file-icons "6.0.0")

(define-obsolete-function-alias 'all-the-icons-octicon-data 'all-the-icons-octicons-data "6.0.0")
(define-obsolete-function-alias 'all-the-icons-octicon 'all-the-icons-octicons "6.0.0")
(define-obsolete-function-alias 'all-the-icons-insert-octicon 'all-the-icons-insert-octicons "6.0.0")

(define-obsolete-function-alias 'all-the-icons-faicon-data 'all-the-icons-fontawesome-4-data "6.0.0")
(define-obsolete-function-alias 'all-the-icons-faicon 'all-the-icons-fontawesome-4 "6.0.0")
(define-obsolete-function-alias 'all-the-icons-insert-faicon 'all-the-icons-insert-fontawesome-4 "6.0.0")

(define-obsolete-function-alias 'all-the-icons-material-data 'all-the-icons-material-icons-data "6.0.0")
(define-obsolete-function-alias 'all-the-icons-material 'all-the-icons-material-icons "6.0.0")
(define-obsolete-function-alias 'all-the-icons-insert-material 'all-the-icons-insert-material-icons "6.0.0")

(define-obsolete-function-alias 'all-the-icons-wicon-data 'all-the-icons-weather-icons-data "6.0.0")
(define-obsolete-function-alias 'all-the-icons-wicon 'all-the-icons-weather-icons "6.0.0")
(define-obsolete-function-alias 'all-the-icons-insert-wicon 'all-the-icons-insert-weather-icons "6.0.0")
(define-obsolete-function-alias 'all-the-icons-auto-mode-match? 'all-the-icons-auto-mode-match-p "6.0.0")

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

(defvar all-the-icons-sets '() "List of defined icon sets.")

(defvar all-the-icons-extension-icon-alist
  '(
    ("7z"              octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("adoc"            file-icons "asciidoc"             :face all-the-icons-lblue     )
    ("aif"             fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("aifc"            fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("aiff"            fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("asciidoc"        file-icons "asciidoc"             :face all-the-icons-lblue     )
    ("asm"             file-icons "assembly-generic"     :face all-the-icons-blue      )
    ("au"              fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("aup"             file-icons "audacity"             :face all-the-icons-yellow    )
    ("babelrc"         file-icons "babel"                :face all-the-icons-yellow    )
    ("bash_profile"    vscode-codicons "terminal-bash"   :face all-the-icons-dpink     )
    ("bashrc"          vscode-codicons "terminal-bash"   :face all-the-icons-dpink     )
    ("beancount"       fontawesome-4 "credit-card"       :face all-the-icons-lgreen    )
    ("bib"             file-icons "bibtex"               :face all-the-icons-maroon    )
    ("bowerrc"         devopicons "bower"                :face all-the-icons-silver    )
    ("bz"              octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("bz2"             octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("c"               mfixx "c"                         :face all-the-icons-blue      )
    ("cabal"           file-icons "cabal"                :face all-the-icons-lblue     )
    ("cache"           material-icons "cached"           :face all-the-icons-green     )
    ("cc"              mfixx "c++"                       :face all-the-icons-blue      )
    ("chs"             devopicons "haskell"              :face all-the-icons-red       )
    ("cl"              file-icons "common-lisp"          :face all-the-icons-lorange   )
    ("clj"             devopicons "clojure"              :face all-the-icons-blue      )
    ("cljc"            devopicons "clojure"              :face all-the-icons-blue      )
    ("cljs"            file-icons "clojurejs"            :face all-the-icons-dblue     )
    ("cmake"           file-icons "cmake"                :face all-the-icons-red       )
    ("coffee"          devopicons "coffeescript"         :face all-the-icons-maroon    )
    ("comp"            file-icons "vertexshader"         :face all-the-icons-dblue     )
    ("cpp"             mfixx "c++"                       :face all-the-icons-blue      )
    ("cr"              file-icons "crystal"              :face all-the-icons-yellow    )
    ("crt"             octicons "key"                    :face all-the-icons-lblue     )
    ("cs"              file-icons "c#"                   :face all-the-icons-dblue     )
    ("cson"            file-icons "config-coffeescript"  :face all-the-icons-yellow    )
    ("css"             devopicons "css3"                 :face all-the-icons-yellow    )
    ("csv"             octicons "graph"                  :face all-the-icons-dblue     )
    ("csv"             octicons "graph"                  :face all-the-icons-dblue     )
    ("csx"             file-icons "c#-script"            :face all-the-icons-dblue     )
    ("cu"              file-icons "nvidia"               :face all-the-icons-green     )
    ("cuh"             file-icons "nvidia"               :face all-the-icons-green     )
    ("cxx"             mfixx "c++"                       :face all-the-icons-blue      )
    ("d3js"            file-icons "d3"                   :face all-the-icons-lgreen    )
    ("dart"            devopicons "dart"                 :face all-the-icons-blue      )
    ("dat"             octicons "file-binary"            :face all-the-icons-cyan      )
    ("dav"             fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("dll"             fontawesome-4 "cog"               :face all-the-icons-silver    )
    ("dmg"             fontawesome-4 "hdd-o"             :face all-the-icons-lsilver   )
    ("doc"             file-icons "microsoft-word"       :face all-the-icons-blue      )
    ("dockerfile"      file-icons "docker"               :face all-the-icons-cyan      )
    ("dockerignore"    file-icons "docker"               :face all-the-icons-dblue     )
    ("docm"            file-icons "microsoft-word"       :face all-the-icons-blue      )
    ("docx"            file-icons "microsoft-word"       :face all-the-icons-blue      )
    ("ds_store"        fontawesome-4 "cog"               :face all-the-icons-silver    )
    ("ebuild"          file-icons "gentoo"               :face all-the-icons-cyan      )
    ("eclass"          file-icons "gentoo"               :face all-the-icons-blue      )
    ("ecr"             file-icons "crystal"              :face all-the-icons-yellow    )
    ("eex"             mfixx "elixir"                    :face all-the-icons-lorange   )
    ("el"              file-icons "elisp"                :face all-the-icons-purple    )
    ("elc"             octicons "file-binary"            :face all-the-icons-dsilver   )
    ("elm"             file-icons "elm"                  :face all-the-icons-blue      )
    ("eml"             fontawesome-4 "envelope"          :face all-the-icons-blue      )
    ("erb"             devopicons "html5"                :face all-the-icons-lred      )
    ("erl"             mfixx "erlang"                    :face all-the-icons-red       )
    ("es"              mfixx "javascript"                :face all-the-icons-yellow    )
    ("eslintignore"    file-icons "eslint"               :face all-the-icons-dpurple   )
    ("ex"              mfixx "elixir"                    :face all-the-icons-lpurple   )
    ("exs"             mfixx "elixir"                    :face all-the-icons-lred      )
    ("f90"             file-icons "fortran"              :face all-the-icons-purple    )
    ("fish"            octicons "terminal"               :face all-the-icons-lpink     )
    ("flac"            fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("flv"             fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("frag"            file-icons "vertexshader"         :face all-the-icons-red       )
    ("fs"              devopicons "fsharp"               :face all-the-icons-blue-alt  )
    ("fsi"             devopicons "fsharp"               :face all-the-icons-blue-alt  )
    ("fsscript"        devopicons "fsharp"               :face all-the-icons-blue-alt  )
    ("fsx"             devopicons "fsharp"               :face all-the-icons-blue-alt  )
    ("gem"             file-icons "rubygems"             :face all-the-icons-red       )
    ("geom"            file-icons "vertexshader"         :face all-the-icons-green     )
    ("gif"             octicons "file-media"             :face all-the-icons-green     )
    ("glsl"            file-icons "vertexshader"         :face all-the-icons-blue      )
    ("go"              file-icons "go"                   :face all-the-icons-blue      )
    ("gpg"             octicons "key"                    :face all-the-icons-lblue     )
    ("gql"             file-icons "graphql"              :face all-the-icons-dpink     )
    ("gradle"          file-icons "gradle"               :face all-the-icons-silver    )
    ("graphql"         file-icons "graphql"              :face all-the-icons-dpink     )
    ("gz"              octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("h"               mfixx "c"                         :face all-the-icons-purple    )
    ("haml"            file-icons "haml"                 :face all-the-icons-lyellow   )
    ("hbs"             file-icons "moustache"            :face all-the-icons-green     )
    ("heex"            mfixx "elixir"                    :face all-the-icons-lorange   )
    ("hh"              mfixx "c++"                       :face all-the-icons-purple    )
    ("hpp"             mfixx "c++"                       :face all-the-icons-purple    )
    ("hrl"             mfixx "erlang"                    :face all-the-icons-dred      )
    ("hs"              devopicons "haskell"              :face all-the-icons-red       )
    ("hsc"             devopicons "haskell"              :face all-the-icons-red       )
    ("htm"             devopicons "html5"                :face all-the-icons-orange    )
    ("html"            devopicons "html5"                :face all-the-icons-orange    )
    ("hxx"             mfixx "c++"                       :face all-the-icons-purple    )
    ("hy"              file-icons "hy"                   :face all-the-icons-blue      )
    ("iced"            devopicons "coffeescript"         :face all-the-icons-lmaroon   )
    ("ico"             octicons "file-media"             :face all-the-icons-blue      )
    ("idr"             file-icons "idris"                :face all-the-icons-red       )
    ("ini"             octicons "sliders"                :face all-the-icons-yellow    )
    ("inky-er"         devopicons "html5"                :face all-the-icons-lred      )
    ("inky-erb"        devopicons "html5"                :face all-the-icons-lred      )
    ("inky-haml"       file-icons "haml"                 :face all-the-icons-lyellow   )
    ("inky-slim"       octicons "meter"                  :face all-the-icons-yellow    )
    ("ipynb"           file-icons "jupyter"              :face all-the-icons-dorange   )
    ("j2"              file-icons "jinja"                :face all-the-icons-silver    )
    ("jade"            file-icons "jade"                 :face all-the-icons-red       )
    ("java"            devopicons "java"                 :face all-the-icons-purple    )
    ("jinja2"          file-icons "jinja"                :face all-the-icons-silver    )
    ("jl"              file-icons "julia"                :face all-the-icons-purple    )
    ("jpeg"            octicons "file-media"             :face all-the-icons-dblue     )
    ("jpg"             octicons "file-media"             :face all-the-icons-dblue     )
    ("js"              mfixx "javascript"                :face all-the-icons-yellow    )
    ("json"            vscode-codicons "json"            :face all-the-icons-yellow    )
    ("jsx"             file-icons "jsx-atom"             :face all-the-icons-cyan-alt  )
    ("key"             octicons "key"                    :face all-the-icons-lblue     )
    ("knt"             file-icons "microsoft-powerpoint" :face all-the-icons-cyan      )
    ("kt"              file-icons "kotlin"               :face all-the-icons-orange    )
    ("kts"             file-icons "kotlin"               :face all-the-icons-orange    )
    ("l"               file-icons "lisp"                 :face all-the-icons-orange    )
    ("ledger"          fontawesome-4 "credit-card"       :face all-the-icons-lgreen    )
    ("leex"            mfixx "elixir"                    :face all-the-icons-lorange   )
    ("less"            devopicons "less"                 :face all-the-icons-dyellow   )
    ("lhs"             devopicons "haskell"              :face all-the-icons-red       )
    ("lisp"            file-icons "lisp"                 :face all-the-icons-orange    )
    ("log"             octicons "log"                    :face all-the-icons-maroon    )
    ("lua"             file-icons "lua"                  :face all-the-icons-dblue     )
    ("ly"              fontawesome-4 "music"             :face all-the-icons-green     )
    ("lz4"             octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("m"               mfixx "objc"                                                    )
    ("m4a"             fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("matlab"          file-icons "matlab"               :face all-the-icons-orange    )
    ("md"              octicons "markdown"               :face all-the-icons-lblue     )
    ("mjs"             devopicons "nodejs-small"         :face all-the-icons-lgreen    )
    ("mk"              file-icons "gnu"                  :face all-the-icons-dorange   )
    ("mkv"             fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("ml"              file-icons "ocaml"                :face all-the-icons-lpink     )
    ("mli"             file-icons "ocaml"                :face all-the-icons-dpink     )
    ("mm"              mfixx "objc"                                                    )
    ("mov"             fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("mp3"             fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("mp4"             fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("mpeg"            fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("mpg"             fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("msg"             fontawesome-4 "envelope"          :face all-the-icons-blue      )
    ("nim"             file-icons "nimrod"               :face all-the-icons-yellow    )
    ("nims"            file-icons "nimrod"               :face all-the-icons-yellow    )
    ("nix"             file-icons "nix"                  :face all-the-icons-blue      )
    ("node"            devopicons "nodejs-small"         :face all-the-icons-green     )
    ("odin"            file-icons "odin"                 :face all-the-icons-lblue     )
    ("ogg"             fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("ogv"             fontawesome-4 "file-video-o"      :face all-the-icons-dblue     )
    ("ogv"             fontawesome-4 "file-video-o"      :face all-the-icons-dblue     )
    ("opus"            fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("org"             file-icons "org"                  :face all-the-icons-lgreen    )
    ("p12"             octicons "key"                    :face all-the-icons-dorange   )
    ("pcss"            file-icons "postcss"              :face all-the-icons-dred      )
    ("pdf"             vscode-codicons "file-pdf"        :face all-the-icons-dred      )
    ("pem"             octicons "key"                    :face all-the-icons-orange    )
    ("php"             file-icons "php"                  :face all-the-icons-lsilver   )
    ("pkg"             octicons "package"                :face all-the-icons-dsilver   )
    ("pkgbuild"        octicons "package"                :face all-the-icons-dsilver   )
    ("pl"              mfixx "perl"                      :face all-the-icons-lorange   )
    ("pl6"             file-icons "perl6"                :face all-the-icons-cyan      )
    ("pm"              mfixx "perl"                      :face all-the-icons-lorange   )
    ("pm6"             file-icons "perl6"                :face all-the-icons-pink      )
    ("png"             octicons "file-media"             :face all-the-icons-orange    )
    ("pod"             devopicons "perl"                 :face all-the-icons-lgreen    )
    ("pony"            file-icons "pony"                 :face all-the-icons-maroon    )
    ("postcss"         file-icons "postcss"              :face all-the-icons-dred      )
    ("pp"              file-icons "puppet"               :face all-the-icons-yellow    )
    ("pps"             file-icons "microsoft-powerpoint" :face all-the-icons-orange    )
    ("ppt"             file-icons "microsoft-powerpoint" :face all-the-icons-orange    )
    ("pptsx"           file-icons "microsoft-powerpoint" :face all-the-icons-orange    )
    ("pptx"            file-icons "microsoft-powerpoint" :face all-the-icons-orange    )
    ("pro"             devopicons "prolog"               :face all-the-icons-lmaroon   )
    ("proog"           devopicons "prolog"               :face all-the-icons-lmaroon   )
    ("ps1"             file-icons "powershell"           :face all-the-icons-blue      )
    ("pub"             octicons "key"                    :face all-the-icons-blue      )
    ("pug"             file-icons "pug"                  :face all-the-icons-red       )
    ("py"              devopicons "python"               :face all-the-icons-dblue     )
    ("r"               file-icons "r"                    :face all-the-icons-lblue     )
    ("raku"            file-icons "perl6"                :face all-the-icons-cyan      )
    ("rakumod"         file-icons "perl6"                :face all-the-icons-pink      )
    ("rb"              vscode-codicons "ruby"            :face all-the-icons-lred      )
    ("rd"              file-icons "r"                    :face all-the-icons-lblue     )
    ("rdx"             file-icons "r"                    :face all-the-icons-lblue     )
    ("re"              file-icons "reason"               :face all-the-icons-red-alt   )
    ("rei"             file-icons "reason"               :face all-the-icons-dred      )
    ("rkt"             file-icons "racket"               :face all-the-icons-red       )
    ("rlib"            devopicons "rust"                 :face all-the-icons-dmaroon   )
    ("rpm"             octicons "package"                :face all-the-icons-dsilver   )
    ("rs"              devopicons "rust"                 :face all-the-icons-maroon    )
    ("rst"             file-icons "restructuredtext"     :face all-the-icons-lblue     )
    ("rsx"             file-icons "r"                    :face all-the-icons-lblue     )
    ("sass"            devopicons "sass"                 :face all-the-icons-dpink     )
    ("sbt"             file-icons "sbt"                  :face all-the-icons-red       )
    ("scala"           devopicons "scala"                :face all-the-icons-red       )
    ("scm"             file-icons "scheme"               :face all-the-icons-red       )
    ("scpt"            file-icons "apple"                :face all-the-icons-pink      )
    ("scrbl"           file-icons "racket"               :face all-the-icons-blue      )
    ("scss"            devopicons "sass"                 :face all-the-icons-pink      )
    ("sh"              octicons "terminal"               :face all-the-icons-purple    )
    ("slim"            octicons "meter"                  :face all-the-icons-yellow    )
    ("sql"             octicons "database"               :face all-the-icons-silver    )
    ("sss"             file-icons "postcss"              :face all-the-icons-dred      )
    ("styl"            file-icons "stylus"               :face all-the-icons-lgreen    )
    ("stylelintignore" file-icons "stylelint"            :face all-the-icons-dyellow   )
    ("sv"              file-icons "verilog"              :face all-the-icons-red       )
    ("sva"             file-icons "verilog"              :face all-the-icons-red       )
    ("svams"           file-icons "verilog"              :face all-the-icons-red       )
    ("svelte"          file-icons "svelte"               :face all-the-icons-red       )
    ("svg"             mfixx "svg"                       :face all-the-icons-lgreen    )
    ("svh"             file-icons "verilog"              :face all-the-icons-red       )
    ("swift"           devopicons "swift"                :face all-the-icons-green     )
    ("tags"            octicons "tag"                    :face all-the-icons-blue      )
    ("tcl"             file-icons "tcl"                  :face all-the-icons-dred      )
    ("tesc"            file-icons "vertexshader"         :face all-the-icons-purple    )
    ("tese"            file-icons "vertexshader"         :face all-the-icons-dpurple   )
    ("tex"             file-icons "latex"                :face all-the-icons-lred      )
    ("texi"            file-icons "gnu"                  :face all-the-icons-lred      )
    ("text"            fontawesome-4 "file-text-o"       :face all-the-icons-cyan      )
    ("tf"              file-icons "terraform"            :face all-the-icons-purple-alt)
    ("tfstate"         file-icons "terraform"            :face all-the-icons-purple-alt)
    ("tfvars"          file-icons "terraform"            :face all-the-icons-purple-alt)
    ("tgz"             octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("ts"              file-icons "typescript"           :face all-the-icons-blue-alt  )
    ("tsx"             file-icons "tsx"                  :face all-the-icons-cyan-alt  )
    ("ttf"             file-icons "font"                 :face all-the-icons-dcyan     )
    ("txt"             fontawesome-4 "file-text-o"       :face all-the-icons-cyan      )
    ("v"               file-icons "verilog"              :face all-the-icons-red       )
    ("vagrantfile"     file-icons "vagrant"              :face all-the-icons-blue      )
    ("vams"            file-icons "verilog"              :face all-the-icons-red       )
    ("vert"            file-icons "vertexshader"         :face all-the-icons-blue      )
    ("vhd"             file-icons "vhdl"                 :face all-the-icons-blue      )
    ("vhdl"            file-icons "vhdl"                 :face all-the-icons-blue      )
    ("vhms"            file-icons "vhdl"                 :face all-the-icons-blue      )
    ("vue"             file-icons "vue"                  :face all-the-icons-lgreen    )
    ("wasm"            file-icons "webassembly"          :face all-the-icons-purple-alt)
    ("wat"             file-icons "webassembly"          :face all-the-icons-purple-alt)
    ("wav"             fontawesome-4 "file-audio-o"      :face all-the-icons-dred      )
    ("webm"            fontawesome-4 "file-video-o"      :face all-the-icons-blue      )
    ("webp"            octicons "file-media"             :face all-the-icons-dblue     )
    ("woff"            file-icons "font"                 :face all-the-icons-cyan      )
    ("woff2"           file-icons "font"                 :face all-the-icons-cyan      )
    ("xlsb"            file-icons "microsoft-excel"      :face all-the-icons-dgreen    )
    ("xlsm"            file-icons "microsoft-excel"      :face all-the-icons-dgreen    )
    ("xlsx"            file-icons "microsoft-excel"      :face all-the-icons-dgreen    )
    ("xltm"            file-icons "microsoft-excel"      :face all-the-icons-dgreen    )
    ("xltx"            file-icons "microsoft-excel"      :face all-the-icons-dgreen    )
    ("xml"             octicons "file-code"              :face all-the-icons-lorange   )
    ("xz"              octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("yaml"            file-icons "yaml-alt4"            :face all-the-icons-dyellow   )
    ("yml"             file-icons "yaml-alt4"            :face all-the-icons-dyellow   )
    ("zig"             file-icons "zig"                  :face all-the-icons-orange    )
    ("zip"             octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("zsh"             octicons "terminal"               :face all-the-icons-lcyan     )
    ("zst"             octicons "file-zip"               :face all-the-icons-lmaroon   )
    ("zstd"            octicons "file-zip"               :face all-the-icons-lmaroon   )))


(defvar all-the-icons-regexp-icon-alist
  '(
    ("-?spec\\.js$"            file-icons "test-js"          :face all-the-icons-lpurple )
    ("-?spec\\.jsx$"           file-icons "test-react"       :face all-the-icons-blue-alt)
    ("-?spec\\.ts$"            file-icons "test-typescript"  :face all-the-icons-blue    )
    ("-?test\\.js$"            file-icons "test-js"          :face all-the-icons-lpurple )
    ("-?test\\.jsx$"           file-icons "test-react"       :face all-the-icons-blue-alt)
    ("-?test\\.ts$"            file-icons "test-typescript"  :face all-the-icons-blue    )
    ("\\.npmignore$"           file-icons "npm"              :face all-the-icons-dred    )
    ("^Brewfile$"              file-icons "homebrew"         :face all-the-icons-lsilver )
    ("^CMakeCache.txt$"        file-icons "cmake"            :face all-the-icons-blue    )
    ("^CMakeLists.txt$"        file-icons "cmake"            :face all-the-icons-red     )
    ("^COMMIT_EDITMSG"         octicons "git-commit"         :face all-the-icons-red     )
    ("^Gemfile\\(\\.lock\\)?$" file-icons "rubygems"         :face all-the-icons-red     )
    ("^LICENSE$"               octicons "book"               :face all-the-icons-blue    )
    ("^MERGE_"                 octicons "git-merge"          :face all-the-icons-red     )
    ("^Makefile$"              file-icons "gnu"              :face all-the-icons-dorange )
    ("^TAGS$"                  octicons "tag"                :face all-the-icons-blue    )
    ("^TODO$"                  octicons "checklist"          :face all-the-icons-lyellow )
    ("^\\*new-tab\\*$"         octicons "star-fill"          :face all-the-icons-cyan    )
    ("^\\*scratch.*"           fontawesome-4 "sticky-note-o" :face all-the-icons-yellow  )
    ("^\\*scratch\\*$"         fontawesome-4 "sticky-note-o" :face all-the-icons-lyellow )
    ("^\\."                    fontawesome-4 "cog"                                       )
    ("^\\.?Dockerfile"         file-icons "docker"           :face all-the-icons-blue    )
    ("^\\.?eslint"             file-icons "eslint"           :face all-the-icons-purple  )
    ("^\\.?stylelint"          file-icons "stylelint"        :face all-the-icons-lyellow )
    ("^bower.json$"            devopicons "bower"            :face all-the-icons-lorange )
    ("^go.mod$"                file-icons "config-go"        :face all-the-icons-blue-alt)
    ("^go.work$"               file-icons "config-go"        :face all-the-icons-blue-alt)
    ("^gruntfile"              devopicons "grunt"            :face all-the-icons-lyellow )
    ("^gulpfile"               devopicons "gulp"             :face all-the-icons-lred    )
    ("^meson.build$"           file-icons "meson"            :face all-the-icons-purple  )
    ("^meson_options.txt$"     file-icons "meson"            :face all-the-icons-purple  )
    ("^mix.lock$"              mfixx "elixir"                :face all-the-icons-lyellow )
    ("^package.json$"          file-icons "npm"              :face all-the-icons-red     )
    ("^package.lock.json$"     file-icons "npm"              :face all-the-icons-dred    )
    ("^readme"                 octicons "book"               :face all-the-icons-lcyan   )
    ("^serverless\\.yml$"      file-icons "serverless"       :face all-the-icons-yellow  )
    ("^stack.*.json$"          mfixx "aws"                   :face all-the-icons-orange  )
    ("^webpack"                file-icons "webpack"          :face all-the-icons-lblue   )
    ("^yarn\\.lock"            file-icons "yarn"             :face all-the-icons-blue-alt)
    ("_?spec\\.rb$"            file-icons "test-ruby"        :face all-the-icons-red     )
    ("_?spec_helper\\.rb$"     file-icons "test-ruby"        :face all-the-icons-dred    )
    ("_?test\\.rb$"            file-icons "test-ruby"        :face all-the-icons-red     )
    ("_?test_helper\\.rb$"     file-icons "test-ruby"        :face all-the-icons-dred    )
    ("apache$"                 mfixx "apache"                :face all-the-icons-dgreen  )
    ("bookmark"                octicons "bookmark"           :face all-the-icons-lpink   )
    ("nginx$"                  file-icons "nginx"            :face all-the-icons-dgreen  )
    ("~$"                      octicons "lock"               :face all-the-icons-maroon  )))

(defvar all-the-icons-default-file-icon
  '(octicons "file" :face all-the-icons-dsilver))

(defvar all-the-icons-dir-icon-alist
  '(
    ("\\.[^.]+"         fluentui-system-icons "folder"  )
    ("\\.git"           devopicons "git"                )
    ("\\.hg"            file-icons "mercurial"          )
    ("\\.svn"           file-icons "svn"                )
    ("code"             octicons "code"                 )
    ("desktop"          octicons "device-desktop"       )
    ("documents"        octicons "book"                 )
    ("download"         vscode-codicons "cloud-download")
    ("dropbox"          devopicons "dropbox"            )
    ("google[ _-]drive" devopicons "google-drive"       )
    ("movies"           fontawesome-4 "film"            )
    ("music"            fontawesome-4 "music"           )
    ("photos"           fontawesome-4 "camera-retro"    )
    ("pictures"         fontawesome-4 "picture-o"       )
    ("test"             file-icons "test-directory"     )
    ("trash"            octicons "trash"                )
    ("workspace"        octicons "code"                 )))

(defvar all-the-icons-default-dir-icon
  '(fluentui-system-icons "folder" :style filled))

(defvar all-the-icons-weather-icon-alist
  '(
    ("blowing.*snow"         weather-icons "snow-wind"              )
    ("blustery"              weather-icons "cloudy-windy"           )
    ("clear.*night"          weather-icons "night-clear"            )
    ("cloudy"                weather-icons "cloudy"                 )
    ("cloudy.*night"         weather-icons "night-alt-cloudy"       )
    ("cold"                  weather-icons "snowflake-cold"         )
    ("cxloudy.*day"          weather-icons "day-cloudy"             )
    ("drizzle"               weather-icons "sprinkle"               )
    ("dust"                  weather-icons "dust"                   )
    ("fair.*day"             weather-icons "horizon"                )
    ("fair.*night"           weather-icons "stars"                  )
    ("fog"                   weather-icons "fog"                    )
    ("hail"                  weather-icons "hail"                   )
    ("haze"                  weather-icons "day-haze"               )
    ("hot"                   weather-icons "hot"                    )
    ("hurricane"             weather-icons "hurricane"              )
    ("not.*available"        weather-icons "na"                     )
    ("partly.*cloudy"        weather-icons "day-cloudy-high"        )
    ("partly.*cloudy.*night" weather-icons "night-alt-partly-cloudy")
    ("rain"                  weather-icons "showers"                )
    ("rain.*hail"            weather-icons "rain-mix"               )
    ("rain.*snow"            weather-icons "rain-mix"               )
    ("showers"               weather-icons "showers"                )
    ("sleet"                 weather-icons "sleet"                  )
    ("smoky"                 weather-icons "smoke"                  )
    ("snow"                  weather-icons "snow"                   )
    ("sunny"                 weather-icons "day-sunny"              )
    ("thunderstorms"         weather-icons "thunderstorm"           )
    ("tornado"               weather-icons "tornado"                )
    ("windy"                 weather-icons "cloudy-gusts"           )))

(defvar all-the-icons-mode-icon-alist
  '(
    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode                          -web-mode-icon)

    (Custom-mode                       octicons "sliders"                                               )
    (ada-ts-mode                       file-icons "ada"                   :face all-the-icons-dblue     )
    (adoc-mode                         file-icons "asciidoc"              :face all-the-icons-lblue     )
    (apache-mode                       mfixx "apache"                     :face all-the-icons-dgreen    )
    (archive-mode                      octicons "file-zip"                :face all-the-icons-lmaroon   )
    (asm-mode                          file-icons "assembly-generic"      :face all-the-icons-blue      )
    (bash-ts-mode                      octicons "terminal"                :face all-the-icons-purple    )
    (beancount-mode                    fontawesome-4 "credit-card"        :face all-the-icons-lgreen    )
    (benchmark-init/tree-mode          octicons "meter"                                                 )
    (bibtex-mode                       file-icons "bibtex"                :face all-the-icons-maroon    )
    (c++-mode                          mfixx "c++"                        :face all-the-icons-blue      )
    (c++-ts-mode                       mfixx "c++"                        :face all-the-icons-blue      )
    (c-mode                            mfixx "c"                          :face all-the-icons-blue      )
    (c-ts-mode                         mfixx "c"                          :face all-the-icons-blue      )
    (cfw:calendar-mode                 octicons "calendar"                                              )
    (cider-repl-mode                   devopicons "clojure"               :face all-the-icons-green     )
    (circe-channel-mode                octicons "comment-discussion"                                    )
    (circe-query-mode                  octicons "comment-discussion"                                    )
    (circe-server-mode                 octicons "comment-discussion"                                    )
    (clojure-mode                      devopicons "clojure"               :face all-the-icons-blue      )
    (clojure-ts-mode                   devopicons "clojure"               :face all-the-icons-blue      )
    (clojurescript-mode                file-icons "clojurejs"             :face all-the-icons-dblue     )
    (cmake-mode                        file-icons "cmake"                 :face all-the-icons-red       )
    (cmake-ts-mode                     file-icons "cmake"                 :face all-the-icons-red       )
    (coffee-mode                       devopicons "coffeescript"          :face all-the-icons-maroon    )
    (comint-mode                       octicons "terminal"                :face all-the-icons-lblue     )
    (compilation-mode                  vscode-codicons "combine"                                        )
    (cperl-mode                        mfixx "perl"                       :face all-the-icons-lorange   )
    (crystal-mode                      file-icons "crystal"               :face all-the-icons-yellow    )
    (csharp-mode                       mfixx "csharp"                     :face all-the-icons-dblue     )
    (csharp-ts-mode                    mfixx "csharp"                     :face all-the-icons-dblue     )
    (css-mode                          devopicons "css3"                  :face all-the-icons-yellow    )
    (css-ts-mode                       devopicons "css3"                  :face all-the-icons-yellow    )
    (csv-mode                          octicons "graph"                   :face all-the-icons-dblue     )
    (cuda-mode                         file-icons "nvidia"                :face all-the-icons-green     )
    (dart-mode                         devopicons "dart"                  :face all-the-icons-blue      )
    (dired-mode                        fontawesome-4 "folder"                                           )
    (wdired-mode                       fontawesome-4 "folder"                                           )
    (docker-compose-mode               file-icons "docker"                :face all-the-icons-lblue     )
    (dockerfile-mode                   file-icons "docker"                :face all-the-icons-blue      )
    (dockerfile-ts-mode                file-icons "docker"                :face all-the-icons-blue      )
    (ediff-mode                        octicons "git-compare"             :face all-the-icons-red       )
    (elfeed-search-mode                fontawesome-4 "rss-square"         :face all-the-icons-orange    )
    (elfeed-show-mode                  octicons "rss"                     :face all-the-icons-orange    )
    (elisp-byte-code-mode              octicons "file-binary"             :face all-the-icons-dsilver   )
    (elixir-mode                       mfixx "elixir"                     :face all-the-icons-lorange   )
    (elixir-ts-mode                    mfixx "elixir"                     :face all-the-icons-lorange   )
    (elm-mode                          file-icons "elm"                   :face all-the-icons-blue      )
    (emacs-lisp-mode                   file-icons "elisp"                 :face all-the-icons-purple    )
    (emms-browser-mode                 fontawesome-4 "music"              :face all-the-icons-silver    )
    (emms-lyrics-mode                  fontawesome-4 "music"              :face all-the-icons-silver    )
    (emms-metaplaylist-mode            fontawesome-4 "music"              :face all-the-icons-silver    )
    (emms-playlist-mode                fontawesome-4 "music"              :face all-the-icons-silver    )
    (emms-show-all-mode                fontawesome-4 "music"              :face all-the-icons-silver    )
    (emms-tag-editor-mode              fontawesome-4 "music"              :face all-the-icons-silver    )
    (enh-ruby-mode                     vscode-codicons "ruby"             :face all-the-icons-lred      )
    (erc-mode                          octicons "comment-discussion"                                    )
    (erlang-mode                       mfixx "erlang"                     :face all-the-icons-red       )
    (eshell-mode                       octicons "terminal"                :face all-the-icons-purple    )
    (eww-mode                          octicons "browser"                 :face all-the-icons-red       )
    (exwm-mode                         vscode-codicons "multiple-windows" :face all-the-icons-purple    )
    (f90-mode                          file-icons "fortran"               :face all-the-icons-purple    )
    (fish-mode                         octicons "terminal"                :face all-the-icons-lpink     )
    (fsharp-mode                       devopicons "fsharp"                :face all-the-icons-blue      )
    (fundamental-mode                  file-icons "elisp"                 :face all-the-icons-dsilver   )
    (glsl-mode                         file-icons "vertexshader"          :face all-the-icons-green     )
    (gnus-article-mode                 vscode-codicons "mail-read"                                      )
    (gnus-group-mode                   octicons "mail"                                                  )
    (gnus-summary-mode                 octicons "mail"                                                  )
    (go-dot-mod-mode                   file-icons "config-go"             :face all-the-icons-blue-alt  )
    (go-dot-work-mode                  file-icons "config-go"             :face all-the-icons-blue-alt  )
    (go-mod-ts-mode                    file-icons "config-go"             :face all-the-icons-blue-alt  )
    (go-mode                           file-icons "go"                    :face all-the-icons-blue      )
    (go-ts-mode                        file-icons "go"                    :face all-the-icons-blue      )
    (gpr-ts-mode                       file-icons "ada"                   :face all-the-icons-lblue     )
    (graphql-mode                      file-icons "graphql"               :face all-the-icons-dpink     )
    (haml-mode                         file-icons "haml"                  :face all-the-icons-lyellow   )
    (haskell-c2hs-mode                 devopicons "haskell"               :face all-the-icons-red       )
    (haskell-cabal-mode                file-icons "cabal"                 :face all-the-icons-lblue     )
    (haskell-mode                      devopicons "haskell"               :face all-the-icons-red       )
    (heex-ts-mode                      mfixx "elixir"                     :face all-the-icons-lorange   )
    (help-mode                         octicons "question"                :face all-the-icons-purple    )
    (helpful-mode                      octicons "question"                :face all-the-icons-purple    )
    (html-mode                         devopicons "html5"                 :face all-the-icons-orange    )
    (html-ts-mode                      devopicons "html5"                 :face all-the-icons-orange    )
    (hy-mode                           file-icons "hy"                    :face all-the-icons-blue      )
    (ibuffer-mode                      vscode-codicons "files"            :face all-the-icons-dsilver   )
    (image-mode                        octicons "file-media"              :face all-the-icons-blue      )
    (inf-ruby-mode                     vscode-codicons "ruby"             :face all-the-icons-red       )
    (inferior-emacs-lisp-mode          file-icons "elisp"                 :face all-the-icons-lblue     )
    (inferior-python-mode              devopicons "python"                :face all-the-icons-dblue     )
    (jade-mode                         file-icons "jade"                  :face all-the-icons-red       )
    (java-mode                         devopicons "java"                  :face all-the-icons-purple    )
    (java-ts-mode                      devopicons "java"                  :face all-the-icons-purple    )
    (jenkins-mode                      file-icons "jenkins"               :face all-the-icons-blue      )
    (jinja2-mode                       file-icons "jinja"                 :face all-the-icons-silver    )
    (js-jsx-mode                       file-icons "jsx-atom"              :face all-the-icons-yellow    )
    (js-mode                           mfixx "javascript"                 :face all-the-icons-yellow    )
    (js-ts-mode                        mfixx "javascript"                 :face all-the-icons-yellow    )
    (js2-mode                          mfixx "javascript"                 :face all-the-icons-yellow    )
    (js3-mode                          mfixx "javascript"                 :face all-the-icons-yellow    )
    (json-mode                         vscode-codicons "json"             :face all-the-icons-yellow    )
    (json-ts-mode                      vscode-codicons "json"             :face all-the-icons-yellow    )
    (jsonian-mode                      vscode-codicons "json"             :face all-the-icons-yellow    )
    (julia-mode                        file-icons "julia"                 :face all-the-icons-purple    )
    (julia-ts-mode                     file-icons "julia"                 :face all-the-icons-purple    )
    (kotlin-mode                       file-icons "kotlin"                :face all-the-icons-orange    )
    (kotlin-ts-mode                    file-icons "kotlin"                :face all-the-icons-orange    )
    (latex-mode                        file-icons "latex"                 :face all-the-icons-lred      )
    (ledger-mode                       fontawesome-4 "credit-card"        :face all-the-icons-lgreen    )
    (less-css-mode                     devopicons "less"                  :face all-the-icons-dyellow   )
    (lilypond-mode                     fontawesome-4 "music"              :face all-the-icons-green     )
    (lisp-interaction-mode             file-icons "lisp"                  :face all-the-icons-orange    )
    (lisp-mode                         file-icons "lisp"                  :face all-the-icons-orange    )
    (literate-haskell-mode             devopicons "haskell"               :face all-the-icons-red       )
    (lua-mode                          file-icons "lua"                   :face all-the-icons-dblue     )
    (magik-cb-mode                     octicons "book"                    :face all-the-icons-blue      )
    (magik-session-mode                octicons "terminal"                :face all-the-icons-blue      )
    (magit-diff-mode                   octicons "git-compare"             :face all-the-icons-lblue     )
    (magit-log-mode                    devopicons "git"                   :face all-the-icons-green     )
    (magit-popup-mode                  devopicons "git"                   :face all-the-icons-red       )
    (magit-process-mode                octicons "mark-github"                                           )
    (magit-refs-mode                   octicons "git-branch"              :face all-the-icons-red       )
    (magit-status-mode                 devopicons "git"                   :face all-the-icons-lred      )
    (makefile-mode                     file-icons "gnu"                   :face all-the-icons-dorange   )
    (man-common                        file-icons "manpage"               :face all-the-icons-blue      )
    (markdown-mode                     octicons "markdown"                :face all-the-icons-lblue     )
    (matlab-mode                       file-icons "matlab"                :face all-the-icons-orange    )
    (meson-mode                        file-icons "meson"                 :face all-the-icons-purple    )
    (message-mode                      octicons "pencil"                                                )
    (messages-buffer-mode              octicons "log"                     :face all-the-icons-dsilver   )
    (mu4e-compose-mode                 octicons "pencil"                                                )
    (mu4e-headers-mode                 octicons "mail"                                                  )
    (mu4e-main-mode                    octicons "mail"                                                  )
    (mu4e-view-mode                    vscode-codicons "mail-read"                                      )
    (mustache-mode                     file-icons "moustache"             :face all-the-icons-green     )
    (nasm-mode                         file-icons "nasm"                  :face all-the-icons-blue      )
    (nginx-mode                        file-icons "nginx"                 :face all-the-icons-dgreen    )
    (nim-mode                          file-icons "nimrod"                :face all-the-icons-yellow    )
    (nix-mode                          file-icons "nix"                   :face all-the-icons-blue      )
    (nix-ts-mode                       file-icons "nix"                   :face all-the-icons-blue      )
    (nxml-mode                         octicons "file-code"               :face all-the-icons-lorange   )
    (objc-mode                         mfixx "objc"                                                     )
    (ocaml-ts-mode                     file-icons "ocaml"                 :face all-the-icons-orange    )
    (odin-mode                         file-icons "odin"                  :face all-the-icons-lblue     )
    (org-agenda-mode                   octicons "checklist"               :face all-the-icons-lgreen    )
    (org-mode                          file-icons "org"                   :face all-the-icons-lgreen    )
    (org-mode                          file-icons "org"                   :face all-the-icons-lgreen    )
    (package-menu-mode                 octicons "package"                 :face all-the-icons-silver    )
    (paradox-menu-mode                 octicons "package"                 :face all-the-icons-silver    )
    (pdf-view-mode                     vscode-codicons "file-pdf"         :face all-the-icons-dred      )
    (perl-mode                         mfixx "perl"                       :face all-the-icons-lorange   )
    (php-mode                          file-icons "php"                   :face all-the-icons-lsilver   )
    (powershell-mode                   file-icons "powershell"            :face all-the-icons-blue      )
    (projectile-rails-compilation-mode mfixx "rails"                      :face all-the-icons-red       )
    (prolog-mode                       devopicons "prolog"                :face all-the-icons-lmaroon   )
    (pug-mode                          file-icons "pug"                   :face all-the-icons-red       )
    (puppet-mode                       file-icons "puppet"                :face all-the-icons-yellow    )
    (purescript-mode                   file-icons "purescript"                                          )
    (python-mode                       devopicons "python"                :face all-the-icons-dblue     )
    (python-ts-mode                    devopicons "python"                :face all-the-icons-dblue     )
    (racket-mode                       file-icons "racket"                :face all-the-icons-red       )
    (rake-compilation-mode             file-icons "config-ruby"           :face all-the-icons-red       )
    (rhtml-mode                        devopicons "html5"                 :face all-the-icons-lred      )
    (rjsx-mode                         file-icons "jsx-atom"              :face all-the-icons-cyan-alt  )
    (rspec-compilation-mode            file-icons "test-ruby"             :face all-the-icons-red       )
    (rst-mode                          file-icons "restructuredtext"      :face all-the-icons-lblue     )
    (ruby-mode                         vscode-codicons "ruby"             :face all-the-icons-lred      )
    (ruby-ts-mode                      vscode-codicons "ruby"             :face all-the-icons-lred      )
    (rust-mode                         devopicons "rust"                  :face all-the-icons-maroon    )
    (rust-ts-mode                      devopicons "rust"                  :face all-the-icons-maroon    )
    (rustic-mode                       devopicons "rust"                  :face all-the-icons-maroon    )
    (sass-mode                         devopicons "sass"                  :face all-the-icons-dpink     )
    (scala-mode                        devopicons "scala"                 :face all-the-icons-red       )
    (scheme-mode                       file-icons "scheme"                :face all-the-icons-red       )
    (scss-mode                         devopicons "sass"                  :face all-the-icons-pink      )
    (sh-mode                           octicons "terminal"                :face all-the-icons-purple    )
    (shell-mode                        octicons "terminal"                :face all-the-icons-purple    )
    (sieve-mode                        octicons "mail"                                                  )
    (slim-mode                         octicons "meter"                   :face all-the-icons-yellow    )
    (slime-repl-mode                   file-icons "common-lisp"           :face all-the-icons-orange    )
    (sly-mrepl-mode                    file-icons "common-lisp"           :face all-the-icons-orange    )
    (spacemacs-buffer-mode             file-icons "elisp"                 :face all-the-icons-purple    )
    (special-mode                      file-icons "elisp"                 :face all-the-icons-yellow    )
    (sql-mode                          octicons "database"                :face all-the-icons-silver    )
    (stylus-mode                       file-icons "stylus"                :face all-the-icons-lgreen    )
    (svelte-mode                       file-icons "svelte"                :face all-the-icons-red       )
    (swift-mode                        devopicons "swift"                 :face all-the-icons-green     )
    (tcl-mode                          file-icons "tcl"                   :face all-the-icons-dred      )
    (term-mode                         octicons "terminal"                                              )
    (terraform-mode                    file-icons "terraform"             :face all-the-icons-purple-alt)
    (tex-mode                          file-icons "latex"                 :face all-the-icons-lred      )
    (texinfo-mode                      file-icons "gnu"                   :face all-the-icons-lred      )
    (text-mode                         fontawesome-4 "file-text-o"        :face all-the-icons-cyan      )
    (toml-ts-mode                      file-icons "toml"                  :face all-the-icons-dmaroon   )
    (tsx-ts-mode                       file-icons "tsx"                   :face all-the-icons-cyan-alt  )
    (tuareg-mode                       file-icons "ocaml"                                               )
    (typescript-mode                   file-icons "typescript"            :face all-the-icons-blue-alt  )
    (typescript-ts-mode                file-icons "typescript"            :face all-the-icons-blue-alt  )
    (typescript-tsx-mode               file-icons "tsx"                   :face all-the-icons-cyan-alt  )
    (verilog-mode                      file-icons "verilog"               :face all-the-icons-red       )
    (verilog-ts-mode                   file-icons "verilog"               :face all-the-icons-red       )
    (vhdl-mode                         file-icons "vhdl"                  :face all-the-icons-blue      )
    (vhdl-ts-mode                      file-icons "vhdl"                  :face all-the-icons-blue      )
    (vterm-mode                        octicons "terminal"                                              )
    (yaml-mode                         file-icons "yaml-alt4"             :face all-the-icons-dyellow   )
    (yaml-ts-mode                      file-icons "yaml-alt4"             :face all-the-icons-dyellow   )
    (zig-mode                          file-icons "zig"                   :face all-the-icons-orange    )))

;; ====================
;;   Functions Start
;; ====================

(defun all-the-icons-auto-mode-match-p (&optional file)
  "Whether or not FILE's `major-mode' match against its `auto-mode-alist'."
  (let* ((file (or file (buffer-file-name) (buffer-name)))
         (auto-mode (all-the-icons-match-to-alist file auto-mode-alist)))
    (eq major-mode auto-mode)))

(defun all-the-icons-match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match-p'."
  (cdr (cl-find-if (lambda (it) (string-match-p (car it) file)) alist)))

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

;;;###autoload
(defun all-the-icons-icon-for-dir-with-chevron (dir &optional chevron padding)
  "Format an icon for DIR with CHEVRON similar to tree based directories.

If PADDING is provided, it will prepend and separate the chevron
and directory with PADDING.

Produces different symbols by inspecting DIR to distinguish
symlinks and git repositories which do not depend on the
directory contents"
  (let ((icon (all-the-icons-icon-for-dir dir))
        (chevron (if chevron (all-the-icons-octicons (format "chevron-%s" chevron)) ""))
        (padding (or padding "\t")))
    (format "%s%s%s%s%s" padding chevron padding icon padding)))

;;;###autoload
(defun all-the-icons-icon-for-buffer ()
  "Get the formatted icon for the current buffer.

This function prioritises the use of the buffers file extension to
discern the icon when its `major-mode' matches its auto mode,
otherwise it will use the buffers `major-mode' to decide its
icon."
  (all-the-icons--icon-info-for-buffer))

(defun all-the-icons--web-mode-icon (&rest arg-overrides)
  "Return icon for `web-mode' based on `web-mode-content-type'.
Providing ARG-OVERRIDES will modify the creation of the icon."
  (let ((non-nil-args (cl-reduce (lambda (acc it)
                                   (if it (append acc (list it)) acc))
                                 arg-overrides :initial-value '())))
    (cond
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "jsx"))
      (apply #'all-the-icons-file-icons (append '("jsx-atom") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "javascript"))
      (apply #'all-the-icons-mfixx (append '("javascript") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "json"))
      (apply #'all-the-icons-vscode-codicons (append '("json") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "xml"))
      (apply #'all-the-icons-octicons (append '("file-code") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "css"))
      (apply #'all-the-icons-devopicons (append '("css3") non-nil-args)))
     (t
      (apply #'all-the-icons-devopicons (append '("html5") non-nil-args))))))

;; Icon Functions

;;;###autoload
(defun all-the-icons-icon-for-dir (dir &rest arg-overrides)
  "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining properties like in
the normal icon inserting functions.

Note: You want chevron, please use `all-the-icons-icon-for-dir-with-chevron'."
  (let* ((dirname (file-name-base (directory-file-name dir)))
         (icon (or (all-the-icons-match-to-alist dirname all-the-icons-dir-icon-alist)
                   all-the-icons-default-dir-icon))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if (file-remote-p dir) ;; don't call expand-file-name on a remote dir as this can make emacs hang
        (apply #'all-the-icons-octicons "terminal" (cdr args))
      (let ((path (expand-file-name dir)))
        (cond
         ((file-symlink-p path)
          (apply #'all-the-icons-vscode-codicons "file-symlink-directory" (cdr args)))
         ((all-the-icons-dir-is-submodule path)
          (apply #'all-the-icons-octicons "file-submodule" (cdr args)))
         ((file-exists-p (format "%s/.git" path))
          (apply #'all-the-icons-octicons "repo" (cdr args)))
         (t (apply (all-the-icons--function-name (car icon)) args)))))))

;;;###autoload
(defun all-the-icons-icon-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining properties like in
the normal icon inserting functions."
  (let* ((ext (file-name-extension file))
         (icon (or (all-the-icons-match-to-alist file all-the-icons-regexp-icon-alist)
                   (and ext
                        (cdr (assoc (downcase ext)
                                    all-the-icons-extension-icon-alist)))
                   all-the-icons-default-file-icon))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (all-the-icons--function-name (car icon)) args)))

;;;###autoload
(defun all-the-icons-icon-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining properties like in
the normal icon inserting functions."
  (let* ((icon (cdr (or (assoc mode all-the-icons-mode-icon-alist)
                        (assoc (get mode 'derived-mode-parent) all-the-icons-mode-icon-alist))))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if icon (apply (all-the-icons--function-name (car icon)) args) mode)))

(defcustom all-the-icons--cache-limit 2048
  "Maximum cache size for functions cached by `all-the-icons-cache'."
  :type 'integer)

(defun all-the-icons-cache (func)
  "Set a cache for FUNC.

Does not work on interactive functions."
  (unless (get func 'all-the-icons--cached)
    (let ((cache (make-hash-table :test #'equal
                                  :size all-the-icons--cache-limit))
          (orig-fn (symbol-function func)))
      (fset func
            (lambda (&rest args)
              (or (gethash args cache)
                  (progn
                    (when (> (hash-table-count cache)
                             all-the-icons--cache-limit)
                      (clrhash cache))
                    (puthash args (apply orig-fn args) cache)))))))

  (put func 'all-the-icons--cached t))

(all-the-icons-cache #'all-the-icons-icon-for-dir)
(all-the-icons-cache #'all-the-icons-icon-for-dir-with-chevron)
(all-the-icons-cache #'all-the-icons-icon-for-file)
(all-the-icons-cache #'all-the-icons-icon-for-mode)
(all-the-icons-cache #'all-the-icons-icon-for-weather)

(defun all-the-icons--icon-info-for-buffer (&optional f)
  "Get icon info for the current buffer.

When F is provided, the info function is calculated with the format
`all-the-icons-icon-%s-for-file' or `all-the-icons-icon-%s-for-mode'."
  (let* ((base-f (concat "all-the-icons-icon" (when f (format "-%s" f))))
         (file-f (intern (concat base-f "-for-file")))
         (mode-f (intern (concat base-f "-for-mode"))))
    (if (and (buffer-file-name)
             (all-the-icons-auto-mode-match-p))
        (funcall file-f (file-name-nondirectory (buffer-file-name)))
      (funcall mode-f major-mode))))

;; Weather icons
(defun all-the-icons-icon-for-weather (weather)
  "Get an icon for a WEATHER status."
  (let ((icon (all-the-icons-match-to-alist weather all-the-icons-weather-icon-alist)))
    (if icon (apply (all-the-icons--function-name (car icon)) (cdr icon)) weather)))

;; Definitions

(eval-and-compile
  (defun all-the-icons--function-name (name)
    "Get the symbol for an icon function name for icon set NAME."
    (intern (concat "all-the-icons-" (downcase (symbol-name name)))))

  (defun all-the-icons--data-name (name)
    "Get the symbol for an icon data function for icon set NAME."
    (intern (concat "all-the-icons-" (downcase (symbol-name name)) "-data")))

  (defun all-the-icons--insert-function-name (name)
    "Get the symbol for an icon insert function for icon set NAME."
    (intern (concat "all-the-icons-insert-" (downcase (symbol-name name))))))

;; Icon insertion functions

(defun all-the-icons--read-candidates ()
  "Helper to build a list of candidates for all icon set."
  (cl-reduce 'append (mapcar (lambda (it) (all-the-icons--read-candidates-for-icon-set it t)) all-the-icons-sets)))

(defun all-the-icons--read-candidates-for-icon-set (icon-set &optional show-icon-set style)
  "Helper to build read candidates for ICON-SET.

If SHOW-ICON-SET is non-nil, displays the icons set name in the
candidate string.

If STYLE is non-nil, displays the chosen style for the icon in
the icon set."
  (let ((data   (funcall (all-the-icons--data-name icon-set)))
        (icon-f (all-the-icons--function-name icon-set)))
    (mapcar
     (lambda (it)
       (let* ((icon-name (car it))
              (icon-name-head (substring icon-name 0 1))
              (icon-name-tail (substring icon-name 1))

              (icon-display (propertize icon-name-head 'display
                                        (format "%s\t%s"
                                                (funcall icon-f icon-name :style style :raise-error nil)
                                                icon-name-head)))
              (icon-set-name (if show-icon-set (format "\t[%s]" icon-set) ""))
              (icon-style (if style (format " (%s) " style) ""))

              (candidate-name (format "%s%s%s%s" icon-display icon-name-tail icon-style icon-set-name))
              (candidate-icon (funcall icon-f icon-name :style style :raise-error nil)))

         (cons candidate-name candidate-icon)))
     data)))

;;;###autoload
(defun all-the-icons-insert (&optional arg icon-set)
  "Interactive icon insertion function.

When Prefix ARG is non-nil, print the icon, else insert it.
When ICON-SET is non-nil, limit the candidates to the icon set matching it."
  (interactive "P")
  (let* ((standard-output (current-buffer))
         (candidates (cond ((eq icon-set 'fluentui-system-icons)
                            (flatten-list
                             (cl-remove
                              nil
                              (mapcar
                               (lambda (style) (all-the-icons--read-candidates-for-icon-set icon-set nil style))
                               '(filled regular)))))
                           ((eq icon-set 'material-icons)
                            (flatten-list
                             (cl-remove
                              nil
                              (mapcar
                               (lambda (style) (all-the-icons--read-candidates-for-icon-set icon-set nil style))
                               '(nil outlined round sharp twotone)))))
                           (icon-set (all-the-icons--read-candidates-for-icon-set icon-set))
                           (t (all-the-icons--read-candidates))))
         (prompt    (if icon-set
                        (format "%s Icon: " icon-set)
                      "Icon : "))

         (selection (completing-read prompt candidates nil t))
         (result    (cdr (assoc selection candidates))))

    (if arg (prin1 result) (insert result))))

;; Debug Helpers

(defun all-the-icons-insert-icons-for (icon-set &optional duration)
  "Insert all of the available icons associated with ICON-SET.

If DURATION is provided, it will pause for DURATION seconds
between printing each character."
  (let* ((data-f    (all-the-icons--data-name icon-set))
         (insert-f  (all-the-icons--function-name icon-set))
         (data      (funcall data-f)))
    (mapc
     (lambda (it)
       (let* ((icon-name (car it)))
         (cond ((eq icon-set 'fluentui-system-icons)
                (dolist (style '(filled regular))
                  (when-let ((icon (funcall insert-f icon-name :style style :raise-error nil)))
                    (insert (format "%s - %s-%s\n" icon icon-name style)))))
               ((eq icon-set 'material-icons)
                (dolist (style '(nil outlined round sharp twotone))
                  (when-let ((icon (funcall insert-f icon-name :style style :raise-error nil)))
                    (insert (format "%s - %s%s\n" icon icon-name (or (and style (format "-%s" style)) ""))))))
               (t (insert (format "%s - %s\n" (funcall insert-f icon-name) icon-name)))))
       (when duration (sit-for duration)))
     data)))

(defun all-the-icons-insert-icons-for-extensions ()
  "Insert all of the icons for `all-the-icons-extension-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-extension-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n"
                    (apply (all-the-icons--function-name (cadr entry))
                           (cddr entry))
                    (car entry)))))

(defun all-the-icons-insert-icons-for-modes ()
  "Insert all of the icons for `all-the-icons-mode-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-mode-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n"
                    (apply (all-the-icons--function-name (cadr entry))
                           (cddr entry))
                    (car entry)))))

(defun all-the-icons-insert-icons-for-regexp ()
  "Insert all of the icons for `all-the-icons-regexp-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-regexp-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n"
                    (apply (all-the-icons--function-name (cadr entry))
                           (cddr entry))
                    (car entry)))))

(defun all-the-icons-insert-icons-for-dirs ()
  "Insert all of the icons for `all-the-icons-dir-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-dir-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n"
                    (apply (all-the-icons--function-name (cadr entry))
                           (cddr entry))
                    (car entry)))))

;; SVG helper functions

(defun all-the-icons--parse-number (s)
  "Parse a number from the string S and convert it to a number."
  (save-match-data
    (string-match "[+-]?[0-9]*\\(\\.[0-9]+\\)?" s)
    (string-to-number (match-string 0 s))))

(defun all-the-icons--load-svg (path)
  "Load the SVG file at PATH as an XML document."
  (with-temp-buffer
    (insert-file-contents path)
    (if (libxml-available-p)
        (libxml-parse-xml-region (point-min) (point-max))
      (car (xml-parse-region (point-min) (point-max))))))

(defun all-the-icons--normalize-svg-doc (doc)
  "Normalize the dimension of a SVG document.

Some icon set icons do not all have the same width.  When
displayed, they result in variable widths, which is usually not
desired in a monospaced text editor.

To deal with this issue, the max of the width, height, and the
width and height components of viewBox attribute in the SVG
document DOC are used to calculate the size of the image.

The size is then used to reset the width, height and the viewBox.

Finally, the paths of the image is then translated back to the
middle of the offset between the size and the original width and
height."
  (let* ((viewbox (dom-attr doc 'viewBox))
         (vw (and viewbox (all-the-icons--parse-number (nth 2 (split-string viewbox)))))
         (vh (and viewbox (all-the-icons--parse-number (nth 3 (split-string viewbox)))))
         (width (dom-attr doc 'width))
         (height (dom-attr doc 'height))
         (w (and width (all-the-icons--parse-number width)))
         (h (and height (all-the-icons--parse-number height)))
         (size (apply 'max (seq-filter 'numberp (list w h vw vh)))))
    (dom-set-attribute doc 'width (format "%s" size))
    (dom-set-attribute doc 'height (format "%s" size))
    (dom-set-attribute doc 'viewBox (format "0 0 %s %s" size size))
    (when (or (and w (< w size)) (and h (< h size)))
      (dom-set-attribute doc 'transform (format "translate(%s %s)"
                                                (/ (- size (or w vw)) 2)
                                                (/ (- size (or h vh)) 2))))
    (svg-image doc)))

(defun all-the-icons--resolve-icon-file-name (icon-name alist icon-set)
  "Look up ALIST for ICON-NAME.

If ICON-NAME is not found in ALIST, and a compatibility alist is
available, look up the icon name from the compatibility list,
otherwise return nil.

Return the icon file name if found."
  (let ((file-name (assoc-default icon-name alist)))
    (or file-name
        (when-let ((mapping
                    (assoc-default
                     icon-name
                     (cond ((eq icon-set 'file-icons)
                            all-the-icons-data-file-icons-compat-alist)
                           ((eq icon-set 'octicons)
                            all-the-icons-data-octicons-compat-alist)
                           ((eq icon-set 'alltheicon)
                            all-the-icons-data-alltheicons-compat-alist))))
                   (fn (all-the-icons--data-name (car mapping))))
          (assoc-default (cadr mapping) (funcall fn))))))

(defconst all-the-icons--lib-dir (file-name-directory (locate-library "all-the-icons")))

(cl-defmacro all-the-icons-define-icon (name alist &key svg-path-finder (svg-doc-processor ''identity) (padding 0))
  "Macro to generate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `all-the-icons-NAME'.

ALIST is the alist containing maps between icon names and the
Unicode for the character.  All of these can be found in the data
directory of this package.

SVG-PATH-FINDER is function that takes the icon name, the icon
set subdirectory, a desired size and a plist specific to the icon
set and returns the path of an SVG image from the icon set
subdirectory.

SVG-DOC-PROCESSOR is a function that takes an SVG document,
processes it arbitrarily and returns the processed document.

PADDING is the number of pixels to be applied to the SVG image."
  `(progn
     (add-to-list 'all-the-icons-sets (quote ,name))
     (defun ,(all-the-icons--data-name name) () ,alist)
     (defun ,(all-the-icons--function-name name) (icon-name &rest args)
       (let* ((file-name (all-the-icons--resolve-icon-file-name icon-name ,alist (quote ,name))) ;; remap icons
              (size (window-default-font-height))
              (lib-dir (concat all-the-icons--lib-dir ,(format "svg/%s/" name)))
              (image-path (concat lib-dir ,(or (and svg-path-finder
                                                    `(apply ,svg-path-finder file-name lib-dir size args))
                                               '(format "%s.svg" file-name))))
              (face (when all-the-icons-color-icons (plist-get args :face)))
              (raise-error (plist-get args :raise-error))
              (custom-padding (plist-get args :padding)))
         (if (and file-name (file-exists-p image-path))
             (let* ((icon (all-the-icons--normalize-svg-doc
                           (funcall ,svg-doc-processor
                                    (all-the-icons--load-svg image-path)))))
               (setf (image-property icon :max-width) (- size (* ,padding 2)))
               (setf (image-property icon :max-height) (- size (* ,padding 2)))
               (setf (image-property icon :ascent) 'center)
               (setf (image-property icon :margin) (or custom-padding ,padding))
               (when face
                 (setf (image-property icon :foreground) (face-foreground face))
                 (setf (image-property icon :background) (face-background face)))

               (propertize "￼"
                           'face (or face 'default)           ;so that this works without `font-lock-mode' enabled
                           'font-lock-face (or face 'default) ;so that `font-lock-mode' leaves this alone
                           'fontified t
                           'display icon
                           'front-sticky nil
                           'rear-nonsticky t))
           (when raise-error
             (error (format "Unable to find icon with name `%s' in icon set `%s'" icon-name (quote ,name)))))))
     (defun ,(all-the-icons--insert-function-name name) (&optional arg)
       ,(format "Insert a %s icon at point." name)
       (interactive "P")
       (all-the-icons-insert arg (quote ,name)))))

(defun all-the-icons--octicons-path (name dir size &rest _)
  "SVG path finder function for Octicons.

See `all-the-icons-define-icon' for the meaning of NAME, DIR and
SIZE."
  (let* ((path-format (apply-partially 'format "%s/%s-%s.svg" dir name))
         (size (car
                (sort (cl-loop for s in '(12 16 24 48 96)
                               if (file-exists-p (funcall path-format s))
                               collect s)
                      (lambda (a b)
                        (if (< (abs (- a size))
                               (abs (- b size)))
                            a
                          b))))))
    (format "%s-%s.svg" name size)))

(defun all-the-icons--fluentui-system-icons-path (name dir size &rest args)
  "SVG path finder function for FluentUI System Icons.

See `all-the-icons-define-icon' for the meaning of NAME, DIR and
SIZE, and ARGS."
  (let* ((style (or (plist-get args :style) 'regular))
         (path-format (lambda (size) (format "%s/ic_fluent_%s_%s_%s.svg" dir name size style)))
         (size (car
                (sort (cl-loop for s in '(10 12 16 20 24 28 32 48)
                               if (file-exists-p (funcall path-format s))
                               collect s)
                      (lambda (a b)
                        (if (< (abs (- a size))
                               (abs (- b size)))
                            a
                          b))))))
    (format "ic_fluent_%s_%s_%s.svg" name size style)))

(defun all-the-icons--material-icons-path (name dir size &rest args)
  "SVG path finder function for Material Icons.

See `all-the-icons-define-icon' for the meaning of NAME, DIR and
SIZE, and ARGS."
  (let* ((style (or (plist-get args :style) ""))
         (path-format (lambda (size) (format "%s/%s/materialicons%s/%spx.svg" dir name style size)))
         (size (car
                (sort (cl-loop for s in '(18 20 24 36)
                               if (file-exists-p (funcall path-format s))
                               collect s)
                      (lambda (a b)
                        (if (< (abs (- a size))
                               (abs (- b size)))
                            a
                          b))))))
    (format "%s/materialicons%s/%spx.svg" name style size)))

(all-the-icons-define-icon alltheicon all-the-icons-data-alltheicons-alist
                           :padding 1)

(all-the-icons-define-icon devopicons all-the-icons-data-devopicons-alist
                           :padding 1)

(all-the-icons-define-icon file-icons all-the-icons-data-file-icons-alist
                           :padding 1)

(all-the-icons-define-icon mfixx all-the-icons-data-mfixx-alist
                           :padding 1)

(all-the-icons-define-icon octicons all-the-icons-data-octicons-alist
                           :svg-path-finder 'all-the-icons--octicons-path
                           :padding 1)

(all-the-icons-define-icon weather-icons all-the-icons-data-weather-icons-alist)

(all-the-icons-define-icon vscode-codicons all-the-icons-data-vscode-codicons-alist
                           :padding 1)

(all-the-icons-define-icon fontawesome-4 all-the-icons-data-fontawesome-4-alist
                           :padding 1)

(all-the-icons-define-icon fluentui-system-icons all-the-icons-data-fluentui-system-icons-alist
                           :svg-path-finder 'all-the-icons--fluentui-system-icons-path)

(all-the-icons-define-icon material-icons all-the-icons-data-material-icons-alist
                           :svg-path-finder 'all-the-icons--material-icons-path)

(defun all-the-icons-weather-icons-advice (fn icon-name &rest args)
  "If ICON-NAME is `\"wind-direction\"', return an icon pointing at
`:direction'.

FN is `all-the-icons-weather-icons'."
  (let ((icon (apply fn icon-name args))
        (direction (plist-get args :direction)))
    (if (and (equal icon-name "wind-direction")
             direction)
        (let* ((image (get-text-property 0 'display icon))
               (image-props (map-into (map-filter (lambda (k _) (not (memq k '(:type :data)))) (cdr image)) 'plist))
               (doc (with-temp-buffer
                      (insert (plist-get (cdr image) :data))
                      (if (libxml-available-p)
                          (libxml-parse-xml-region (point-min) (point-max))
                        (car (xml-parse-region (point-min) (point-max))))))
               (viewbox (dom-attr doc 'viewBox))
               (vw (and viewbox (all-the-icons--parse-number (nth 2 (split-string viewbox)))))
               (vh (and viewbox (all-the-icons--parse-number (nth 3 (split-string viewbox)))))
               (path (car (dom-by-tag doc 'path)))
               (transform (dom-attr path 'transform)))
          (save-match-data
            (cond ((string-match "\\(towards\\|from\\)-\\([[:digit:]]+\\)-deg" direction)
                   (let* ((face (match-string 1 direction))
                          (deg (string-to-number (match-string 2 direction)))
                          (offset (if (equal face "from") 180 0)))

                     (dom-set-attribute
                      path
                      'transform
                      (concat (if transform (concat (string-trim transform) " ") "")
                              (format "rotate(%s %s %s)"
                                      (mod (+ offset deg) 360)
                                      (/ vw 2)
                                      (/ vh 2))))

                     (propertize
                      icon
                      'display (append `(image :type svg
                                               :data ,(with-temp-buffer
                                                        (svg-print doc)
                                                        (buffer-string)))
                                       image-props))))

                  ((string-match "\\(towards\\|from\\)-\\([nsew]+\\)" direction)
                   (let* ((face (match-string 1 direction))
                          (cardinal (match-string 2 direction))
                          (offset (if (equal face "from") 180 0))
                          (deg (cond ((equal cardinal "n") 0)
                                     ((equal cardinal "nne") 23)
                                     ((equal cardinal "ne") 45)
                                     ((equal cardinal "ene") 68)
                                     ((equal cardinal "e") 90)
                                     ((equal cardinal "ese") 113)
                                     ((equal cardinal "se") 135)
                                     ((equal cardinal "sse") 158)
                                     ((equal cardinal "s") 180)
                                     ((equal cardinal "ssw") 203)
                                     ((equal cardinal "sw") 225)
                                     ((equal cardinal "wsw") 248)
                                     ((equal cardinal "w") 270)
                                     ((equal cardinal "wnw") 293)
                                     ((equal cardinal "nw") 313)
                                     ((equal cardinal "nnw") 336))))

                     (apply 'all-the-icons-weather-icons
                            icon-name
                            (plist-put
                             (copy-sequence args)
                             :direction
                             (format "%s-%s-deg" face (mod (+ offset deg) 360))))))

                  (t icon))))
      icon)))

(advice-add 'all-the-icons-weather-icons :around #'all-the-icons-weather-icons-advice)

(provide 'all-the-icons)

;;; all-the-icons.el ends here
