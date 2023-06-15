;;; all-the-icons.el --- A library for inserting Developer icons -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Version: 5.0.0
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
;;   `all-the-icons-icon-for-dir'
;;   `all-the-icons-icon-for-file'
;;   `all-the-icons-icon-for-mode'
;;   `all-the-icons-icon-for-url'

;; Which can be used to get a formatted icon for the current buffer, a
;; file name, a major mode, or an URL respectively.  e.g.

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
(require 'cl-lib)

(require 'data-alltheicons  "./data/data-alltheicons")
(require 'data-faicons      "./data/data-faicons")
(require 'data-fileicons    "./data/data-fileicons")
(require 'data-octicons     "./data/data-octicons")
(require 'data-weathericons "./data/data-weathericons")
(require 'data-material     "./data/data-material")

(require 'all-the-icons-faces)

(defvar web-mode-content-type) ;silence byte-compiler warning
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

(defcustom all-the-icons-fonts-subdirectory nil
  "The subdirectory within the system fonts folder where the icons are installed."
  :group 'all-the-icons
  :type 'directory)

(defvar all-the-icons-font-families '() "List of defined icon font families.")
(defvar all-the-icons-font-names '() "List of defined font file names this package was built with.")

(defvar all-the-icons-extension-icon-alist
  '(
    ("fish"         all-the-icons-alltheicon "terminal"       :face all-the-icons-lpink)
    ("zsh"          all-the-icons-alltheicon "terminal"       :face all-the-icons-lcyan)
    ("sh"           all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
    ;; Meta
    ("tags"         all-the-icons-octicon "tag"               :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("log"          all-the-icons-octicon "bug"               :height 1.0 :v-adjust 0.0 :face all-the-icons-maroon)
    ;; Config
    ("node"         all-the-icons-alltheicon "nodejs"         :height 1.0  :face all-the-icons-green)
    ("babelrc"      all-the-icons-fileicon "babel"            :face all-the-icons-yellow)
    ("bashrc"       all-the-icons-alltheicon "script"         :height 0.9  :face all-the-icons-dpink)
    ("bowerrc"      all-the-icons-alltheicon "bower"          :height 1.0 :v-adjust 0.0 :face all-the-icons-silver)
    ("cr"           all-the-icons-fileicon "crystal"          :v-adjust 0.0 :face all-the-icons-yellow)
    ("ecr"           all-the-icons-fileicon "crystal"          :v-adjust 0.0 :face all-the-icons-yellow)
    ("ini"          all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-yellow)
    ("eslintignore" all-the-icons-fileicon "eslint"           :height 0.9  :face all-the-icons-purple)
    ("eslint"       all-the-icons-fileicon "eslint"           :height 0.9  :face all-the-icons-lpurple)
    ("git"          all-the-icons-alltheicon "git"            :height 1.0  :face all-the-icons-lred)
    ("mk"           all-the-icons-fileicon "gnu"              :face all-the-icons-dorange)
    ("cmake"        all-the-icons-fileicon "cmake"            :face all-the-icons-red)
    ("dockerignore" all-the-icons-fileicon "dockerfile"       :height 1.2  :face all-the-icons-dblue)
    ("xml"          all-the-icons-faicon "file-code-o"        :height 0.95 :face all-the-icons-lorange)
    ("json"         all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-yellow)
    ("cson"         all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-yellow)
    ("yml"          all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
    ("yaml"         all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
    ;; ?
    ("pkg"          all-the-icons-octicon "package"           :v-adjust 0.0 :face all-the-icons-dsilver)
    ("rpm"          all-the-icons-octicon "package"           :v-adjust 0.0 :face all-the-icons-dsilver)
    ("pkgbuild"     all-the-icons-octicon "package"           :v-adjust 0.0 :face all-the-icons-dsilver)
    ("elc"          all-the-icons-octicon "file-binary"       :v-adjust 0.0 :face all-the-icons-dsilver)
    ("gz"           all-the-icons-octicon "file-binary"       :v-adjust 0.0 :face all-the-icons-lmaroon)
    ("zip"          all-the-icons-octicon "file-zip"          :v-adjust 0.0 :face all-the-icons-lmaroon)
    ("7z"           all-the-icons-octicon "file-zip"          :v-adjust 0.0 :face all-the-icons-lmaroon)
    ("dat"          all-the-icons-faicon "bar-chart"          :face all-the-icons-cyan :height 0.9)
    ("dmg"          all-the-icons-octicon "tools"             :v-adjust 0.0 :face all-the-icons-lsilver)
    ("dll"          all-the-icons-faicon "cogs"               :face all-the-icons-silver)
    ("ds_store"     all-the-icons-faicon "cogs"               :face all-the-icons-silver)
    ;; Source Codes
    ("scpt"         all-the-icons-fileicon "apple"            :face all-the-icons-pink)
    ("aup"          all-the-icons-fileicon "audacity"         :face all-the-icons-yellow)
    ("elm"          all-the-icons-fileicon "elm"              :face all-the-icons-blue)
    ("erl"          all-the-icons-alltheicon "erlang"         :face all-the-icons-red :v-adjust -0.1 :height 0.9)
    ("hrl"          all-the-icons-alltheicon "erlang"         :face all-the-icons-dred :v-adjust -0.1 :height 0.9)
    ("eex"          all-the-icons-alltheicon "elixir"         :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
    ("leex"         all-the-icons-alltheicon "elixir"         :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
    ("heex"         all-the-icons-alltheicon "elixir"         :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
    ("ex"           all-the-icons-alltheicon "elixir"         :face all-the-icons-lpurple :v-adjust -0.1 :height 0.9)
    ("exs"          all-the-icons-alltheicon "elixir"         :face all-the-icons-lred :v-adjust -0.1 :height 0.9)
    ("java"         all-the-icons-alltheicon "java"           :height 1.0  :face all-the-icons-purple)
    ("gradle"       all-the-icons-fileicon "gradle"           :height 1.0  :face all-the-icons-silver)
    ("ebuild"       all-the-icons-fileicon "gentoo"           :face all-the-icons-cyan)
    ("eclass"       all-the-icons-fileicon "gentoo"           :face all-the-icons-blue)
    ("go"           all-the-icons-fileicon "go"               :height 1.0  :face all-the-icons-blue)
    ("jl"           all-the-icons-fileicon "julia"            :face all-the-icons-purple :v-adjust 0.0)
    ("magik"        all-the-icons-faicon "magic"              :face all-the-icons-blue)
    ("matlab"       all-the-icons-fileicon "matlab"           :face all-the-icons-orange)
    ("nix"          all-the-icons-fileicon "nix"              :face all-the-icons-blue)
    ("pl"           all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
    ("pm"           all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
    ("pl6"          all-the-icons-fileicon "raku"            :face all-the-icons-cyan)
    ("pm6"          all-the-icons-fileicon "raku"            :face all-the-icons-pink)
    ("pod"          all-the-icons-alltheicon "perldocs"       :height 1.2  :face all-the-icons-lgreen)
    ("php"          all-the-icons-fileicon "php"              :face all-the-icons-lsilver)
    ("pony"         all-the-icons-fileicon "pony"             :face all-the-icons-maroon)
    ("ps1"          all-the-icons-fileicon "powershell"       :face all-the-icons-blue)
    ("pro"          all-the-icons-alltheicon "prolog"         :height 1.1  :face all-the-icons-lmaroon)
    ("proog"        all-the-icons-alltheicon "prolog"         :height 1.1  :face all-the-icons-lmaroon)
    ("py"           all-the-icons-alltheicon "python"         :height 1.0  :face all-the-icons-dblue)
    ("idr"          all-the-icons-fileicon "idris"            :face all-the-icons-red)
    ("ipynb"        all-the-icons-fileicon "jupyter"          :height 1.0  :face all-the-icons-dorange)
    ("gem"          all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    ("raku"         all-the-icons-fileicon "raku"            :face all-the-icons-cyan)
    ("rakumod"      all-the-icons-fileicon "raku"            :face all-the-icons-pink)
    ("rb"           all-the-icons-octicon "ruby"              :v-adjust 0.0 :face all-the-icons-lred)
    ("rs"           all-the-icons-alltheicon "rust"           :height 1.2  :face all-the-icons-maroon)
    ("rlib"         all-the-icons-alltheicon "rust"           :height 1.2  :face all-the-icons-dmaroon)
    ("r"            all-the-icons-fileicon "R"                :face all-the-icons-lblue)
    ("rd"           all-the-icons-fileicon "R"                :face all-the-icons-lblue)
    ("rdx"          all-the-icons-fileicon "R"                :face all-the-icons-lblue)
    ("rsx"          all-the-icons-fileicon "R"                :face all-the-icons-lblue)
    ("svelte"       all-the-icons-fileicon "svelte"           :v-adjust 0.0 :face all-the-icons-red)
    ("gql"          all-the-icons-fileicon "graphql"          :face all-the-icons-dpink)
    ("graphql"      all-the-icons-fileicon "graphql"          :face all-the-icons-dpink)
    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("c"            all-the-icons-alltheicon "c-line"         :face all-the-icons-blue)
    ("h"            all-the-icons-alltheicon "c-line"         :face all-the-icons-purple)
    ("m"            all-the-icons-fileicon "apple"            :v-adjust 0.0 :height 1.0)
    ("mm"           all-the-icons-fileicon "apple"            :v-adjust 0.0 :height 1.0)
    ;;
    ("cc"           all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-blue)
    ("cpp"          all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-blue)
    ("cxx"          all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-blue)
    ("hh"           all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-purple)
    ("hpp"          all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-purple)
    ("hxx"          all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-purple)
    ;; Lisps
    ("cl"           all-the-icons-fileicon "clisp"            :face all-the-icons-lorange)
    ("l"            all-the-icons-fileicon "lisp"             :face all-the-icons-orange)
    ("lisp"         all-the-icons-fileicon "lisp"             :face all-the-icons-orange)
    ("hy"           all-the-icons-fileicon "hy"               :face all-the-icons-blue)
    ("el"           all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.2 :face all-the-icons-purple)
    ("clj"          all-the-icons-alltheicon "clojure-line"   :height 1.0 :face all-the-icons-blue :v-adjust 0.0)
    ("cljc"         all-the-icons-alltheicon "clojure-line"   :height 1.0 :face all-the-icons-blue :v-adjust 0.0)
    ("cljs"         all-the-icons-fileicon "cljs"             :height 1.0 :face all-the-icons-dblue :v-adjust 0.0)
    ("coffee"       all-the-icons-alltheicon "coffeescript"   :height 1.0  :face all-the-icons-maroon)
    ("iced"         all-the-icons-alltheicon "coffeescript"   :height 1.0  :face all-the-icons-lmaroon)
    ("dart"         all-the-icons-fileicon "dart"             :height 1.0 :face all-the-icons-blue :v-adjust 0.0)
    ("rkt"          all-the-icons-fileicon "racket"           :height 1.2 :face all-the-icons-red)
    ("scrbl"        all-the-icons-fileicon "racket"           :height 1.2 :face all-the-icons-blue)
    ;; Stylesheeting
    ("css"          all-the-icons-alltheicon "css3"           :face all-the-icons-yellow)
    ("scss"         all-the-icons-alltheicon "sass"           :face all-the-icons-pink)
    ("sass"         all-the-icons-alltheicon "sass"           :face all-the-icons-dpink)
    ("less"         all-the-icons-alltheicon "less"           :height 0.8  :face all-the-icons-dyellow)
    ("postcss"      all-the-icons-fileicon "postcss"          :face all-the-icons-dred)
    ("pcss"         all-the-icons-fileicon "postcss"          :face all-the-icons-dred)
    ("sss"          all-the-icons-fileicon "postcss"          :face all-the-icons-dred)
    ("styl"         all-the-icons-alltheicon "stylus"         :face all-the-icons-lgreen)
    ("csv"          all-the-icons-octicon "graph"             :v-adjust 0.0 :face all-the-icons-dblue)
    ;; haskell
    ("hs"           all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    ("chs"          all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    ("lhs"          all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    ("hsc"          all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    ;; Web modes
    ("inky-haml"    all-the-icons-fileicon "haml"             :face all-the-icons-lyellow)
    ("haml"         all-the-icons-fileicon "haml"             :face all-the-icons-lyellow)
    ("htm"          all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
    ("html"         all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
    ("inky-er"      all-the-icons-alltheicon "html5"          :face all-the-icons-lred)
    ("inky-erb"     all-the-icons-alltheicon "html5"          :face all-the-icons-lred)
    ("erb"          all-the-icons-alltheicon "html5"          :face all-the-icons-lred)
    ("hbs"          all-the-icons-fileicon "moustache"        :face all-the-icons-green)
    ("inky-slim"    all-the-icons-octicon "dashboard"         :v-adjust 0.0 :face all-the-icons-yellow)
    ("slim"         all-the-icons-octicon "dashboard"         :v-adjust 0.0 :face all-the-icons-yellow)
    ("jade"         all-the-icons-fileicon "jade"             :face all-the-icons-red)
    ("pug"          all-the-icons-fileicon "pug-alt"          :face all-the-icons-red)
    ;; Javascript
    ("d3js"         all-the-icons-alltheicon "d3"             :height 0.8  :face all-the-icons-lgreen)
    ("re"           all-the-icons-fileicon "reason"           :height 1.0  :face all-the-icons-red-alt)
    ("rei"          all-the-icons-fileicon "reason"           :height 1.0  :face all-the-icons-dred)
    ("ml"           all-the-icons-fileicon "ocaml"            :height 1.0  :face all-the-icons-lpink)
    ("mli"          all-the-icons-fileicon "ocaml"            :height 1.0  :face all-the-icons-dpink)
    ("react"        all-the-icons-alltheicon "react"          :height 1.1  :face all-the-icons-lblue)
    ("ts"           all-the-icons-fileicon "typescript"       :height 1.0 :v-adjust -0.1 :face all-the-icons-blue-alt)
    ("js"           all-the-icons-alltheicon "javascript"     :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow)
    ("mjs"          all-the-icons-alltheicon "javascript"     :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow)
    ("es"           all-the-icons-alltheicon "javascript"     :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow)
    ("jsx"          all-the-icons-fileicon "jsx-2"            :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
    ("tsx"          all-the-icons-fileicon "tsx"              :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
    ("njs"          all-the-icons-alltheicon "nodejs"         :height 1.2  :face all-the-icons-lgreen)
    ("vue"          all-the-icons-fileicon "vue"              :face all-the-icons-lgreen)
    ("wasm"         all-the-icons-fileicon "wasm"             :height 1.0 :face all-the-icons-purple-alt)
    ("wat"          all-the-icons-fileicon "wasm"             :height 1.0 :face all-the-icons-purple-alt)

    ("sbt"          all-the-icons-fileicon   "sbt"            :face all-the-icons-red)
    ("scala"        all-the-icons-alltheicon "scala"          :face all-the-icons-red)
    ("scm"          all-the-icons-fileicon   "scheme"         :height 1.2 :face all-the-icons-red)
    ("swift"        all-the-icons-alltheicon "swift"          :height 1.0 :v-adjust -0.1 :face all-the-icons-green)

    ("tcl"          all-the-icons-fileicon "tcl"              :height 1.0 :face all-the-icons-dred)

    ("tf"           all-the-icons-fileicon "terraform"        :height 1.0 :face all-the-icons-purple-alt)
    ("tfvars"       all-the-icons-fileicon "terraform"        :height 1.0 :face all-the-icons-purple-alt)
    ("tfstate"      all-the-icons-fileicon "terraform"        :height 1.0 :face all-the-icons-purple-alt)

    ("asm"          all-the-icons-fileicon "assembly"         :height 1.0 :face all-the-icons-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("v"            all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("vams"         all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("sv"           all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("sva"          all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("svh"          all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ("svams"        all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    ;; VHDL(-AMS)
    ("vhd"          all-the-icons-fileicon "vhdl"             :face all-the-icons-blue)
    ("vhdl"         all-the-icons-fileicon "vhdl"             :face all-the-icons-blue)
    ("vhms"         all-the-icons-fileicon "vhdl"             :face all-the-icons-blue)
    ;; Cabal
    ("cabal"        all-the-icons-fileicon "cabal"            :face all-the-icons-lblue)
    ;; Kotlin
    ("kt"           all-the-icons-fileicon "kotlin"           :face all-the-icons-orange)
    ("kts"          all-the-icons-fileicon "kotlin"           :face all-the-icons-orange)
    ;; Nimrod
    ("nim"          all-the-icons-fileicon "nimrod"           :face all-the-icons-yellow)
    ("nims"         all-the-icons-fileicon "nimrod"           :face all-the-icons-yellow)
    ;; SQL
    ("sql"          all-the-icons-octicon "database"          :face all-the-icons-silver)
    ;; Styles
    ("styles"       all-the-icons-material "style"            :face all-the-icons-red)
    ;; Lua
    ("lua"          all-the-icons-fileicon "lua"              :face all-the-icons-dblue)
    ;; ASCII doc
    ("adoc"         all-the-icons-fileicon "asciidoc"         :face all-the-icons-lblue)
    ("asciidoc"     all-the-icons-fileicon "asciidoc"         :face all-the-icons-lblue)
    ;; Puppet
    ("pp"           all-the-icons-fileicon "puppet"           :face all-the-icons-yellow)
    ;; Jinja
    ("j2"           all-the-icons-fileicon "jinja"            :face all-the-icons-silver)
    ("jinja2"       all-the-icons-fileicon "jinja"            :face all-the-icons-silver)
    ;; Docker
    ("dockerfile"   all-the-icons-fileicon "dockerfile"       :face all-the-icons-cyan)
    ;; Vagrant
    ("vagrantfile"  all-the-icons-fileicon "vagrant"          :face all-the-icons-blue)
    ;; GLSL
    ("glsl"     all-the-icons-fileicon "vertex-shader"        :face all-the-icons-blue)
    ("vert"     all-the-icons-fileicon "vertex-shader"        :face all-the-icons-blue)
    ("tesc"     all-the-icons-fileicon "vertex-shader"        :face all-the-icons-purple)
    ("tese"     all-the-icons-fileicon "vertex-shader"        :face all-the-icons-dpurple)
    ("geom"     all-the-icons-fileicon "vertex-shader"        :face all-the-icons-green)
    ("frag"     all-the-icons-fileicon "vertex-shader"        :face all-the-icons-red)
    ("comp"     all-the-icons-fileicon "vertex-shader"        :face all-the-icons-dblue)
    ;; CUDA
    ("cu"       all-the-icons-fileicon "nvidia"               :face all-the-icons-green)
    ("cuh"      all-the-icons-fileicon "nvidia"               :face all-the-icons-green)
    ;; Fortran
    ("f90"      all-the-icons-fileicon "fortran"              :face all-the-icons-purple)
    ;; C#
    ("cs"           all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
    ("csx"          all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
    ;; F#
    ("fs"           all-the-icons-fileicon "fsharp"           :face all-the-icons-blue-alt)
    ("fsi"          all-the-icons-fileicon "fsharp"           :face all-the-icons-blue-alt)
    ("fsx"          all-the-icons-fileicon "fsharp"           :face all-the-icons-blue-alt)
    ("fsscript"     all-the-icons-fileicon "fsharp"           :face all-the-icons-blue-alt)
    ;; zig
    ("zig"          all-the-icons-fileicon "zig"              :face all-the-icons-orange)
    ;; odin
    ("odin"         all-the-icons-fileicon "odin"             :height 1.1 :face all-the-icons-lblue)
    ;; File Types
    ("ico"          all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-blue)
    ("png"          all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-orange)
    ("gif"          all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-green)
    ("jpeg"         all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-dblue)
    ("jpg"          all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-dblue)
    ("webp"         all-the-icons-octicon "file-media"        :v-adjust 0.0 :face all-the-icons-dblue)
    ;; Audio
    ("mp3"          all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("wav"          all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("m4a"          all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("ogg"          all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("flac"         all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("opus"         all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("au"           all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("aif"          all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("aifc"         all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("aiff"         all-the-icons-faicon "volume-up"          :face all-the-icons-dred)
    ("svg"          all-the-icons-alltheicon "svg"            :height 0.9  :face all-the-icons-lgreen)
    ;; Video
    ("mov"          all-the-icons-faicon "film"               :face all-the-icons-blue)
    ("mp4"          all-the-icons-faicon "film"               :face all-the-icons-blue)
    ("ogv"          all-the-icons-faicon "film"               :face all-the-icons-dblue)
    ("mpg"          all-the-icons-faicon "film"               :face all-the-icons-blue)
    ("mpeg"         all-the-icons-faicon "film"               :face all-the-icons-blue)
    ("flv"          all-the-icons-faicon "film"               :face all-the-icons-blue)
    ("ogv"          all-the-icons-faicon "film"               :face all-the-icons-dblue)
    ("mkv"          all-the-icons-faicon "film"               :face all-the-icons-blue)
    ("webm"         all-the-icons-faicon "film"               :face all-the-icons-blue)
    ("dav"          all-the-icons-faicon "film"               :face all-the-icons-blue)
    ;; Fonts
    ("ttf"          all-the-icons-fileicon "font"             :v-adjust 0.0 :face all-the-icons-dcyan)
    ("woff"         all-the-icons-fileicon "font"             :v-adjust 0.0 :face all-the-icons-cyan)
    ("woff2"        all-the-icons-fileicon "font"             :v-adjust 0.0 :face all-the-icons-cyan)
    ;; Doc
    ("pdf"          all-the-icons-octicon "file-pdf"          :v-adjust 0.0 :face all-the-icons-dred)
    ("text"         all-the-icons-octicon "file-text"         :v-adjust 0.0 :face all-the-icons-cyan)
    ("txt"          all-the-icons-octicon "file-text"         :v-adjust 0.0 :face all-the-icons-cyan)
    ("doc"          all-the-icons-fileicon "word"             :face all-the-icons-blue)
    ("docx"         all-the-icons-fileicon "word"             :face all-the-icons-blue)
    ("docm"         all-the-icons-fileicon "word"             :face all-the-icons-blue)
    ("eml"          all-the-icons-faicon "envelope"              :face all-the-icons-blue)
    ("msg"          all-the-icons-faicon "envelope"              :face all-the-icons-blue)
    ("texi"         all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    ("tex"          all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    ("md"           all-the-icons-octicon "markdown"          :v-adjust 0.0 :face all-the-icons-lblue)
    ("bib"          all-the-icons-fileicon "bib"              :face all-the-icons-maroon)
    ("org"          all-the-icons-fileicon "org"              :face all-the-icons-lgreen)
    ("pps"          all-the-icons-fileicon "powerpoint"       :face all-the-icons-orange)
    ("ppt"          all-the-icons-fileicon "powerpoint"       :face all-the-icons-orange)
    ("pptsx"        all-the-icons-fileicon "powerpoint"       :face all-the-icons-orange)
    ("pptx"         all-the-icons-fileicon "powerpoint"       :face all-the-icons-orange)
    ("knt"          all-the-icons-fileicon "powerpoint"       :face all-the-icons-cyan)
    ("xlsx"         all-the-icons-fileicon "excel"            :face all-the-icons-dgreen)
    ("xlsm"         all-the-icons-fileicon "excel"            :face all-the-icons-dgreen)
    ("xlsb"         all-the-icons-fileicon "excel"            :face all-the-icons-dgreen)
    ("xltx"         all-the-icons-fileicon "excel"            :face all-the-icons-dgreen)
    ("xltm"         all-the-icons-fileicon "excel"            :face all-the-icons-dgreen)
    ("ly"           all-the-icons-faicon   "music"            :face all-the-icons-green)
    ;;
    ("key"          all-the-icons-octicon "key"               :v-adjust 0.0 :face all-the-icons-lblue)
    ("pem"          all-the-icons-octicon "key"               :v-adjust 0.0 :face all-the-icons-orange)
    ("p12"          all-the-icons-octicon "key"               :v-adjust 0.0 :face all-the-icons-dorange)
    ("crt"          all-the-icons-octicon "key"               :v-adjust 0.0 :face all-the-icons-lblue)
    ("pub"          all-the-icons-octicon "key"               :v-adjust 0.0 :face all-the-icons-blue)
    ("gpg"          all-the-icons-octicon "key"               :v-adjust 0.0 :face all-the-icons-lblue)
    ("cache"        all-the-icons-octicon "database"          :height 1.0 :v-adjust 0.0 :face all-the-icons-green)))


(define-obsolete-variable-alias 'all-the-icons-icon-alist
  'all-the-icons-regexp-icon-alist
  "5.0.0"
  "`all-the-icons-icon-alist' has been split to
`all-the-icons-extension-icon-alist' and `all-the-icons-regexp-icon-alist'
for performance sake.")

(defvar all-the-icons-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"           all-the-icons-octicon "tag"                     :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("^TODO$"           all-the-icons-octicon "checklist"               :v-adjust 0.0 :face all-the-icons-lyellow)
    ("^LICENSE$"        all-the-icons-octicon "book"                    :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("^readme"          all-the-icons-octicon "book"                    :height 1.0 :v-adjust 0.0 :face all-the-icons-lcyan)

    ;; Config
    ("nginx$"            all-the-icons-fileicon "nginx"                 :height 0.9  :face all-the-icons-dgreen)
    ("apache$"           all-the-icons-alltheicon "apache"              :height 0.9  :face all-the-icons-dgreen)

    ;; C
    ("^Makefile$"       all-the-icons-fileicon "gnu"                    :face all-the-icons-dorange)
    ("^CMakeLists.txt$" all-the-icons-fileicon "cmake"                  :face all-the-icons-red)
    ("^CMakeCache.txt$" all-the-icons-fileicon "cmake"                  :face all-the-icons-blue)
    ("^meson.build$"    all-the-icons-fileicon "meson"                  :face all-the-icons-purple)
    ("^meson_options.txt$" all-the-icons-fileicon "meson"               :face all-the-icons-purple)

    ;; Docker
    ("^\\.?Dockerfile"  all-the-icons-fileicon "dockerfile"             :face all-the-icons-blue)

    ;; Homebrew
    ("^Brewfile$"       all-the-icons-faicon "beer"                     :face all-the-icons-lsilver)

    ;; ;; AWS
    ("^stack.*.json$"   all-the-icons-alltheicon "aws"                  :face all-the-icons-orange)
    ("^serverless\\.yml$" all-the-icons-faicon "bolt"                   :v-adjust 0.0 :face all-the-icons-yellow)

    ;; lock files
    ("~$"               all-the-icons-octicon "lock"                    :v-adjust 0.0 :face all-the-icons-maroon)

    ;; Source Codes
    ("^mix.lock$"       all-the-icons-alltheicon "elixir"               :face all-the-icons-lyellow :v-adjust -0.1 :height 0.9)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$" all-the-icons-alltheicon "ruby-alt"      :face all-the-icons-red)
    ("_?test\\.rb$"        all-the-icons-fileicon "test-ruby"           :height 1.0 :v-adjust 0.0 :face all-the-icons-red)
    ("_?test_helper\\.rb$" all-the-icons-fileicon "test-ruby"           :height 1.0 :v-adjust 0.0 :face all-the-icons-dred)
    ("_?spec\\.rb$"        all-the-icons-fileicon "test-ruby"           :height 1.0 :v-adjust 0.0 :face all-the-icons-red)
    ("_?spec_helper\\.rb$" all-the-icons-fileicon "test-ruby"           :height 1.0 :v-adjust 0.0 :face all-the-icons-dred)

    ("-?spec\\.ts$"     all-the-icons-fileicon "test-typescript"        :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("-?test\\.ts$"     all-the-icons-fileicon "test-typescript"        :height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
    ("-?spec\\.js$"     all-the-icons-fileicon "test-js"                :height 1.0 :v-adjust 0.0 :face all-the-icons-lpurple)
    ("-?test\\.js$"     all-the-icons-fileicon "test-js"                :height 1.0 :v-adjust 0.0 :face all-the-icons-lpurple)
    ("-?spec\\.jsx$"    all-the-icons-fileicon "test-react"             :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt)
    ("-?test\\.jsx$"    all-the-icons-fileicon "test-react"             :height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt)

    ;; Git
    ("^MERGE_"          all-the-icons-octicon "git-merge"               :v-adjust 0.0 :face all-the-icons-red)
    ("^COMMIT_EDITMSG"  all-the-icons-octicon "git-commit"              :v-adjust 0.0 :face all-the-icons-red)

    ;; Stylesheeting
    ("stylelint"        all-the-icons-fileicon "stylelint"              :face all-the-icons-lyellow)

    ;; JavaScript
    ("^package.json$"   all-the-icons-fileicon "npm"                    :face all-the-icons-red)
    ("^package.lock.json$" all-the-icons-fileicon "npm"                 :face all-the-icons-dred)
    ("^yarn\\.lock"     all-the-icons-fileicon "yarn"                   :face all-the-icons-blue-alt)
    ("\\.npmignore$"    all-the-icons-fileicon "npm"                    :face all-the-icons-dred)
    ("^bower.json$"     all-the-icons-alltheicon "bower"                :height 1.0 :v-adjust 0.0 :face all-the-icons-lorange)
    ("^gulpfile"        all-the-icons-alltheicon "gulp"                 :height 1.0  :face all-the-icons-lred)
    ("^gruntfile"       all-the-icons-alltheicon "grunt"                :height 1.0 :v-adjust -0.1 :face all-the-icons-lyellow)
    ("^webpack"         all-the-icons-fileicon "webpack"                :face all-the-icons-lblue)

    ;; Go
    ("^go.mod$"         all-the-icons-fileicon "config-go"              :height 1.0 :face all-the-icons-blue-alt)
    ("^go.work$"        all-the-icons-fileicon "config-go"              :height 1.0 :face all-the-icons-blue-alt)

    ;; Emacs
    ("bookmark"         all-the-icons-octicon "bookmark"                :height 1.1 :v-adjust 0.0 :face all-the-icons-lpink)

    ("^\\*scratch\\*$"  all-the-icons-faicon "sticky-note"              :face all-the-icons-lyellow)
    ("^\\*scratch.*"    all-the-icons-faicon "sticky-note"              :face all-the-icons-yellow)
    ("^\\*new-tab\\*$"  all-the-icons-material "star"                   :face all-the-icons-cyan)

    ("^\\."             all-the-icons-octicon "gear"                    :v-adjust 0.0)
    ))

(defvar all-the-icons-default-file-icon
  '(all-the-icons-faicon "file-o" :v-adjust 0.0 :face all-the-icons-dsilver))

(defvar all-the-icons-dir-icon-alist
  '(
    ("trash"            all-the-icons-faicon "trash-o"          :height 1.2 :v-adjust -0.1)
    ("dropbox"          all-the-icons-faicon "dropbox"          :height 1.0 :v-adjust -0.1)
    ("google[ _-]drive" all-the-icons-alltheicon "google-drive" :height 1.0 :v-adjust -0.1)
    ("^atom$"           all-the-icons-alltheicon "atom"         :height 1.2 :v-adjust -0.1)
    ("documents"        all-the-icons-faicon "book"             :height 1.0 :v-adjust -0.1)
    ("download"         all-the-icons-faicon "cloud-download"   :height 0.9 :v-adjust -0.1)
    ("desktop"          all-the-icons-octicon "device-desktop"  :height 1.0 :v-adjust -0.1)
    ("pictures"         all-the-icons-faicon "picture-o"        :height 0.9 :v-adjust -0.2)
    ("photos"           all-the-icons-faicon "camera-retro"     :height 1.0 :v-adjust -0.1)
    ("music"            all-the-icons-faicon "music"            :height 1.0 :v-adjust -0.1)
    ("movies"           all-the-icons-faicon "film"             :height 0.9 :v-adjust -0.1)
    ("code"             all-the-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("workspace"        all-the-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("test"             all-the-icons-fileicon "test-dir"       :height 0.9)
    ("\\.git"           all-the-icons-alltheicon "git"          :height 1.0)
    (".?"               all-the-icons-octicon "file-directory"  :height 1.0 :v-adjust -0.1)
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
    (emacs-lisp-mode           all-the-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.1 :face all-the-icons-purple)
    (circe-server-mode         all-the-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0)
    (circe-channel-mode        all-the-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0)
    (circe-query-mode          all-the-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0)
    (crystal-mode              all-the-icons-fileicon "crystal"            :v-adjust 0.0 :face all-the-icons-yellow)
    (erc-mode                  all-the-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0)
    (inferior-emacs-lisp-mode  all-the-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.1 :face all-the-icons-lblue)
    (dired-mode                all-the-icons-octicon "file-directory"      :v-adjust 0.0)
    (lisp-interaction-mode     all-the-icons-fileicon "lisp"               :v-adjust -0.1 :face all-the-icons-orange)
    (sly-mrepl-mode            all-the-icons-fileicon "clisp"               :v-adjust -0.1 :face all-the-icons-orange)
    (slime-repl-mode           all-the-icons-fileicon "clisp"               :v-adjust -0.1 :face all-the-icons-orange)
    (org-mode                  all-the-icons-fileicon "org"                :v-adjust 0.0 :face all-the-icons-lgreen)
    (typescript-mode           all-the-icons-fileicon "typescript"         :v-adjust -0.1 :face all-the-icons-blue-alt)
    (typescript-ts-mode        all-the-icons-fileicon "typescript"         :v-adjust -0.1 :face all-the-icons-blue-alt)
    (typescript-tsx-mode       all-the-icons-fileicon "tsx"                :v-adjust -0.1 :face all-the-icons-cyan-alt)
    (tsx-ts-mode               all-the-icons-fileicon "tsx"                :v-adjust -0.1 :face all-the-icons-cyan-alt)
    (js-mode                   all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js-jsx-mode               all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js2-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (js3-mode                  all-the-icons-alltheicon "javascript"       :v-adjust -0.1 :face all-the-icons-yellow)
    (rjsx-mode                 all-the-icons-fileicon "jsx-2"              :v-adjust -0.1 :face all-the-icons-cyan-alt)
    (term-mode                 all-the-icons-octicon "terminal"            :v-adjust 0.2)
    (vterm-mode                all-the-icons-octicon "terminal"            :v-adjust 0.2)
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
    (messages-buffer-mode      all-the-icons-faicon "file-o"               :v-adjust 0.0 :face all-the-icons-dsilver)
    (help-mode                 all-the-icons-faicon "info"                 :v-adjust -0.1 :face all-the-icons-purple)
    (helpful-mode              all-the-icons-faicon "info"                 :v-adjust -0.1 :face all-the-icons-purple)
    (benchmark-init/tree-mode  all-the-icons-octicon "dashboard"           :v-adjust 0.0)
    (jenkins-mode              all-the-icons-fileicon "jenkins"            :face all-the-icons-blue)
    (magit-popup-mode          all-the-icons-alltheicon "git"              :face all-the-icons-red)
    (magit-status-mode         all-the-icons-alltheicon "git"              :face all-the-icons-lred)
    (magit-log-mode            all-the-icons-alltheicon "git"              :face all-the-icons-green)
    (mu4e-compose-mode         all-the-icons-octicon "pencil"              :v-adjust 0.0)
    (mu4e-headers-mode         all-the-icons-octicon "mail"                :v-adjust 0.0)
    (mu4e-main-mode            all-the-icons-octicon "mail"                :v-adjust 0.0)
    (mu4e-view-mode            all-the-icons-octicon "mail-read"           :v-adjust 0.0)
    (sieve-mode                all-the-icons-octicon "mail"                :v-adjust 0.0)
    (gnus-group-mode           all-the-icons-octicon "mail"                :v-adjust 0.0)
    (gnus-summary-mode         all-the-icons-octicon "mail"                :v-adjust 0.0)
    (gnus-article-mode         all-the-icons-octicon "mail-read"           :v-adjust 0.0)
    (message-mode              all-the-icons-octicon "pencil"              :v-adjust 0.0)
    (package-menu-mode         all-the-icons-faicon "archive"              :height 1.0 :v-adjust 0.0 :face all-the-icons-silver)
    (paradox-menu-mode         all-the-icons-faicon "archive"              :height 1.0 :v-adjust 0.0 :face all-the-icons-silver)
    (Custom-mode               all-the-icons-octicon "settings"            :v-adjust -0.1)

    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode             all-the-icons--web-mode-icon)

    (fundamental-mode                   all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.1 :face all-the-icons-dsilver)
    (special-mode                       all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.1 :face all-the-icons-yellow)
    (text-mode                          all-the-icons-octicon "file-text"         :v-adjust 0.0 :face all-the-icons-cyan)
    (enh-ruby-mode                      all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
    (ruby-mode                          all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
    (ruby-ts-mode                       all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-lred)
    (inf-ruby-mode                      all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (projectile-rails-compilation-mode  all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (rspec-compilation-mode             all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (rake-compilation-mode              all-the-icons-alltheicon "ruby-alt"       :face all-the-icons-red)
    (sh-mode                            all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
    (shell-mode                         all-the-icons-alltheicon "terminal"       :face all-the-icons-purple)
    (fish-mode                          all-the-icons-alltheicon "terminal"       :face all-the-icons-lpink)
    (nginx-mode                         all-the-icons-fileicon "nginx"            :height 0.9  :face all-the-icons-dgreen)
    (apache-mode                        all-the-icons-alltheicon "apache"         :height 0.9  :face all-the-icons-dgreen)
    (makefile-mode                      all-the-icons-fileicon "gnu"              :face all-the-icons-dorange)
    (cmake-mode                         all-the-icons-fileicon "cmake"            :face all-the-icons-red)
    (cmake-ts-mode                      all-the-icons-fileicon "cmake"            :face all-the-icons-red)
    (dockerfile-mode                    all-the-icons-fileicon "dockerfile"       :face all-the-icons-blue)
    (dockerfile-ts-mode                 all-the-icons-fileicon "dockerfile"       :face all-the-icons-blue)
    (docker-compose-mode                all-the-icons-fileicon "dockerfile"       :face all-the-icons-lblue)
    (nxml-mode                          all-the-icons-faicon "file-code-o"        :height 0.95 :face all-the-icons-lorange)
    (json-mode                          all-the-icons-octicon "settings"          :face all-the-icons-yellow)
    (json-ts-mode                       all-the-icons-octicon "settings"          :face all-the-icons-yellow)
    (jsonian-mode                       all-the-icons-octicon "settings"          :face all-the-icons-yellow)
    (yaml-mode                          all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
    (yaml-ts-mode                       all-the-icons-octicon "settings"          :v-adjust 0.0 :face all-the-icons-dyellow)
    (elisp-byte-code-mode               all-the-icons-octicon "file-binary"       :v-adjust 0.0 :face all-the-icons-dsilver)
    (archive-mode                       all-the-icons-octicon "file-zip"          :v-adjust 0.0 :face all-the-icons-lmaroon)
    (elm-mode                           all-the-icons-fileicon "elm"              :face all-the-icons-blue)
    (erlang-mode                        all-the-icons-alltheicon "erlang"         :face all-the-icons-red :v-adjust -0.1 :height 0.9)
    (elixir-mode                        all-the-icons-alltheicon "elixir"         :face all-the-icons-lorange :v-adjust -0.1 :height 0.9)
    (java-mode                          all-the-icons-alltheicon "java"           :height 1.0 :face all-the-icons-purple)
    (java-ts-mode                       all-the-icons-alltheicon "java"           :height 1.0 :face all-the-icons-purple)
    (go-mode                            all-the-icons-fileicon "go"               :height 1.0 :face all-the-icons-blue)
    (go-ts-mode                         all-the-icons-fileicon "go"               :height 1.0 :face all-the-icons-blue)
    (go-dot-mod-mode                    all-the-icons-fileicon "config-go"        :height 1.0 :face all-the-icons-blue-alt)
    (go-dot-work-mode                   all-the-icons-fileicon "config-go"        :height 1.0 :face all-the-icons-blue-alt)
    (graphql-mode                       all-the-icons-fileicon "graphql"          :face all-the-icons-dpink)
    (matlab-mode                        all-the-icons-fileicon "matlab"           :face all-the-icons-orange)
    (nix-mode                           all-the-icons-fileicon "nix"              :face all-the-icons-blue)
    (perl-mode                          all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
    (cperl-mode                         all-the-icons-alltheicon "perl"           :face all-the-icons-lorange)
    (php-mode                           all-the-icons-fileicon "php"              :face all-the-icons-lsilver)
    (prolog-mode                        all-the-icons-alltheicon "prolog"         :height 1.1  :face all-the-icons-lmaroon)
    (python-mode                        all-the-icons-alltheicon "python"         :height 1.0  :face all-the-icons-dblue)
    (python-ts-mode                     all-the-icons-alltheicon "python"         :height 1.0  :face all-the-icons-dblue)
    (inferior-python-mode               all-the-icons-alltheicon "python"         :height 1.0  :face all-the-icons-dblue)
    (racket-mode                        all-the-icons-fileicon "racket"           :height 1.2 :face all-the-icons-red)
    (rust-mode                          all-the-icons-alltheicon "rust"           :height 1.2  :face all-the-icons-maroon)
    (rustic-mode                        all-the-icons-alltheicon "rust"           :height 1.2  :face all-the-icons-maroon)
    (rust-ts-mode                       all-the-icons-alltheicon "rust"           :height 1.2  :face all-the-icons-maroon)
    (scala-mode                         all-the-icons-alltheicon "scala"          :face all-the-icons-red)
    (scheme-mode                        all-the-icons-fileicon   "scheme"         :height 1.2 :face all-the-icons-red)
    (swift-mode                         all-the-icons-alltheicon "swift"          :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
    (svelte-mode                        all-the-icons-fileicon "svelte"           :v-adjust 0.0 :face all-the-icons-red)
    (c-mode                             all-the-icons-alltheicon "c-line"         :face all-the-icons-blue)
    (c-ts-mode                          all-the-icons-alltheicon "c-line"         :face all-the-icons-blue)
    (c++-mode                           all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-blue)
    (c++-ts-mode                        all-the-icons-alltheicon "cplusplus-line" :v-adjust -0.2 :face all-the-icons-blue)
    (csharp-mode                        all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
    (csharp-ts-mode                     all-the-icons-alltheicon "csharp-line"    :face all-the-icons-dblue)
    (clojure-mode                       all-the-icons-alltheicon "clojure"        :height 1.0  :face all-the-icons-blue)
    (cider-repl-mode                    all-the-icons-alltheicon "clojure"        :height 1.0  :face all-the-icons-green)
    (clojurescript-mode                 all-the-icons-fileicon "cljs"             :height 1.0  :face all-the-icons-dblue)
    (coffee-mode                        all-the-icons-alltheicon "coffeescript"   :height 1.0  :face all-the-icons-maroon)
    (lisp-mode                          all-the-icons-fileicon "lisp"             :face all-the-icons-orange)
    (css-mode                           all-the-icons-alltheicon "css3"           :face all-the-icons-yellow)
    (css-ts-mode                        all-the-icons-alltheicon "css3"           :face all-the-icons-yellow)
    (scss-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-pink)
    (sass-mode                          all-the-icons-alltheicon "sass"           :face all-the-icons-dpink)
    (less-css-mode                      all-the-icons-alltheicon "less"           :height 0.8  :face all-the-icons-dyellow)
    (stylus-mode                        all-the-icons-alltheicon "stylus"         :face all-the-icons-lgreen)
    (csv-mode                           all-the-icons-octicon "graph"             :v-adjust 0.0 :face all-the-icons-dblue)
    (haskell-mode                       all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    (haskell-c2hs-mode                  all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    (literate-haskell-mode              all-the-icons-alltheicon "haskell"        :height 1.0  :face all-the-icons-red)
    (haml-mode                          all-the-icons-fileicon "haml"             :face all-the-icons-lyellow)
    (html-mode                          all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
    (html-ts-mode                       all-the-icons-alltheicon "html5"          :face all-the-icons-orange)
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
    (tuareg-mode                        all-the-icons-fileicon "ocaml"            :v-adjust 0.0 :height 1.0)
    (purescript-mode                    all-the-icons-fileicon "purescript"       :v-adjust 0.0 :height 1.0)
    (verilog-mode                       all-the-icons-fileicon "verilog"          :height 1.0 :v-adjust -0.2 :face all-the-icons-red)
    (vhdl-mode                          all-the-icons-fileicon "vhdl"             :face all-the-icons-blue)
    (haskell-cabal-mode                 all-the-icons-fileicon "cabal"            :face all-the-icons-lblue)
    (kotlin-mode                        all-the-icons-fileicon "kotlin"           :face all-the-icons-orange)
    (kotlin-ts-mode                     all-the-icons-fileicon "kotlin"           :face all-the-icons-orange)
    (nim-mode                           all-the-icons-fileicon "nimrod"           :face all-the-icons-yellow)
    (sql-mode                           all-the-icons-octicon  "database"         :face all-the-icons-silver)
    (lua-mode                           all-the-icons-fileicon "lua"              :face all-the-icons-dblue)
    (adoc-mode                          all-the-icons-fileicon "asciidoc"         :face all-the-icons-lblue)
    (puppet-mode                        all-the-icons-fileicon "puppet"           :face all-the-icons-yellow)
    (jinja2-mode                        all-the-icons-fileicon "jinja"            :face all-the-icons-silver)
    (powershell-mode                    all-the-icons-fileicon "powershell"       :face all-the-icons-blue)
    (tex-mode                           all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    (latex-mode                         all-the-icons-fileicon "tex"              :face all-the-icons-lred)
    (dart-mode                          all-the-icons-fileicon "dart"             :height 1.0  :face all-the-icons-blue)
    (fsharp-mode                        all-the-icons-fileicon "fsharp"           :height 1.0  :face all-the-icons-blue)
    (asm-mode                           all-the-icons-fileicon "assembly"         :height 1.0  :face all-the-icons-blue)
    (nasm-mode                          all-the-icons-fileicon "assembly"         :height 1.0  :face all-the-icons-blue)
    (tcl-mode                           all-the-icons-fileicon "tcl"              :height 1.0  :face all-the-icons-dred)
    (cuda-mode                          all-the-icons-fileicon "nvidia"           :face all-the-icons-green)
    (f90-mode                           all-the-icons-fileicon "fortran"          :face all-the-icons-purple)
    (hy-mode                            all-the-icons-fileicon "hy"               :face all-the-icons-blue)
    (glsl-mode                          all-the-icons-fileicon "vertex-shader"    :face all-the-icons-green)
    (zig-mode                           all-the-icons-fileicon "zig"              :face all-the-icons-orange)
    (odin-mode                          all-the-icons-fileicon "odin"             :height 1.1 :face all-the-icons-lblue)
    (pdf-view-mode                      all-the-icons-octicon  "file-pdf"         :v-adjust 0.0 :face all-the-icons-dred)
    (spacemacs-buffer-mode              all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.1 :face all-the-icons-purple)
    (elfeed-search-mode                 all-the-icons-faicon   "rss-square"       :face all-the-icons-orange)
    (elfeed-show-mode                   all-the-icons-faicon   "rss"              :face all-the-icons-orange)
    (emms-browser-mode                  all-the-icons-faicon   "music"            :face all-the-icons-silver)
    (emms-lyrics-mode                   all-the-icons-faicon   "music"            :face all-the-icons-silver)
    (emms-show-all-mode                 all-the-icons-faicon   "music"            :face all-the-icons-silver)
    (emms-metaplaylist-mode             all-the-icons-faicon   "music"            :face all-the-icons-silver)
    (emms-tag-editor-mode               all-the-icons-faicon   "music"            :face all-the-icons-silver)
    (emms-playlist-mode                 all-the-icons-faicon   "music"            :face all-the-icons-silver)
    (lilypond-mode                      all-the-icons-faicon   "music"            :face all-the-icons-green)
    (magik-session-mode                 all-the-icons-alltheicon "terminal"       :face all-the-icons-blue)
    (magik-cb-mode                      all-the-icons-faicon "book"               :face all-the-icons-blue)
    (meson-mode                         all-the-icons-fileicon "meson"            :face all-the-icons-purple)
    (man-common                         all-the-icons-fileicon "man-page"         :face all-the-icons-blue)))

(defvar all-the-icons-url-alist
  '(
    ;; Social media and communities
    ("^\\(https?://\\)?\\(www\\.\\)?del\\.icio\\.us" all-the-icons-faicon "delicious")
    ("^\\(https?://\\)?\\(www\\.\\)?behance\\.net" all-the-icons-faicon "behance")
    ("^\\(https?://\\)?\\(www\\.\\)?dribbble\\.com" all-the-icons-faicon "dribbble")
    ("^\\(https?://\\)?\\(www\\.\\)?facebook\\.com" all-the-icons-faicon "facebook-official")
    ("^\\(https?://\\)?\\(www\\.\\)?glide\\.me" all-the-icons-faicon "glide-g")
    ("^\\(https?://\\)?\\(www\\.\\)?plus\\.google\\.com" all-the-icons-faicon "google-plus")
    ("linkedin\\.com" all-the-icons-faicon "linkedin")
    ("^\\(https?://\\)?\\(www\\.\\)?ok\\.ru" all-the-icons-faicon "odnoklassniki")
    ("^\\(https?://\\)?\\(www\\.\\)?reddit\\.com" all-the-icons-faicon "reddit-alien")
    ("^\\(https?://\\)?\\(www\\.\\)?slack\\.com" all-the-icons-faicon "slack")
    ("^\\(https?://\\)?\\(www\\.\\)?snapchat\\.com" all-the-icons-faicon "snapchat-ghost")
    ("^\\(https?://\\)?\\(www\\.\\)?weibo\\.com" all-the-icons-faicon "weibo")
    ("^\\(https?://\\)?\\(www\\.\\)?twitter\\.com" all-the-icons-faicon "twitter")
    ;; Blogging
    ("joomla\\.org" all-the-icons-faicon "joomla")
    ("^\\(https?://\\)?\\(www\\.\\)?medium\\.com" all-the-icons-faicon "medium")
    ("tumblr\\.com" all-the-icons-faicon "tumblr")
    ("^wordpress\\.com" all-the-icons-faicon "wordpress")
    ;; Programming
    ("^\\(https?://\\)?\\(www\\.\\)?bitbucket\\.org" all-the-icons-faicon "bitbucket")
    ("^\\(https?://\\)?\\(www\\.\\)?codepen\\.io" all-the-icons-faicon "codepen")
    ("^\\(https?://\\)?\\(www\\.\\)?codiepie\\.com" all-the-icons-faicon "codiepie")
    ("^\\(https?://\\)?\\(www\\.\\)?gist\\.github\\.com" all-the-icons-octicon "gist")
    ("^\\(https?://\\)?\\(www\\.\\)?github\\.com" all-the-icons-octicon "mark-github")
    ("^\\(https?://\\)?\\(www\\.\\)?gitlab\\.com" all-the-icons-faicon "gitlab")
    ("^\\(https?://\\)?\\(www\\.\\)?news\\.ycombinator\\.com" all-the-icons-faicon "hacker-news")
    ("^\\(https?://\\)?\\(www\\.\\)?jsfiddle\\.net" all-the-icons-faicon "jsfiddle")
    ("^\\(https?://\\)?\\(www\\.\\)?maxcdn\\.com" all-the-icons-faicon "maxcdn")
    ("^\\(https?://\\)?\\(www\\.\\)?stackoverflow\\.com" all-the-icons-faicon "stack-overflow")
    ;; Video
    ("^\\(https?://\\)?\\(www\\.\\)?twitch\\.tv" all-the-icons-faicon "twitch")
    ("^\\(https?://\\)?\\(www\\.\\)?vimeo\\.com" all-the-icons-faicon "vimeo")
    ("^\\(https?://\\)?\\(www\\.\\)?youtube\\.com" all-the-icons-faicon "youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?youtu\\.be" all-the-icons-faicon "youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?vine\\.co" all-the-icons-faicon "vine")
    ;; Sound
    ("^\\(https?://\\)?\\(www\\.\\)?last\\.fm" all-the-icons-faicon "lastfm")
    ("^\\(https?://\\)?\\(www\\.\\)?mixcloud\\.com" all-the-icons-faicon "mixcloud")
    ("^\\(https?://\\)?\\(www\\.\\)?soundcloud\\.com" all-the-icons-faicon "soundcloud")
    ("spotify\\.com" all-the-icons-faicon "spotify")
    ;; Shopping
    ("^\\(https?://\\)?\\(www\\.\\)?amazon\\." all-the-icons-faicon "amazon")
    ("^\\(https?://\\)?\\(www\\.\\)?opencart\\.com" all-the-icons-faicon "opencart")
    ("^\\(https?://\\)?\\(www\\.\\)?paypal\\.com" all-the-icons-faicon "paypal")
    ("^\\(https?://\\)?\\(www\\.\\)?shirtsinbulk\\.com" all-the-icons-faicon "shitsinbulk")
    ;; Images
    ("^\\(https?://\\)?\\(www\\.\\)?500px\\.com" all-the-icons-faicon "500px")
    ("^\\(https?://\\)?\\(www\\.\\)?deviantart\\.com" all-the-icons-faicon "deviantart")
    ("^\\(https?://\\)?\\(www\\.\\)?flickr\\.com" all-the-icons-faicon "flickr")
    ("^\\(https?://\\)?\\(www\\.\\)?instagram\\.com" all-the-icons-faicon "instagram")
    ("^\\(https?://\\)?\\(www\\.\\)?pinterest\\." all-the-icons-faicon "pinterest")
    ;; Information and books
    ("^\\(https?://\\)?\\(www\\.\\)?digg\\.com" all-the-icons-faicon "digg")
    ("^\\(https?://\\)?\\(www\\.\\)?foursquare\\.com" all-the-icons-faicon "foursquare")
    ("^\\(https?://\\)?\\(www\\.\\)?getpocket\\.com" all-the-icons-faicon "get-pocket")
    ("^\\(https?://\\)?\\(www\\.\\)?scribd\\.com" all-the-icons-faicon "scribd")
    ("^\\(https?://\\)?\\(www\\.\\)?slideshare\\.net" all-the-icons-faicon "slideshare")
    ("stackexchange\\.com" all-the-icons-faicon "stack-exchange")
    ("^\\(https?://\\)?\\(www\\.\\)?stumbleupon\\.com" all-the-icons-faicon "stumbleupon")
    ("^\\(https?://\\)?\\(www\\.\\)?tripadvisor\\." all-the-icons-faicon "tripadvisor")
    ("^\\(https?://\\)?\\(www\\.\\)?yelp\\." all-the-icons-faicon "yelp")

    ("wikipedia\\.org" all-the-icons-faicon "wikipedia-w")
    ;; Various companies and tools
    ("^\\(https?://\\)?\\(www\\.\\)?angel\\.co" all-the-icons-faicon "angellist")
    ("^\\(https?://\\)?\\(www\\.\\)?apple\\.com" all-the-icons-faicon "apple")
    ("^\\(https?://\\)?\\(www\\.\\)?buysellads\\.com" all-the-icons-faicon "buysellads")
    ("^\\(https?://\\)?\\(www\\.\\)?connectdevelop\\.com" all-the-icons-faicon "connectdevelop")
    ("^\\(https?://\\)?\\(www\\.\\)?dashcube\\.com" all-the-icons-faicon "dashcube")
    ("^\\(https?://\\)?\\(www\\.\\)?dropbox\\.com" all-the-icons-faicon "dropbox")
    ("^\\(https?://\\)?\\(www\\.\\)?enviragallery\\.com" all-the-icons-faicon "envira")
    ("^\\(https?://\\)?\\(www\\.\\)?fortawesome\\.com" all-the-icons-faicon "fort-awesome")
    ("^\\(https?://\\)?\\(www\\.\\)?forumbee\\.com" all-the-icons-faicon "forumbee")
    ("^\\(https?://\\)?\\(www\\.\\)?gratipay\\.com" all-the-icons-faicon "gratipay")
    ("^\\(https?://\\)?\\(www\\.\\)?modx\\.com" all-the-icons-faicon "modx")
    ("^\\(https?://\\)?\\(www\\.\\)?pagelines\\.com" all-the-icons-faicon "pagelines")
    ("^\\(https?://\\)?\\(www\\.\\)?producthunt\\.com" all-the-icons-faicon "product-hunt")
    ("sellsy\\.com" all-the-icons-faicon "sellsy")
    ("^\\(https?://\\)?\\(www\\.\\)?simplybuilt\\.com" all-the-icons-faicon "simplybuilt")
    ("^\\(https?://\\)?\\(www\\.\\)?skyatlas\\.com" all-the-icons-faicon "skyatlas")
    ("^\\(https?://\\)?\\(www\\.\\)?skype\\.com" all-the-icons-faicon "skype")
    ("steampowered\\.com" all-the-icons-faicon "steam")
    ("^\\(https?://\\)?\\(www\\.\\)?themeisle\\.com" all-the-icons-faicon "themeisle")
    ("^\\(https?://\\)?\\(www\\.\\)?trello\\.com" all-the-icons-faicon "trello")
    ("^\\(https?://\\)?\\(www\\.\\)?whatsapp\\.com" all-the-icons-faicon "whatsapp")
    ("^\\(https?://\\)?\\(www\\.\\)?ycombinator\\.com" all-the-icons-faicon "y-combinator")
    ("yahoo\\.com" all-the-icons-faicon "yahoo")
    ("^\\(https?://\\)?\\(www\\.\\)?yoast\\.com" all-the-icons-faicon "yoast")
    ;; Catch all
    ("android" all-the-icons-faicon "android")
    ("creativecommons" all-the-icons-faicon "creative-commons")
    ("forums?" all-the-icons-octicon "comment-discussion")
    ("\\.pdf$" all-the-icons-octicon "file-pdf" :v-adjust 0.0 :face all-the-icons-dred)
    ("google" all-the-icons-faicon "google")
    ("\\.rss" all-the-icons-faicon "rss")
    ))

;; ====================
;;   Functions Start
;; ====================

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
(defun all-the-icons-icon-for-dir-with-chevron (dir &optional chevron padding)
  "Format an icon for DIR with CHEVRON similar to tree based directories.

If PADDING is provided, it will prepend and separate the chevron
and directory with PADDING.

Produces different symbols by inspecting DIR to distinguish
symlinks and git repositories which do not depend on the
directory contents"
  (let ((icon (all-the-icons-icon-for-dir dir))
        (chevron (if chevron (all-the-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
        (padding (or padding "\t")))
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
(defun all-the-icons-icon-for-dir (dir &rest arg-overrides)
  "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

Note: You want chevron, please use `all-the-icons-icon-for-dir-with-chevron'."
  (let* ((dirname (file-name-base (directory-file-name dir)))
         (icon (all-the-icons-match-to-alist dirname all-the-icons-dir-icon-alist))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if (file-remote-p dir) ;; don't call expand-file-name on a remote dir as this can make emacs hang
        (apply #'all-the-icons-octicon "terminal" (cdr args))
      (let
          ((path (expand-file-name dir)))
        (cond
         ((file-symlink-p path)
          (apply #'all-the-icons-octicon "file-symlink-directory" (cdr args)))
         ((all-the-icons-dir-is-submodule path)
          (apply #'all-the-icons-octicon "file-submodule" (cdr args)))
         ((file-exists-p (format "%s/.git" path))
          (apply #'all-the-icons-octicon "repo" (cdr args)))
         (t (apply (car icon) args)))))))

;;;###autoload
(defun all-the-icons-icon-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((ext (file-name-extension file))
         (icon (or (all-the-icons-match-to-alist file all-the-icons-regexp-icon-alist)
                   (and ext
                        (cdr (assoc (downcase ext)
                                    all-the-icons-extension-icon-alist)))
                   all-the-icons-default-file-icon))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;;###autoload
(defun all-the-icons-icon-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (cdr (or (assoc mode all-the-icons-mode-icon-alist)
                        (assoc (get mode 'derived-mode-parent) all-the-icons-mode-icon-alist))))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if icon (apply (car icon) args) mode)))

;;;###autoload
(defun all-the-icons-icon-for-url (url &rest arg-overrides)
  "Get the formatted icon for URL.
If an icon for URL isn't found in `all-the-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (all-the-icons-match-to-alist url all-the-icons-url-alist))
         (args (cdr icon)))
    (unless icon
      (setq icon '(all-the-icons-faicon "globe"))
      (setq args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

(defcustom all-the-icons--cache-limit 2048
  "Maximum cache size for functions cached by `all-the-icons-cache'."
  :type 'integer)

(defun all-the-icons-cache (func)
  "Set a cache for FUNC. Does not work on interactive functions."
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
(all-the-icons-cache #'all-the-icons-icon-for-file)
(all-the-icons-cache #'all-the-icons-icon-for-mode)
(all-the-icons-cache #'all-the-icons-icon-for-url)

;; Family Face Functions
(defun all-the-icons-icon-family-for-file (file)
  "Get the icons font family for FILE."
  (let* ((ext (file-name-extension file))
	     (icon (or (all-the-icons-match-to-alist file all-the-icons-regexp-icon-alist)
                   (and ext
                        (cdr (assoc (downcase ext)
                                    all-the-icons-extension-icon-alist)))
                   all-the-icons-default-file-icon)))
    (funcall (intern (format "%s-family" (car icon))))))

(defun all-the-icons-icon-family-for-mode (mode)
  "Get the icons font family for MODE."
  (let ((icon (cdr (assoc mode all-the-icons-mode-icon-alist))))
    (if icon (funcall (intern (format "%s-family" (car icon)))) nil)))

(defun all-the-icons-icon-family (icon)
  "Get a propertized ICON family programmatically."
  (plist-get (get-text-property 0 'face icon) :family))

(all-the-icons-cache #'all-the-icons-icon-family-for-file)
(all-the-icons-cache #'all-the-icons-icon-family-for-mode)
(all-the-icons-cache #'all-the-icons-icon-family)

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
    (intern (concat "all-the-icons-insert-" (downcase (symbol-name name)))))

  (defun all-the-icons--family-scale-factor (family)
    (intern (concat "all-the-icons-" (symbol-name family) "-scale-factor")))

  (defun all-the-icons--family-adjust (family)
    (intern (concat "all-the-icons-default-" (symbol-name family) "-adjust"))))

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
    (let* ((url-format "https://raw.githubusercontent.com/domtronn/all-the-icons.el/master/fonts/%s")
           (font-dest (cond
                       ;; Default Linux install directories
                       ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                        (concat (or (getenv "XDG_DATA_HOME")
                                    (concat (getenv "HOME") "/.local/share"))
                                "/fonts/"
                                all-the-icons-fonts-subdirectory))
                       ;; Default MacOS install directory
                       ((eq system-type 'darwin)
                        (concat (getenv "HOME")
                                "/Library/Fonts/"
                                all-the-icons-fonts-subdirectory))))
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
       (when duration (sit-for duration)))
     data)))

(defmacro all-the-icons-define-icon (name alist family &optional font-name)
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
     (defcustom ,(all-the-icons--family-scale-factor name) 1.0
       ,(format "The additional `height' face property Scale Factor for %s icons."
                (symbol-name name))
       :group 'all-the-icons
       :type 'number)
     (defcustom ,(all-the-icons--family-adjust name) 0.0
       ,(format "The additional `raise' display property adjustment for %s icons."
                (symbol-name name))
       :group 'all-the-icons
       :type 'number)
     (defun ,(all-the-icons--family-name name) () ,family)
     (defun ,(all-the-icons--data-name name) () ,alist)
     (defun ,(all-the-icons--function-name name) (icon-name &rest args)
       (let ((icon (cdr (assoc icon-name ,alist)))
             (other-face (when all-the-icons-color-icons (plist-get args :face)))
             (height   (* all-the-icons-scale-factor
                          ,(all-the-icons--family-scale-factor name)
                          (or (plist-get args :height) 1.0)))
             (v-adjust (* all-the-icons-scale-factor ,(all-the-icons--family-scale-factor name)
                          (+ (or (plist-get args :v-adjust) all-the-icons-default-adjust)
                             ,(all-the-icons--family-adjust name))))
             (family ,family))
         (unless icon
           (error (format "Unable to find icon with name `%s' in icon set `%s'" icon-name (quote ,name))))
         (let ((face (if other-face
                         `(:family ,family :height ,height :inherit ,other-face)
                       `(:family ,family :height ,height))))
           (propertize icon
                       'face face           ;so that this works without `font-lock-mode' enabled
                       'font-lock-face face ;so that `font-lock-mode' leaves this alone
                       'display `(raise ,v-adjust)
                       'rear-nonsticky t))))
     (defun ,(all-the-icons--insert-function-name name) (&optional arg)
       ,(format "Insert a %s icon at point." family)
       (interactive "P")
       (all-the-icons-insert arg (quote ,name)))))

(all-the-icons-define-icon alltheicon all-the-icons-data/alltheicons-alist    "all-the-icons")
(all-the-icons-define-icon fileicon   all-the-icons-data/file-icon-alist      "file-icons")
(all-the-icons-define-icon faicon     all-the-icons-data/fa-icon-alist        "FontAwesome")
(all-the-icons-define-icon octicon    all-the-icons-data/octicons-alist       "github-octicons" "octicons")
(all-the-icons-define-icon wicon      all-the-icons-data/weather-icons-alist  "Weather Icons"   "weathericons")
(all-the-icons-define-icon material   all-the-icons-data/material-icons-alist "Material Icons"  "material-design-icons")

(provide 'all-the-icons)

;;; all-the-icons.el ends here
