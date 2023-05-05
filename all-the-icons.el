;;; all-the-icons.el --- A library for inserting Developer icons -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Dominic Charlesworth <dgc336@gmail.com>
;; Copyright (C) 2019-2022  Jimmy Yuen Ho Wong <wyuenho@gmail.com>

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

;; This package is a utility for using and formatting various Icon
;; fonts within Emacs.  Icon Fonts allow you to propertize and format
;; icons the same way you would normal text.  This enables things such
;; as better scaling of and anti aliasing of the icons.

;; This package was inspired by

;; - `mode-icons' for Emacs, found at https://github.com/ryuslash/mode-icons
;; - `file-icons' for Atom, found at https://atom.io/packages/file-icons

;; Currently, this package provides an interface to the following Icon Fonts

;; - Atom File Icons,       found at https://atom.io/packages/file-icons
;; - GitHub Octicons,       found at http://octicons.github.com
;; - Weather Icons,         found at https://erikflowers.github.io/weather-icons
;; - VSCode Icons,          found at https://github.com/microsoft/vscode-codicons
;; - Optimized Devicons,    found at https://github.com/file-icons/DevOpicons
;; - Optimized MFizz,       found at https://github.com/file-icons/MFixx


;; Requests for new icons will be accepted and added to the AllTheIcons Icon Font

;;; Usage:

;; The simplest usage for this package is to use the following functions;

;;   `all-the-icons-icon-for-buffer'
;;   `all-the-icons-icon-for-dir'
;;   `all-the-icons-icon-for-file'
;;   `all-the-icons-icon-for-mode'

;; Which can be used to get a formatted icon for the current buffer, a
;; file name, or a major mode respectively.  e.g.

;;   (insert (all-the-icons-icon-for-file "foo.js"))

;; Inserts a JavaScript icon formatted like this

;;   #("some-icon" 0 1 (display (raise -0.24)
;;              face (:family "dev-icons" :height 1.08 :foreground "#FFD446")))

;; You can also insert icons directly using the individual icon family
;; functions

;;   `all-the-icons-file-icons'       // File Icons from the Atom File Icons package
;;   `all-the-icons-octicons'         // GitHub Octicons
;;   `all-the-icons-vscode-codicons'  // Visual Studio Code Icons
;;   `all-the-icons-weather-icons'    // Weather Icons
;;   `all-the-icons-devopicons'       // Devicons
;;   `all-the-icons-mdiff'            // MFizz Icons

;; You can call these functions with the icon name you want to insert, e.g.

;;   (all-the-icons-octicons        "file-binary")  // GitHub Octicon for Binary File
;;   (all-the-icons-weather-icons   "tornado")      // Weather Icon for tornado

;; A list of all the icon names for the different font families can be
;; found in the data directory, or by inspecting the alist variables.
;; All the alist variables are prefixed with `all-the-icons-data/'

;;; Code:
(require 'cl-lib)
(require 'svg)

(require 'all-the-icons-data-devopicons)
(require 'all-the-icons-data-file-icons)
(require 'all-the-icons-data-mfixx)
(require 'all-the-icons-data-octicons)
(require 'all-the-icons-data-vscode-codicons)
(require 'all-the-icons-data-weather-icons)
(require 'all-the-icons-data-fontawesome-4)
(require 'all-the-icons-data-fluentui-system-icons)
(require 'all-the-icons-data-material-icons)
(require 'all-the-icons-data-clockface)

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

(defvar all-the-icons-sets '() "List of defined icon sets.")

(defvar all-the-icons-extension-icon-alist
  '(
    ("fish"            all-the-icons-octicons "terminal"               :face all-the-icons-lpink)
    ("zsh"             all-the-icons-octicons "terminal"               :face all-the-icons-lcyan)
    ("sh"              all-the-icons-octicons "terminal"               :face all-the-icons-purple)
    ;; Meta
    ("tags"            all-the-icons-octicons "tag"                    :face all-the-icons-blue)
    ("log"             all-the-icons-octicons "log"                    :face all-the-icons-maroon)
    ;; Config
    ("node"            all-the-icons-devopicons "nodejs-small"         :face all-the-icons-green)
    ("babelrc"         all-the-icons-file-icons "babel"                :face all-the-icons-yellow)
    ("bashrc"          all-the-icons-vscode-codicons "terminal-bash"   :face all-the-icons-dpink)
    ("bash_profile"    all-the-icons-vscode-codicons "terminal-bash"   :face all-the-icons-dpink)
    ("bowerrc"         all-the-icons-devopicons "bower"                :face all-the-icons-silver)
    ("cr"              all-the-icons-file-icons "crystal"              :face all-the-icons-yellow)
    ("ecr"             all-the-icons-file-icons "crystal"              :face all-the-icons-yellow)
    ("ini"             all-the-icons-octicons "sliders"                :face all-the-icons-yellow)
    ("mk"              all-the-icons-file-icons "gnu"                  :face all-the-icons-dorange)
    ("cmake"           all-the-icons-file-icons "cmake"                :face all-the-icons-red)
    ("xml"             all-the-icons-octicons "file-code"              :face all-the-icons-lorange)
    ("json"            all-the-icons-vscode-codicons "json"            :face all-the-icons-yellow)
    ("cson"            all-the-icons-file-icons "config-coffeescript"  :face all-the-icons-yellow)
    ("yml"             all-the-icons-file-icons "yaml-alt4"            :face all-the-icons-dyellow)
    ("yaml"            all-the-icons-file-icons "yaml-alt4"            :face all-the-icons-dyellow)
    ;; ?
    ("pkg"             all-the-icons-octicons "package"                :face all-the-icons-dsilver)
    ("rpm"             all-the-icons-octicons "package"                :face all-the-icons-dsilver)
    ("pkgbuild"        all-the-icons-octicons "package"                :face all-the-icons-dsilver)
    ("elc"             all-the-icons-octicons "file-binary"            :face all-the-icons-dsilver)
    ("7z"              all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("bz"              all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("bz2"             all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("gz"              all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("lz4"             all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("tgz"             all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("xz"              all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("zip"             all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("zst"             all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("zstd"            all-the-icons-octicons "file-zip"               :face all-the-icons-lmaroon)
    ("dat"             all-the-icons-octicons "file-binary"            :face all-the-icons-cyan)
    ("dmg"             all-the-icons-fontawesome-4 "hdd-o"             :face all-the-icons-lsilver)
    ("dll"             all-the-icons-fontawesome-4 "cog"               :face all-the-icons-silver)
    ("ds_store"        all-the-icons-fontawesome-4 "cog"               :face all-the-icons-silver)
    ;; Source Codes
    ("scpt"            all-the-icons-file-icons "apple"                :face all-the-icons-pink)
    ("aup"             all-the-icons-file-icons "audacity"             :face all-the-icons-yellow)
    ("elm"             all-the-icons-file-icons "elm"                  :face all-the-icons-blue)
    ("erl"             all-the-icons-mfixx "erlang"                    :face all-the-icons-red)
    ("hrl"             all-the-icons-mfixx "erlang"                    :face all-the-icons-dred)
    ("eex"             all-the-icons-mfixx "elixir"                    :face all-the-icons-lorange)
    ("leex"            all-the-icons-mfixx "elixir"                    :face all-the-icons-lorange)
    ("heex"            all-the-icons-mfixx "elixir"                    :face all-the-icons-lorange)
    ("ex"              all-the-icons-mfixx "elixir"                    :face all-the-icons-lpurple)
    ("exs"             all-the-icons-mfixx "elixir"                    :face all-the-icons-lred)
    ("java"            all-the-icons-devopicons "java"                 :face all-the-icons-purple)
    ("gradle"          all-the-icons-file-icons "gradle"               :face all-the-icons-silver)
    ("ebuild"          all-the-icons-file-icons "gentoo"               :face all-the-icons-cyan)
    ("eclass"          all-the-icons-file-icons "gentoo"               :face all-the-icons-blue)
    ("go"              all-the-icons-file-icons "go"                   :face all-the-icons-blue)
    ("jl"              all-the-icons-file-icons "julia"                :face all-the-icons-purple)
    ("matlab"          all-the-icons-file-icons "matlab"               :face all-the-icons-orange)
    ("nix"             all-the-icons-file-icons "nix"                  :face all-the-icons-blue)
    ("pl"              all-the-icons-mfixx "perl"                      :face all-the-icons-lorange)
    ("pm"              all-the-icons-mfixx "perl"                      :face all-the-icons-lorange)
    ("pl6"             all-the-icons-file-icons "perl6"                :face all-the-icons-cyan)
    ("pm6"             all-the-icons-file-icons "perl6"                :face all-the-icons-pink)
    ("pod"             all-the-icons-devopicons "perl"                 :face all-the-icons-lgreen)
    ("php"             all-the-icons-file-icons "php"                  :face all-the-icons-lsilver)
    ("pony"            all-the-icons-file-icons "pony"                 :face all-the-icons-maroon)
    ("ps1"             all-the-icons-file-icons "powershell"           :face all-the-icons-blue)
    ("pro"             all-the-icons-devopicons "prolog"               :face all-the-icons-lmaroon)
    ("proog"           all-the-icons-devopicons "prolog"               :face all-the-icons-lmaroon)
    ("py"              all-the-icons-devopicons "python"               :face all-the-icons-dblue)
    ("idr"             all-the-icons-file-icons "idris"                :face all-the-icons-red)
    ("ipynb"           all-the-icons-file-icons "jupyter"              :face all-the-icons-dorange)
    ("gem"             all-the-icons-file-icons "rubygems"             :face all-the-icons-red)
    ("raku"            all-the-icons-file-icons "perl6"                :face all-the-icons-cyan)
    ("rakumod"         all-the-icons-file-icons "perl6"                :face all-the-icons-pink)
    ("rb"              all-the-icons-vscode-codicons "ruby"            :face all-the-icons-lred)
    ("rs"              all-the-icons-devopicons "rust"                 :face all-the-icons-maroon)
    ("rlib"            all-the-icons-devopicons "rust"                 :face all-the-icons-dmaroon)
    ("r"               all-the-icons-file-icons "r"                    :face all-the-icons-lblue)
    ("rd"              all-the-icons-file-icons "r"                    :face all-the-icons-lblue)
    ("rdx"             all-the-icons-file-icons "r"                    :face all-the-icons-lblue)
    ("rsx"             all-the-icons-file-icons "r"                    :face all-the-icons-lblue)
    ("svelte"          all-the-icons-file-icons "svelte"               :face all-the-icons-red)
    ("gql"             all-the-icons-file-icons "graphql"              :face all-the-icons-dpink)
    ("graphql"         all-the-icons-file-icons "graphql"              :face all-the-icons-dpink)
    ("c"               all-the-icons-mfixx "c"                         :face all-the-icons-blue)
    ("h"               all-the-icons-mfixx "c"                         :face all-the-icons-purple)
    ("m"               all-the-icons-mfixx "objc"                      )
    ("mm"              all-the-icons-mfixx "objc"                      )
    ;;
    ("cc"              all-the-icons-mfixx "c++"                       :face all-the-icons-blue)
    ("cpp"             all-the-icons-mfixx "c++"                       :face all-the-icons-blue)
    ("cxx"             all-the-icons-mfixx "c++"                       :face all-the-icons-blue)
    ("hh"              all-the-icons-mfixx "c++"                       :face all-the-icons-purple)
    ("hpp"             all-the-icons-mfixx "c++"                       :face all-the-icons-purple)
    ("hxx"             all-the-icons-mfixx "c++"                       :face all-the-icons-purple)
    ;; Lisps
    ("cl"              all-the-icons-file-icons "common-lisp"          :face all-the-icons-lorange)
    ("l"               all-the-icons-file-icons "lisp"                 :face all-the-icons-orange)
    ("lisp"            all-the-icons-file-icons "lisp"                 :face all-the-icons-orange)
    ("hy"              all-the-icons-file-icons "hy"                   :face all-the-icons-blue)
    ("el"              all-the-icons-file-icons "elisp"                :face all-the-icons-purple)
    ("clj"             all-the-icons-devopicons "clojure"              :face all-the-icons-blue)
    ("cljc"            all-the-icons-devopicons "clojure"              :face all-the-icons-blue)
    ("cljs"            all-the-icons-file-icons "clojurejs"            :face all-the-icons-dblue)
    ("coffee"          all-the-icons-devopicons "coffeescript"         :face all-the-icons-maroon)
    ("iced"            all-the-icons-devopicons "coffeescript"         :face all-the-icons-lmaroon)
    ("dart"            all-the-icons-devopicons "dart"                 :face all-the-icons-blue)
    ("rkt"             all-the-icons-file-icons "racket"               :face all-the-icons-red)
    ("scrbl"           all-the-icons-file-icons "racket"               :face all-the-icons-blue)
    ;; Stylesheeting
    ("css"             all-the-icons-devopicons "css3"                 :face all-the-icons-yellow)
    ("scss"            all-the-icons-devopicons "sass"                 :face all-the-icons-pink)
    ("sass"            all-the-icons-devopicons "sass"                 :face all-the-icons-dpink)
    ("less"            all-the-icons-devopicons "less"                 :face all-the-icons-dyellow)
    ("postcss"         all-the-icons-file-icons "postcss"              :face all-the-icons-dred)
    ("pcss"            all-the-icons-file-icons "postcss"              :face all-the-icons-dred)
    ("sss"             all-the-icons-file-icons "postcss"              :face all-the-icons-dred)
    ("styl"            all-the-icons-file-icons "stylus"               :face all-the-icons-lgreen)
    ("stylelintignore" all-the-icons-file-icons "stylelint"            :face all-the-icons-dyellow)
    ("csv"             all-the-icons-octicons "graph"                  :face all-the-icons-dblue)
    ;; haskell
    ("hs"              all-the-icons-devopicons "haskell"              :face all-the-icons-red)
    ("chs"             all-the-icons-devopicons "haskell"              :face all-the-icons-red)
    ("lhs"             all-the-icons-devopicons "haskell"              :face all-the-icons-red)
    ("hsc"             all-the-icons-devopicons "haskell"              :face all-the-icons-red)
    ;; Web modes
    ("inky-haml"       all-the-icons-file-icons "haml"                 :face all-the-icons-lyellow)
    ("haml"            all-the-icons-file-icons "haml"                 :face all-the-icons-lyellow)
    ("htm"             all-the-icons-devopicons "html5"                :face all-the-icons-orange)
    ("html"            all-the-icons-devopicons "html5"                :face all-the-icons-orange)
    ("inky-er"         all-the-icons-devopicons "html5"                :face all-the-icons-lred)
    ("inky-erb"        all-the-icons-devopicons "html5"                :face all-the-icons-lred)
    ("erb"             all-the-icons-devopicons "html5"                :face all-the-icons-lred)
    ("hbs"             all-the-icons-file-icons "moustache"            :face all-the-icons-green)
    ("inky-slim"       all-the-icons-octicons "meter"                  :face all-the-icons-yellow)
    ("slim"            all-the-icons-octicons "meter"                  :face all-the-icons-yellow)
    ("jade"            all-the-icons-file-icons "jade"                 :face all-the-icons-red)
    ("pug"             all-the-icons-file-icons "pug"                  :face all-the-icons-red)
    ;; Javascript
    ("d3js"            all-the-icons-file-icons "d3"                   :face all-the-icons-lgreen)
    ("re"              all-the-icons-file-icons "reason"               :face all-the-icons-red-alt)
    ("rei"             all-the-icons-file-icons "reason"               :face all-the-icons-dred)
    ("ml"              all-the-icons-file-icons "ocaml"                :face all-the-icons-lpink)
    ("mli"             all-the-icons-file-icons "ocaml"                :face all-the-icons-dpink)
    ("ts"              all-the-icons-file-icons "typescript"           :face all-the-icons-blue-alt)
    ("js"              all-the-icons-mfixx "javascript"                :face all-the-icons-yellow)
    ("es"              all-the-icons-mfixx "javascript"                :face all-the-icons-yellow)
    ("jsx"             all-the-icons-file-icons "jsx-atom"             :face all-the-icons-cyan-alt)
    ("tsx"             all-the-icons-file-icons "tsx"                  :face all-the-icons-cyan-alt)
    ("mjs"             all-the-icons-devopicons "nodejs-small"         :face all-the-icons-lgreen)
    ("vue"             all-the-icons-file-icons "vue"                  :face all-the-icons-lgreen)
    ("wasm"            all-the-icons-file-icons "webassembly"          :face all-the-icons-purple-alt)
    ("wat"             all-the-icons-file-icons "webassembly"          :face all-the-icons-purple-alt)
    ("eslintignore"    all-the-icons-file-icons "eslint"               :face all-the-icons-dpurple)

    ("sbt"             all-the-icons-file-icons "sbt"                  :face all-the-icons-red)
    ("scala"           all-the-icons-devopicons "scala"                :face all-the-icons-red)
    ("scm"             all-the-icons-file-icons "scheme"               :face all-the-icons-red)
    ("swift"           all-the-icons-devopicons "swift"                :face all-the-icons-green)

    ("tcl"             all-the-icons-file-icons "tcl"                  :face all-the-icons-dred)

    ("tf"              all-the-icons-file-icons "terraform"            :face all-the-icons-purple-alt)
    ("tfvars"          all-the-icons-file-icons "terraform"            :face all-the-icons-purple-alt)
    ("tfstate"         all-the-icons-file-icons "terraform"            :face all-the-icons-purple-alt)

    ("asm"             all-the-icons-file-icons "assembly-generic"     :face all-the-icons-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("v"               all-the-icons-file-icons "verilog"              :face all-the-icons-red)
    ("vams"            all-the-icons-file-icons "verilog"              :face all-the-icons-red)
    ("sv"              all-the-icons-file-icons "verilog"              :face all-the-icons-red)
    ("sva"             all-the-icons-file-icons "verilog"              :face all-the-icons-red)
    ("svh"             all-the-icons-file-icons "verilog"              :face all-the-icons-red)
    ("svams"           all-the-icons-file-icons "verilog"              :face all-the-icons-red)
    ;; VHDL(-AMS)
    ("vhd"             all-the-icons-file-icons "vhdl"                 :face all-the-icons-blue)
    ("vhdl"            all-the-icons-file-icons "vhdl"                 :face all-the-icons-blue)
    ("vhms"            all-the-icons-file-icons "vhdl"                 :face all-the-icons-blue)
    ;; Cabal
    ("cabal"           all-the-icons-file-icons "cabal"                :face all-the-icons-lblue)
    ;; Kotlin
    ("kt"              all-the-icons-file-icons "kotlin"               :face all-the-icons-orange)
    ("kts"             all-the-icons-file-icons "kotlin"               :face all-the-icons-orange)
    ;; Nimrod
    ("nim"             all-the-icons-file-icons "nimrod"               :face all-the-icons-yellow)
    ("nims"            all-the-icons-file-icons "nimrod"               :face all-the-icons-yellow)
    ;; SQL
    ("sql"             all-the-icons-octicons "database"               :face all-the-icons-silver)
    ;; Lua
    ("lua"             all-the-icons-file-icons "lua"                  :face all-the-icons-dblue)
    ;; ASCII doc
    ("adoc"            all-the-icons-file-icons "asciidoc"             :face all-the-icons-lblue)
    ("asciidoc"        all-the-icons-file-icons "asciidoc"             :face all-the-icons-lblue)
    ;; Puppet
    ("pp"              all-the-icons-file-icons "puppet"               :face all-the-icons-yellow)
    ;; Jinja
    ("j2"              all-the-icons-file-icons "jinja"                :face all-the-icons-silver)
    ("jinja2"          all-the-icons-file-icons "jinja"                :face all-the-icons-silver)
    ;; Docker
    ("dockerfile"      all-the-icons-file-icons "docker"               :face all-the-icons-cyan)
    ("dockerignore"    all-the-icons-file-icons "docker"               :face all-the-icons-dblue)
    ;; Vagrant
    ("vagrantfile"     all-the-icons-file-icons "vagrant"              :face all-the-icons-blue)
    ;; GLSL
    ("glsl"            all-the-icons-file-icons "vertexshader"         :face all-the-icons-blue)
    ("vert"            all-the-icons-file-icons "vertexshader"         :face all-the-icons-blue)
    ("tesc"            all-the-icons-file-icons "vertexshader"         :face all-the-icons-purple)
    ("tese"            all-the-icons-file-icons "vertexshader"         :face all-the-icons-dpurple)
    ("geom"            all-the-icons-file-icons "vertexshader"         :face all-the-icons-green)
    ("frag"            all-the-icons-file-icons "vertexshader"         :face all-the-icons-red)
    ("comp"            all-the-icons-file-icons "vertexshader"         :face all-the-icons-dblue)
    ;; CUDA
    ("cu"              all-the-icons-file-icons "nvidia"               :face all-the-icons-green)
    ("cuh"             all-the-icons-file-icons "nvidia"               :face all-the-icons-green)
    ;; Fortran
    ("f90"             all-the-icons-file-icons "fortran"              :face all-the-icons-purple)
    ;; C#
    ("cs"              all-the-icons-file-icons "c#"                   :face all-the-icons-dblue)
    ("csx"             all-the-icons-file-icons "c#-script"            :face all-the-icons-dblue)
    ;; F#
    ("fs"              all-the-icons-devopicons "fsharp"               :face all-the-icons-blue-alt)
    ("fsi"             all-the-icons-devopicons "fsharp"               :face all-the-icons-blue-alt)
    ("fsx"             all-the-icons-devopicons "fsharp"               :face all-the-icons-blue-alt)
    ("fsscript"        all-the-icons-devopicons "fsharp"               :face all-the-icons-blue-alt)
    ;; zig
    ("zig"             all-the-icons-file-icons "zig"                  :face all-the-icons-orange)
    ;; odin
    ("odin"            all-the-icons-file-icons "odin"                 :face all-the-icons-lblue)
    ;; File Types
    ("ico"             all-the-icons-octicons "file-media"             :face all-the-icons-blue)
    ("png"             all-the-icons-octicons "file-media"             :face all-the-icons-orange)
    ("gif"             all-the-icons-octicons "file-media"             :face all-the-icons-green)
    ("jpeg"            all-the-icons-octicons "file-media"             :face all-the-icons-dblue)
    ("jpg"             all-the-icons-octicons "file-media"             :face all-the-icons-dblue)
    ("webp"            all-the-icons-octicons "file-media"             :face all-the-icons-dblue)
    ("svg"             all-the-icons-mfixx "svg"                       :face all-the-icons-lgreen)
    ;; Audio
    ("mp3"             all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("wav"             all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("m4a"             all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("ogg"             all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("flac"            all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("opus"            all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("au"              all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("aif"             all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("aifc"            all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ("aiff"            all-the-icons-fontawesome-4 "file-audio-o"      :face all-the-icons-dred)
    ;; Video
    ("mov"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ("mp4"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ("ogv"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-dblue)
    ("mpg"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ("mpeg"            all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ("flv"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ("ogv"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-dblue)
    ("mkv"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ("webm"            all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ("dav"             all-the-icons-fontawesome-4 "file-video-o"      :face all-the-icons-blue)
    ;; Fonts
    ("ttf"             all-the-icons-file-icons "font"                 :face all-the-icons-dcyan)
    ("woff"            all-the-icons-file-icons "font"                 :face all-the-icons-cyan)
    ("woff2"           all-the-icons-file-icons "font"                 :face all-the-icons-cyan)
    ;; Doc
    ("pdf"             all-the-icons-vscode-codicons "file-pdf"        :face all-the-icons-dred)
    ("text"            all-the-icons-fontawesome-4 "file-text-o"       :face all-the-icons-cyan)
    ("txt"             all-the-icons-fontawesome-4 "file-text-o"       :face all-the-icons-cyan)
    ("doc"             all-the-icons-file-icons "microsoft-word"       :face all-the-icons-blue)
    ("docx"            all-the-icons-file-icons "microsoft-word"       :face all-the-icons-blue)
    ("docm"            all-the-icons-file-icons "microsoft-word"       :face all-the-icons-blue)
    ("texi"            all-the-icons-file-icons "gnu"                  :face all-the-icons-lred)
    ("tex"             all-the-icons-file-icons "latex"                :face all-the-icons-lred)
    ("md"              all-the-icons-octicons "markdown"               :face all-the-icons-lblue)
    ("rst"             all-the-icons-file-icons "restructuredtext"     :face all-the-icons-lblue)
    ("bib"             all-the-icons-file-icons "bibtex"               :face all-the-icons-maroon)
    ("org"             all-the-icons-file-icons "org"                  :face all-the-icons-lgreen)
    ("pps"             all-the-icons-file-icons "microsoft-powerpoint" :face all-the-icons-orange)
    ("ppt"             all-the-icons-file-icons "microsoft-powerpoint" :face all-the-icons-orange)
    ("pptsx"           all-the-icons-file-icons "microsoft-powerpoint" :face all-the-icons-orange)
    ("pptx"            all-the-icons-file-icons "microsoft-powerpoint" :face all-the-icons-orange)
    ("knt"             all-the-icons-file-icons "microsoft-powerpoint" :face all-the-icons-cyan)
    ("xlsx"            all-the-icons-file-icons "microsoft-excel"      :face all-the-icons-dgreen)
    ("xlsm"            all-the-icons-file-icons "microsoft-excel"      :face all-the-icons-dgreen)
    ("xlsb"            all-the-icons-file-icons "microsoft-excel"      :face all-the-icons-dgreen)
    ("xltx"            all-the-icons-file-icons "microsoft-excel"      :face all-the-icons-dgreen)
    ("xltm"            all-the-icons-file-icons "microsoft-excel"      :face all-the-icons-dgreen)
    ("ly"              all-the-icons-fontawesome-4 "music"             :face all-the-icons-green)

    ;; Email
    ("eml"             all-the-icons-fontawesome-4 "envelope"          :face all-the-icons-blue)
    ("msg"             all-the-icons-fontawesome-4 "envelope"          :face all-the-icons-blue)

    ;;
    ("key"             all-the-icons-octicons "key"                    :face all-the-icons-lblue)
    ("pem"             all-the-icons-octicons "key"                    :face all-the-icons-orange)
    ("p12"             all-the-icons-octicons "key"                    :face all-the-icons-dorange)
    ("crt"             all-the-icons-octicons "key"                    :face all-the-icons-lblue)
    ("pub"             all-the-icons-octicons "key"                    :face all-the-icons-blue)
    ("gpg"             all-the-icons-octicons "key"                    :face all-the-icons-lblue)
    ("cache"           all-the-icons-material-icons "cached"           :face all-the-icons-green)
    ("csv"             all-the-icons-octicons "graph"                  :face all-the-icons-dblue)))


(define-obsolete-variable-alias 'all-the-icons-icon-alist
  'all-the-icons-regexp-icon-alist
  "5.0.0"
  "`all-the-icons-icon-alist' has been split to
`all-the-icons-extension-icon-alist' and `all-the-icons-regexp-icon-alist'
for performance sake.")

(defvar all-the-icons-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"                  all-the-icons-octicons "tag"               :face all-the-icons-blue)
    ("^TODO$"                  all-the-icons-octicons "checklist"         :face all-the-icons-lyellow)
    ("^LICENSE$"               all-the-icons-octicons "book"              :face all-the-icons-blue)
    ("^readme"                 all-the-icons-octicons "book"              :face all-the-icons-lcyan)

    ;; Config
    ("nginx$"                  all-the-icons-file-icons "nginx"           :face all-the-icons-dgreen)
    ("apache$"                 all-the-icons-mfixx "apache"               :face all-the-icons-dgreen)

    ;; C
    ("^Makefile$"              all-the-icons-file-icons "gnu"             :face all-the-icons-dorange)
    ("^CMakeLists.txt$"        all-the-icons-file-icons "cmake"           :face all-the-icons-red)
    ("^CMakeCache.txt$"        all-the-icons-file-icons "cmake"           :face all-the-icons-blue)
    ("^meson.build$"           all-the-icons-file-icons "meson"           :face all-the-icons-purple)
    ("^meson_options.txt$"     all-the-icons-file-icons "meson"           :face all-the-icons-purple)

    ;; Docker
    ("^\\.?Dockerfile"         all-the-icons-file-icons "docker"          :face all-the-icons-blue)

    ;; Homebrew
    ("^Brewfile$"              all-the-icons-file-icons "homebrew"        :face all-the-icons-lsilver)

    ;; ;; AWS
    ("^stack.*.json$"          all-the-icons-mfixx "aws"                  :face all-the-icons-orange)
    ("^serverless\\.yml$"      all-the-icons-file-icons "serverless"      :face all-the-icons-yellow)

    ;; lock files
    ("~$"                      all-the-icons-octicons "lock"              :face all-the-icons-maroon)

    ;; Source Codes
    ("^mix.lock$"              all-the-icons-mfixx "elixir"               :face all-the-icons-lyellow)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$" all-the-icons-file-icons "rubygems"         :face all-the-icons-red)
    ("_?test\\.rb$"            all-the-icons-file-icons "test-ruby"        :face all-the-icons-red)
    ("_?test_helper\\.rb$"     all-the-icons-file-icons "test-ruby"        :face all-the-icons-dred)
    ("_?spec\\.rb$"            all-the-icons-file-icons "test-ruby"        :face all-the-icons-red)
    ("_?spec_helper\\.rb$"     all-the-icons-file-icons "test-ruby"        :face all-the-icons-dred)

    ("-?spec\\.ts$"            all-the-icons-file-icons "test-typescript"  :face all-the-icons-blue)
    ("-?test\\.ts$"            all-the-icons-file-icons "test-typescript"  :face all-the-icons-blue)
    ("-?spec\\.js$"            all-the-icons-file-icons "test-js"          :face all-the-icons-lpurple)
    ("-?test\\.js$"            all-the-icons-file-icons "test-js"          :face all-the-icons-lpurple)
    ("-?spec\\.jsx$"           all-the-icons-file-icons "test-react"       :face all-the-icons-blue-alt)
    ("-?test\\.jsx$"           all-the-icons-file-icons "test-react"       :face all-the-icons-blue-alt)

    ;; Git
    ("^MERGE_"                 all-the-icons-octicons "git-merge"          :face all-the-icons-red)
    ("^COMMIT_EDITMSG"         all-the-icons-octicons "git-commit"         :face all-the-icons-red)

    ;; Stylesheeting
    ("^\\.?stylelint"          all-the-icons-file-icons "stylelint"        :face all-the-icons-lyellow)

    ;; JavaScript
    ("^package.json$"          all-the-icons-file-icons "npm"              :face all-the-icons-red)
    ("^package.lock.json$"     all-the-icons-file-icons "npm"              :face all-the-icons-dred)
    ("^yarn\\.lock"            all-the-icons-file-icons "yarn"             :face all-the-icons-blue-alt)
    ("\\.npmignore$"           all-the-icons-file-icons "npm"              :face all-the-icons-dred)
    ("^bower.json$"            all-the-icons-devopicons "bower"            :face all-the-icons-lorange)
    ("^gulpfile"               all-the-icons-devopicons "gulp"             :face all-the-icons-lred)
    ("^gruntfile"              all-the-icons-devopicons "grunt"            :face all-the-icons-lyellow)
    ("^webpack"                all-the-icons-file-icons "webpack"          :face all-the-icons-lblue)
    ("^\\.?eslint"             all-the-icons-file-icons "eslint"           :face all-the-icons-purple)

    ;; Go
    ("^go.mod$"                all-the-icons-file-icons "config-go"        :face all-the-icons-blue-alt)
    ("^go.work$"               all-the-icons-file-icons "config-go"        :face all-the-icons-blue-alt)

    ;; Emacs
    ("bookmark"                all-the-icons-octicons "bookmark"           :face all-the-icons-lpink)

    ("^\\*scratch\\*$"         all-the-icons-fontawesome-4 "sticky-note-o" :face all-the-icons-lyellow)
    ("^\\*scratch.*"           all-the-icons-fontawesome-4 "sticky-note-o" :face all-the-icons-yellow)
    ("^\\*new-tab\\*$"         all-the-icons-octicons "star-fill"          :face all-the-icons-cyan)

    ("^\\."                    all-the-icons-fontawesome-4 "cog"           )))

(defvar all-the-icons-default-file-icon
  '(all-the-icons-octicons "file" :face all-the-icons-dsilver))

(defvar all-the-icons-dir-icon-alist
  '(
    ("trash"            all-the-icons-octicons "trash"                )
    ("dropbox"          all-the-icons-devopicons "dropbox"            )
    ("google[ _-]drive" all-the-icons-devopicons "google-drive"       )
    ("documents"        all-the-icons-octicons "book"                 )
    ("download"         all-the-icons-vscode-codicons "cloud-download")
    ("desktop"          all-the-icons-octicons "device-desktop"       )
    ("pictures"         all-the-icons-fontawesome-4 "picture-o"       )
    ("photos"           all-the-icons-fontawesome-4 "camera-retro"    )
    ("music"            all-the-icons-fontawesome-4 "music"           )
    ("movies"           all-the-icons-fontawesome-4 "film"            )
    ("code"             all-the-icons-octicons "code"                 )
    ("workspace"        all-the-icons-octicons "code"                 )
    ("test"             all-the-icons-file-icons "test-directory"     )
    ("\\.git"           all-the-icons-devopicons "git"                )
    ("\\.hg"            all-the-icons-file-icons "mercurial"          )
    ("\\.svn"           all-the-icons-file-icons "svn"                )
    ("\\.[^.]+"         all-the-icons-fluentui-system-icons "folder"  )))

(defvar all-the-icons-default-dir-icon
  '(all-the-icons-fluentui-system-icons "folder" :style filled))

(defvar all-the-icons-weather-icon-alist
  '(
    ("tornado"               all-the-icons-weather-icons "tornado"                )
    ("hurricane"             all-the-icons-weather-icons "hurricane"              )
    ("thunderstorms"         all-the-icons-weather-icons "thunderstorm"           )
    ("sunny"                 all-the-icons-weather-icons "day-sunny"              )
    ("rain.*snow"            all-the-icons-weather-icons "rain-mix"               )
    ("rain.*hail"            all-the-icons-weather-icons "rain-mix"               )
    ("sleet"                 all-the-icons-weather-icons "sleet"                  )
    ("hail"                  all-the-icons-weather-icons "hail"                   )
    ("drizzle"               all-the-icons-weather-icons "sprinkle"               )
    ("rain"                  all-the-icons-weather-icons "showers"                )
    ("showers"               all-the-icons-weather-icons "showers"                )
    ("blowing.*snow"         all-the-icons-weather-icons "snow-wind"              )
    ("snow"                  all-the-icons-weather-icons "snow"                   )
    ("dust"                  all-the-icons-weather-icons "dust"                   )
    ("fog"                   all-the-icons-weather-icons "fog"                    )
    ("haze"                  all-the-icons-weather-icons "day-haze"               )
    ("smoky"                 all-the-icons-weather-icons "smoke"                  )
    ("blustery"              all-the-icons-weather-icons "cloudy-windy"           )
    ("windy"                 all-the-icons-weather-icons "cloudy-gusts"           )
    ("cold"                  all-the-icons-weather-icons "snowflake-cold"         )
    ("partly.*cloudy.*night" all-the-icons-weather-icons "night-alt-partly-cloudy")
    ("partly.*cloudy"        all-the-icons-weather-icons "day-cloudy-high"        )
    ("cloudy.*night"         all-the-icons-weather-icons "night-alt-cloudy"       )
    ("cxloudy.*day"          all-the-icons-weather-icons "day-cloudy"             )
    ("cloudy"                all-the-icons-weather-icons "cloudy"                 )
    ("clear.*night"          all-the-icons-weather-icons "night-clear"            )
    ("fair.*night"           all-the-icons-weather-icons "stars"                  )
    ("fair.*day"             all-the-icons-weather-icons "horizon"                )
    ("hot"                   all-the-icons-weather-icons "hot"                    )
    ("not.*available"        all-the-icons-weather-icons "na"                     )))

(defvar all-the-icons-mode-icon-alist
  '(
    (emacs-lisp-mode          all-the-icons-file-icons "elisp"            :face all-the-icons-purple)
    (circe-server-mode        all-the-icons-octicons "comment-discussion" )
    (circe-channel-mode       all-the-icons-octicons "comment-discussion" )
    (circe-query-mode         all-the-icons-octicons "comment-discussion" )
    (crystal-mode             all-the-icons-file-icons "crystal"          :face all-the-icons-yellow)
    (erc-mode                 all-the-icons-octicons "comment-discussion" )
    (inferior-emacs-lisp-mode all-the-icons-file-icons "elisp"            :face all-the-icons-lblue)
    (dired-mode               all-the-icons-fontawesome-4 "folder"        )
    (lisp-interaction-mode    all-the-icons-file-icons "lisp"             :face all-the-icons-orange)
    (sly-mrepl-mode           all-the-icons-file-icons "common-lisp"      :face all-the-icons-orange)
    (slime-repl-mode          all-the-icons-file-icons "common-lisp"      :face all-the-icons-orange)
    (org-mode                 all-the-icons-file-icons "org"              :face all-the-icons-lgreen)
    (tsx-ts-mode              all-the-icons-file-icons "tsx"              :face all-the-icons-cyan-alt)
    (typescript-mode          all-the-icons-file-icons "typescript"       :face all-the-icons-blue-alt)
    (typescript-ts-mode       all-the-icons-file-icons "typescript"       :face all-the-icons-blue-alt)
    (typescript-tsx-mode      all-the-icons-file-icons "tsx"              :face all-the-icons-cyan-alt)
    (js-mode                  all-the-icons-mfixx "javascript"            :face all-the-icons-yellow)
    (js-jsx-mode              all-the-icons-file-icons "jsx-atom"         :face all-the-icons-yellow)
    (js2-mode                 all-the-icons-mfixx "javascript"            :face all-the-icons-yellow)
    (js3-mode                 all-the-icons-mfixx "javascript"            :face all-the-icons-yellow)
    (rjsx-mode                all-the-icons-file-icons "jsx-atom"         :face all-the-icons-cyan-alt)
    (term-mode                all-the-icons-octicons "terminal"           )
    (vterm-mode               all-the-icons-octicons "terminal"           )
    (eshell-mode              all-the-icons-octicons "terminal"           :face all-the-icons-purple)
    (magit-refs-mode          all-the-icons-octicons "git-branch"         :face all-the-icons-red)
    (magit-process-mode       all-the-icons-octicons "mark-github"        )
    (magit-diff-mode          all-the-icons-octicons "git-compare"        :face all-the-icons-lblue)
    (ediff-mode               all-the-icons-octicons "git-compare"        :face all-the-icons-red)
    (comint-mode              all-the-icons-octicons "terminal"           :face all-the-icons-lblue)
    (eww-mode                 all-the-icons-octicons "browser"            :face all-the-icons-red)
    (org-agenda-mode          all-the-icons-octicons "checklist"          :face all-the-icons-lgreen)
    (cfw:calendar-mode        all-the-icons-octicons "calendar"           )
    (ibuffer-mode             all-the-icons-vscode-codicons "files"       :face all-the-icons-dsilver)
    (messages-buffer-mode     all-the-icons-octicons "log"                :face all-the-icons-dsilver)
    (help-mode                all-the-icons-octicons "question"           :face all-the-icons-purple)
    (helpful-mode             all-the-icons-octicons "question"           :face all-the-icons-purple)
    (benchmark-init/tree-mode all-the-icons-octicons "meter"              )
    (jenkins-mode             all-the-icons-file-icons "jenkins"          :face all-the-icons-blue)
    (magit-popup-mode         all-the-icons-devopicons "git"              :face all-the-icons-red)
    (magit-status-mode        all-the-icons-devopicons "git"              :face all-the-icons-lred)
    (magit-log-mode           all-the-icons-devopicons "git"              :face all-the-icons-green)
    (mu4e-compose-mode        all-the-icons-octicons "pencil"             )
    (mu4e-headers-mode        all-the-icons-octicons "mail"               )
    (mu4e-main-mode           all-the-icons-octicons "mail"               )
    (mu4e-view-mode           all-the-icons-vscode-codicons "mail-read"   )
    (sieve-mode               all-the-icons-octicons "mail"               )
    (gnus-group-mode          all-the-icons-octicons "mail"               )
    (gnus-summary-mode        all-the-icons-octicons "mail"               )
    (gnus-article-mode        all-the-icons-vscode-codicons "mail-read"   )
    (message-mode             all-the-icons-octicons "pencil"             )
    (package-menu-mode        all-the-icons-octicons "package"            :face all-the-icons-silver)
    (paradox-menu-mode        all-the-icons-octicons "package"            :face all-the-icons-silver)
    (Custom-mode              all-the-icons-octicons "sliders"            )

    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode                 all-the-icons--web-mode-icon)

    (fundamental-mode                  all-the-icons-file-icons "elisp"            :face all-the-icons-dsilver)
    (special-mode                      all-the-icons-file-icons "elisp"            :face all-the-icons-yellow)
    (text-mode                         all-the-icons-fontawesome-4 "file-text-o"   :face all-the-icons-cyan)
    (enh-ruby-mode                     all-the-icons-vscode-codicons "ruby"        :face all-the-icons-lred)
    (ruby-mode                         all-the-icons-vscode-codicons "ruby"        :face all-the-icons-lred)
    (ruby-ts-mode                      all-the-icons-vscode-codicons "ruby"        :face all-the-icons-lred)
    (inf-ruby-mode                     all-the-icons-vscode-codicons "ruby"        :face all-the-icons-red)
    (projectile-rails-compilation-mode all-the-icons-mfixx "rails"                 :face all-the-icons-red)
    (rspec-compilation-mode            all-the-icons-file-icons "test-ruby"        :face all-the-icons-red)
    (rake-compilation-mode             all-the-icons-file-icons "config-ruby"      :face all-the-icons-red)
    (sh-mode                           all-the-icons-octicons "terminal"           :face all-the-icons-purple)
    (bash-ts-mode                      all-the-icons-octicons "terminal"           :face all-the-icons-purple)
    (shell-mode                        all-the-icons-octicons "terminal"           :face all-the-icons-purple)
    (fish-mode                         all-the-icons-octicons "terminal"           :face all-the-icons-lpink)
    (nginx-mode                        all-the-icons-file-icons "nginx"            :face all-the-icons-dgreen)
    (apache-mode                       all-the-icons-mfixx "apache"                :face all-the-icons-dgreen)
    (makefile-mode                     all-the-icons-file-icons "gnu"              :face all-the-icons-dorange)
    (cmake-mode                        all-the-icons-file-icons "cmake"            :face all-the-icons-red)
    (cmake-ts-mode                     all-the-icons-file-icons "cmake"            :face all-the-icons-red)
    (dockerfile-mode                   all-the-icons-file-icons "docker"           :face all-the-icons-blue)
    (dockerfile-ts-mode                all-the-icons-file-icons "docker"           :face all-the-icons-blue)
    (docker-compose-mode               all-the-icons-file-icons "docker"           :face all-the-icons-lblue)
    (nxml-mode                         all-the-icons-octicons "file-code"          :face all-the-icons-lorange)
    (json-mode                         all-the-icons-vscode-codicons "json"        :face all-the-icons-yellow)
    (json-ts-mode                      all-the-icons-vscode-codicons "json"        :face all-the-icons-yellow)
    (jsonian-mode                      all-the-icons-vscode-codicons "json"        :face all-the-icons-yellow)
    (yaml-mode                         all-the-icons-file-icons "yaml-alt4"        :face all-the-icons-dyellow)
    (yaml-ts-mode                      all-the-icons-file-icons "yaml-alt4"        :face all-the-icons-dyellow)
    (elisp-byte-code-mode              all-the-icons-octicons "file-binary"        :face all-the-icons-dsilver)
    (archive-mode                      all-the-icons-octicons "file-zip"           :face all-the-icons-lmaroon)
    (elm-mode                          all-the-icons-file-icons "elm"              :face all-the-icons-blue)
    (erlang-mode                       all-the-icons-mfixx "erlang"                :face all-the-icons-red)
    (heex-ts-mode                      all-the-icons-mfixx "elixir"                :face all-the-icons-lorange)
    (elixir-mode                       all-the-icons-mfixx "elixir"                :face all-the-icons-lorange)
    (elixir-ts-mode                    all-the-icons-mfixx "elixir"                :face all-the-icons-lorange)
    (java-mode                         all-the-icons-devopicons "java"             :face all-the-icons-purple)
    (java-ts-mode                      all-the-icons-devopicons "java"             :face all-the-icons-purple)
    (julia-mode                        all-the-icons-file-icons "julia"            :face all-the-icons-purple)
    (julia-ts-mode                     all-the-icons-file-icons "julia"            :face all-the-icons-purple)
    (go-mode                           all-the-icons-file-icons "go"               :face all-the-icons-blue)
    (go-ts-mode                        all-the-icons-file-icons "go"               :face all-the-icons-blue)
    (go-mod-ts-mode                    all-the-icons-file-icons "config-go"        :face all-the-icons-blue-alt)
    (go-dot-mod-mode                   all-the-icons-file-icons "config-go"        :face all-the-icons-blue-alt)
    (go-dot-work-mode                  all-the-icons-file-icons "config-go"        :face all-the-icons-blue-alt)
    (graphql-mode                      all-the-icons-file-icons "graphql"          :face all-the-icons-dpink)
    (matlab-mode                       all-the-icons-file-icons "matlab"           :face all-the-icons-orange)
    (nix-mode                          all-the-icons-file-icons "nix"              :face all-the-icons-blue)
    (perl-mode                         all-the-icons-mfixx "perl"                  :face all-the-icons-lorange)
    (cperl-mode                        all-the-icons-mfixx "perl"                  :face all-the-icons-lorange)
    (php-mode                          all-the-icons-file-icons "php"              :face all-the-icons-lsilver)
    (prolog-mode                       all-the-icons-devopicons "prolog"           :face all-the-icons-lmaroon)
    (python-mode                       all-the-icons-devopicons "python"           :face all-the-icons-dblue)
    (python-ts-mode                    all-the-icons-devopicons "python"           :face all-the-icons-dblue)
    (inferior-python-mode              all-the-icons-devopicons "python"           :face all-the-icons-dblue)
    (racket-mode                       all-the-icons-file-icons "racket"           :face all-the-icons-red)
    (rust-mode                         all-the-icons-devopicons "rust"             :face all-the-icons-maroon)
    (rust-ts-mode                      all-the-icons-devopicons "rust"             :face all-the-icons-maroon)
    (rustic-mode                       all-the-icons-devopicons "rust"             :face all-the-icons-maroon)
    (scala-mode                        all-the-icons-devopicons "scala"            :face all-the-icons-red)
    (scheme-mode                       all-the-icons-file-icons "scheme"           :face all-the-icons-red)
    (swift-mode                        all-the-icons-devopicons "swift"            :face all-the-icons-green)
    (svelte-mode                       all-the-icons-file-icons "svelte"           :face all-the-icons-red)
    (c-mode                            all-the-icons-mfixx "c"                     :face all-the-icons-blue)
    (c-ts-mode                         all-the-icons-mfixx "c"                     :face all-the-icons-blue)
    (c++-mode                          all-the-icons-mfixx "c++"                   :face all-the-icons-blue)
    (c++-ts-mode                       all-the-icons-mfixx "c++"                   :face all-the-icons-blue)
    (csharp-mode                       all-the-icons-mfixx "csharp"                :face all-the-icons-dblue)
    (csharp-ts-mode                    all-the-icons-mfixx "csharp"                :face all-the-icons-dblue)
    (clojure-mode                      all-the-icons-devopicons "clojure"          :face all-the-icons-blue)
    (cider-repl-mode                   all-the-icons-devopicons "clojure"          :face all-the-icons-green)
    (clojurescript-mode                all-the-icons-file-icons "clojurejs"        :face all-the-icons-dblue)
    (coffee-mode                       all-the-icons-devopicons "coffeescript"     :face all-the-icons-maroon)
    (lisp-mode                         all-the-icons-file-icons "lisp"             :face all-the-icons-orange)
    (css-mode                          all-the-icons-devopicons "css3"             :face all-the-icons-yellow)
    (css-ts-mode                       all-the-icons-devopicons "css3"             :face all-the-icons-yellow)
    (scss-mode                         all-the-icons-devopicons "sass"             :face all-the-icons-pink)
    (sass-mode                         all-the-icons-devopicons "sass"             :face all-the-icons-dpink)
    (less-css-mode                     all-the-icons-devopicons "less"             :face all-the-icons-dyellow)
    (stylus-mode                       all-the-icons-file-icons "stylus"           :face all-the-icons-lgreen)
    (csv-mode                          all-the-icons-octicons "graph"              :face all-the-icons-dblue)
    (haskell-mode                      all-the-icons-devopicons "haskell"          :face all-the-icons-red)
    (haskell-c2hs-mode                 all-the-icons-devopicons "haskell"          :face all-the-icons-red)
    (literate-haskell-mode             all-the-icons-devopicons "haskell"          :face all-the-icons-red)
    (haml-mode                         all-the-icons-file-icons "haml"             :face all-the-icons-lyellow)
    (html-mode                         all-the-icons-devopicons "html5"            :face all-the-icons-orange)
    (html-ts-mode                      all-the-icons-devopicons "html5"            :face all-the-icons-orange)
    (rhtml-mode                        all-the-icons-devopicons "html5"            :face all-the-icons-lred)
    (mustache-mode                     all-the-icons-file-icons "moustache"        :face all-the-icons-green)
    (slim-mode                         all-the-icons-octicons "meter"              :face all-the-icons-yellow)
    (jade-mode                         all-the-icons-file-icons "jade"             :face all-the-icons-red)
    (pug-mode                          all-the-icons-file-icons "pug"              :face all-the-icons-red)
    (image-mode                        all-the-icons-octicons "file-media"         :face all-the-icons-blue)
    (texinfo-mode                      all-the-icons-file-icons "gnu"              :face all-the-icons-lred)
    (markdown-mode                     all-the-icons-octicons "markdown"           :face all-the-icons-lblue)
    (rst-mode                          all-the-icons-file-icons "restructuredtext" :face all-the-icons-lblue)
    (bibtex-mode                       all-the-icons-file-icons "bibtex"           :face all-the-icons-maroon)
    (org-mode                          all-the-icons-file-icons "org"              :face all-the-icons-lgreen)
    (compilation-mode                  all-the-icons-vscode-codicons "combine"     )
    (objc-mode                         all-the-icons-mfixx "objc"                  )
    (tuareg-mode                       all-the-icons-file-icons "ocaml"            )
    (purescript-mode                   all-the-icons-file-icons "purescript"       )
    (verilog-mode                      all-the-icons-file-icons "verilog"          :face all-the-icons-red)
    (vhdl-mode                         all-the-icons-file-icons "vhdl"             :face all-the-icons-blue)
    (haskell-cabal-mode                all-the-icons-file-icons "cabal"            :face all-the-icons-lblue)
    (kotlin-mode                       all-the-icons-file-icons "kotlin"           :face all-the-icons-orange)
    (kotlin-ts-mode                    all-the-icons-file-icons "kotlin"           :face all-the-icons-orange)
    (nim-mode                          all-the-icons-file-icons "nimrod"           :face all-the-icons-yellow)
    (sql-mode                          all-the-icons-octicons "database"           :face all-the-icons-silver)
    (lua-mode                          all-the-icons-file-icons "lua"              :face all-the-icons-dblue)
    (adoc-mode                         all-the-icons-file-icons "asciidoc"         :face all-the-icons-lblue)
    (puppet-mode                       all-the-icons-file-icons "puppet"           :face all-the-icons-yellow)
    (jinja2-mode                       all-the-icons-file-icons "jinja"            :face all-the-icons-silver)
    (powershell-mode                   all-the-icons-file-icons "powershell"       :face all-the-icons-blue)
    (tex-mode                          all-the-icons-file-icons "latex"            :face all-the-icons-lred)
    (latex-mode                        all-the-icons-file-icons "latex"            :face all-the-icons-lred)
    (dart-mode                         all-the-icons-devopicons "dart"             :face all-the-icons-blue)
    (fsharp-mode                       all-the-icons-devopicons "fsharp"           :face all-the-icons-blue)
    (asm-mode                          all-the-icons-file-icons "assembly-generic" :face all-the-icons-blue)
    (nasm-mode                         all-the-icons-file-icons "nasm"             :face all-the-icons-blue)
    (tcl-mode                          all-the-icons-file-icons "tcl"              :face all-the-icons-dred)
    (cuda-mode                         all-the-icons-file-icons "nvidia"           :face all-the-icons-green)
    (f90-mode                          all-the-icons-file-icons "fortran"          :face all-the-icons-purple)
    (hy-mode                           all-the-icons-file-icons "hy"               :face all-the-icons-blue)
    (glsl-mode                         all-the-icons-file-icons "vertexshader"     :face all-the-icons-green)
    (zig-mode                          all-the-icons-file-icons "zig"              :face all-the-icons-orange)
    (odin-mode                         all-the-icons-file-icons "odin"             :face all-the-icons-lblue)
    (pdf-view-mode                     all-the-icons-vscode-codicons "file-pdf"    :face all-the-icons-dred)
    (spacemacs-buffer-mode             all-the-icons-file-icons "elisp"            :face all-the-icons-purple)
    (elfeed-search-mode                all-the-icons-fontawesome-4 "rss-square"    :face all-the-icons-orange)
    (elfeed-show-mode                  all-the-icons-octicons "rss"                :face all-the-icons-orange)
    (emms-browser-mode                 all-the-icons-fontawesome-4 "music"         :face all-the-icons-silver)
    (emms-lyrics-mode                  all-the-icons-fontawesome-4 "music"         :face all-the-icons-silver)
    (emms-show-all-mode                all-the-icons-fontawesome-4 "music"         :face all-the-icons-silver)
    (emms-metaplaylist-mode            all-the-icons-fontawesome-4 "music"         :face all-the-icons-silver)
    (emms-tag-editor-mode              all-the-icons-fontawesome-4 "music"         :face all-the-icons-silver)
    (emms-playlist-mode                all-the-icons-fontawesome-4 "music"         :face all-the-icons-silver)
    (lilypond-mode                     all-the-icons-fontawesome-4 "music"         :face all-the-icons-green)
    (magik-session-mode                all-the-icons-octicons "terminal"           :face all-the-icons-blue)
    (magik-cb-mode                     all-the-icons-octicons "book"               :face all-the-icons-blue)
    (meson-mode                        all-the-icons-file-icons "meson"            :face all-the-icons-purple)
    (man-common                        all-the-icons-file-icons "manpage"          :face all-the-icons-blue)
    (terraform-mode                    all-the-icons-file-icons "terraform"        :face all-the-icons-purple-alt)))

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
        (chevron (if chevron (all-the-icons-octicons (format "chevron-%s" chevron)) ""))
        (padding (or padding "\t")))
    (format "%s%s%s%s%s" padding chevron padding icon padding)))

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
      (apply 'all-the-icons-file-icons (append '("jsx-atom") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "javascript"))
      (apply 'all-the-icons-mfixx (append '("javascript") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "json"))
      (apply 'all-the-icons-vscode-codicons (append '("json") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "xml"))
      (apply 'all-the-icons-octicons (append '("file-code") non-nil-args)))
     ((and (boundp 'web-mode-content-type) (equal web-mode-content-type "css"))
      (apply 'all-the-icons-devopicons (append '("css3") non-nil-args)))
     (t
      (apply 'all-the-icons-devopicons (append '("html5") non-nil-args))))))

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
         (t (apply (car icon) args)))))))

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
    (apply (car icon) args)))

;;;###autoload
(defun all-the-icons-icon-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining properties like in
the normal icon inserting functions."
  (let* ((icon (cdr (or (assoc mode all-the-icons-mode-icon-alist)
                        (assoc (get mode 'derived-mode-parent) all-the-icons-mode-icon-alist))))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if icon (apply (car icon) args) mode)))

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
(all-the-icons-cache #'all-the-icons-icon-for-file)
(all-the-icons-cache #'all-the-icons-icon-for-mode)

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

(eval-when-compile
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

(defun all-the-icons-insert (&optional arg icon-set)
  "Interactive icon insertion function.

When Prefix ARG is non-nil, print the icon, else insert it.
When ICON-SET is non-nil, limit the candidates to the icon set matching it.
When STYLE is non-nil, limit the candidates to the style matching the icon set."
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
                           ((eq icon-set 'clockface)
                            (flatten-list
                             (cl-remove
                              nil
                              (mapcar
                               (lambda (style) (all-the-icons--read-candidates-for-icon-set icon-set nil style))
                               '(nil fathands fatrect fatrectsolid fatsolid fatsquare fatsquaresolid rect rectsolid solid square squaresolid)))))
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
               ((eq icon-set 'clockface)
                (dolist (style '(nil fathands fatrect fatrectsolid fatsolid fatsquare fatsquaresolid rect rectsolid solid square squaresolid))
                  (when-let ((icon (funcall insert-f icon-name :style style :raise-error nil)))
                    (insert (format "%s - %s%s\n" icon icon-name (or (and style (format "-%s" style)) ""))))))
               (t (insert (format "%s - %s\n" (funcall insert-f icon-name) icon-name)))))
       (when duration (sit-for duration)))
     data)))

(defun all-the-icons-insert-icons-for-extensions ()
  "Insert all of the icons for `all-the-icons-extension-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-extension-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n" (apply (cadr entry) (cddr entry)) (car entry)))))

(defun all-the-icons-insert-icons-for-modes ()
  "Insert all of the icons for `all-the-icons-mode-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-mode-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n" (apply (cadr entry) (cddr entry)) (car entry)))))

(defun all-the-icons-insert-icons-for-regexp ()
  "Insert all of the icons for `all-the-icons-regexp-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-regexp-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n" (apply (cadr entry) (cddr entry)) (car entry)))))

(defun all-the-icons-insert-icons-for-dirs ()
  "Insert all of the icons for `all-the-icons-dir-icon-alist'."
  (interactive)
  (dolist (entry (cl-sort (copy-sequence all-the-icons-dir-icon-alist) 'string< :key 'car))
    (insert (format "%s - %s\n" (apply (cadr entry) (cddr entry)) (car entry)))))

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

Some icon set icons do not all have the same width. When
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
       (let* ((file-name (cdr (assoc icon-name ,alist))) ;; remap icons
              (size (window-default-font-height))
              (lib-dir (concat (file-name-directory (locate-library "all-the-icons")) ,(format "svg/%s/" name)))
              (image-path (concat lib-dir ,(or (and svg-path-finder
                                                    `(apply ,svg-path-finder file-name lib-dir size args))
                                               '(format "%s.svg" file-name))))
              (face (when all-the-icons-color-icons (plist-get args :face)))
              (raise-error (plist-get args :raise-error)))
         (if (and file-name (file-exists-p image-path))
             (let* ((icon (all-the-icons--normalize-svg-doc
                           (funcall ,svg-doc-processor
                                    (all-the-icons--load-svg image-path)))))
               (setf (image-property icon :max-width) (- size (* ,padding 2)))
               (setf (image-property icon :max-height) (- size (* ,padding 2)))
               (setf (image-property icon :ascent) 'center)
               (setf (image-property icon :margin) ,padding)
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

(defun all-the-icons--clockface-path (name _ _ &rest args)
  "SVG path finder function for Clockface.

See `all-the-icons-define-icon' for the meaning of NAME and ARGS."
  (let* ((style (or (plist-get args :style) "")))
    (format "clockface%s/clock_%s.svg" style name)))

(all-the-icons-define-icon devopicons all-the-icons-data/devopicons-alist
                           :padding 1)

(all-the-icons-define-icon file-icons all-the-icons-data/file-icons-alist
                           :padding 1)

(all-the-icons-define-icon mfixx all-the-icons-data/mfixx-alist
                           :padding 1)

(all-the-icons-define-icon octicons all-the-icons-data/octicons-alist
                           :svg-path-finder 'all-the-icons--octicons-path
                           :padding 1)

(all-the-icons-define-icon weather-icons all-the-icons-data/weather-icons-alist)

(all-the-icons-define-icon vscode-codicons all-the-icons-data/vscode-codicons-alist
                           :padding 1)

(all-the-icons-define-icon fontawesome-4 all-the-icons-data/fontawesome-4-alist
                           :padding 1)

(all-the-icons-define-icon fluentui-system-icons all-the-icons-data/fluentui-system-icons-alist
                           :svg-path-finder 'all-the-icons--fluentui-system-icons-path)

(all-the-icons-define-icon material-icons all-the-icons-data/material-icons-alist
                           :svg-path-finder 'all-the-icons--material-icons-path)

(all-the-icons-define-icon clockface all-the-icons-data/clockface-alist
                           :svg-path-finder 'all-the-icons--clockface-path)

(provide 'all-the-icons)

;;; all-the-icons.el ends here
