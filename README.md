![img](logo.png)

## Migrating from 5.x to 6.x

Starting from 6.0.0, `all-the-icons` will no longer provide any icon fonts, or
require installation of any icon fonts into the operating system. Instead, it
will come bundled with over 18000 icons from 9 different popular SVG icon sets
and totaling over 31000 variations.

The behavior of documented public APIs are preserved as much as possible, but
text properties other than `:face` will be ignored because each icon will
automatically scale itself to respect the default font size.

### Breaking Changes

- `all-the-icons-data/**` variables are renamed to `all-the-icons-data-**`
- Data files are moved to the top level directory to unify the different ways
  Emacs package managers unpack packages.
- `all-the-icons-icon-for-url` is removed.
- Icon sets are updated to their latest versions, and many are remapped. Please
  see the alist variables for details.
- If you modify any of the internal alist variables, you will probably need to
  update the icon set names and icon names.

# Introduction

`all-the-icons` is a package that provides a convenient API for users and
package authors to insert SVG icons into buffers and mode lines.

# System Requirements

`all-the-icons` does not work when Emacs is running inside a terminal, so you
will need to use a GUI Emacs in order to render the icons properly.

In addition, you will need to use a Emacs installation that is compiled with
`RSVG` support. You can check if this is the case by checking if `RSVG` is
included in the list of the `system-configuration-features` variable.

Please check the manual of your OS' package manager to find out how to install
Emacs with RSVG support, or [compile your
own](https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html).

# Installation

`all-the-icons` 6.0 currently is in beta, and installation currently requires
one of the following ways:

## straight

```elisp
(use-package all-the-icons
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el" :branch "svg" :files (:defaults "svg"))
  :if (display-graphic-p))
```

## use-package + quelpa

```elisp
(use-package all-the-icons
  :quelpa (all-the-icons :fetcher github :repo "domtronn/all-the-icons.el" :branch "svg" :files (:defaults "svg"))
  :if (display-graphic-p))
```

## Emacs 29+ package-vc

```elisp
(package-vc-install "https://github.com/domtronn/all-the-icons.el.git" "svg")
```

# Usage

The simplest usage for this package is to use the following functions;

-   `all-the-icons-icon-for-buffer`
-   `all-the-icons-icon-for-dir`
-   `all-the-icons-icon-for-file`
-   `all-the-icons-icon-for-mode`
-   `all-the-icons-icon-for-weather`
-   `all-the-icons-icon-for-dir-with-chevron`

Which can be used to get a formatted icon which you can insert into
buffers. Try the following in the `*scratch*` buffer:

```elisp
(insert (all-the-icons-icon-for-file "foo.js")) ;; Inserts a javascript icon
```

## Inserting Icons Directly

The above is fine if you want this package to automatically decide on
the icon you want for files and things, however, to insert the icons
directly you will want to use these icons functions;

-   `all-the-icons-file-icons`
-   `all-the-icons-devopicons`
-   `all-the-icons-mfixx`
-   `all-the-icons-fontawesome-4`
-   `all-the-icons-octicons`
-   `all-the-icons-weather-icons`
-   `all-the-icons-fluentui-system-icons`
-   `all-the-icons-vscode-codicons`
-   `all-the-icons-material-icons`

You can then call these functions with the icon you want to insert,
*e.g.*

```elisp
(all-the-icons-octicons "file-binary")  ;; GitHub Octicon for Binary File
(all-the-icons-fontawesome-4 "cogs")    ;; FontAwesome icon for cogs
(all-the-icons-weather-icons "tornado") ;; Weather Icon for tornado
```

A list of all the icon names for an icon set can be found by inspecting the
`all-the-icons-data-[icon-set].el` files or the alist variables.

The alist variables are all prefixed with

-   `all-the-icons-data-`

For example `C-h v all-the-icons-data <TAB>` will give a list of all the data
alist you can describe *(and the icon sets they're associated with)*

## Inserting icons with properties

Each of the above icon functions can also be given different the `face` property
to slightly adjust the way they're displayed.

-   `:face` - The face to apply to the icon, defaults to `'default`

So you would call, for example

```elisp
(all-the-icons-icon-for-mode 'python-mode :face 'all-the-icons-blue)
```

# Resource Icon Sets

| Icon Set | License |
| --- | --- |
| [Atom File Icons](https://github.com/file-icons/icons) | [LICENSE](https://github.com/file-icons/icons/blob/master/LICENSE.md) |
| [Atom DevOpicons](https://github.com/file-icons/DevOpicons) | [LICENSE](https://github.com/vorillaz/devicons#meet--devicons) |
| [Atom MFixx](https://github.com/file-icons/MFixx) | [LICENSE](https://github.com/fizzed/font-mfizz/#license) |
| [FontAwesome 4](https://fontawesome.com/v4/icons/) | [LICENSE](https://fontawesome.com/v4/license/) |
| [GitHub OctIcons](https://github.com/primer/octicons) | [LICENSE](https://github.com/primer/octicons/blob/main/LICENSE) |
| [Weather Icons](https://erikflowers.github.io/weather-icons/) | [LICENSE](https://github.com/erikflowers/weather-icons#licensing) |
| [Material Icons](https://github.com/google/material-design-icons) | [LICENSE](https://github.com/google/material-design-icons/blob/master/LICENSE) |
| [VSCode Codicons](https://github.com/microsoft/vscode-codicons) | [LICENSE](https://github.com/microsoft/vscode-codicons/blob/main/LICENSE-CODE) |
| [VSCode Codicons](https://github.com/microsoft/fluentui-system-icons) | [LICENSE](https://github.com/microsoft/fluentui-system-icons/blob/main/LICENSE) |

I would like to thank all the authors for the creation and use of these
fantastic fonts.

# Troubleshooting

If you experience a slow down in performance when rendering many icons, you can
try adjusting the following variable:

```elisp
(setq image-cache-eviction-delay nil) ;; or set it to a much longer time than 5 minutes
```

[▲ back to top](#readme)
