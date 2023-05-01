## Source

https://github.com/google/material-design-icons

### Vendoring Process

1. `git clone --depth=1 https://github.com/google/material-design-icons.git`
2. `mkdir -p /path/to/all-the-icons.el/svg/material-icons`
3. `for dir in $(ls -1 ./material-design-icons/src/); do find $dir -depth 1 -type d -exec cp -R {} /path/to/all-the-icons.el/svg/material-icons/ \; ; done`
4. `cd /path/to/all-the-icons.el/svg/material-icons`
5. `\ls -1 | awk '{ print "(\"" $1 "\" . \"" $1 "\")"; }' >> ../../data/data-material-icons.el`
6. `find /path/to/all-the-icons.el/svg/material-icons -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
7. Every icon is uniquely identified by `[name]/materialicons[style]/[size]px.svg`
