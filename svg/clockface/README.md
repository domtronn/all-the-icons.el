## Source

https://github.com/ocodo/ClockFace-font

### Vendoring Process

1. `git clone https://github.com/ocodo/ClockFace-font.git`
2. `cd ClockFace-font`
3. `rm -rf !(ClockFace*-glyphs)`
4. `for dir in $(ls .); do mv $dir $(echo $dir | tr '[:upper:]' '[:lower:]' | cut -d '-' -f 1); done`
5. `mkdir -p /path/to/all-the-icons.el/svg/clockface`
6. `cp -R * /path/to/all-the-icons.el/svg/clockface`
7. `find /path/to/all-the-icons.el/svg/clockface -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
8. Every icon is uniquely identified by `clockface[style]/clock_[name].svg`
