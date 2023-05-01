## Source

https://github.com/ocodo/ClockFace-font

### Vendoring Process

1. `git clone https://github.com/ocodo/ClockFace-font.git`
2. `cd ClockFace-font`
3. `rm -rf !(ClockFace*-glyphs)`
4. `for dir in $(ls .); do mv $dir $(echo $dir | tr '[:upper:]' '[:lower:]' | cut -d '-' -f 1); done`
5. `cd clockfacefathands`
6. `for f in $(ls -1); do mv $f ${f/_\./\.}; done`
7. `mkdir -p /path/to/all-the-icons.el/svg/clockface`
8. `cp -R * /path/to/all-the-icons.el/svg/clockface`
9. `find /path/to/all-the-icons.el/svg/clockface -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
10. Every icon is uniquely identified by `clockface[style]/clock_[name].svg`
