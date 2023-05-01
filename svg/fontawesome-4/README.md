## Source

https://github.com/FortAwesome/Font-Awesome/blob/4.x/src/assets/font-awesome/fonts/fontawesome-webfont.svg

### Vendoring Process

1. `curl -O https://raw.githubusercontent.com/FortAwesome/Font-Awesome/4.x/src/assets/font-awesome/fonts/fontawesome-webfont.svg`
2. `mkdir -p /path/to/all-the-icons.el/svg/fontawesome-4`
3. `/path/to/all-the-icons.el/script/extract-font-awesome-svg.py ./fontawesome-webfont.svg /path/to/all-the-icons.el/svg/fontawesome-4`
4. `find /path/to/all-the-icons.el/svg/fontawesome-4 -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
5. Every icon is uniquely identified by `[name].svg`
