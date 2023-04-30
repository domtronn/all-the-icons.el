## Source

https://github.com/file-icons/icons

### Vendoring Process

1. `git clone https://github.com/file-icons/icons.git`
2. `cd icons`
3. `mkdir -p /path/to/all-the-icons.el/svg/file-icons`
4. `cp svg/* /path/to/all-the-icons.el/svg/file-icons`
5. `find /path/to/all-the-icons.el/svg/file-icons -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
6. Every icon is uniquely identified by `[lowercase-name].svg`
