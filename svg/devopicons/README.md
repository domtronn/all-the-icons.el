## Source

https://github.com/file-icons/DevOpicons

### Vendoring Process

1. `git clone https://github.com/file-icons/DevOpicons.git`
2. `cd icons`
3. `mkdir -p /path/to/all-the-icons.el/svg/devopicons`
4. `cp svg/* /path/to/all-the-icons.el/svg/devopicons`
5. `find /path/to/all-the-icons.el/svg/devopicons -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
6. Every icon is uniquely identified by `[lowercase-name].svg`
