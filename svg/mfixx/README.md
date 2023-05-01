## Source

https://github.com/file-icons/MFixx

### Vendoring Process

1. `git clone https://github.com/file-icons/MFixx.git`
2. `cd MFixx`
3. `mkdir -p /path/to/all-the-icons.el/svg/mfixx`
4. `cp svg/*.svg /path/to/all-the-icons.el/svg/mfixx`
5. `find /path/to/all-the-icons.el/svg/mfixx -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
6. Every icon is uniquely identified by `[lowercase-name].svg`
