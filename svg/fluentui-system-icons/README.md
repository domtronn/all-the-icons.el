## Source

https://github.com/microsoft/fluentui-system-icons

### Vendoring Process

1. `git clone https://github.com/microsoft/fluentui-system-icons.git`
2. `mkdir -p /path/to/all-the-icons.el/svg/fluentui-system-icons`
3. `cd fluentui-system-icons`
4. `find ./assets -type f -name "*.svg" -exec \cp {} /path/to/all-the-icons.el/svg/fluentui-system-icons/ \;`
5. `find /path/to/all-the-icons.el/svg/fluentui-system-icons -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
6. `pushd /path/to/all-the-icons.el/svg/fluentui-system-icons`
7. `ls -1 *.svg | cut -d _ -f 3- | cut -d '.' -f 1 | sed -E 's/_[0-9]+_[a-z]+//g' | sort | uniq | awk '{ print "(\"" $1 "\" . \"" $1 "\")"; }' >> ../../data/data-fluentui-system-icons.el`
8. Every icon is uniquely identified by `ic_fluent_[name]_[size]_[style].svg`
