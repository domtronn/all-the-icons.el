## Source

https://github.com/primer/octicons/tree/v18.3.0

### Vendoring Process

1. `git clone https://github.com/primer/octicons.git`
2. `mkdir -p /path/to/all-the-icons.el/svg/octicons`
3. `cp octicons/icons/* /path/to/all-the-icons.el/svg/octicons`
4. `ls -1 *.svg | cut -d '.' -f 1 | sed -E 's/-[0-9]+//g' | sort | uniq | awk '{ print "(\"" $1 "\" . \"" $1 "\")"; }' >> ../../data/data-octicons.el `
5. Every icon is uniquely identified by `[name]-[size].svg`
