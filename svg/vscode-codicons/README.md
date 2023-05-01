## Source

https://github.com/microsoft/vscode-codicons

### Vendoring Process

1. `git clone https://github.com/microsoft/vscode-codicons.git`
2. `mkdir -p /path/to/all-the-icons.el/svg/vscode-codicons`
3. `cp vscode-codicons/src/icons/* /path/to/all-the-icons.el/svg/vscode-codicons`
4. `ls -1 *.svg | cut -d '.' -f 1 | sort | uniq | awk '{ print "(\"" $1 "\" . \"" $1 "\")"; }' >> ../../data/data-vscode-codicons.el`
5. Every icon is uniquely identified by `[name].svg`
