## Source

https://github.com/erikflowers/weather-icons/

### Vendoring Process

1. `git clone https://github.com/erikflowers/weather-icons.git`
2. `mkdir -p /path/to/all-the-icons.el/svg/weather-icons`
3. `cp weather-icons/svg/* /path/to/all-the-icons.el/svg/weather-icons`
4. `find /path/to/all-the-icons.el/svg/weather-icons -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
5. `cd /path/to/all-the-icons.el`
6. `pushd ./svg/weather-icons`
7. `mv wi-wind-deg.svg wi-wind-direction.svg`
8. `mv wi-moon-waxing-6 moon-waxing-crescent-6.svg`
9. `ls -1 *.svg | cut -d '.' -f 1 | sed -E 's/wi-//g' | sort | uniq | awk '{ print "(\"" $1 "\" . \"wi-" $1 "\")"; }'>> ../../all-the-icons-data-weather-icons.el`
10. `popd`
11. Every icon is uniquely identified by `wi-[name].svg`
12. `/path/to/all-the-icons.el/scripts/extract-weather-icons-api-data.mjs weather-icons/less | sed -E 's/([a-z0-9-]+) ([a-z0-9-]+)/\(\"\1\" \. \"\2\"\)/' >> /path/to/all-the-icons.el/all-the-icons-data-weather-icons.el`
