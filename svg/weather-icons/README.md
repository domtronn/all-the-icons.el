## Source

https://github.com/erikflowers/weather-icons/

### Vendoring Process

1. `git clone https://github.com/erikflowers/weather-icons.git`
2. `mkdir -p /path/to/all-the-icons.el/svg/weather-icons`
3. `cp weather-icons/svg/* /path/to/all-the-icons.el/svg/weather-icons`
4. `find /path/to/all-the-icons.el/svg/weather-icons -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
5. Every icon is uniquely identified by `wi-[name].svg`
6. `/path/to/all-the-icons.el/scripts/extract-weather-icons-api-data.mjs weather-icons/less/mappings | sed -E 's/([a-z0-9-]+) ([a-z0-9-]+)/\(\"\1\" \. \"\2\"\)/' >> //path/to/all-the-icons.el/all-the-icons-data-weather-icons.el`
