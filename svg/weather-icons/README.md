## Source

https://github.com/erikflowers/weather-icons/

### Vendoring Process

1. `git clone https://github.com/erikflowers/weather-icons.git`
2. `mkdir -p /path/to/all-the-icons.el/svg/weather-icons`
3. `cp weather-icons/svg/* /path/to/all-the-icons.el/svg/weather-icons`
4. `find /path/to/all-the-icons.el/svg/weather-icons -name '*.svg' | xargs npx svgo --config /path/to/all-the-icons.el/svgo.config.js`
5. Every icon is uniquely identified by `wi-[name].svg`
