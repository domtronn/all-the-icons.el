#!/usr/bin/env node

import fs from "fs";
import coffee from "coffeescript";
import less from "less";
import cssom from "cssom";
import { RegExpParser } from "regexpp";

const CONFIG_CSON = "./config.cson";
const ICONS_LESS = "./icons.less";

const config = coffee.eval(
  fs.readFileSync(new URL(CONFIG_CSON, import.meta.url)).toString()
);

const css = (
  await less.render(
    fs.readFileSync(new URL(ICONS_LESS, import.meta.url)).toString()
  )
).css;

const stylesheet = cssom.parse(css);

function extractIconData(entries) {
  const iconData = {};
  for (const { icon, match, colour, ...rest } of entries) {
    if (!iconData[icon])
      iconData[icon] = {
        extension: {},
        regexp: {},
      };

    let matches = [];
    if (Array.isArray(match)) {
      matches = match.map(([match, colour, ...leftover]) => [
        match,
        colour,
        { ...rest, ...leftover },
      ]);
    } else {
      matches.push([match, colour, rest]);
    }

    for (const [pat, colour, { matchPath, noSuffix }] of matches) {
      if (typeof pat === "string") {
        const ext = pat.replace(/^./, "");
        iconData[icon].extension[ext] = {
          colour,
          noSuffix: !!noSuffix,
        };
      } else {
        const regexp = pat.toString();
        iconData[icon].regexp[regexp] = {
          colour,
          matchPath: !!matchPath,
          noSuffix: !!noSuffix,
        };
      }
    }
  }
  return iconData;
}

const directoryIconData = extractIconData(Object.values(config.directoryIcons));
const fileIconData = extractIconData(Object.values(config.fileIcons));

const iconNameFamilyMap = stylesheet.cssRules
  .filter(
    (_) =>
      (_.selectorText.startsWith(".icon") ||
        _.selectorText.endsWith("icon:before")) &&
      _.style["font-family"]
  )
  .map((_) => ({
    name: _.selectorText.match(/\.(.+)-icon:before/)[1],
    family: _.style["font-family"].replace(/"/g, "").toLowerCase(),
  }))
  .reduce((prev, curr) => ({ ...prev, [curr.name]: curr.family }), {});

const familyMap = {
  "file-icons": "file-icons",
  "octicons regular": "octicons",
  fontawesome: "fontawesome-4",
  mfizz: "mfixx",
  devicons: "devopicons",
};

function colourToFace(colour) {
  const prefixMap = {
    medium: "",
    light: "l",
    dark: "d",
    auto: "auto-",
  };

  if (typeof colour === "string") {
    const [prefix, color] = colour.split("-");
    return ` :face all-the-icons-${prefixMap[prefix]}${color}`;
  } else if (Array.isArray(colour)) {
    const [lprefix, lcolor] = colour[0].split("-");
    const [rprefix, rcolor] = colour[1].split("-");
    return ` :face ((((background light)) :inherit all-the-icons-${prefixMap[lprefix]}${lcolor}) (((background dark)) :inherit all-the-icons-${prefixMap[rprefix]}${rcolor}))`;
  }

  return "";
}

const allTheIconsExtensionIconAlist = [];
const allTheIconsRegexpIconAlist = [];
for (const [iconName, { extension, regexp }] of Object.entries(fileIconData)) {
  for (const [ext, { colour, noSuffix }] of Object.entries(extension)) {
    let family = null;
    if (noSuffix) family = "octicons";
    else family = familyMap[iconNameFamilyMap[iconName]];

    if (family) {
      allTheIconsExtensionIconAlist.push(
        `("${ext}" all-the-icons-${family} "${iconName}"${colourToFace(
          colour
        )})`
      );
    } else {
      console.error("%s not styled", iconName);
    }
  }

  for (const [re, { colour, noSuffix }] of Object.entries(regexp)) {
    let family = null;
    if (noSuffix) family = "octicons";
    else family = familyMap[iconNameFamilyMap[iconName]];

    if (family) {
      allTheIconsRegexpIconAlist.push(
        `("${re}" all-the-icons-${family} "${iconName}"${colourToFace(colour)})`
      );
    } else {
      console.error("%s not styled", iconName);
    }
  }
}

allTheIconsExtensionIconAlist.sort();
allTheIconsRegexpIconAlist.sort();

console.log(JSON.stringify(allTheIconsRegexpIconAlist, null, 2));
console.log(JSON.stringify(allTheIconsExtensionIconAlist, null, 2));
