#!/usr/bin/env node

import fs from "node:fs/promises";
import path from "node:path";
import less from "less";

if (!process.argv[2]) {
  console.error("Weather Icons less-css directory required.");
  process.exit(1);
}

const LESS_CSS_DIR = path.normalize(process.argv[2]);
const MAPPINGS_DIR = path.join(LESS_CSS_DIR, "mappings");
const CLASSES_DIR = path.join(LESS_CSS_DIR, "icon-classes");

async function printAPIMapppings() {
  const dir = await fs.opendir(MAPPINGS_DIR);
  for await (const dirent of dir) {
    const fileContent = (
      await fs.readFile(path.join(dir.path, dirent.name))
    ).toString();

    const tree = await less.parse(fileContent, { filename: dirent.name });
    for (const rule of tree.rules) {
      // comments are parsed as a rule...
      if (rule.selectors) {
        // e.g. .@{wi-css-prefix}-wu-chanceflurries:before -> wu-chanceflurries
        for (const selector of rule.selectors) {
          const mapFrom = selector.elements[2].value.slice(1);
          // e.g thermometer
          const mapTo =
            "wi-" + rule.rules[0].value.value[0].value[0].name.slice(1);
          console.log(mapFrom, mapTo);
        }
      }
    }
  }
}

async function printMoonAliases() {
  const fileName = "classes-moon-aliases.less";
  const filePath = path.join(CLASSES_DIR, fileName);
  const fileContent = (await fs.readFile(filePath)).toString();

  const tree = await less.parse(fileContent, { filename: fileName });
  for (const rule of tree.rules) {
    // comments are parsed as a rule...
    if (rule.selectors) {
      // e.g. .wi-moon-0:before -> moon-0
      for (const selector of rule.selectors) {
        const mapFrom = selector.elements[0].value.slice(4);
        // e.g moon-new
        const mapTo =
          "wi-" + rule.rules[0].value.value[0].value[0].name.slice(1);
        console.log(mapFrom, mapTo);
      }
    }
  }
}

await printAPIMapppings();
await printMoonAliases();
