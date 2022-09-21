#!/usr/bin/env node

import * as fs from 'fs/promises';
import * as process from 'process';

const exampleDirStr = process.argv[2];
const style = process.argv[3];
const styles = ["json", "lines"];
if (exampleDirStr === undefined
    || exampleDirStr === "--help"
    || style === undefined
    || ! styles.includes(style)
   ){
  // takes a path to the examples dir, and accepts nothing because I haven't bothered to learn how to properly parse argv for nodejs yet.
  console.log("Usage: <this-script-path> <example-dir-path> <style: json or lines>");
  process.exit(1);
}

const loadEthByteCode = async (modulePath) => {
  const mod = await import(modulePath);
  return mod._Connectors.ETH.Bytecode;
}

const dirToEntList = async (dir) => {
  let done = false;
  const entList = [];
  while (! done) {
    const ent = await dir.readSync();
    if (ent === null){
      done = true;
    } else {
      entList.push(ent);
    }
  }

  return entList;
}

const sizes = {};

const edir = await fs.opendir(exampleDirStr);
const edirFiles = await dirToEntList(edir);

for (const example of edirFiles) {
  sizes[example.name] = {};
  const buildDirStr = exampleDirStr + "/" + example.name + "/build/"
  try {
    const bdir = await fs.opendir(buildDirStr);
    const bdirFiles = await dirToEntList(bdir);
    for await (const buildfile of bdirFiles) {
      if (buildfile.name.match(".*\\..*.mjs")){
        try {
          const code = await loadEthByteCode(buildDirStr + buildfile.name);
          const bytelen = (code.length - 2) / 2;
          sizes[example.name][buildfile.name] = bytelen;
          if (style === "lines") {
            console.log(example.name + " " + buildfile.name + ": " + bytelen);
          }
        } catch (e) {}
      }
    }
    await bdir.closeSync()
  } catch (e) {}
}
await edir.closeSync()

if (style === "json") {
  console.log(JSON.stringify(sizes, null, 2));
}
