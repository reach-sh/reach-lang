#!/usr/bin/env node

import * as fs from 'fs/promises';
import * as process from 'process';
import * as cp from "child_process";

// Probably I should use an arg parsing package, but I don't want to bother dealing with dependencies right now...
const argv = process.argv.slice(2);
let argvI = 0;
let examplesdirStr = undefined;
let oneDirStr = undefined;
let style = "json";
let compile = false;

const exitWithHelp = () => {
  console.log(`Usage: ${process.argv[1]} <options>`)
  console.log("Options:")
  console.log("  --dir <some-dir>")
  console.log("  --examples <examples-dir>")
  console.log("  --style <json|lines> (default json)")
  console.log("  --compile <t|f> (default f)")
  console.log("Note that either --examples or --dir is required.")
  process.exit(1);
}

while(argvI < argv.length) {
  if (argv[argvI] === "--help") {
    exitWithHelp();
  } else if (argv[argvI] === "--examples") {
    examplesdirStr = argv[argvI + 1];
    argvI += 2;
  } else if (argv[argvI] === "--dir") {
    oneDirStr = argv[argvI + 1];
    argvI += 2;
  } else if (argv[argvI] === "--style") {
    style = argv[argvI + 1];
    if (style !== "json" && style !== "lines") {
      console.log(`Bad style: ${style}`);
    }
    argvI += 2;
  } else if (argv[argvI] === "--compile") {
    compile = argv[argvI + 1];
    argvI += 2;
  } else {
    console.log(`Bad option at position ${argvI}: ${argv[argvI]}`)
    exitWithHelp();
  }
}

if (examplesdirStr === undefined && oneDirStr === undefined) {
  console.log("Error: no directory given.")
  exitWithHelp();
} else if (examplesdirStr !== undefined && oneDirStr !== undefined) {
  console.log("Error: both --dir and --examples given.")
  exitWithHelp();
}

const pathAbsolute = (p) => {
  if (p.substring(0,1) !== "/") {
    return process.cwd() + "/" + p;
  } else {
    return p;
  }
}

if (oneDirStr !== undefined) {
  oneDirStr = pathAbsolute(oneDirStr);
}
if (examplesdirStr !== undefined) {
  examplesdirStr = pathAbsolute(examplesdirStr);
}



const loadByteCodeSizes = async (modulePath) => {
  const mod = await import(modulePath);
  const con = mod._Connectors;
  const ETH = con.ETH ?? {Bytecode: "0x"};
  const ethCode = ETH.Bytecode;
  const ethSize = (ethCode.length - 2) / 2;
  const ALGO = con.ALGO ?? {appApproval: ""};
  const algoCode = ALGO.appApproval;
  const algoSize = Buffer.byteLength(algoCode, "base64");
  return [ethSize, algoSize];
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


const doOneDir = async (dirName) => {
  let ret = {};
  if (compile === "t") {
    try {
      const exDir = await fs.opendir(dirName);
      try {
        const exdirFiles = await dirToEntList(exDir);
        for await (const exfile of exdirFiles) {
          if (exfile.name.match(".*\\.rsh")) {
            try {
              console.error(`Compiling ${exStr}/${exfile.name}...`);
              const proc = await cp.spawnSync("reach", ["compile", exfile.name], {cwd: exStr});
            } catch (e) {}
          }
        }
      } catch (e) {
        await exDir.closeSync();
      }
    } catch (e) {}
  }
  const buildDirStr = dirName + "/build/"
  try {
    const bdir = await fs.opendir(buildDirStr);
    const bdirFiles = await dirToEntList(bdir);
    for await (const buildfile of bdirFiles) {
      if (buildfile.name.match(".*\\..*\\.mjs")){
        try {
          const f = buildDirStr + buildfile.name;
          const [ethSize, algoSize]= await loadByteCodeSizes(f);
          const ethAlgoRatio = (algoSize !== 0 && ethSize !== 0) ?
                ethSize * 1.0 / algoSize : 0;
          ret[buildfile.name] = {algo: algoSize, eth: ethSize, ethAlgoRatio: ethAlgoRatio};
          if (style === "lines") {
            console.log("ETH: " + dirName + " " + buildfile.name + ": " + ethSize);
            console.log("ALGO: " + dirName + " " + buildfile.name + ": " + algoSize);
            console.log("ratio: " + dirName + " " + buildfile.name + ": " + ethAlgoRatio);
          }
        } catch (e) {throw e}
      }
    }
    await bdir.closeSync();
  } catch (e) {}
  return ret;
}

let jsonRet = {};

if (examplesdirStr !== undefined) {
  const esdir = await fs.opendir(examplesdirStr);
  const esdirFiles = await dirToEntList(esdir);
  for (const example of esdirFiles) {
    jsonRet[example.name] = await doOneDir(examplesdirStr + "/" + example.name);
  }
  await esdir.closeSync()
}
if (oneDirStr !== undefined) {
  jsonRet = await doOneDir(oneDirStr);
}

if (style === "json") {
  console.log(JSON.stringify(jsonRet, null, 2));
}
