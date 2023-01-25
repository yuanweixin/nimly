# Package

version       = "1.0.1"
author        = "Wei Xin Yuan"
description   = "Parser generator macro library"
license       = "MIT"
srcDir        = "src"

installExt    = @["nim"]

bin = @["nimyacc/yexe"] 

requires "nim >= 1.6.8"

import ospaths

proc buildHelper(name: string) =
  if not fileExists(name.toExe):
    exec "nim c -d:release --mm:orc " & name

task make, "builds yexe":
  buildHelper "yexe"

# Dependencies

requires "nim >= 1.4.0"
requires "patty >= 0.3.3"
requires "fusion >= 1.2"
requires "https://github.com/yuanweixin/dotted >= 1.0.0"
requires "https://github.com/yuanweixin/lexim >= 1.0.0"

import ospaths
task gendoc, "generate docs":
  exec "nim doc --project --outdir: htmldocs src/nimyacc.nim"

