# Package

version       = "0.9.0"
author        = "Wei Xin Yuan"
description   = "Parser generator macro library"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.4.0"
requires "patty >= 0.3.3"
requires "fusion >= 1.2"
requires "https://github.com/yuanweixin/dotted"
requires "https://github.com/yuanweixin/lexim"

import ospaths
task gendoc, "generate docs":
  exec "nim doc --project --outdir: htmldocs src/nimyacc.nim"

