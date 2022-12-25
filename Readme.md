# Advent of Code solutions

## Folder structure

* `<year>`
	* `src`: Implementations
		* `<day>/1.lua`: Part 1 solution
		* `<day>/2.lua`: Part 2 solution
		* MIT-licensed
	* `input`
		* `<day>.txt`: Input file
		* Copyright 2015-2022 Advent of Code. All rights reserved.
	* `test.lua`: Regression tests
	* `run.lua`: Utility to run files
		* Usage: `./run.lua <day> <part>`
		* Requires ~~Lua 5.4~~ LuaJIT (for speed); most should also run with Lua 5.4 or newer
		* Files are chunks getting a line iterator in `...`
		  and `return`ing the solution
