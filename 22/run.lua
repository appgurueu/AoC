#!/usr/bin/lua5.4

local day, part = ...
print(assert(loadfile(("src/%d/%d.lua"):format(day, part)))(io.lines(("input/%d.txt"):format(day))))
