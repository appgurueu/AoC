#!/usr/bin/lua5.4

local answers = {
	{71506, 209603},
	{12855, 13726},
	{7990, 2602},
	{503, 827},
}

for day = 1, 4 do
	for part = 1, 2 do
		local got = assert(loadfile(("src/%d/%d.lua"):format(day, part)))(io.lines(("input/%d.txt"):format(day)))
		local expected = answers[day][part]
		assert(got == expected)
	end
end
