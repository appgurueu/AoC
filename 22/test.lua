#!/usr/bin/lua5.4

local answers = {
	{71506, 209603},
	{12855, 13726},
	{7990, 2602},
	{503, 827},
	{"TBVFVDZPN", "VLCWHTDSZ"},
	{1361, 3263},
}

for day, expected_parts in ipairs(answers) do
	for part, expected in pairs(expected_parts) do
		local got = assert(loadfile(("src/%d/%d.lua"):format(day, part)))(io.lines(("input/%d.txt"):format(day)))
		assert(got == expected)
	end
end
