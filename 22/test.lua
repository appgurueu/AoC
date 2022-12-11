#!/usr/bin/lua5.4

local answers = {
	{71506, 209603},
	{12855, 13726},
	{7990, 2602},
	{503, 827},
	{"TBVFVDZPN", "VLCWHTDSZ"},
	{1361, 3263},
	{1427048, 2940614},
	{1787, 440640},
	{5779, 2331},
	{12460, "EZFPRAKL"},
	{56595, 15693274740},
}

for day, expected_parts in ipairs(answers) do
	for part, expected in pairs(expected_parts) do
		local got = assert(loadfile(("src/%d/%d.lua"):format(day, part)))(io.lines(("input/%d.txt"):format(day)))
		assert(got == expected)
	end
end
