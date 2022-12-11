local X = 1
local cycle = 1
local sum = 0
local function check()
	if (cycle - 20) % 40 == 0 then
		local signal_strength = cycle * X
		sum = sum + signal_strength
	end
end
for instr in ... do
	cycle = cycle + 1
	check()
	local to_add = instr:match"^addx (-?%d+)$"
	if to_add then
		cycle = cycle + 1
		X = X + to_add
		check()
	else
		assert(instr == "noop")
	end
end
return sum
