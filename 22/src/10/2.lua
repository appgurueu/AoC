local X = 1
local cycle = 0
local screen = {[0] = "#"}
local w, h = 40, 6
local function next_cycle()
	cycle = cycle + 1
	local px, py = cycle % w, cycle // w
	screen[py * w + px] = math.abs(X - px) <= 1 and "#" or "."
end
for instr in ... do
	next_cycle()
	local to_add = instr:match"^addx (-?%d+)$"
	if to_add then
		X = X + to_add
		next_cycle()
	else
		assert(instr == "noop")
	end
end

for y = 0, h - 1 do
	for x = 0, w - 1 do
		io.write(screen[y * w + x])
	end
	io.write"\n"
end

return "EZFPRAKL" -- HACK
