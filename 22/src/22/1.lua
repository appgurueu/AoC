local unpack = unpack or table.unpack

local next_line = ...
local grid = {}
local min_x_by_y, max_x_by_y = {}, {}
local min_y_by_x, max_y_by_x = {}, {}
for line in next_line do
	if line == "" then break end
	local y = #grid + 1
	local row = {}
	for x, char in line:gmatch"()(.)" do
		if char ~= " " then
			min_y_by_x[x], max_y_by_x[x] = min_y_by_x[x] or y, y
			min_x_by_y[y], max_x_by_y[y] = min_x_by_y[y] or x, x
			row[x] = char
		end
	end
	grid[y] = row
end

-- Basic tokenization
local instr_str = next_line()
local instrs = {}
local i = 1
while i <= #instr_str do
	local c = instr_str:sub(i, i)
	if c:find"[LR]" then
		table.insert(instrs, c)
		i = i + 1
	else
		local _, j, substr = instr_str:find("^(%d+)", i)
		table.insert(instrs, assert(tonumber(substr)))
		i = j + 1
	end
end

local function get(x, y)
	return (grid[y] or {})[x]
end

local dirs = {
	[0] = {1, 0},	-- right
	[1] = {0, 1},	-- down
	[2] = {-1, 0},	-- left
	[3] = {0, -1},	-- up
}

local dir_wraparound = {
	[0] = function(_, y)
		return min_x_by_y[y], y
	end,
	[1] = function(x, _)
		return x, min_y_by_x[x]
	end,
	[2] = function(_, y)
		return max_x_by_y[y], y
	end,
	[3] = function(x, _)
		return x, max_y_by_x[x]
	end,
}

local facing = 0
local y = 1
local x = min_x_by_y[y]
while assert(get(x, y)) ~= "." do
	x = x + 1
end

for _, instr in ipairs(instrs) do
	if instr == "L" then
		facing = (facing - 1) % 4
	elseif instr == "R" then
		facing = (facing + 1) % 4
	else
		assert(type(instr) == "number")
		local dx, dy = unpack(dirs[facing])
		local wraparound = dir_wraparound[facing]
		for _ = 1, instr do
			local nx, ny = x + dx, y + dy
			::try_next_pos::
			local ntile = get(nx, ny)
			if ntile == "#" then
				break
			end
			if ntile == "." then
				x, y = nx, ny
			else
				assert(not ntile)
				nx, ny = wraparound(x, y)
				goto try_next_pos
			end
		end
	end
end

return 1e3 * y + 4 * x + facing
