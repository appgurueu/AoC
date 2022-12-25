local pattern = (...)()
local pushes = {}
local dirs = {["<"] = -1, [">"] = 1}
for i = 1, #pattern do
	local c = pattern:sub(i, i)
	pushes[i] = assert(dirs[c])
end
local pi = 0
local function get_push()
	pi = pi + 1
	if pi > #pushes then
		pi = 1
	end
	return pushes[pi]
end

local shapes
do
	local x, _ = true, nil
	shapes = {
		{
			{x, x, x, x}
		},
		{
			{_, x, _},
			{x, x, x},
			{_, x, _},
		},
		{
			[3] = {_, _, x},
			[2] = {_, _, x},
			[1] = {x, x, x},
		},
		{
			{x},
			{x},
			{x},
			{x},
		},
		{
			{x, x},
			{x, x},
		}
	}
end

local grid = {}
local max_y = 0

local function grid_get(x, y)
	if x == 0 or x == 8 or y == 0 then
		return true
	end
	return (grid[y] or {})[x]
end

local function grid_set(x, y)
	max_y = math.max(max_y, y)
	grid[y] = grid[y] or {}
	grid[y][x] = true
end

local si = 0
local function get_shape()
	si = si + 1
	if si > #shapes then
		si = 1
	end
	return shapes[si]
end

for _ = 1, 2022 do
	local shape = get_shape()
	local sx, sy = 2, max_y + 3
	local function try_move(dx, dy)
		local next_sx, next_sy = sx + dx, sy + dy
		for y, row in pairs(shape) do
			for x in pairs(row) do
				if grid_get(x + next_sx, y + next_sy) then
					return true
				end
			end
		end
		sx, sy = next_sx, next_sy
	end
	repeat
		try_move(get_push(), 0)
	until try_move(0, -1)
	for y, row in pairs(shape) do
		for x in pairs(row) do
			grid_set(x + sx, y + sy)
		end
	end
end

return max_y
