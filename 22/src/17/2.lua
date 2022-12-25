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

local seen = {}
local max_iter = 1e12
local iter = 1
while iter <= max_iter do
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
	local cnt = 0
	local visited = {}
	local reachable = {}
	local function find_reachable(x, y)
		if x == 0 or x == 8 or y == 0 or y > max_y + 1 or (visited[y] or {})[x] then
			return
		end
		visited[y] = visited[y] or {}
		visited[y][x] = true
		if grid_get(x, y) then
			reachable[y] = reachable[y] or {}
			reachable[y][x] = true
			cnt = cnt + 1
			return
		end
		find_reachable(x + 1, y)
		find_reachable(x - 1, y)
		find_reachable(x, y + 1)
		find_reachable(x, y - 1)
	end
	find_reachable(1, max_y + 1)
	grid = reachable
	local rope = {}
	for y, row in pairs(reachable) do
		for x in pairs(row) do
			table.insert(rope, ("%d,%d"):format(x, y - max_y))
		end
	end
	table.sort(rope)
	table.insert(rope, ("%d,%d"):format(si, pi))
	local str = table.concat(rope, ";")
	local prev = seen[str]
	if prev then -- cycle found - use it to fast-forward
		local iters = iter - prev.iter
		local height_gain = max_y - prev.max_y
		local times = math.floor((max_iter - iter) / iters)
		local total_gain = times * height_gain
		local total_iters = times * iters
		local new_grid = {}
		for y, row in pairs(grid) do
			new_grid[y + total_gain] = row
		end
		grid = new_grid
		max_y = max_y + total_gain
		iter = iter + total_iters
	end
	seen[str] = {iter = iter, max_y = max_y}
	iter = iter + 1
end

return max_y
