local unpack = unpack or table.unpack

local grid = {}
local function occupied(x, y)
	return grid[y] and grid[y][x]
end
for line in ... do
	local row = {}
	for x = 1, #line do
		local c = line:sub(x, x)
		if c == "#" then
			row[x] = true
		else
			assert(c == ".")
		end
	end
	table.insert(grid, row)
end
local dirs = {
	{0, -1},
	{0, 1},
	{-1, 0},
	{1, 0},
}
local function propose_move(x, y)
	local adjacent = 0
	for rx = -1, 1 do
		for ry = -1, 1 do
			if occupied(x + rx, y + ry) then
				adjacent = adjacent + 1
			end
		end
	end
	assert(adjacent >= 1)
	if adjacent == 1 then
		return 0, 0	-- no neighbors
	end

	local function free(rx, ry)
		return not occupied(x + rx, y + ry)
	end
	for _, dir in ipairs(dirs) do
		local dx, dy = unpack(dir)
		if dx == 0 then
			if free(dx - 1, dy) and free(dx, dy) and free(dx + 1, dy) then
				return dx, dy
			end
		else
			assert(dy == 0)
			if free(dx, dy - 1) and free(dx, dy) and free(dx, dy + 1) then
				return dx, dy
			end
		end
	end
	return 0, 0 -- TODO is this correct?
end

local rounds = 10
for _ = 1, rounds do
	local proposed_targets = {}
	for y, row in pairs(grid) do
		for x in pairs(row) do
			local dx, dy = propose_move(x, y)
			local tx, ty = x + dx, y + dy
			proposed_targets[ty] = proposed_targets[ty] or {}
			proposed_targets[ty][tx] = (proposed_targets[ty][tx] or 0) + 1
			row[x] = {tx, ty}
		end
	end
	-- Move
	local new_grid = {}
	for y, row in pairs(grid) do
		for x, target in pairs(row) do
			local tx, ty = unpack(target)
			if proposed_targets[ty][tx] == 1 then
				if tx ~= x or ty ~= y then
					assert(not occupied(tx, ty))
				end
			else
				assert(proposed_targets[ty][tx] > 1)
				tx, ty = x, y
			end
			new_grid[ty] = new_grid[ty] or {}
			assert(not new_grid[ty][tx])
			new_grid[ty][tx] = true
		end
	end
	grid = new_grid
	table.insert(dirs, table.remove(dirs, 1))
end

local cnt = 0
local mnx, mxx = math.huge, -math.huge
local mny, mxy = math.huge, -math.huge
for y, row in pairs(grid) do
	if next(row) ~= nil then
		mny, mxy = math.min(mny, y), math.max(mxy, y)
	end
	for x in pairs(row) do
		cnt = cnt + 1
		mnx, mxx = math.min(mnx, x), math.max(mxx, x)
	end
end
return (mxx - mnx + 1) * (mxy - mny + 1) - cnt
