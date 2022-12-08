local grid = {}

for line in ... do
	local row = {}
	for i = 1, #line do
		row[i] = assert(tonumber(line:sub(i, i)))
	end
	table.insert(grid, row)
end

local visible_trees = {}

local function find_visible_trees(x, y, step_x, step_y)
	local max_height = -1
	while grid[y] and grid[y][x] do
		local height = grid[y][x]
		if height > max_height then
			visible_trees[y] = visible_trees[y] or {}
			visible_trees[y][x] = true
			max_height = height
		end
		x, y = x + step_x, y + step_y
	end
end

local w, h = #grid[1], #grid

for x = 1, w do
	find_visible_trees(x, 1, 0, 1)
	find_visible_trees(x, h, 0, -1)
end

for y = 1, h do
	find_visible_trees(1, y, 1, 0)
	find_visible_trees(w, y, -1, 0)
end

local count = 0
for _, row in pairs(visible_trees) do
	for _ in pairs(row) do
		count = count + 1
	end
end
return count
