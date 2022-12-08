local grid = {}

for line in ... do
	local row = {}
	for i = 1, #line do
		row[i] = assert(tonumber(line:sub(i, i)))
	end
	table.insert(grid, row)
end

local max_scenic_score = 0
local function try_location(x, y)
	local house_height = grid[y][x]
	local function view_dist(step_x, step_y)
		local tx, ty = x, y
		local dist = 0
		while true do
			tx, ty = tx + step_x, ty + step_y
			local height = (grid[ty] or {})[tx]
			if not height then return dist end
			dist = dist + 1
			if height >= house_height then
				return dist
			end
		end
	end
	local scenic_score = view_dist(1, 0) * view_dist(-1, 0) * view_dist(0, 1) * view_dist(0, -1)
	max_scenic_score = math.max(max_scenic_score, scenic_score)
end

local w, h = #grid[1], #grid
for x = 1, w do
	for y = 1, h do
		try_location(x, y)
	end
end

return max_scenic_score
