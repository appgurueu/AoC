local unpack = unpack or table.unpack

local grid = {}
local start_x, start_y
for line in ... do
	local row = {}
	for char in line:gmatch"." do
		local height, target
		if char == "S" then
			height = 0
			start_x, start_y = #row + 1, #grid + 1
		elseif char == "E" then
			height = 25
			target = true
		else
			height = char:byte() - ("a"):byte()
		end
		table.insert(row, {
			target = target,
			height = height,
			visited = false
		})
	end
	table.insert(grid, row)
end

-- BFS from start
local dirs = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}}
local level = {{start_x, start_y}}
grid[start_y][start_x].visited = true
local step = 1
while true do
	local next_level = {}
	for _, pos in pairs(level) do
		local x, y = unpack(pos)
		local maxheight = grid[y][x].height + 1
		for _, dir in pairs(dirs) do
			local dx, dy = unpack(dir)
			local nx, ny = x + dx, y + dy
			local cell = (grid[ny] or {})[nx]
			if cell and not cell.visited and cell.height <= maxheight then
				if cell.target then -- target reached
					return step
				end
				table.insert(next_level, {nx, ny})
				cell.visited = true
			end
		end
	end
	level = next_level
	step = step + 1
end
