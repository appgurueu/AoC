local grid = {}
local start_x, start_y
for line in ... do
	local row = {}
	for char in line:gmatch"." do
		local height
		if char == "S" then
			height = 0
		elseif char == "E" then
			height = 25
			start_x, start_y = #row + 1, #grid + 1
		else
			height = char:byte() - ("a"):byte()
		end
		table.insert(row, {
			height = height,
			visited = false
		})
	end
	table.insert(grid, row)
end

-- BFS from end
local dirs = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}}
local level = {{start_x, start_y}}
grid[start_y][start_x].visited = true
local step = 1
repeat
	local next_level = {}
	for _, pos in pairs(level) do
		local x, y = table.unpack(pos)
		local minheight = grid[y][x].height - 1
		for _, dir in pairs(dirs) do
			local dx, dy = table.unpack(dir)
			local nx, ny = x + dx, y + dy
			local cell = (grid[ny] or {})[nx]
			if cell and not cell.visited and cell.height >= minheight then
				if cell.height == 0 then -- closest starting point found
					return step
				end
				table.insert(next_level, {nx, ny})
				cell.visited = true
			end
		end
	end
	level = next_level
	step = step + 1
until false
