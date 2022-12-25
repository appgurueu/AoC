local unpack = unpack or table.unpack

local grid = {}
local function wall_idx_err() error"attempt to index a wall" end
local wall = setmetatable({}, {__index = wall_idx_err, __newindex = wall_idx_err})
local dirs = {
	[">"] = {1, 0},
	["<"] = {-1, 0},
	["^"] = {0, -1},
	["v"] = {0, 1},
}
for line in ... do
	local row = {}
	for x = 1, #line do
		local c = line:sub(x, x)
		if c == "#" then
			row[x] = wall
		else
			row[x] = {
				reachable = false,
				blizzards = {c ~= "." and c or nil},
			}
		end
	end
	table.insert(grid, row)
end

local min_x, max_x = 2, #grid[1] - 1
local min_y, max_y = 2, #grid - 1
local function simulate_blizzards()
	for _, row in pairs(grid) do
		for _, cell in pairs(row) do
			if cell ~= wall then
				cell.new_blizzards = {}
			end
		end
	end
	for y, row in pairs(grid) do
		for x, cell in pairs(row) do
			if cell == wall then goto continue end
			for _, dir in ipairs(cell.blizzards) do
				local dx, dy = unpack(dirs[dir])
				local nx, ny = x + dx, y + dy
				local neighbor = assert(grid[ny][nx])
				if neighbor == wall then
					if dx == 0 then
						if dy < 0 then
							assert(ny < min_y)
							ny = max_y
						else
							assert(dy > 0)
							assert(ny > max_y)
							ny = min_y
						end
					else
						assert(dy == 0)
						if dx < 0 then
							assert(nx < min_x)
							nx = max_x
						else
							assert(dx > 0)
							assert(nx > max_x)
							nx = min_x
						end
					end
					neighbor = assert(grid[ny][nx])
				end
				table.insert(neighbor.new_blizzards, dir)
			end
			::continue::
		end
	end
	for _, row in pairs(grid) do
		for _, cell in pairs(row) do
			if cell ~= wall then
				cell.blizzards = cell.new_blizzards
			end
		end
	end
end

local function find_x(y)
	local x = 0
	repeat
		x = x + 1
	until assert(grid[y][x]) ~= wall
	return x
end

local sy = 1
local sx = find_x(sy)
grid[sy][sx].reachable = true

local ty = #grid
local tx = find_x(ty)

local minute = 0
while not grid[ty][tx].reachable do
	simulate_blizzards()
	for _, row in pairs(grid) do
		for _, cell in pairs(row) do
			if cell ~= wall then
				cell.new_reachable = false
			end
		end
	end
	for y, row in pairs(grid) do
		for x, cell in pairs(row) do
			if cell ~= wall and cell.reachable then
				for _, delta in pairs(dirs) do
					local dx, dy = unpack(delta)
					local neighbor = (grid[y + dy] or {})[x + dx]
					if neighbor and neighbor ~= wall then
						neighbor.new_reachable = true
					end
				end
			end
		end
	end
	for _, row in pairs(grid) do
		for _, cell in pairs(row) do
			if cell ~= wall then
				if cell.blizzards[1] then
					cell.reachable = false
				else
					cell.reachable = cell.reachable or cell.new_reachable
				end
			end
		end
	end
	minute = minute + 1
end

return minute

