local grid = {}
local bottom = -math.huge
local function set_tile(x, y)
	grid[y] = grid[y] or {}
	grid[y][x] = true
end
local function get_tile(x, y)
	return (grid[y] or {})[x]
end
for path in ... do
	local last_x, last_y
	for x, y in path:gmatch"(%d+),(%d+)" do
		local x, y = tonumber(x), tonumber(y)
		if last_x == x then
			for y = math.min(y, last_y), math.max(y, last_y) do
				set_tile(x, y)
			end
		elseif last_y == y then
			for x = math.min(x, last_x), math.max(x, last_x) do
				set_tile(x, y)
			end
		else
			assert(not (last_x or last_y))
		end
		bottom = math.max(bottom, y)
		last_x, last_y = x, y
	end
end

local sand = 0
while true do
	local sx, sy = 500, 0
	while true do
		if sy > bottom then
			return sand
		end
		if not get_tile(sx, sy+1) then
			sy = sy + 1
		elseif not get_tile(sx-1, sy+1) then
			sx, sy = sx - 1, sy + 1
		elseif not get_tile(sx+1, sy+1) then
			sx, sy = sx + 1, sy + 1
		else
			set_tile(sx, sy)
			break
		end
	end
	sand = sand + 1
end
