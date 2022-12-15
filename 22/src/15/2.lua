local max = 4000000

local function map_varg(f, ...)
	if select("#", ...) == 0 then
		return
	end
	return f((...)), map_varg(f, select(2, ...))
end

local function distance(sx, sy, bx, by)
	return math.abs(sx - bx) + math.abs(sy - by)
end

local sensors = {}
for line in ... do
	local sx, sy, bx, by = map_varg(tonumber,
		line:match"^Sensor at x=(%-?%d+), y=(%-?%d+): closest beacon is at x=(%-?%d+), y=(%-?%d+)$")
	local range = distance(sx, sy, bx, by)
	table.insert(sensors, {x = sx, y = sy, range = range})
end

return coroutine.wrap(function()
	-- Note: Positions being tried multiple times doesn't matter.
	local function try_pos(x, y)
		for _, sensor in ipairs(sensors) do
			if distance(sensor.x, sensor.y, x, y) <= sensor.range then
				return -- in range
			end
		end
		coroutine.yield(y + max * x)
	end

	for _, sensor in ipairs(sensors) do
		-- Iterate the outline of just barely out-of-range positions
		local sx, sy, sr = sensor.x, sensor.y, sensor.range
		for i = 0, sr + 1 do
			try_pos(sx + sr + 1 - i, sy + i)
			try_pos(sx + sr + 1 - i, sy - i)
			try_pos(sx + i, sy + sr + 1 - i)
			try_pos(sx - i, sy + sr + 1 - i)
		end
	end

	-- Try borders
	for i = 0, max do
		try_pos(0, i)
		try_pos(i, 0)
		try_pos(max, i)
		try_pos(i, max)
	end

	error"no position found"
end)()

