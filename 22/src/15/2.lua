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

local function intersect(a_min, a_max, b_min, b_max)
	return a_min <= b_max and a_max >= b_min
end

-- ideally we'd be intersecting the diamond shapes produced by the Manhattan distance
-- (which are just rects if rotated 45°) instead
for row_y = 0, max do
	local intervals = {} -- ideally this would use a sorted set data structure
	for _, sensor in ipairs(sensors) do
		local dist = math.abs(sensor.y - row_y)
		if dist <= sensor.range then
			local dist_left = sensor.range - dist
			local min_x, max_x = sensor.x - dist_left, sensor.x + dist_left
			local new_intervals = {}
			for _, interval in ipairs(intervals) do
				if intersect(min_x, max_x, interval[1], interval[2]) then
					min_x = math.min(min_x, interval[1])
					max_x = math.max(max_x, interval[2])
				else
					table.insert(new_intervals, interval)
				end
			end
			table.insert(new_intervals, {min_x, max_x})
			intervals = new_intervals
		end
	end
	table.sort(intervals, function(a, b)
		return a[1] < b[1]
	end)
	local x
	if intervals[1][1] == 1 then
		x = 0
	elseif intervals[#intervals][2] == max - 1 then
		x = max
	else
		for i = 1, #intervals - 1 do
			if intervals[i][2] + 2 == intervals[i+1][1] then -- gap of exactly one
				x = intervals[i][2] + 1
				break
			end
		end
	end
	if x then
		return row_y + max * x
	end
end
