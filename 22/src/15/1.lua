local row_y = 2000000

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
local beacons_in_row_x = {}
local beacons_in_row = 0
for line in ... do
	local sx, sy, bx, by = map_varg(tonumber,
		line:match"^Sensor at x=(%-?%d+), y=(%-?%d+): closest beacon is at x=(%-?%d+), y=(%-?%d+)$")
	if by == row_y and not beacons_in_row_x[bx] then
		beacons_in_row = beacons_in_row + 1
		beacons_in_row_x[bx] = true
	end
	local range = distance(sx, sy, bx, by)
	table.insert(sensors, {x = sx, y = sy, range = range})
end

local function intersect(a_min, a_max, b_min, b_max)
	return a_min <= b_max and a_max >= b_min
end

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

for _, int in pairs(intervals) do
	for _, int2 in pairs(intervals) do
		if int ~= int2 then
			assert(not intersect(int[1], int[2], int2[1], int2[2]))
		end
	end
end

local sum = -beacons_in_row
for _, interval in ipairs(intervals) do
	sum = sum + interval[2] - interval[1] + 1
end
return sum
