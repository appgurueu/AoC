local function map(f, t)
	local res = {}
	for k, v in pairs(t) do
		res[k] = f(v)
	end
	return res
end

local function combine(f, t1, t2)
	local t = {}
	for k, v in pairs(t1) do
		t[k] = f(v, t2[k])
	end
	return t
end

local function copy(t)
	local res = {}
	for k, v in pairs(t) do
		res[k] = v
	end
	return res
end

local function vadd(dst, src)
	for k, v in pairs(src) do
		dst[k] = dst[k] + v
	end
end

local function add(t, pos)
	for i = 1, #pos - 1 do
		t[pos[i]] = t[pos[i]] or {}
		t = t[pos[i]]
	end
	t[pos[#pos]] = true
end

local function has(t, pos)
	for i = 1, #pos - 1 do
		t = t[pos[i]] or {}
		if not t then
			return
		end
	end
	return t[pos[#pos]]
end

local grid = {}
local function cnt(pos)
	return has(grid, pos) and 1 or 0
end
local function cnt_neighbors(pos)
	local sum = 0
	for i = 1, 3 do
		for d = -1, 1, 2 do
			pos[i] = pos[i] + d
			sum = sum + cnt(pos)
			pos[i] = pos[i] - d
		end
	end
	return sum
end

local huge = math.huge
local minpos, maxpos = {huge, huge, huge}, {-huge, -huge, -huge}
for line in ... do
	local pos = map(tonumber, {line:match"^(%d+),(%d+),(%d+)$"})
	add(grid, pos)
	minpos = combine(math.min, minpos, pos)
	maxpos = combine(math.max, maxpos, pos)
end
vadd(minpos, {-1, -1, -1})
vadd(maxpos, {1, 1, 1})

local sides = 0
local border = {}
local function dfs_border(pos)
	for k, v in pairs(pos) do
		if v < minpos[k] or v > maxpos[k] then
			return
		end
	end
	if has(border, pos) or has(grid, pos) then
		return -- already visited or not part of border
	end
	add(border, pos)
	assert(has(border, pos))
	sides = sides + cnt_neighbors(pos)
	for i = 1, 3 do
		for d = -1, 1, 2 do
			pos[i] = pos[i] + d
			dfs_border(pos)
			pos[i] = pos[i] - d
		end
	end
end
dfs_border(copy(minpos))
return sides
