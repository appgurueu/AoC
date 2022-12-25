local function map_varg(f, ...)
	if select("#", ...) == 0 then
		return
	end
	return f((...)), map_varg(f, select(2, ...))
end

local function nilset(t, k, ...)
	if select("#", ...) == 1 then
		t[k] = ...
		return
	end
	t[k] = t[k] or {}
	return nilset(t[k], ...)
end

local function nilget(t, k, ...)
	if t[k] == nil or select("#", ...) == 0 then
		return t[k]
	end
	return nilget(t[k], ...)
end

local grid = {}

for voxel in ... do
	local x, y, z = map_varg(tonumber, voxel:match"^(%d+),(%d+),(%d+)$")
	nilset(grid, x, y, z, true)
end

local sides = 0
local function check_side(x, y, z)
	if not nilget(grid, x, y, z) then
		sides = sides + 1
	end
end
for x, a in pairs(grid) do
	for y, b in pairs(a) do
		for z in pairs(b) do
			check_side(x + 1, y, z)
			check_side(x - 1, y, z)
			check_side(x, y + 1, z)
			check_side(x, y - 1, z)
			check_side(x, y, z + 1)
			check_side(x, y, z - 1)
		end
	end
end
return sides
