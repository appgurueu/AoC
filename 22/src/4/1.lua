local unpack = unpack or table.unpack

local function list(...)
	local t = {}
	for v in ... do
		table.insert(t, v)
	end
	return t
end

local function map(t, f)
	for k, v in pairs(t) do
		t[k] = f(v)
	end
	return t
end

local count = 0
for pair in ... do
	local min, max, min_other, max_other = unpack(map(list(pair:gmatch"%d+"), tonumber))
	if (min >= min_other and max <= max_other) or (min_other >= min and max_other <= max) then
		count = count + 1
	end
end
return count
