local str = (...)() -- first line

local last_bytes = {}
local unique_count = 0

local function dec_byte(b)
	last_bytes[b] = last_bytes[b] - 1
	if last_bytes[b] == 0 then
		unique_count = unique_count - 1
	end
end

local function inc_byte(b)
	last_bytes[b] = (last_bytes[b] or 0) + 1
	if last_bytes[b] == 1 then
		last_bytes[b] = 1
		unique_count = unique_count + 1
	end
end

for i = 1, 4 do
	inc_byte(str:byte(i))
end

if unique_count == 4 then
	return 4
end

for i = 5, #str do
	dec_byte(str:byte(i-4))
	inc_byte(str:byte(i))
	if unique_count == 4 then
		return i
	end
end

error"no start-of-packet marker"
