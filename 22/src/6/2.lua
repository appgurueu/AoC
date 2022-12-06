local marker_length = 14

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

for i = 1, marker_length do
	inc_byte(str:byte(i))
end

if unique_count == marker_length then
	return marker_length
end

for i = marker_length + 1, #str do
	dec_byte(str:byte(i - marker_length))
	inc_byte(str:byte(i))
	if unique_count == marker_length then
		return i
	end
end

error"no start-of-message marker"
