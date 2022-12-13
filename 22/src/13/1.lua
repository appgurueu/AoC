local function parse_packet(line)
	local i = 1
	local function peek()
		return line:sub(i, i)
	end
	local function get()
		i = i + 1
		return line:sub(i-1, i-1)
	end
	local function list()
		assert(get() == "[")
		local res = {}
		repeat
			local _, j = line:find("^%d+", i)
			if j then
				table.insert(res, tonumber(line:sub(i, j)))
				i = j + 1
			elseif peek() == "[" then
				table.insert(res, list())
			end
			local c = get()
			assert(c == "," or c == "]", c)
		until c == "]"
		return res
	end
	return list()
end

local function cmp(l, r)
	local tl, tr = type(l), type(r)
	if tl ~= tr then
		if tl == "number" then l = {l}
		elseif tr == "number" then r = {r} end
	elseif tl == "number" then
		if l < r then return -1 end
		if l > r then return 1 end
		return 0
	end
	local i = 1
	while true do
		if not l[i] then
			if r[i] then return -1 end
			return 0
		end
		if not r[i] then
			return 1
		end
		local decision = cmp(l[i], r[i])
		if decision ~= 0 then
			return decision
		end
		i = i + 1
	end
end

local next_line = ...
local idx, idxsum = 1, 0
repeat
	local pkt1 = parse_packet(next_line())
	local pkt2 = parse_packet(next_line())
	if cmp(pkt1, pkt2) < 0 then
		idxsum = idxsum + idx
	end
	idx = idx + 1
until not next_line()
return idxsum
