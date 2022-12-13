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
local div1, div2 = {{2}}, {{6}}
local pkts = {div1, div2}
repeat
	for _ = 1, 2 do
		table.insert(pkts, parse_packet(next_line()))
	end
until not next_line()
table.sort(pkts, function(l, r)
	local order = cmp(l, r)
	return order < 0
end)

local function find_pkt_idx(search_pkt)
	for i, pkt in ipairs(pkts) do
		if pkt == search_pkt then
			return i
		end
	end
end

return find_pkt_idx(div1) * find_pkt_idx(div2)
