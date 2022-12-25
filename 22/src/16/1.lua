local bit = require("bit")
local band, bor, lshift = bit.band, bit.bor, bit.lshift

local valves = {}
local working_valves = 0
local defect_valves = {}
local name_to_idx = {}
for line in ... do
	local name, flow, tunnelstr = line:match"^Valve ([A-Z][A-Z]) has flow rate=(%d+); tunnels? leads? to valves? (.+)"
	local valve = {flow = tonumber(flow), name = name, cache = {}}
	for to_name in tunnelstr:gmatch"[A-Z][A-Z]" do
		table.insert(valve, to_name)
	end
	if valve.flow ~= 0 then
		working_valves = working_valves + 1
		valves[working_valves] = valve
		valve.idx = working_valves
		name_to_idx[name] = working_valves
	else
		table.insert(defect_valves, valve)
	end
end

-- Resolve names
for _, valve in ipairs(defect_valves) do
	valve.idx = #valves + 1
	valves[valve.idx] = valve
	name_to_idx[valve.name] = valve.idx
end
for _, valve in ipairs(valves) do
	for i, name in ipairs(valve) do
		valve[i] = assert(name_to_idx[name])
	end
end

-- Find shortest paths to working valves using BFS
-- This could be optimized, but that's not necessary for the small inputs
for start_idx in ipairs(valves) do
	local distances = {}
	local dist = 1
	local level = {start_idx}
	local visited = {[start_idx] = true}
	while level[1] do
		local next_level = {}
		for _, idx in ipairs(level) do
			for _, neighbor_idx in ipairs(valves[idx]) do
				if not visited[neighbor_idx] then
					if neighbor_idx <= working_valves then
						distances[neighbor_idx] = dist
					else
						table.insert(next_level, neighbor_idx)
					end
					visited[neighbor_idx] = true
				end
			end
		end
		level = next_level
		dist = dist + 1
	end
	valves[start_idx].distances = distances
end

local pairs, math_max = pairs, math.max
local function get_max_flow(idx, mins_left, valves_used)
	if mins_left <= 1 then
		return 0
	end
	local valve = valves[idx]
	local res = (valve.cache[mins_left] or {})[valves_used]
	if res then
		return res
	end

	local dont_open = 0
	for nidx, dist in pairs(valve.distances) do
		if dist < mins_left then
			dont_open = math_max(dont_open, get_max_flow(nidx, mins_left - dist, valves_used))
		end
	end

	if band(valves_used, lshift(1, idx)) ~= 0 or valve.flow == 0 then
		return dont_open
	end

	local do_open = 0
	local valves_used_open = bor(valves_used, lshift(1, idx))
	for nidx, dist in pairs(valve.distances) do
		if dist < mins_left - 1 then
			do_open = math_max(do_open, get_max_flow(nidx, mins_left - dist - 1, valves_used_open))
		end
	end
	res = math_max(dont_open, do_open + valve.flow * (mins_left - 1))
	valve.cache[mins_left] = valve.cache[mins_left] or {}
	valve.cache[mins_left][valves_used] = res
	return res
end

return get_max_flow(name_to_idx.AA, 30, 0)
