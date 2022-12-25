local bit = require("bit")
local band, bor, bnot, lshift = bit.band, bit.bor, bit.bnot, bit.lshift

local valves = {}
local working_valves = 0
local defect_valves = {}
local name_to_idx = {}
for line in ... do
	local name, flow, tunnelstr = line:match"^Valve ([A-Z][A-Z]) has flow rate=(%d+); tunnels? leads? to valves? (.+)"
	local valve = {flow = tonumber(flow), name = name}
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

local math_max = math.max
local function get_max_flow(idx, mins_left, valves_used, mask)
	if mins_left <= 1 or valves_used == mask then
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
			dont_open = math_max(dont_open, get_max_flow(nidx, mins_left - dist, valves_used, mask))
		end
	end

	local idxbit = lshift(1, idx - 1)
	if
		idx > working_valves -- not working?
		or band(valves_used, idxbit) ~= 0 -- already used?
		or band(mask, idxbit) == 0 -- to be opened by the other?
	then
		return dont_open
	end

	local do_open = 0
	local valves_used_open = bor(valves_used, idxbit)
	for nidx, dist in pairs(valve.distances) do
		if dist < mins_left - 1 then
			do_open = math_max(do_open, get_max_flow(nidx, mins_left - dist - 1, valves_used_open, mask))
		end
	end
	res = math_max(dont_open, do_open + valve.flow * (mins_left - 1))
	valve.cache[mins_left] = valve.cache[mins_left] or {}
	valve.cache[mins_left][valves_used] = res
	return res
end

local function reset_cache()
	for _, valve in ipairs(valves) do
		valve.cache = {}
	end
end

local root_idx = name_to_idx.AA
local mins = 26
local all_mask = lshift(1, working_valves) - 1

local max_flow = 0
for elephant_mask = 0, all_mask, 2 do
	local you_mask = band(all_mask, bnot(elephant_mask))
	assert(bor(elephant_mask, you_mask) == all_mask)
	assert(band(you_mask, elephant_mask) == 0)
	reset_cache()
	local you_max_flow = get_max_flow(root_idx, mins, 0, you_mask)
	reset_cache()
	local elephant_max_flow = get_max_flow(root_idx, mins, 0, elephant_mask)
	max_flow = math_max(max_flow, you_max_flow + elephant_max_flow)
	io.write(("\rDay 16 part 2: Progress: %02.04f %%"):format(elephant_mask / all_mask * 100))
	io.flush()
end
print()
return max_flow
