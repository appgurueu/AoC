local unpack = unpack or table.unpack

local function map_varg(f, ...)
	if select("#", ...) == 0 then
		return
	end
	return f((...)), map_varg(f, select(2, ...))
end

local blueprints = {}
local line_pattern = table.concat({
	"^Blueprint (%d+): Each ore robot costs (%d+) ore.",
	"Each clay robot costs (%d+) ore.",
	"Each obsidian robot costs (%d+) ore and (%d+) clay.",
	"Each geode robot costs (%d+) ore and (%d+) obsidian.$",
}, " ")
local ore, clay, obsidian, geode = 1, 2, 3, 4 -- "enum"
for line in ... do
	local bp_id, ore_ore, clay_ore, obsidian_ore, obsidian_clay, geode_ore, geode_obsidian
		= map_varg(tonumber, line:match(line_pattern))
	blueprints[bp_id] = {
		[ore] = {[ore] = ore_ore},
		[clay] = {[ore] = clay_ore},
		[obsidian] = {[ore] = obsidian_ore, [clay] = obsidian_clay},
		[geode] = {[ore] = geode_ore, [obsidian] = geode_obsidian},
	}
end

local robot = geode
local initial_state = {
	[ore] = 0, [clay] = 0, [obsidian] = 0, [geode] = 0, -- resources
	[ore + robot] = 1, [clay + robot] = 0, [obsidian + robot] = 0, [geode + robot] = 0, -- robots
}

local function state_to_str(state)
	return ("%d;%d;%d;%d;%d;%d;%d;%d"):format(unpack(state))
end

local function copy(s)
	return {unpack(s)}
end

local function geq(s1, s2)
	for i, v in ipairs(s1) do
		if s2[i] > v then
			return false
		end
	end
	return true
end

local function kdtree(states, depth)
	if #states == 0 then
		return
	end
	if #states == 1 then
		return {pivot = states[1]}
	end
	local axis = (depth % #states[1]) + 1
	table.sort(states, function(a, b)
		return a[axis] < b[axis]
	end)
	local midx = math.floor(#states / 2)
	local smaller, larger = {}, {}
	for i = 1, midx - 1 do
		smaller[i] = states[i]
	end
	for i = midx + 1, #states do
		larger[i - midx] = states[i]
	end
	return {
		pivot = states[midx],
		smaller = kdtree(smaller, depth + 1),
		larger = kdtree(larger, depth + 1),
	}
end

local function dominant_state_exists(root, state, depth)
	if not root then
		return false
	end
	if root.pivot ~= state and geq(root.pivot, state) then
		return true
	end
	local axis = (depth % #root.pivot) + 1
	if dominant_state_exists(root.larger, state, depth + 1) then
		return true
	end
	if state[axis] <= root.pivot[axis] then
		return dominant_state_exists(root.smaller, state, depth + 1)
	end
end

local function prune_duplicates(states)
	local by_str = {}
	for _, state in ipairs(states) do
		by_str[state_to_str(state)] = state
	end
	local dupe_free = {}
	for _, state in pairs(by_str) do
		table.insert(dupe_free, state)
	end
	return dupe_free
end

local function prune_prospective_geodes(states, blueprint, mins_left)
	local max_min_prospective_geodes = 0
	for _, state in ipairs(states) do
		local min_prospective_geodes = state[geode] + state[robot + geode] * mins_left
		max_min_prospective_geodes = math.max(max_min_prospective_geodes,
			min_prospective_geodes)
	end
	local pruned = {}
	for _, state in ipairs(states) do
		local max_prospective_obsidian = state[obsidian]
			+ state[robot + obsidian] * mins_left
			+ mins_left * (mins_left - 1) / 2
		local max_new_geode_robots = math.min(mins_left - 1,
			math.floor(max_prospective_obsidian / blueprint[geode][obsidian]))
		local max_prospective_geodes = state[geode]
			+ state[robot + geode] * mins_left
			+ (max_new_geode_robots + 1) * max_new_geode_robots / 2
		if max_prospective_geodes >= max_min_prospective_geodes then
			table.insert(pruned, state)
		end
	end
	return pruned
end

local function prune_states(states, blueprint, mins_left)
	states = prune_prospective_geodes(prune_duplicates(states), blueprint, mins_left)
	local root = kdtree(states, 0)
	local pruned = {}
	for _, state in ipairs(states) do
		if not dominant_state_exists(root, state, 0) then
			table.insert(pruned, state)
		end
	end
	return pruned
end

local function calc_max_geodes(blueprint)
	local states = {copy(initial_state)}
	for min = 1, 24 do
		-- Create new robots
		local new_states = {}
		for _, state in ipairs(states) do
			local can_build_all = true
			for robot_type, req in pairs(blueprint) do
				local new_state = copy(state)
				for res, req_cnt in pairs(req) do
					if state[res] < req_cnt then
						can_build_all = false
						goto continue
					end
					new_state[res] = state[res] - req_cnt
				end
				for j = ore, geode do
					new_state[j] = new_state[j] + new_state[j + robot]
				end
				new_state[robot + robot_type] = new_state[robot + robot_type] + 1
				table.insert(new_states, new_state)
				::continue::
			end
			if not can_build_all then -- if we can build all robots, build any
				for j = ore, geode do
					state[j] = state[j] + state[j + robot]
				end
				table.insert(new_states, state)
			end
		end
		states = prune_states(new_states, blueprint, 24 - min)
	end
	local max_geodes = -math.huge
	for _, state in ipairs(states) do
		max_geodes = math.max(max_geodes, state[geode])
	end
	return max_geodes
end

local sum = 0
for id, blueprint in pairs(blueprints) do
	local max_geodes = calc_max_geodes(blueprint)
	sum = sum + id * max_geodes
end
return sum
