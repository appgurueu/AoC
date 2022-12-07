local root = {}
local parent = {}
local cur_dir = root

local function create_dir(name)
	cur_dir[name] = {}
	parent[cur_dir[name]] = cur_dir
end

for line in ... do
	local cd_to = line:match("^%$%s*cd%s+(.+)")
	if cd_to then
		if cd_to == ".." then
			cur_dir = assert(parent[cur_dir])
		elseif cd_to == "/" then
			cur_dir = root
		else
			if not cur_dir[cd_to] then
				create_dir(cd_to)
			end
			cur_dir = cur_dir[cd_to]
		end
	elseif line:sub(1, 1) ~= "$" then
		local type_or_size, name = line:match"^(.-)%s+(.+)"
		if type_or_size == "dir" then
			create_dir(name)
		else
			cur_dir[name] = assert(tonumber(type_or_size))
		end
	end
end

local function size(entry, visitor)
	if type(entry) == "number" then
		return entry
	end
	local sum = 0
	for _, child in pairs(entry) do
		sum = sum + size(child, visitor)
	end
	visitor(sum)
	return sum
end

local disk_space, space_needed = 7e7, 3e7
local space_used = size(root, function() end)
local free_space = disk_space - space_used
local to_free = space_needed - free_space

local min_larger_dir_size = math.huge
-- This could be optimized since we know that we can skip visiting the parent
-- if a child already is large enough, but that wouldn't change time complexity
size(root, function(sum)
	if sum >= to_free then
		min_larger_dir_size = math.min(min_larger_dir_size, sum)
	end
end)

return min_larger_dir_size
