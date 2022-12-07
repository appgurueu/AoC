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

local small_sum = 0

local function size(entry)
	if type(entry) == "number" then
		return entry
	end
	local sum = 0
	for _, child in pairs(entry) do
		sum = sum + size(child)
	end
	if sum <= 1e5 then
		small_sum = small_sum + sum
	end
	return sum
end

size(root)

return small_sum
