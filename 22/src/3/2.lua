assert("a" > "A")
local function get_priority(char)
	if char >= "a" then
		assert(char <= "z")
		return char:byte() - ("a"):byte() + 1
	end
	assert(char >= "A" and char <= "Z")
	return char:byte() - ("A"):byte() + 27
end

local priority_sum = 0
local rucksacks, counts = 0, {}

for rucksack in ... do
	local seen = {}
	for item in rucksack:gmatch"." do
		local priority = get_priority(item)
		if not seen[priority] then
			seen[priority] = true
			counts[priority] = (counts[priority] or 0) + 1
		end
	end
	rucksacks = rucksacks + 1
	if rucksacks == 3 then
		local common_priority
		for priority, count in pairs(counts) do
			if count == rucksacks then
				common_priority = priority
				break
			end
		end
		priority_sum = priority_sum + common_priority
		rucksacks = 0
		counts = {}
	end
end
return priority_sum
