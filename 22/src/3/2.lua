assert("a" > "A")
local function get_priority(char)
	if char >= "a" then
		assert(char <= "z")
		return char:byte() - ("a"):byte() + 1
	end
	assert(char >= "A" and char <= "Z")
	return char:byte() - ("A"):byte() + 27
end

local all_priorities <const> = (1 << 53) - 1
local priority_sum = 0
local rucksacks, common_priorities = 0, all_priorities
for rucksack in ... do
	rucksacks = rucksacks + 1
	if rucksacks == 3 then
		local common_priority
		for item in rucksack:gmatch"." do
			local priority = get_priority(item)
			if (common_priorities & (1 << priority)) ~= 0 then
				common_priority = priority
				break
			end
		end
		priority_sum = priority_sum + common_priority
		rucksacks, common_priorities = 0, all_priorities
	else
		local priorities = 0
		for item in rucksack:gmatch"." do
			priorities = priorities | (1 << get_priority(item))
		end
		common_priorities = common_priorities & priorities
	end
end
return priority_sum
