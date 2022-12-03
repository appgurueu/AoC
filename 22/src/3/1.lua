assert("a" > "A")
local function get_priority(char)
	if char >= "a" then
		return char:byte() - ("a"):byte() + 1
	end
	return char:byte() - ("A"):byte() + 27
end

local priority_sum = 0
for rucksack in ... do
	local first, second = rucksack:sub(1, #rucksack/2), rucksack:sub(#rucksack/2 + 1)
	local first_items = {}
	for item in first:gmatch"." do
		first_items[item] = true
	end
	local common_item
	for item in second:gmatch"." do
		if first_items[item] then
			common_item = item
			break
		end
	end
	priority_sum = priority_sum + get_priority(assert(common_item))
end
return priority_sum
