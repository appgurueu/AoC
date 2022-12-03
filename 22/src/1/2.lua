local maximums = {0, 0, 0}
local cur_total = 0

local function cmp_total()
	for i, max in ipairs(maximums) do
		if cur_total >= max then
			table.insert(maximums, i, cur_total)
			maximums[#maximums] = nil
			break
		end
	end
end

for line in ... do
	local num = line:match"%d+"
	if num then
		cur_total = cur_total + num
	else
		cmp_total()
		cur_total = 0
	end
end
cmp_total()

local sum = 0
for _, max in ipairs(maximums) do
	sum = sum + max
end
return sum
