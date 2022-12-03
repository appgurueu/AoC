local max = 0
local cur_total = 0
for line in ... do
	local num = line:match"%d+"
	if num then
		cur_total = cur_total + num
		max = math.max(max, cur_total)
	else
		cur_total = 0
	end
end
return max
