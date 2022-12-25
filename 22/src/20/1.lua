local numbers = {}
for line in ... do
	local idx = #numbers + 1
	numbers[idx] = {number = assert(tonumber(line)), idx = idx}
end

local initial_idx = {}
for i, t in ipairs(numbers) do
	initial_idx[i] = t.idx
end

local function find_index(idx)
	local i = 0
	repeat
		i = i + 1
	until assert(numbers[i]).idx == idx
	return i
end

for _, idx in ipairs(initial_idx) do
	local i = find_index(idx)
	local t = numbers[i]
	local number = t.number
	local j = i
	if number > 0 then
		if i == #numbers then
			j = 1
		end
		for _ = 1, number do
			j = j + 1
			if j == #numbers then
				j = 1
			end
		end
	elseif number < 0 then
		if i == 1 then
			j = #numbers
		end
		for _ = 1, -number do
			j = j - 1
			if j == 1 then
				j = #numbers
			end
		end
	end
	assert(j >= 1 and j <= #numbers, j)
	if j > i then
		-- Shift down
		for k = i, j - 1 do
			numbers[k] = numbers[k + 1]
		end
	elseif j < i then
		-- Shift up
		for k = i, j + 1, -1 do
			numbers[k] = numbers[k - 1]
		end
	end
	numbers[j] = t
end

local zero_index = 0
repeat
	zero_index = zero_index + 1
until assert(numbers[zero_index]).number == 0
local sum = 0
for i = 1, 3 do
	local t = numbers[((zero_index - 1 + i * 1000) % #numbers) + 1]
	sum = sum + t.number
end
return sum
