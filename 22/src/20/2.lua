local key = 811589153
local numbers = {}
for line in ... do
	local idx = #numbers + 1
	local num = assert(tonumber(line)) * key
	assert(num < 2^53)
	numbers[idx] = {number = num, idx = idx}
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

local function calc_wraparound_insertion_index(i, number, len)
	if number > 0 then
		return ((i - 1 + number) % (len - 1)) + 1
	end if number < 0 then
		-- HACK we know our function works for positive indices,
		-- so use reverse indices (from the end), apply our function,
		-- then convert back to indices from the beginning
		local inv_i = len - i + 1
		local inv_j = calc_wraparound_insertion_index(inv_i, -number, len)
		local j = len - inv_j + 1
		return j
	end
	return i
end

for _ = 1, 10 do
	for _, idx in ipairs(initial_idx) do
		local i = find_index(idx)
		local t = numbers[i]
		local number = t.number
		local j = calc_wraparound_insertion_index(i, number, #numbers)
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
