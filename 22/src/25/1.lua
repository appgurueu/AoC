local digit_val = {
	["2"] = 2,
	["1"] = 1,
	["0"] = 0,
	["-"] = -1,
	["="] = -2,
}

local val_digit = {}
for k, v in pairs(digit_val) do
	val_digit[v] = k
end

local function parse_snafu(str)
	local val = 0
	local pow = 1
	for i = #str, 1, -1 do
		val = val + pow * digit_val[str:sub(i, i)]
		pow = pow * 5
	end
	return val
end

local function stringify_snafu(num)
	local rope = {}
	repeat
		local digit = num % 5
		num = (num - digit) / 5
		local carry = 0
		if digit == 3 then
			digit, carry = -2, 1
		elseif digit == 4 then
			digit, carry = -1, 1
		end
		table.insert(rope, assert(val_digit[digit]))
		num = num + carry
	until num == 0
	return table.concat(rope):reverse()
end

local sum = 0
for line in ... do
	sum = sum + parse_snafu(line)
end
return stringify_snafu(sum)
