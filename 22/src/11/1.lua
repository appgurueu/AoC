local next_line = ...

local ops = {
	["+"] = function(a, b) return a + b end,
	["*"] = function(a, b) return a * b end,
}

local monkeys = {}
local i = 0
while true do
	local line = next_line()
	local id = tonumber(line:match"^Monkey (%d+):$")
	assert(id == i); i = i + 1
	local items = {}
	for item in next_line():gmatch"%d+" do
		table.insert(items, tonumber(item))
	end
	local lhs, op, rhs = next_line():match"Operation: new = (.-) ([*+]) (.+)"
	local function operation(old)
		return ops[op](lhs == "old" and old or lhs,
				rhs == "old" and old or rhs)
	end
	local div = next_line():match"Test: divisible by (%d+)"
	local if_ = tonumber(next_line():match"If true: throw to monkey (%d+)")
	local else_ = tonumber(next_line():match"If false: throw to monkey (%d+)")
	local function get_recipient(worry)
		return worry % div == 0 and if_ or else_
	end
	monkeys[id] = {
		items = items,
		operation = operation,
		get_recipient = get_recipient,
		inspections = 0,
	}
	if not next_line() then break end
end

for _ = 1, 20 do
	for i = 0, #monkeys do
		local monkey = monkeys[i]
		monkey.inspections = monkey.inspections + #monkey.items
		for _, item in ipairs(monkey.items) do
			item = monkey.operation(item) // 3
			local recipient = monkeys[monkey.get_recipient(item)]
			assert(recipient and recipient ~= monkey)
			table.insert(recipient.items, item)
		end
		monkey.items = {}
	end
end

local max_i = 0
for i = 1, #monkeys do
	if monkeys[i].inspections > monkeys[max_i].inspections then
		max_i = i
	end
end
local second_max = -math.huge
for i = 0, #monkeys do
	if i ~= max_i then
		second_max = math.max(second_max, monkeys[i].inspections)
	end
end
return monkeys[max_i].inspections * second_max
