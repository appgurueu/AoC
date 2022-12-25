local nodes = {}
for line in ... do
	local name, calc = line:match"^([a-z]+): (.+)$"
	local num = calc:match"^%d+$"
	local node
	if num then
		node = tonumber(num)
	else
		local lhs, op, rhs = calc:match"^([a-z]+) ([%+%-%*%/]) ([a-z]+)$"
		assert(op, calc)
		node = {
			lhs = lhs,
			op = op,
			rhs = rhs
		}
	end
	nodes[name] = node
end

local ops = {
	["+"] = function(lhs, rhs) return lhs + rhs end,
	["-"] = function(lhs, rhs) return lhs - rhs end,
	["*"] = function(lhs, rhs) return lhs * rhs end,
	["/"] = function(lhs, rhs) return lhs / rhs end,
}

local function eval(name)
	local node = assert(nodes[name], name)
	if type(node) == "number" then
		return node
	end
	local res = node.res
	if res then
		return res
	end
	res = ops[node.op](eval(node.lhs), eval(node.rhs))
	node.res = res
	return res
end

return eval"root"
