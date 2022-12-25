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

nodes.humn = {depends_on_human = true}

local function depends_on_human(name)
	local node = assert(nodes[name], name)
	if type(node) == "number" then
		return false
	end
	if node.depends_on_human then
		return true
	end
	local res = depends_on_human(node.lhs) or depends_on_human(node.rhs)
	node.depends_on_human = res
	return res
end

depends_on_human"root"

local function eval(name)
	local node = assert(nodes[name], name)
	if type(node) == "number" then
		return node
	end
	assert(not depends_on_human(name))
	local res = node.res
	if res then
		return res
	end
	res = ops[node.op](eval(node.lhs), eval(node.rhs))
	node.res = res
	return res
end

local function solve_human(name, target_value)
	if name == "humn" then
		return target_value
	end
	local node = assert(nodes[name], name)
	assert(depends_on_human(name))
	local res = node.res
	if res then
		return res
	end
	local op = node.op
	if depends_on_human(node.lhs) then
		local rhs = eval(node.rhs)
		local lhs
		if op == "+" then -- lhs + rhs = target_value =>
			lhs = target_value - rhs
		elseif op == "-" then -- lhs - rhs = target_value =>
			lhs = target_value + rhs
		elseif op == "*" then -- lhs * rhs = target_value =>
			lhs = target_value / rhs
		elseif op == "/" then -- lhs * rhs = target_value =>
			lhs = target_value * rhs
		end
		res = solve_human(node.lhs, lhs)
	else
		local lhs = eval(node.lhs)
		assert(depends_on_human(node.rhs))
		local rhs
		if op == "+" then -- lhs + rhs = target_value =>
			rhs = target_value - lhs
		elseif op == "-" then -- lhs - rhs = target_value =>
			rhs = lhs - target_value
		elseif op == "*" then -- lhs * rhs = target_value =>
			rhs = target_value / lhs
		elseif op == "/" then -- lhs * rhs = target_value =>
			rhs = target_value / lhs
		end
		res = solve_human(node.rhs, rhs)
	end
	node.res = res
	return res
end

if depends_on_human(nodes.root.lhs) then
	return solve_human(nodes.root.lhs, eval(nodes.root.rhs))
end
assert(depends_on_human(nodes.root.rhs))
return solve_human(nodes.root.rhs, eval(nodes.root.lhs))
