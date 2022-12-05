local lines = ...

local function reverse(t)
	for i = 1, #t / 2 do
		local j = #t + 1 - i
		t[i], t[j] = t[j], t[i]
	end
end

local function map_varg(f, ...)
	if select("#", ...) == 0 then
		return
	end
	return f((...)), map_varg(f, select(2, ...))
end

local stacks = {}
for line in lines do
	if line:find"%d+" then
		for istr in line:gmatch"%d+" do
			local i = tonumber(istr)
			stacks[i] = stacks[i] or {}
		end
		break
	end
	local i = 1
	for j = 1 + #"[", #line - #"]", 1 + #"] [" do
		local char = line:sub(j, j)
		if char >= "A" and char <= "Z" then
			stacks[i] = stacks[i] or {}
			table.insert(stacks[i], char)
		end
		i = i + 1
	end
end

for _, stack in ipairs(stacks) do
	reverse(stack)
end

assert(lines() == "") -- skip blank line

for command in lines do
	local count, src, dst = map_varg(tonumber, command:match"^%s*move%s+(%d+)%s+from%s+(%d+)%s+to%s+(%d+)%s*$")
	local src_stack, dst_stack = stacks[src], stacks[dst]
	for _ = 1, count do
		table.insert(dst_stack, table.remove(src_stack))
	end
end

local tops = {}
for _, stack in ipairs(stacks) do
	table.insert(tops, stack[#stack])
end
return table.concat(tops)
