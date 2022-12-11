local function sign(x)
	return (x < 0 and -1)
		or (x > 0 and 1)
		or 0 -- no need to handle NaN
end

local visited = {}
local function visit(x, y)
	visited[x] = visited[x] or {}
	visited[x][y] = true
end

local rx, ry = {}, {}
for i = 1, 10 do rx[i], ry[i] = 1, 1 end
visit(rx[1], ry[1])

for move in ... do
	local letter, steps = move:match("([RLUD])%s+(%d+)")
	local mx, my = 0, 0
	if letter == "R" then
		mx = 1
	elseif letter == "L" then
		mx = -1
	elseif letter == "U" then
		my = 1
	elseif letter == "D" then
		my = -1
	end
	for _ = 1, steps do
		rx[1], ry[1] = rx[1] + mx, ry[1] + my
		for i = 2, #rx do
			local dx, dy = rx[i-1] - rx[i], ry[i-1] - ry[i]
			if math.max(math.abs(dx), math.abs(dy)) >= 2 then
				rx[i], ry[i] = rx[i] + sign(dx), ry[i] + sign(dy)
			end
		end
		visit(rx[#rx], ry[#ry])
	end
end

local count = 0
for _, col in pairs(visited) do
	for _ in pairs(col) do
		count = count + 1
	end
end

return count
