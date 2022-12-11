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

local hx, hy = 1, 1
local tx, ty = 1, 1
visit(tx, ty)

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
		hx, hy = hx + mx, hy + my
		local dx, dy = hx - tx, hy - ty
		if math.max(math.abs(dx), math.abs(dy)) >= 2 then
			tx, ty = tx + sign(dx), ty + sign(dy)
			visit(tx, ty)
		end
	end
end

local count = 0
for _, col in pairs(visited) do
	for _ in pairs(col) do
		count = count + 1
	end
end

return count
