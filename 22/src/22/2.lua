local unpack = unpack or table.unpack

local next_line = ...
local grid = {}
local cnt = 0
local min_x_by_y, max_x_by_y = {}, {}
local min_y_by_x = {}
for line in next_line do
	if line == "" then break end
	local y = #grid + 1
	local row = {}
	for x, char in line:gmatch"()(.)" do
		if char ~= " " then
			min_y_by_x[x] = min_y_by_x[x] or y
			min_x_by_y[y], max_x_by_y[y] = min_x_by_y[y] or x, x
			row[x] = char
			cnt = cnt + 1
		end
	end
	grid[y] = row
end

local instr_str = next_line()
local instrs = {}
local i = 1
while i <= #instr_str do
	local c = instr_str:sub(i, i)
	if c:find"[LR]" then
		table.insert(instrs, c)
		i = i + 1
	else
		local _, j, substr = instr_str:find("^(%d+)", i)
		table.insert(instrs, assert(tonumber(substr)))
		i = j + 1
	end
end

local function get(x, y)
	return (grid[y] or {})[x]
end

local right, down, left, up = 0, 1, 2, 3

local dirs = {
	[right] = {1, 0},
	[down] = {0, 1},
	[left] = {-1, 0},
	[up] = {0, -1},
}

local face_size = (cnt / 6)^.5
assert(face_size^2 * 6 == cnt)
local faces = {}
for y = 1, #grid, face_size do
	for x = min_x_by_y[y], max_x_by_y[y], face_size do
		table.insert(faces, {x, y})
	end
end
assert(#faces == 6)

local facing = 0
local y = 1
local x = min_x_by_y[y]
while assert(get(x, y)) ~= "." do
	x = x + 1
end

local function get_face_idx()
	for idx, face in ipairs(faces) do
		if
			x >= face[1]
			and y >= face[2]
			and x < face[1] + face_size
			and y < face[2] + face_size
		then
			return idx
		end
	end
	error"out of bounds"
end

-- HACK Hardcoded wrapping rules: Detect the example by its small size
local wraparound = face_size > 10 and {
	-- Real input
	[1] = {
		[up] = {fidx = 6, dir = right, rev = false},
		[left] = {fidx = 4, dir = right, rev = true},
	},
	[2] = {
		[up] = {fidx = 6, dir = up, rev = false},
		[right] = {fidx = 5, dir = left, rev = true},
		[down] = {fidx = 3, dir = left, rev = false},
	},
	[3] = {
		[left] = {fidx = 4, dir = down, rev = false},
	},
	[4] = {
		[up] = {fidx = 3, dir = right, rev = false},
		[left] = {fidx = 1, dir = right, rev = true},
	},
	[5] = {
		[down] = {fidx = 6, dir = left, rev = false},
	},
	[6] = {}
	} or {
	-- Example input
	[1] = {
		[right] = {fidx = 6, dir = left, rev = true},
		[left] = {fidx = 3, dir = down, rev = false},
		[up] = {fidx = 2, dir = down, rev = true},
	},
	[2] = {
		[left] = {fidx = 6, dir = up, rev = true},
		[down] = {fidx = 5, dir = up, rev = true},
	},
	[3] = {
		[down] = {fidx = 5, dir = right, rev = true},
	},
	[4] = {
		[right] = {fidx = 6, dir = down, rev = true},
	},
	[5] = {},
	[6] = {},
}

for fidx, wrap in ipairs(wraparound) do
	for dir, wrap_to in pairs(wrap) do
		wraparound[wrap_to.fidx][(wrap_to.dir + 2) % 4] = {
			fidx = fidx,
			dir = (dir + 2) % 4,
			rev = wrap_to.rev
		}
	end
end

local function get_wraparound()
	local face_idx = get_face_idx()
	local face = faces[face_idx]
	local wrap_to = assert(wraparound[face_idx][facing], face_idx .. ";" .. facing)
	local to_face = faces[wrap_to.fidx]
	local new_facing = wrap_to.dir

	local fx, fy = unpack(face)
	local tfx, tfy = unpack(to_face)
	local rx, ry = x - fx, y - fy
	if wrap_to.rev then
		rx, ry = face_size - 1 - rx, face_size - 1 - ry
	end
	if facing % 2 ~= new_facing % 2 then
		rx, ry = ry, rx
	end
	local tx, ty = tfx + rx, tfy + ry

	if new_facing == right then
		return new_facing, tfx, ty
	end if new_facing == down then
		return new_facing, tx, tfy
	end if new_facing == left then
		return new_facing, tfx + face_size - 1, ty
	end if new_facing == up then
		return new_facing, tx, tfy + face_size - 1
	end
	error"invalid dir"
end

for _, instr in ipairs(instrs) do
	if instr == "L" then
		facing = (facing - 1) % 4
	elseif instr == "R" then
		facing = (facing + 1) % 4
	else
		assert(type(instr) == "number")
		local dx, dy = unpack(dirs[facing])
		for _ = 1, instr do
			local new_facing, nx, ny = facing, x + dx, y + dy
			::try_next_pos::
			local ntile = get(nx, ny)
			if ntile == "#" then
				break
			end
			if ntile == "." then
				facing, x, y = new_facing, nx, ny
				dx, dy = unpack(dirs[facing])
			else
				assert(not ntile)
				new_facing, nx, ny = get_wraparound()
				assert(get(nx, ny))
				goto try_next_pos
			end
		end
	end
end

return 1e3 * y + 4 * x + facing
