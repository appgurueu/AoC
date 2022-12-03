local action_scores = {
	X = 0,
	Y = 3,
	Z = 6,
}

local shape_scores = {
	A = 1,
	B = 2,
	C = 3,
}

local shape_beats = {
	A = "C", -- rock beats scissors
	B = "A", -- paper beats rock
	C = "B", -- scissors beat paper
}

local shape_beaten = {}
for shape, beaten_shape in pairs(shape_beats) do
	shape_beaten[beaten_shape] = shape
end

local score = 0
for line in ... do
	local opponent_shape, action = line:match"([ABC]) ([XYZ])"
	assert(opponent_shape)
	local shape
	if action == "X" then -- loss
		shape = shape_beats[opponent_shape]
	elseif action == "Y" then -- draw
		shape = opponent_shape
	elseif action == "Z" then -- win
		shape = shape_beaten[opponent_shape]
	end
	score = score + shape_scores[shape] + action_scores[action]
end
return score
