local mapping = {
	X = "A",
	Y = "B",
	Z = "C",
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

local score = 0
for line in ... do
	local opponent_shape, shape = line:match"([ABC]) ([XYZ])"
	shape = mapping[shape]
	assert(opponent_shape)
	score = score + shape_scores[shape]
		+ ((shape == opponent_shape and 3) -- draw
		  or (shape_beats[shape] == opponent_shape and 6) -- win
		  or 0) -- loss
end
return score
