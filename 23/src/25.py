# The number one Haskell's graph library (fgl) seems insufficient; its max-flow algorithm doesn't seem to terminate.
# I'm probably violating some undocumented preconditions imposed by the Edmonds-Karp implementation,
# but I won't guess at what those are when I can solve this much more efficiently and conveniently
# by leveraging the Python ecosystem.

import networkx as nx
G = nx.Graph()
while True:
	try:
		line = input()
		[v, ws] = line.split(": ")
		for w in ws.split(" "):
			G.add_edge(v, w, capacity=1)
	except EOFError:
		break
s = next(iter(G.nodes.keys()))
for t in G.nodes.keys():
	if s == t:
		continue
	cut_value, (S, T) = nx.minimum_cut(G, s, t)
	if cut_value == 3:
		print(len(S) * len(T))
		break
