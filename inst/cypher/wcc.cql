// create the graph projection
with "<<year>>" as assessmentYear, "<<outputName>>" as graphName
match
(n:!Parcel where
    (n.year = assessmentYear or n.year is NULL) and
    (n.agent = "FALSE" or n.agent is NULL))
-[r where
    (assessmentYear in r.years_valid or r.years_valid is NULL) and
    (r.poisoned is NULL)]-
(m:!Parcel where
    (m.year = assessmentYear or m.year is NULL) and
    (m.agent = "FALSE" or m.agent is NULL))
with gds.graph.project(graphName, n, m) as g
return g.graphName as graph, g.nodeCount AS nodes, g.relationshipCount AS rels;

// then call wcc in stream mode

CALL apoc.export.json.query(
    "call gds.wcc.stream('<<outputName>>') yield nodeId, componentId return gds.util.asNode(nodeId) as node, componentId ORDER BY componentId;",
    "<<outputName>>.json"
);
