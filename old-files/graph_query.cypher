// create the projected graph

match (source where source.addr_is_agent <> "TRUE" or source.addr_is_agent IS NULL)--(target where target.addr_is_agent <> "TRUE" or target.addr_is_agent IS NULL)
with gds.graph.project(
    'ownership',
    source,
    target
    ) as g
return g.graphName, g.nodeCount, g.relationshipCount;

// run wcc

CALL gds.wcc.stream('ownership')
YIELD nodeId, componentId
RETURN gds.util.asNode(nodeId), componentId;