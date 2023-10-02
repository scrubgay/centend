// evictions

// create constraints
create constraint evic_unique for (e:Eviction) require e.case_id is unique;
create constraint plaint_unique for (pL:Plaintiff) require pL.id is unique;
create constraint plaintName_unique for (pN:PlaintiffName) require pN.name is unique;
create constraint plaintAddr_unique for (pA:PlaintiffAddress) require pA.address is unique;

// load eviction data
:auto load csv with headers from "file:///evictions.csv" as r
call {
    with r
    merge (e:Eviction {case_id: r.xcasenum})
    set e.filing_date = r.xfiledate
    set e.court = r.xcourtco

    merge (pL:Plaintiff {id: r.plaintiff_id})
    merge (e)-[:HAS_PLAINTIFF]->(pL)

    with r, pL, r.xplaintiff as xpln, e
    where xpln is not null
    set pL.name = xpln
    merge (pN:PlaintiffName {name: xpln})
    merge (pL)-[:HAS_NAME]->(pN)

    with r, pL, r.xplaintiff_address as xpla, e
    where xpla is not null
    set pL.address = xpla
    merge (pA:PlaintiffAddress {address: xpla})
    merge (pL)-[:HAS_ADDRESS]->(pA)

    with r, e
    create (df:Defendant {id: r.defendant_id})
    create (e)-[:HAS_DEFENDANT]->(df)
    set df.primary = r.include
    set df.commercial = r.commercial

    with r, df, r.xdefendant as xdefn, e
    where xdefn is not null
    set df.name = xdefn

    with r, df, r.xdefendant_address as xdefa, e
    where xdefa is not null
    set df.address = r.xdefa

    with r, r.parcel_id as xpa
    where xpa is not null
    match (e:Eviction {case_id: r.xcasenum})
    match (p:Property {id: r.property_id})
    merge (e)-[F:FILED_AT]->(p)
    set F.accuracy_type = r.new_accuracy_type

} in transactions;

:auto load csv with headers from "file:///evictions_parcels.csv" as r
call {
    with r
    match (e:Eviction where e.case_id = r.xcasenum)
    match (p:Property where p.id = r.property_id)
    merge (e)-[F:FILED_AT]->(p)
    set F.accuracy_type = r.new_accuracy_type
} in transactions