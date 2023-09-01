// create constraints first

create constraint property_uniq for (py:Property) require py.id is unique;
create constraint parcel_uniq for (p:Parcel) require p.id is unique;
create constraint owner_unique for (o:Owner) require o.id is unique;
create constraint owner_addr_unique for (oa:OwnerAddress) require oa.address_full is unique;
create constraint owner_name_unique for (on:OwnerName) require on.name is unique;

// load the parcel data
:auto load csv with headers from "file:///parcels.csv" as r
call {
    with r
    merge (py:Property {id: r.py_id})
    set py.parcel_id = r.parcel_id
    set py.year = r.parcel_year
    merge (p:Parcel {id: r.parcel_id})
    merge (py)-[:HAS_PARCEL]->(p)
    merge (o:Owner {id: r.owner_id})
    merge (o)-[:OWNS]-(py)
    merge (oa:OwnerAddress {address_full: r.owner_address_full})
    set oa.address = r.owner_address
    set oa.city = r.owner_city
    set oa.agent = r.agent
    merge (on:OwnerName {name: r.owner_name_adj})
    set on.name_unadj = r.owner_name
    set on.type = r.owner_type
    merge (o)-[HA:HAS_ADDRESS]->(oa)
    set HA.years_valid = case
        when HA.years_valid is null then [r.parcel_year]
        when not r.parcel_year in HA.years_valid then HA.years_valid + r.parcel_year
        else HA.years_valid
        end
    merge (o)-[HN:HAS_NAME]->(on)
    set HN.years_valid = case
        when HN.years_valid is null then [r.parcel_year]
        when not r.parcel_year in HN.years_valid then HN.years_valid + r.parcel_year
        else HN.years_valid
        end
} in transactions;

// create constraints for sunbiz stuff
create constraint corp_uniq for (c:Corp) require c.id is unique;
create constraint officer_uniq for (of:Officer) require of.id is unique;

// load the filing data first
:auto load csv with headers from "file:///filings.csv" as r
call {
    with r
    merge (c:Corp {id: r.corp_id})
    with r, c
    match (on:OwnerName) where on.name = r.owner_name
    merge (on)-[:MATCHES_FILING]->(c)
    set c.name = r.corp_name
} in transactions;

// then the officer data
:auto load csv with headers from "file:///officers.csv" as r
call {
    with r
    merge (of:Officer {id: r.off_id})
    set of.name = r.officer_name
    set of.address = r.officer_address
    set of.city = r.officer_city
    with r, of
    match (c:Corp) where c.id = r.corp_id
    merge (c)-[:HAS_OFFICER]->(of)
} in transactions;