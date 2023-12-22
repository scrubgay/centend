// create general constraints
create constraint property_uniq for (py:Property) require (py.id, py.year) is unique;
create constraint parcel_uniq for (p:Parcel) require p.id is unique;
create constraint owner_unique for (o:Owner) require (o.address, o.city, o.name) is unique;
create constraint owner_addr_unique for (oa:OwnerAddress) require (oa.address, oa.city) is unique;
create constraint owner_name_unique for (on:OwnerName) require on.name is unique;

// create constraints for sunbiz stuff
create constraint corp_uniq for (c:Corp) require c.id is unique;
create constraint officer_uniq for (of:Officer) require (of.name, of.address, of.city) is unique;

// load the parcel data
:auto load csv with headers from "file:///<<parcels_file>>" as r
call {
    with r
    merge (py:Property {id: r.parcel_id, year: r.parcel_year})
    set py.homestead = r.homestead,
    py.use = r.dor_uc,
    py.year_built = r.eff_yr_blt,
    py.units = r.residential_units,
    py.county = r.county,
    py.address = r.address,
    py.address2 = r.address2,
    py.city = r.city_from_dor,
    py.zip = r.zip

    merge (p:Parcel {id: r.parcel_id})
    merge (py)-[:HAS_PARCEL]->(p)

    merge (o:Owner {address: r.owner_address, city: r.owner_city, name: r.owner_name_adj})
    set o.support = case
        when o.support is null then 1
        else o.support + 1
        end
    merge (o)-[:OWNS]-(py)

    merge (oa:OwnerAddress {address: r.owner_address, city: r.owner_city})
    set oa.state = r.owner_state,
    oa.agent = r.agent,
    oa.support = case
        when oa.support is null then 1
        else oa.support + 1
        end

    merge (on:OwnerName {name: r.owner_name_adj})
    set on.name_unadj = r.owner_name,
    on.type = r.owner_type,
    on.support = case
        when on.support is null then 1
        else on.support + 1
        end

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

// find entities connected to trust custodians and disconnect them
match p = ((:OwnerName {type: "Trust custodian"})-[:HAS_NAME|HAS_ADDRESS*1..10]-(:Owner|OwnerName|OwnerAddress))
unwind relationships(p) as r
set r.poisoned = "Trust custodian";

// find entities connected to hidden/unknown and disconnect them
match p = ((:OwnerName {type: "Hidden or unknown"})-[:HAS_NAME|HAS_ADDRESS*1..10]-(:Owner|OwnerName|OwnerAddress))
unwind relationships(p) as r
set r.poisoned = "Hidden or unknown";

// load the filing data first
:auto load csv with headers from "file:///<<filings_file>>" as r
call {
    with r
    merge (c:Corp {id: r.corp_id})
    with r, c
    match (on:OwnerName) where on.name = r.owner_name
    merge (on)-[:MATCHES_FILING]->(c)
    set c.name = r.corp_name
} in transactions;

// then the officer data
:auto load csv with headers from "file:///<<officers_file>>" as r
call {
    with r
    merge (of:Officer {name: r.OFFICER_NAME, address: r.OFFICER_ADDRESS, city: r.OFFICER_CITY})

    with r, of
    match (c:Corp) where c.id = r.corp_id
    merge (c)-[:HAS_OFFICER]->(of)
} in transactions;
