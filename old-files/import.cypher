// Load the parcel data
create constraint for (p:Property) require p.id is unique

:auto load csv with headers from "file:///parcels.csv" as r
call {
  with r
  merge (p:Property {id: r.ID})
  set
    p.parcel_id = r.PARCEL_ID,
    p.assessment_year = r.ASMNT_YR,
    p.dor_use_code = r.DOR_UC,
    p.n_residential_units = r.NO_RES_UNTS,
    p.just_value = r.JV,
    p.address1 = r.PHY_ADDR1,
    p.address2 = r.PHY_ADDR2,
    p.city = r.PHY_CITY,
    p.zip = r.PHY_ZIPCD,
    p.homestead_exemption = r.HOMESTEAD_EX
} in transactions

create constraint for (pp:Parcel) require l.id is unique

:auto load csv with headers from "file:///parcels.csv" as r
call {
  with r
  merge (pp:Parcel {id: r.parcel_id})
} in transactions

:auto load csv with headers from "file:///parcels.csv" as r
call {
  with r
  match 
    (p:Property {id: r.ID})
    (pp:Parcel {id: r.parcel_id})
  merge (p)-[:HAS_PARCEL_ID]->(pp)
} in transactions


// load owner, owner-address, owner-name relationships

create constraint for (o:Owner) require o.id is unique

:auto load csv with headers from "file:///owners_crosswalk.csv" as r
call {
  with r
  merge(o:Owner {id: r.OWN_ID})
} in transactions

create constraint for (oa:Owner_address) require oa.id is unique

:auto load csv with headers from "file:///owners_addr.csv" as r
call {
  with r
  merge (oa:Owner_address {id: r.OWN_ADDR_ID})
  set
    oa.address = r.OWN_ADDR,
    oa.address1 = r.OWN_ADDR1,
    oa.address2 = r.OWN_ADDR2,
    oa.city = r.OWN_CITY,
    oa.state = r.OWN_STATE,
    oa.zip = r.OWN_ZIPCD,
    oa.addr_is_agent = r.ADDR_IS_AGENT
} in transactions

create constraint for (on:Owner_name) require on.id is unique

:auto load csv with headers from "file:///owners_name.csv" as r
call {
  with r
  merge (on:Owner_name {id: r.OWN_NAME_ID})
  set
    on.name = r.OWN_NAME,
    on.type = r.OWN_TYPE
} in transactions

:auto load csv with headers from "file:///owners_crosswalk.csv" as r
call {
  with r
  match
    (o:Owner {id: r.OWN_ID}),
    (oa:Owner_address {id: r.OWN_ADDR_ID})
  merge (o)-[:HAS_ADDRESS]->(oa)
} in transactions

:auto load csv with headers from "file:///owners_crosswalk.csv" as r
call {
  with r
  match
    (o:Owner {id: r.OWN_ID}),
    (on:Owner_name {id: r.OWN_NAME_ID})
  merge (o)-[:HAS_NAME]->(on)
} in transactions

// :auto load csv with headers from "file:///owners_test.csv" as r
// call {
//   with r
//   merge (o:Owner {id: r.OWN_ID})
//   set
//     o.name = r.OWN_NAME,
// 	o.type = r.OWN_TYPE,
//     o.address = r.OWN_ADDR,
//     o.address1 = r.OWN_ADDR1,
//     o.address2 = r.OWN_ADDR2,
//     o.city = r.OWN_CITY,
//     o.state = r.OWN_STATE,
//     o.zip = r.OWN_ZIPCD,
//     o.addr_is_agent = r.ADDR_IS_AGENT
// } in transactions

// // address relationships
// create constraint for (oa:OwnerAddress) require
// (oa.address, oa.city, oa.addr_is_agent) is unique

// match (o:Owner)
// merge (oa:OwnerAddress {address: o.address, city: o.city, addr_is_agent: o.addr_is_agent})
// merge (o)-[:HAS_ADDRESS]->(oa)

// // name relationships
// match (o:Owner)
// merge (on:OwnerName {name o.name})
// merge (o)-[:HAS_NAME]->(on)

// load the ownership relationships
:auto load csv with headers from "file:///parcels.csv" as r
call {
  with r
  match
    (p:Property {id: r.ID}),
    (o:Owner {id: r.OWN_ID})
  merge (o)-[:OWNS]->(p)
} in transactions

// // create relationships on address
// match (o1:Owner), (o2:Owner)
// where o1.OWN_ADDR = o2.OWN_ADDR and o1.OWN_ID <> o2.OWN_ID and o1.ADDR_IS_AGENT <> TRUE and o2.ADDR_IS_AGENT <> TRUE
// create (o1)-[:HAS_SAME_ADDRESS]->(o2)

// // create owner relationships on name
// match (o1:Owner), (o2:Owner)
// where o1.OWN_NAME = o2.OWN_NAME and o1.OWN_ID <> o2.OWN_ID
// create (o1)-[:HAS_SAME_NAME]->(o2)


// load the corporation and officer listings
create constraint for (c:Corporation) require c.corporation_id is unique

:auto load csv with headers from "file:///filings.csv" as r
call {
  with r
  merge (c:Corporation {corporation_id: r.CORPORATION_NUMBER})
  set
    c.name = r.COR_NAME,
    c.status = r.STATUS,
    c.address1 = r.ADDR1,
    c.address2 = r.ADDR2,
    c.city = r.CITY,
    c.state = r.STATE,
    c.zip = r.ZIP,
    c.mailing_address1 = r.MAIL_ADDR1,
    c.mailing_address2 = r.MAIL_ADDR2,
    c.mailing_city = r.MAIL_CITY,
    c.mailing_zip = r.MAIL_ZIP,
    c.file_date = r.FILE_DATE
} in transactions


create constraint for (co:Officer) require co.id is unique

:auto load csv with headers from "file:///officers.csv" as r
call {
  with r
  merge (co:Officer {id: r.OFF_ID})
  set
    co.title = r.OFFICER_TITLE,
    co.type = r.OFFICER_TYPE,
    co.name = r.OFFICER_NAME,
    co.address = r.OFFICER_ADDRESS,
    co.city = r.OFFICER_CITY,
    co.state = r.OFFICER_STATE
} in transactions

create index for (c:Corporation) on (c.name)
create index for (on:Owner_name) on (on.name)

// create officer to corporation relationship
load csv with headers from "file:///officers.csv" as r
match
  (c:Corporation {corporation_id: r.CORPORATION_NUMBER}),
  (co:Officer {id: r.OFF_ID})
merge (c)-[:HAS_OFFICER]->(co)

// create corporation to owner link
load csv with headers from "file:///crosswalk.csv" as r
match
  (c:Corporation {name: r.SUNBIZ_NAME}),
  (on:Owner_name {name: r.DOR_NAME})
merge (on)-[:IN_SUNBIZ]->(c)