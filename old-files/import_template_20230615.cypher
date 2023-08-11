// Load the parcel data
create constraint for (p:Property) require p.id is unique;

:auto load csv with headers from "file:///src/export_22_20230615/parcels.csv" as r
call {
  with r
  merge (p:Property {id: r.id})
  set
    p.parcel_id = r.parcel_id,
    p.county = r.county,
    p.year = p.year,
    p.dor_use_code = r.dor_use_code,
    p.address1 = r.physical_address,
    p.address2 = r.physical_address2,
    p.city = r.physical_city,
    p.zip = r.physical_zip,
    p.homestead = r.homestead
} in transactions;

create constraint for (pp:Parcel) require pp.id is unique;

:auto load csv with headers from "file:///src/export_22_20230615/parcels.csv" as r
call {
  with r
  merge (pp:Parcel {id: r.parcel_id})
} in transactions;

:auto load csv with headers from "file:///src/export_22_20230615/parcels.csv" as r
call {
  with r
  match 
    (p:Property {id: r.id}),
    (l:Parcel {id: r.parcel_id})
  merge (p)-[:HAS_PARCEL_ID]->(l)
} in transactions;

// load owner, owner-address, owner-name relationships

create constraint for (o:Owner) require o.id is unique;

:auto load csv with headers from "file:///src/export_22_20230615/owners_crosswalk.csv" as r
call {
  with r
  merge(o:Owner {id: r.id})
} in transactions;

create constraint for (oa:Owner_address) require oa.id is unique

:auto load csv with headers from "file:///src/export_22_20230615/owners_addr.csv" as r
call {
  with r
  merge (oa:Owner_address {id: r.id})
  set
    oa.address = r.owner_address,
    oa.city = r.owner_city,
    oa.state = r.owner_state,
    oa.zip = r.owner_zip,
    oa.addr_is_agent = r.ADDR_IS_AGENT
} in transactions;

create constraint for (on:Owner_name) require on.id is unique;

:auto load csv with headers from "file:///src/export_22_20230615/owners_name.csv" as r
call {
  with r
  merge (on:Owner_name {id: r.id})
  set
    on.name = r.owner_name,
    on.type = r.owner_type
} in transactions;

:auto load csv with headers from "file:///src/export_22_20230615/owners_crosswalk.csv" as r
call {
  with r
  match
    (o:Owner {id: r.id}),
    (oa:Owner_address {id: r.owner_address_id})
  merge (o)-[:HAS_ADDRESS]->(oa)
} in transactions;

:auto load csv with headers from "file:///src/export_22_20230615/owners_crosswalk.csv" as r
call {
  with r
  match
    (o:Owner {id: r.id}),
    (on:Owner_name {id: r.owner_name_id})
  merge (o)-[:HAS_NAME]->(on)
} in transactions;

// load the ownership relationships
:auto load csv with headers from "file:///src/export_22_20230615/parcels.csv" as r
call {
  with r
  match
    (p:Property {id: r.id}),
    (o:Owner {id: r.owner_id})
  merge (o)-[:OWNS]->(p)
} in transactions;


// load the corporation and officer listings
create constraint for (c:Corporation) require c.corporation_id is unique;

:auto load csv with headers from "file:///src/export_22_20230615/filings.csv" as r
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
} in transactions;


create constraint for (co:Officer) require co.id is unique;

:auto load csv with headers from "file:///src/export_22_20230615/officers.csv" as r
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
} in transactions;

create index for (c:Corporation) on (c.name);
create index for (on:Owner_name) on (on.name);

// create officer to corporation relationship
load csv with headers from "file:///src/export_22_20230615/officers.csv" as r
match
  (c:Corporation {corporation_id: r.CORPORATION_NUMBER}),
  (co:Officer {id: r.OFF_ID})
merge (c)-[:HAS_OFFICER]->(co)

// create corporation to owner link
load csv with headers from "file:///src/export_22_20230615/crosswalk.csv" as r
match
  (c:Corporation {name: r.SUNBIZ_NAME}),
  (on:Owner_name {name: r.DOR_NAME})
merge (on)-[:IN_SUNBIZ]->(c)