// connect.cql
// connect arbitrary connections that become disconnected
// you could probably make this operatable on csv

with "19" as year
match (on:OwnerName where on.name = "HERITAGE VI SUB I LLC")
match (on2:OwnerName where on2.name = "HERITAGE VII HOLDINGS LLC")
merge (on)-[r:MANUAL_LINK]->(on2)
set r.years_valid = case
    when r.years_valid is null then [year]
    when not year in r.years_valid then r.years_valid + year
    else r.years_valid
    end
set r.on = "name";

with "19" as year
match (o:Owner where o.id = "72723b7f71f6bffe845a6dc766d4ee22")
match (o2:Owner where o2.id = "001f57780d51cc86e51637eb1d12e03b")
merge (o)-[r:MANUAL_LINK]->(o2)
set r.years_valid = case
    when r.years_valid is null then [year]
    when not year in r.years_valid then r.years_valid + year
    else r.years_valid
    end
set r.on = "owner";

with "19" as year
match (o:Owner where o.id = "911fa04980aef35a79f360a8a69bf583")
match (o2:Owner where o2.id = "2cecb394da0f82bfbe6e9da5e54fc96c")
merge (o)-[r:MANUAL_LINK]->(o2)
set r.years_valid = case
    when r.years_valid is null then [year]
    when not year in r.years_valid then r.years_valid + r.parcel_year
    else r.years_valid
    end
set r.on = "owner";

with "19" as year
match (o:Owner where o.id = "911fa04980aef35a79f360a8a69bf583")
match (o2:Owner where o2.id = "2cecb394da0f82bfbe6e9da5e54fc96c")
merge (o)-[r:MANUAL_LINK]->(o2)
set r.years_valid = case
    when r.years_valid is null then [year]
    when not year in r.years_valid then r.years_valid + year
    else r.years_valid
    end
set r.on = "owner";


with "19" as year
match (o:Owner where o.id = "911fa04980aef35a79f360a8a69bf583")
match (o2:Owner where o2.id = "f24c2d162e8496dc298d545ee016e166")
merge (o)-[r:MANUAL_LINK]->(o2)
set r.years_valid = case
    when r.years_valid is null then [year]
    when not year in r.years_valid then r.years_valid + year
    else r.years_valid
    end
set r.on = "owner";