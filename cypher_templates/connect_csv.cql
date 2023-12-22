// connect_csv.cql
// connect arbitrary owners that become disconnected

:auto load csv with headers from "file:///connect.csv" as row
call {
    with row
    match (o:Owner where o.id = row.id1)
    match (o2:Owner where o2.id = row.id2)
    merge (o)-[r:MANUAL_LINK]->(o2)
    set r.years_valid = case
        when r.years_valid is null then [row.year]
        when not row.year in r.years_valid then r.years_valid + row.year
        else r.years_valid
        end
    set r.on = "owner"
} in transactions;