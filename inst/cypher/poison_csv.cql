// poison problems with erroneous data using csv

:auto load csv with headers from "file:///poison.csv" as row
call {
    with row
    match (o:Owner where o.name = row.name and o.address = row.address)-[r:HAS_ADDRESS where row.year in r.years_valid]-()
    set r.poisoned = row.poison_type
} in transactions;