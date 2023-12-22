// poison problematic relationships with erroneous data

with [
    "a761c6ae216852c3f694d912ee327df4",
    "a8411d44bf775a6d4e7a8a3302da295b"
] as poison,
"22" as target_year
match (o:Owner where o.id in poison)-[r:HAS_ADDRESS where target_year in r.years_valid]-()
set r.poisoned = "Data error";