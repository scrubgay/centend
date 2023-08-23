    :auto load csv with headers from "file:///book1.csv" as r
    call {
        with r
        merge (owner:owner {id: r.ID})
        merge (address:address {address: r.Address})
        merge (name:name {name: r.Owner})
        merge (owner)-[:HAS_ADDRESS]-(address)
        merge (owner)-[:HAS_NAME]-(name)
    } in transactions