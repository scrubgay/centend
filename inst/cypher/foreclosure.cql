create constraint fc_uniq for (fc:Foreclosure) require fc.id is unique;
create constraint td_uniq for (td:TaxDeed) require td.id is unique;

:auto load csv with headers from "file:///duval_rfc.csv" as r
call {
    with r
    merge (fc:Foreclosure {id: r.auction_id})
    set fc.case = r.case
    set fc.date = r.auction_datetime
    set fc.auction_status = r.auction_status
    set fc.soldto = r.auction_soldto
    set fc.parcel_id = r.PARCELNO

    with r, fc
    match (p:Parcel {id: r.PARCELNO})
    merge (p)-[:HAS_FORECLOSURE_AUCTION]->(fc)
    set p.foreclosed = 1
} in transactions;

:auto load csv with headers from "file:///duval_rtd.csv" as r
call {
    with r
    merge (td:TaxDeed {id: r.auction_id})
    set td.case = r.case
    set td.date = r.auction_datetime
    set td.auction_status = r.auction_status
    set td.soldto = r.auction_soldto
    set td.parcel_id = r.PARCELNO

    with r, td
    match (p:Parcel {id: r.PARCELNO})
    merge (p)-[:HAS_TAXDEED_AUCTION]->(td)
    set p.taxdelinquent = 1
} in transactions;