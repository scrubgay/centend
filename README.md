# centend

R-Cypher library for graphing relationships between properties and owners

## Description

```centend``` is a library written in R and Cypher to investigate property ownership using parcel and business registry data. It provides an answer to the LLC problem in property ownership: rental and commercial property owners in the United States structure their ownership using multiple subsidiaries. It is often not readily apparent that these subsidiaries are related to each other -- thus it's difficult to understand who owns what, and specifically *what all* is owned by a single entity.

This library attempts to relate these subsidiaries to each other by relating entities based on shared names, addresses, and officers. It transforms tabular parcel data into a graph data structure by extracting parcel, owner, owner address, and owner name nodes from the parcel data, and corporation filings, officers, and agent addresses from business registry data. Properties and owners become connected directly or indirectly (through intermediary paths through nodes). Part of the transformation, including normalization, is carried out in R; the graph data structure is stored in Neo4j.

![centend data model](/media/data_model.png)

By design, ```centend``` cannot name the "parent" of a linked subsidiary group, mostly because those data are neither present in the business registry or parcel data. Once the data creates a linked group of subsidiaries, you can use names or addresses to look up the ultimate parent; ideal sources include SEC EDGAR filings for publicly traded companies, company websites, or Dun and Bradstreet corporate family data, if you have an institution that allows access to those.

## Known problems and limitations

As of November 2023, ```centend``` is hugely a work in progress and unstable - consider it not even version 0.00001. I've only tested it on Florida property appraiser data, but even then some of the underlying code has workarounds that don't transfer to other datasets. The goal is to make sure it works with at least common United States-wide parcel datasets like Regrid or CoreLogic.

The data model assumes that if corporations share addresses that aren't agent addresses (that is, the businesses are linked to an address that is a known corporate filing agency), then they are related. This may not be true always, for example Landis is a property owner present in Jacksonville, FL that has the same New York address as an unrelated company. Similarly, it also assumes that if they don't share an address, name, or officer they are not related. This is problematic where the data are small or limited - there may not be enough data to link actually shared entities together. The agent address discount may also be aggressive - if the business registry data lack adequate officer information, related entities may not be reconnected. This was the case for Heritage Holdings in Jacksonville, FL. Entities can be manually disconnected and reconnected using ```poison_csv.cql``` and ```connect_csv.cql```, queries that either label existing relationships as "poisoned" and discounted from connection, or create custom labelled relationships.

## Roadmap

Below is a rough sketch of features I want to add to ```centend``` (no immediate timeline):

  1. stabilize the library's core functions, test on Regrid, CoreLogic, and other publicly accessible parcel/registry data
    - at this point, I'll be able to label the library with a "version 0.1" or similar beta quality.
  1. implement OpenRefine-esque data cleaning/pre-clustering techniques used by Brian An in his 2023 paper (this will cluster misspellings together to hopefully find cleaner name and address results)
  1. integrate Cypher queries in R using ```neo4r```, with the goal of solely using R to interface with the analysis
  1. design a method to integrate evictions in the analysis (see ```evictions.cql```)

## Libraries

```centend``` relies on the following libraries:

  - R
    - tidyverse (mostly dplyr, readr, tidyr, stringr, and purrr)
    - stringdist
    - stringi
  - Neo4j
    - APOC (for exporting results)
    - Graph Data Science

## How to install

*crickets*

## Read more

You can read more about results taken from this analysis in my Cityscape publication, *Who owns our homes: Methods for analyzing property ownership*, to be published in Winter 2024.