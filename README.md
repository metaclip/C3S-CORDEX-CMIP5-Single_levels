# C3S-CORDEX-CMIP5-Single_levels — Provenance (JSON‑LD)

This repository implements and stores a **complete provenance description** for **CORDEX regional climate model (RCM) data on single levels** used in the **Copernicus C3S** context. The provenance is encoded in **JSON‑LD**, aligned with **W3C PROV‑O**, and follows the METACLIP semantic framework [[1]](https://github.com/metaclip)

> **Scope**  
> Single‑level CORDEX RCM datasets as catalogued in the Copernicus Climate Data Store (CDS). (General background on the CDS entry for CORDEX single levels is available here) [[2]](https://cds.climate.copernicus.eu/datasets/projections-cordex-domains-single-levels?tab=overview)
>
> 
> **Note.** This repository intentionally **does not** bundle the ontologies; they are maintained separately under the METACLIP organization. [[3]](https://github.com/metaclip/ontologies/tree/devel)
---

## Repository structure

```
C3S-CORDEX-CMIP5-Single_levels/
├── R/                    # R scripts to build the provenance graphs (JSON-LD)
├── inst/                 # A template for JSON-LD generation containing direct ontology imports
├── json_ld/
│   └── EUR-11/           # Output directory with JSON-LD graphs (EUR-11)
|   └── CORDEX-CORE/      # Output directory with JSON-LD graphs (CORDEX-CORE 'C3S Grand Ensemble')
├── master_files/         # Master CSVs - harmonized metadata tables (inventory + model component metadata)
├── LICENSE               # GPL-3.0
└── README.md
```
