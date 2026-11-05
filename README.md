# Freshwater Acidification Alters Phytoplankton Community Structure and Diversity: A Systematic Review

Data and code accompanying Cantalapiedra Mateo et al. (2026, submitted).

## Literature search
Scopus (Victoria University of Wellington subscription) was searched on 28 August 2026 using:

`(Alga* OR Cyanobacteria* OR Microalgae) AND (Acidif* OR CO2 OR "Carbon Dioxide" OR pH) AND NOT (Ocean* OR Marine OR Biofuel OR Bioenerg*)`

- **Fields searched:** title, abstract. 
- **Filters:** none. 
- **Records retrieved:** 4,136. 
- **Records remaining after removing duplicates:** 4,118. 
- **Studies read in full:** 238. Cross-referencing with Brown et al., 2020 and Hasler et al., 2016 resulted in an additional 17 and 8 studies, respectively (**total =263**). Of these, **218 studies** provided information suitable for further analysis.

The Scopus search export is **not** included in this repository because of database licensing terms.

## Contents
| File | Description |
|---|---|
| `Data/Cantalapiedra_et_al_full_text_screening.csv` | Studies that reached full-text reading, with the inclusion decision, study type, algal phyla and species. The "Included in the analysis" column records the final decision (Yes/No) and "Classification" groups only the included studies (Yes) into their appropriate categories (Growth & Physiology / Community composition). "Source" shows whether a study came from the Scopus search or from cross-referencing with Brown et al. (2020) or Hasler et al. (2016).|
| `Data/Cantalapiedra_et_al_Donut_plot_DIC.csv` | Counts of positive (+), negative (-) and null responses to DIC addition, by taxon and response variable |
| `Data/Cantalapiedra_et_al_Donut_plot_pH.csv` | As above, for pH |
| `Data/Cantalapiedra_et_al_Community_composition.csv` | Counts used for the community composition/diversity and abundance plots |
| `Figure_code/` | R scripts used to produce the manuscript figures (see `Figure_code/README.md`)|

## Licensing

Different components of the repository are made available under different licences:

- Original data and documentation created by the authors (`Data/` folder): [CC BY 4.0](Data/LICENSE_CC)
- R scripts (`Figure_code/` folder): [MIT](Figure_code/LICENSE_MIT)

Bibliographic details of published papers (authors, year, journal, title) are included only to identify them. They remain the property of their respective authors and publishers and are not covered by these licences. The Scopus export is not included.

## Citation
Cantalapiedra Mateo et al. (2026), submitted.
Data and code archived at Zenodo: https://doi.org/10.5281/zenodo.23988465
