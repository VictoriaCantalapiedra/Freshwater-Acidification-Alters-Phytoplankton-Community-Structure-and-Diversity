# Figure code

R scripts used to produce the figures in Cantalapiedra Mateo et al. (2026, submitted).
All scripts are released under the MIT licence (see `LICENSE_MIT` in this folder).

## Scripts

| Script | What it produces | Output file(s) |
|---|---|---|
| `Donut_plot_pH.R` | Donut plots of the responses of each algal taxon to reduced pH, by response variable | `measured_responses_decreased_pH.pdf` / `.tiff` |
| `Donut_plot_DIC.R` | As above, for increased DIC | `measured_responses_increased_DIC.pdf` / `.tiff` |
| `Donut_plot_Community_composition_Diversity.R` | Donut plots of the effects of pH, DIC and both combined on community composition and diversity | `community_composition_donut.pdf`, `diversity_donut.pdf`, `composition_diversity_figure.pdf` |
| `Donut_plot_Abundances.R` | Donut plots of changes in abundance of the main algal groups under increased DIC and reduced pH, plus a matrix of filamentous/picoplanktonic forms | `abundance_donuts_matrix.pdf` |
| `Field_studies_map.R` | World map of the number of field surveys per country, with a Europe inset | `All_studies_map_with_inset.png` |

## Data
The count data used in each script are embedded in the script, so no input files are needed.
The same counts are provided as tables in `../Data/` (e.g. `Cantalapiedra_et_al_Donut_plot_pH.csv`)
and were derived from the studies listed in `../Data/Cantalapiedra_et_al_full_text_screening.csv`.
The map script's country counts were extracted by the authors from the included field studies.
Country boundaries are from Natural Earth (via the `rnaturalearth` package).

## Running the scripts
1. Open R and run each script **in full** from a fresh session (Session > Restart R, then Source).
   Running only part of a script can reuse objects left over from a previous one and produce the wrong figure.
2. Output files are written to the current working directory.

## Required packages
`ggplot2`, `dplyr`, `tidyr`, `tibble`, `ggnewscale`, `patchwork` (all donut figures; `tidyverse` covers the first four),
plus `sf`, `rnaturalearth`, `rnaturalearthdata`, `cowplot` and `lwgeom` for the map.
Each script has a commented-out `install.packages()` line at the top.
