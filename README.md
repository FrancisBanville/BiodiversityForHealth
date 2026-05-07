# BiodiversityForHealth

Data and code repository for the manuscript entitled "Towards the monitoring of One Health action tracks using the indicators of the Kunming-Montreal Global Biodiversity Framework" (Banville et al.).

## Folder organization
- `code/`: contains the R script that imports, cleans, and analyzes the data and generates the figures
- `data/`: contains the dataset and its description
- `figures/`: contains all figures produced by the script and included in the manuscript
- `images/`: contains the logo of the One Health Joint Plan of Actions that is added to certain figures

### Figures

The R script produces the following figures:

- `figures/link_health.png`: Number of indicators of the Kunming-Montreal Global Biodiversity Framework linked with human, animal, plant, and environmental health (Figure 1)
- `figures/usability_all.png`: Number of indicators of the Kunming-Montreal Global Biodiversity Framework that can be used to monitor at least one action in each of the action tracks of the One Health Joint Plan of Action (Figure 2)
- `figures/usability_categories.png`: Proportion of indicators of the Kunming-Montreal Global Biodiversity Framework that can be used to monitor at least one action in each of the action tracks of the One Health Joint Plan of Action, partitioned by the different groups of indicators (Figure 3)
- `figures/usability_GAP.png`: Proportion of indicators of the Kunming-Montreal Global Biodiversity Framework that can be used to monitor at least one action in each of the action tracks of the One Health Joint Plan of Action, partitioned by the different thematic categories of the Global Action Plan on Biodiversity and Health (Figure 4)


## Installation and execution

### Install R
R can be installed following the instructions provided by [Posit](https://posit.co/download/rstudio-desktop). We used R 4.3.2 for this project.

### Execute the R script
Execute the script `code/data_analysis_and_visualization.R` to import the data and reproduce the analyses. 

