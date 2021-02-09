# cropProbaTables - Crop probability tables

## General
`cropProbaTables` computing crop probability table for PICSA project.


## Installation

```r
library(devtools)
install_github("rijaf-iri/cropProbaTables")
```

## Quick start

```r
library(cropProbaTables)

# read daily precipitation data
precip_file <- "D:/DATA/DGM2021/precip_daily_1981-2020.csv"
precip_daily <- readCDTStationData(precip_file, sep = ",", missing = "-99")

# read onset data
onset_file <- "D:/DATA/DGM2021/ONSET_data/CDTSTATIONS/Onset_days.txt"
onset <- readCDTStationData(onset_file, sep = " ", missing = "-99")

# read cessation data
cessation_file <- "D:/DATA/DGM2021/CESSATION_data/CDTSTATIONS/Cessation_days.txt"
cessation <- readCDTStationData(cessation_file, sep = " ", missing = "-99")

out <- cropProbaTables(
             onset, cessation, precip_daily,
             planting_dates = c("10/11", "20/11", "01/12", "10/12"),
             crop_water_requirements = c(300, 400, 500),
             crop_growing_season_lengths = c(90, 100, 110, 120),
             minimum_fraction = 0.95
          )

output_directory <- "D:/DATA/DGM2021/CROP_PROBA"
writeProbaTables(out$corp_proba, output_directory)
```
