# NT_caribou

## Getting started

### Getting the code

All modules are written in R and all model code was developed collaboratively using GitHub (<https://github.com>), with each module contained in its own git repository (see below).
Code that is shared among modules was bundled into R packages, and hosted in open git repositories.
All package code is automatically and regularly tested using cross-platform continuous integration frameworks to ensure the code is reliable and free of errors.

```bash
git clone --recurse-submodules -j8 https://github.com/FOR-CAST/NT_caribou
```

**NOTE:** you will need access to each of the following repositories:

#### Modules

##### caribou

- [PredictiveEcology/caribouNT_studyArea](https://github.com/PredictiveEcology/caribouNT_studyArea)
- [tati-micheletti/caribouPopGrowthModel](https://github.com/tati-micheletti/caribouPopGrowthModel)
- [tati-micheletti/caribouRSF_NT](https://github.com/tati-micheletti/caribouRSF_NT)

##### climate data

- [PredictiveEcology/canClimateData](https://github.com/PredictiveEcology/canClimateData)

##### LandR Biomass

- [PredictiveEcology/Biomass_borealDataPrep](https://github.com/PredictiveEcology/Biomass_borealDataPrep)
- [PredictiveEcology/Biomass_core](https://github.com/PredictiveEcology/Biomass_core)
- [PredictiveEcology/Biomass_regeneration](https://github.com/PredictiveEcology/Biomass_regeneration)
- [PredictiveEcology/Biomass_speciesData](https://github.com/PredictiveEcology/Biomass_speciesData)
- [PredictiveEcology/Biomass_speciesFactorial](https://github.com/PredictiveEcology/Biomass_speciesFactorial)
- [PredictiveEcology/Biomass_speciesParameters](https://github.com/PredictiveEcology/Biomass_speciesParameters)
- [ianmseddy/gmcsDataPrep](https://github.com/ianmseddy/gmcsDataPrep)

##### fireSense

- [PredictiveEcology/fireSense](https://github.com/PredictiveEcology/fireSense)
- [PredictiveEcology/fireSense_EscapeFit](https://github.com/PredictiveEcology/fireSense_EscapeFit)
- [PredictiveEcology/fireSense_EscapePredict](https://github.com/PredictiveEcology/fireSense_EscapePredict)
- [PredictiveEcology/fireSense_IgnitionFit](https://github.com/PredictiveEcology/fireSense_IgnitionFit)
- [PredictiveEcology/fireSense_IgnitionPredict](https://github.com/PredictiveEcology/fireSense_IgnitionPredict)
- [PredictiveEcology/fireSense_SpreadFit](https://github.com/PredictiveEcology/fireSense_SpreadFit)
- [PredictiveEcology/fireSense_SpreadPredict](https://github.com/PredictiveEcology/fireSense_SpreadPredict)
- [PredictiveEcology/fireSense_dataPrepFit](https://github.com/PredictiveEcology/fireSense_dataPrepFit)
- [PredictiveEcology/fireSense_dataPrepPredict](https://github.com/PredictiveEcology/fireSense_dataPrepPredict)

### Prerequisites

First, verify your installation of package development tools by running:

```{r has_devel}
install.packages('devtools')
devtools::has_devel()
```

The code is mostly self-sufficient: additional packages than those below are needed, but will be installed automatically.
See `01-packages.R` to see which additional packages will be used.

