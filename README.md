# prod-instability
R code and data accompanying **Productive instability of coral reef fisheries after climate-driven regime shifts**. *In review.*

The following R packages were used to analyse data and create figures.

```
install.packages(c("tidyverse", "mgcv", "itsadug", "scales"))
```

*R scripts demonstrate the models fitted to each variable in each main figure, but are not reproducible because datasets could not be provided openly*. 


R scripts for **[analyses:](https://github.com/jpwrobinson/prod-instability/tree/master/analysis)** 

CPUE GAM structures in [1_cpue_gamm.R](analysis/1_cpue_gamm.R) 		

CV-CPUE GAM structures in [2_cvcpue_gamm.R](analysis/2_cvcpue_gamm.R)		

Catches GAM structures in [3_catch_gamm.R](analysis/3_catch_gamm.R)			

Multivariate dispersion of reef benthos from UVC data in [4_benthic_dispersion.R](analysis/4_benthic_dispersion.R)		

Multivariate dispersion of target fish biomass from UVC data in [5_fish_dispersion.R](analysis/5_fish_dispersion.R)

UVC biomass GAM structures in [6_biomass_gamm.R](analysis/6_biomass_gamm.R)

Bootstrap simulation for CPUE lower and upper thresholds in [7_cpue_threshold_analysis.R](analysis/7_cpue_threshold_analysis.R)

General function for scaling and centering covariates before fitting GAMs in [scaling_function.R](analysis/scaling_function.R)


R scripts for **[figures:](https://github.com/jpwrobinson/prod-instability/tree/master/figures)**

[Fig1_CPUE.R](figures/Fig1_CPUE.R)

[Fig2_CVCPUE.R](figures/Fig2_CVCPUE.R)

[Fig3_biomass.R](figures/Fig3_biomass.R)


**[Model predictions](https://github.com/jpwrobinson/prod-instability/tree/master/model-predictions)** underlying results:

Model predictions underlying figures and results. Following folders contain csv (fishery catches) and Rdata (UVC models) files. 

[cpue](model-predictions/cpue)

[cv-cpue](model-predictions/cv-cpue)

[uvc](model-predictions/uvc)

[catches](model-predictions/catches)

File names inside these folders indicate which models are used to generate predictions. FIXS and FIXA may refer to fixed and active traps, respectively. 

[CPUE_quantile_thresholds.Rdata](model-predictions/cpue/CPUE_quantile_thresholds.Rdata) contains simulated upper (h) and lower (l) CPUE quantiles for siganid (ch, cl), mixed species (oh, ol) and lethrinid (caph, capl).
