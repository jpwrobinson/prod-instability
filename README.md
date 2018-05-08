# prod-instability
R code and data accompanying **Productive instability of coral reef fisheries after climate-driven regime shifts**. *In review.*

[analysis/](https://github.com/jpwrobinson/prod-instability/tree/master/analysis)

CPUE GAM structures in 1_cpue_gamm.R 		

CV-CPUE GAM structures in 2_cvcpue_gamm.R			

Multivariate dispersion of reef benthos from UVC data in 3_benthic_dispersion.R		

Multivariate dispersion of target fish biomass from UVC data in4_fish_dispersion.R

UVC biomass GAM structures in 5_biomass_gamm.R

Bootstrap simulation for CPUE lower and upper thresholds in 6_cpue_threshold_analysis.R

General function for scaling and centering covariates before fitting GAMs in scaling_function.R

[figures/](https://github.com/jpwrobinson/prod-instability/tree/master/figures)

Fig1_CPUE.R

Fig2_CVCPUE.R

Fig3_biomass.R 

[model-predictions/](https://github.com/jpwrobinson/prod-instability/tree/master/model-predictions)

Model predictions underlying figures and results. Following folders contain csv (fishery catches) and Rdata (UVC models) files. 

cpue

cv-cpue

uvc

yield

File names inside these folders indicate which models are used to generate predictions. FIXS and FIXA may refer to fixed and active traps, respectively. 

CPUE_quantile_thresholds.Rdata contains simulated upper (h) and lower (l) CPUE quantiles for siganid (ch, cl), mixed species (oh, ol) and lethrinid (caph, capl).