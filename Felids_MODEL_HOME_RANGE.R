#Space-use factors in Felids

#MODELLING HOME RANGE

# quick data read
setwd("C:/Users/Microsoft/Desktop/data_and_codes_HRfelids")
library(openxlsx)
library(dplyr)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(ggplot2)
library(car)
library(corrplot)
library(ggeffects)
library(tidyr) 
library(stringr)
HR_WHOLE.adult_scaled<-read.xlsx("Felids_home_range_data_species_2025_05_02")

# correlation plot

exp_num_var<-c("ele","npp","fr","cr","ps","hfi","rd","hpd","log_abm_sex","log_nloc")
HR_WHOLE_unique <- HR_WHOLE.adult_scaled %>%
  distinct(key, .keep_all = TRUE)

mycov.num <- HR_WHOLE_unique[,exp_num_var]
cor.mat <- cor(mycov.num,method="spearman")
display_names <- c(
  ele = "ELEVATION",
  npp = "NET PRIMARY PRODUCTIVITY",
  fr = "FELID RICHNESS",
  cr = "CROPLAND AREA",
  ps = "PASTURE AREA",
  hfi = "HUMAN FOOTPRINT INDEX",
  rd = "ROAD DENSITY",
  hpd = "HUMAN POP DENSITY",
  log_abm_sex = "SPECIES ADULT BODY MASS",
  log_nloc = "MEAN NB OF LOCATIONS"
)
colnames(cor.mat) <- display_names
rownames(cor.mat) <- display_names

#ELE = ELEVATION
#NPP= NET PRIMARY PRODUCTIVITY
#FR = FELID RICHNESS
#CR = CROPLAND AREA
#PS = PASTURE AREA
#HFI = HUMAN FOOTPRINT INDEX
#RD = ROAD DENSITY
#HPD = HUMAN POP DENSITY
#ABM = LOG SPECIES ADULT BODY MASS BY SEX
#NLOC = LOG MEAN NUMBER OF LOCATIONS

corrplot(cor.mat, method="number",tl.col="black")

#NOTE : remove hpd and hfi

# glmm gamma modelling

## random structure selection

full.model_gamma<- glmmTMB(
  formula =hr ~ (1|Genus.x/Species/ID)+(1|Study_ID)
  +ele
  +npp
  +cr
  +cr:log_abm_sex
  +rd
  +rd:log_abm_sex
  +fr
  +ps
  +ps:log_abm_sex
  +Sex
  +log_abm_sex
  +hrm
  +tm
  +log_nloc,
  data = HR_WHOLE.adult_scaled,
  na.action = na.fail,
  family = Gamma(link = log)
)

summary(full.model_gamma)
full.model_gamma_nogenus <- glmmTMB(
  formula = hr ~ (1|Species/ID)+(1|Study_ID)
  +ele
  +npp
  +cr
  +cr:log_abm_sex
  +rd
  +rd:log_abm_sex
  +fr
  +ps
  +ps:log_abm_sex
  +Sex
  +log_abm_sex
  +hrm
  +tm
  +log_nloc,
  data = HR_WHOLE.adult_scaled,
  na.action = na.fail,
  family = Gamma(link = log)
)

summary(full.model_gamma_nogenus)

anova(full.model_gamma,full.model_gamma_nogenus,refit=FALSE)

full.model_gamma<-full.model_gamma_nogenus
#NOTE : keep model without genus

glmm_gamma_nostudy <- glmmTMB(
  formula = hr ~ (1|Species/ID)
  +ele
  +npp
  +cr
  +cr:log_abm_sex
  +rd
  +rd:log_abm_sex
  +fr
  +ps
  +ps:log_abm_sex
  +Sex
  +log_abm_sex
  +hrm
  +tm
  +log_nloc,
  data = HR_WHOLE.adult_scaled,
  na.action = na.fail, 
  family = Gamma(link = log)
)
```

anova(full.model_gamma,glmm_gamma_nostudy,refit=FALSE)


#NOTE : keep model with study id
full.model_gamma <- glmmTMB(
  formula = hr ~ (1|Species/ID) +(1|Study_ID)
  +ele
  +npp
  +cr
  +cr:log_abm_sex
  +rd
  +rd:log_abm_sex
  +fr
  +ps
  +ps:log_abm_sex
  +Sex
  +log_abm_sex
  +hrm
  +tm
  +log_nloc,
  data = HR_WHOLE.adult_scaled,
  na.action = na.fail,
  family = Gamma(link = log)
)
summary(full.model_gamma)

## fixed structure selection

dredge.glmm_gamma<- dredge(full.model_gamma,fixed=~(1|Species/ID)+(1|Study_ID),trace=2)

dredge.glmm_gamma[c(1:5),]
# Global model call: glmmTMB(formula = hr ~ (1 | Species/ID) + (1 | Study_ID) + ele + 
#                              npp + cr + cr:log_abm_sex + rd + rd:log_abm_sex + fr + ps + 
#                              ps:log_abm_sex + Sex + log_abm_sex + hrm + tm + log_nloc, 
#                            data = HR_WHOLE.adult_scaled, family = Gamma(link = log), 
#                            na.action = na.fail, ziformula = ~0, dispformula = ~1)
# ---
#   Model selection table 
# cnd((Int)) dsp((Int)) cnd(cr) cnd(ele) cnd(fr) cnd(hrm) cnd(log_abm_sex) cnd(log_nlc) cnd(npp)  cnd(ps)
# 6912      3.815          + -0.5280   0.2346 -0.3296        +           0.8980       0.1983  -0.2866 -0.11890
# 2688      3.813          + -0.5348   0.1819 -0.3537        +           0.9371       0.1970  -0.2708         
# 7936      3.799          + -0.5246   0.2356 -0.3272        +           0.8918       0.1962  -0.2891 -0.11830
# 7168      3.815          + -0.5270   0.2340 -0.3317        +           0.8973       0.1987  -0.2862 -0.11910
# 2816      3.819          + -0.5324   0.2003 -0.3613        +           0.9287       0.1942  -0.2887 -0.05869
# cnd(rd) cnd(Sex) cnd(tm) cnd(cr:log_abm_sex) cnd(log_abm_sex:ps) df    logLik    AICc delta weight
# 6912                 +                      0.1668              0.1336 16 -5750.304 11533.1  0.00  0.399
# 2688                 +                      0.1536                     14 -5753.079 11534.5  1.44  0.194
# 7936                 +       +              0.1690              0.1334 17 -5750.205 11535.0  1.86  0.157
# 7168 -0.00797        +                      0.1671              0.1337 17 -5750.295 11535.1  2.04  0.143
# 2816                 +                      0.1557                     15 -5752.647 11535.7  2.63  0.107
# Models ranked by AICc(x) 
# Random terms (all models): 
#   cond(1 | Species/ID), cond(1 | Study_ID)

tt1=get.models( dredge.glmm_gamma[c(1:20),], subset = delta <2)
modavg1=model.avg(tt1)

summary(modavg1)

# Call:
#   model.avg(object = tt1)
# 
# Component model call: 
#   glmmTMB(formula = hr ~ <3 unique rhs>, data = HR_WHOLE.adult_scaled, family = Gamma(link = log), 
#           ziformula = ~0, dispformula = ~1, na.action = na.fail)
# 
# Component models: 
#   df   logLik     AICc delta weight
# 1+2+3+4+5+6+7+8+9+11+12    16 -5750.30 11533.09  0.00   0.53
# 1+2+3+4+5+6+7+9+11         14 -5753.08 11534.53  1.44   0.26
# 1+2+3+4+5+6+7+8+9+10+11+12 17 -5750.20 11534.96  1.86   0.21
# 
# Term codes: 
#   cond(cr)            cond(ele)             cond(fr)            cond(hrm)    cond(log_abm_sex) 
# 1                    2                    3                    4                    5 
# cond(log_nloc)            cond(npp)             cond(ps)            cond(Sex)             cond(tm) 
# 6                    7                    8                    9                   10 
# cond(cr:log_abm_sex) cond(log_abm_sex:ps) 
# 11                   12 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# cond((Int))                       3.81117    0.20884     0.20907  18.229  < 2e-16 ***
#   cond(cr)                         -0.52903    0.08687     0.08696   6.083  < 2e-16 ***
#   cond(ele)                         0.22116    0.08764     0.08773   2.521 0.011705 *  
#   cond(fr)                         -0.33533    0.08877     0.08887   3.773 0.000161 ***
#   cond(hrmMCP)                      0.18715    0.03279     0.03282   5.702  < 2e-16 ***
#   cond(log_abm_sex)                 0.90679    0.15323     0.15340   5.911  < 2e-16 ***
#   cond(log_nloc)                    0.19752    0.03294     0.03298   5.990  < 2e-16 ***
#   cond(npp)                        -0.28306    0.06521     0.06528   4.336 1.45e-05 ***
#   cond(ps)                         -0.08797    0.07884     0.07889   1.115 0.264789    
# cond(SexM)                        0.51911    0.06635     0.06642   7.816  < 2e-16 ***
#   cond(cr:log_abm_sex)              0.16386    0.07391     0.07398   2.215 0.026775 *  
#   cond(log_abm_sex:ps)              0.09898    0.07933     0.07937   1.247 0.212367    
# cond(tmSatellite tracking (GPS))  0.01579    0.08343     0.08351   0.189 0.850014    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# cond((Int))                       3.81117    0.20884     0.20907  18.229  < 2e-16 ***
#   cond(cr)                         -0.52903    0.08687     0.08696   6.083  < 2e-16 ***
#   cond(ele)                         0.22116    0.08764     0.08773   2.521 0.011705 *  
#   cond(fr)                         -0.33533    0.08877     0.08887   3.773 0.000161 ***
#   cond(hrmMCP)                      0.18715    0.03279     0.03282   5.702  < 2e-16 ***
#   cond(log_abm_sex)                 0.90679    0.15323     0.15340   5.911  < 2e-16 ***
#   cond(log_nloc)                    0.19752    0.03294     0.03298   5.990  < 2e-16 ***
#   cond(npp)                        -0.28306    0.06521     0.06528   4.336 1.45e-05 ***
#   cond(ps)                         -0.11872    0.06884     0.06891   1.723 0.084945 .  
# cond(SexM)                        0.51911    0.06635     0.06642   7.816  < 2e-16 ***
#   cond(cr:log_abm_sex)              0.16386    0.07391     0.07398   2.215 0.026775 *  
#   cond(log_abm_sex:ps)              0.13357    0.06223     0.06230   2.144 0.032032 *  
#   cond(tmSatellite tracking (GPS))  0.07540    0.16953     0.16972   0.444 0.656831    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

best.glmm_gamma <- glmmTMB(
  formula = hr ~ (1|Species/ID)+(1|Study_ID)
  +(0+ele|Species)
  +(0+npp|Species)
  +(0+cr|Species)
  +(0+ps|Species)
  +(0+log_nloc|Species)
  +(0+cr:log_abm_sex|Species)
  +(0+ps:log_abm_sex|Species)
  +(0+fr|Species)
  +ele
  +npp
  +cr
  +cr:log_abm_sex
  +ps
  +ps:log_abm_sex
  +fr
  +Sex
  +log_abm_sex
  +hrm
  +log_nloc,
  data = HR_WHOLE.adult_scaled,
  na.action = na.fail,
  family = Gamma(link = log)
)
summary(best.glmm_gamma)


r.squaredGLMM(best.glmm_gamma)

## Diagnostics

### spatial autocorrelation test


# Step 1: Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = best.glmm_gamma, plot = FALSE)

# Step 2: Recalculate residuals for spatial groups, ensuring aggregation
sims_aggregated <- recalculateResiduals(simulationOutput, group = interaction(HR_WHOLE.adult_scaled$Latitude, HR_WHOLE.adult_scaled$Longitude))

# Step 3: Extract unique locations (latitude and longitude)
unique_locations <- unique(data.frame(Latitude = HR_WHOLE.adult_scaled$Latitude, Longitude = HR_WHOLE.adult_scaled$Longitude))

# Step 4: Test spatial autocorrelation on aggregated residuals
testSpatialAutocorrelation(sims_aggregated, x = unique_locations$Latitude, y = unique_locations$Longitude, plot = FALSE)

### dharma residuals plot
plot(simulationOutput)

par(mfrow = c(2, 2)) 
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$ele)
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$npp)
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$cr)
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$fr)
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$Sex)
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$log_abm_sex)
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$hrm)
plotResiduals(simulationOutput, form = HR_WHOLE.adult_scaled$log_nloc)
par(mfrow = c(1, 1)) 

## predicted responses 

# List of fixed effects (vars)


vars <- c("log_abm_sex","Sex","hrm","log_nloc","npp","fr","cr","ps")
# Labels
var_labels <- list(
  Sex = "SEX",
  log_abm_sex = "ABM",
  npp = "NPP",
  fr = "FR",
  cr = "CR",
  ps = "PS",
  hrm = "HRM",
  log_nloc = "NLOC"
)

# Create an empty list to store the plots
plots_list <- list()


# Loop through each fixed effect
for (var in vars) {
  
  best.glmm_gamma <- glmmTMB(
    formula = hr ~ (1|Species/ID)+(1|Study_ID)
    +(0+ele|Species)
    +(0+npp|Species)
    +(0+cr|Species)
    +(0+ps|Species)
    +(0+log_nloc|Species)
    +(0+fr|Species)
    +ele
    +npp
    +ps
    +ps:log_abm_sex
    +cr
    +cr:log_abm_sex
    +fr
    +Sex
    +log_abm_sex
    +hrm
    +log_nloc,
    data = HR_WHOLE.adult_scaled,
    na.action = na.fail,
    family = Gamma(link = log)
  )
  # Generate predicted response values and prediction intervals
  mydf <- predict_response(best.glmm_gamma, terms = var, type = "random")
  
  # Ensure predictions are in log scale
  mydf$predicted <- log(mydf$predicted)
  mydf$conf.low <- log(mydf$conf.low)
  mydf$conf.high <- log(mydf$conf.high)
  
  # Plot observed data with jitter and model predictions
  if (var %in% c("Sex", "hrm")) {
    p <- ggplot() +
      geom_jitter(data = HR_WHOLE.adult_scaled, aes(x = !!sym(var), y = log(hr)), 
                  alpha = 0.3, color = "black", width = 0.2, height = 0) +
      geom_boxplot(data = mydf, aes(x = x, y = predicted), outlier.shape = NA) +
      geom_errorbar(data = mydf, aes(x = x, ymin = conf.low, ymax = conf.high), 
                    width = 0.2) +
      labs(x = var_labels[[var]], y = "log(HR)") +
      theme_bw() +
      theme(
        text = element_text(size = 18),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 18),
        panel.grid.minor = element_blank()
      )
  } else {
    p <- ggplot() +
      geom_jitter(data = HR_WHOLE.adult_scaled, aes(x = !!sym(var), y = log(hr)), 
                  alpha = 0.3, color = "black", width = 0.2, height = 0) +
      geom_line(data = mydf, aes(x = x, y = predicted), color = "black") +
      geom_ribbon(data = mydf, aes(x = x, ymin = conf.low, ymax = conf.high), 
                  alpha = 0.1) +
      labs(x = var_labels[[var]], y = "log(HR)") +
      theme_bw() +
      theme(
        text = element_text(size = 18),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 18),
        panel.grid.minor = element_blank()
      )
  }
  
  plots_list[[var]] <- p
}


# Arrange the plots in a grid
plots_grid <- cowplot::plot_grid(plotlist = plots_list, ncol = 3)


print(plots_grid)
