#Space-use factors in Felids

#MODELLING CORE AREA 

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
HR_CORE.adult_scaled<-read.xlsx("Felids_core_area_data_species_2025_05_02.xlsx")

# correlation plot
exp_num_var<-c("ele","npp","fr","cr","ps","hfi","rd","hpd","log_abm_sex","log_nloc")
HR_CORE_unique <- HR_CORE.adult_scaled %>%
  distinct(key, .keep_all = TRUE)

mycov.num <- HR_CORE_unique[,exp_num_var]
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
  data = HR_CORE.adult_scaled,
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
  data = HR_CORE.adult_scaled,
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
  data = HR_CORE.adult_scaled,
  na.action = na.fail, 
  family = Gamma(link = log)
)

anova(full.model_gamma,glmm_gamma_nostudy,refit=FALSE)

#NOTE : keep model without study id
full.model_gamma <- glmmTMB(
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
  data = HR_CORE.adult_scaled,
  na.action = na.fail,
  family = Gamma(link = log)
)
summary(full.model_gamma)

## fixed structure selection

dredge.glmm_gamma<- dredge(full.model_gamma,fixed=~(1|Species/ID),trace=2)

dredge.glmm_gamma[c(1:10),]

# Global model call: glmmTMB(formula = hr ~ (1 | Species/ID) + ele + npp + cr + cr:log_abm_sex + 
#                              rd + rd:log_abm_sex + fr + ps + ps:log_abm_sex + Sex + log_abm_sex + 
#                              hrm + tm + log_nloc, data = HR_CORE.adult_scaled, family = Gamma(link = log), 
#                            na.action = na.fail, ziformula = ~0, dispformula = ~1)
# ---
#   Model selection table 
# cnd((Int)) dsp((Int)) cnd(cr) cnd(ele) cnd(fr) cnd(hrm) cnd(log_abm_sex) cnd(log_nlc) cnd(npp) cnd(ps)  cnd(rd)
# 1758      2.666          + -0.3947          -0.2663        +           0.9005               -0.4541 -0.1976         
# 1630      2.629          + -0.4019          -0.2126        +           0.9144               -0.3140                 
# 5854      2.666          + -0.4068          -0.2423        +           0.9091               -0.4288 -0.1918         
# 1760      2.678          + -0.3879  0.06065 -0.2645        +           0.9065               -0.4401 -0.2253         
# 3806      2.661          + -0.3775          -0.2511        +           0.9038               -0.4484 -0.1914         
# 1790      2.705          + -0.3918          -0.2631        +           0.9015     -0.00896  -0.4507 -0.2002         
# 2014      2.661          + -0.3952          -0.2644        +           0.8986               -0.4470 -0.2089 -0.01099
# 1886      2.665          + -0.3959          -0.2424        +           0.9199               -0.4027          0.06381
# 736       2.722          + -0.3700  0.22700 -0.2165        +           0.9430               -0.3463 -0.2789         
# 3678      2.619          + -0.3577          -0.1782        +           0.9204               -0.3103                 
# cnd(Sex) cnd(tm) cnd(cr:log_abm_sex) cnd(log_abm_sex:ps) df    logLik   AICc delta weight
# 1758        +       +                                         12 -1027.524 2080.2  0.00  0.248
# 1630        +       +                                         11 -1029.285 2081.5  1.35  0.127
# 5854        +       +                                 0.05721 13 -1027.386 2082.1  1.92  0.095
# 1760        +       +                                         13 -1027.423 2082.2  1.99  0.092
# 3806        +       +            -0.02003                     13 -1027.489 2082.3  2.12  0.086
# 1790        +       +                                         13 -1027.511 2082.3  2.16  0.084
# 2014        +       +                                         13 -1027.514 2082.3  2.17  0.084
# 1886        +       +                                         12 -1028.754 2082.6  2.46  0.072
# 736         +                                                 12 -1028.959 2083.0  2.87  0.059
# 3678        +       +            -0.05037                     12 -1029.058 2083.2  3.07  0.053
# Models ranked by AICc(x) 
# Random terms (all models): 
#   cond(1 | Species/ID)

tt1=get.models( dredge.glmm_gamma[c(1:20),], subset = delta <2)
modavg1=model.avg(tt1)

summary(modavg1)

# Call:
#   model.avg(object = tt1)
# 
# Component model call: 
#   glmmTMB(formula = hr ~ <4 unique rhs>, data = HR_CORE.adult_scaled, family = Gamma(link = log), ziformula = 
#             ~0, dispformula = ~1, na.action = na.fail)
# 
# Component models: 
#   df   logLik    AICc delta weight
# 1+3+4+5+6+7+8+9    12 -1027.52 2080.17  0.00   0.44
# 1+3+4+5+6+8+9      11 -1029.28 2081.51  1.35   0.23
# 1+3+4+5+6+7+8+9+10 13 -1027.39 2082.08  1.92   0.17
# 1+2+3+4+5+6+7+8+9  13 -1027.42 2082.16  1.99   0.16
# 
# Term codes: 
#   cond(cr)            cond(ele)             cond(fr)            cond(hrm)    cond(log_abm_sex) 
# 1                    2                    3                    4                    5 
# cond(npp)             cond(ps)            cond(Sex)             cond(tm) cond(log_abm_sex:ps) 
# 6                    7                    8                    9                   10 
# 
# Model-averaged coefficients:  
#   (full average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# cond((Int))                       2.659295   0.280369    0.281582   9.444  < 2e-16 ***
#   cond(cr)                         -0.397249   0.078149    0.078487   5.061 4.00e-07 ***
#   cond(fr)                         -0.249859   0.096139    0.096535   2.588  0.00965 ** 
#   cond(hrmMCP)                     -0.287247   0.089136    0.089523   3.209  0.00133 ** 
#   cond(log_abm_sex)                 0.906047   0.192785    0.193621   4.679 2.90e-06 ***
#   cond(npp)                        -0.415948   0.139571    0.140081   2.969  0.00298 ** 
#   cond(ps)                         -0.156628   0.128180    0.128491   1.219  0.22285    
# cond(SexM)                        0.497817   0.123455    0.123986   4.015 5.94e-05 ***
#   cond(tmSatellite tracking (GPS))  0.460144   0.181737    0.182506   2.521  0.01169 *  
#   cond(log_abm_sex:ps)              0.009698   0.049796    0.049973   0.194  0.84612    
# cond(ele)                         0.009912   0.058835    0.059054   0.168  0.86671    
# 
# (conditional average) 
# Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# cond((Int))                       2.65929    0.28037     0.28158   9.444  < 2e-16 ***
#   cond(cr)                         -0.39725    0.07815     0.07849   5.061 4.00e-07 ***
#   cond(fr)                         -0.24986    0.09614     0.09654   2.588  0.00965 ** 
#   cond(hrmMCP)                     -0.28725    0.08914     0.08952   3.209  0.00133 ** 
#   cond(log_abm_sex)                 0.90605    0.19278     0.19362   4.679 2.90e-06 ***
#   cond(npp)                        -0.41595    0.13957     0.14008   2.969  0.00298 ** 
#   cond(ps)                         -0.20221    0.10952     0.10999   1.838  0.06601 .  
# cond(SexM)                        0.49782    0.12345     0.12399   4.015 5.94e-05 ***
#   cond(tmSatellite tracking (GPS))  0.46014    0.18174     0.18251   2.521  0.01169 *  
#   cond(log_abm_sex:ps)              0.05721    0.10913     0.10961   0.522  0.60169    
# cond(ele)                         0.06065    0.13455     0.13514   0.449  0.65357    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ```

best.glmm_gamma <- glmmTMB(
  formula = hr ~ (1|Species/ID)
  +(0+ps|Species)
  +(0+npp|Species)
  +(0+cr|Species)
  +(0+fr|Species)
  +npp
  +cr
  +fr
  +ps
  +Sex
  +log_abm_sex
  +hrm
  +tm,
  data = HR_CORE.adult_scaled,
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
sims_aggregated <- recalculateResiduals(simulationOutput, group = interaction(HR_CORE.adult_scaled$Latitude, HR_CORE.adult_scaled$Longitude))

# Step 3: Extract unique locations (latitude and longitude)
unique_locations <- unique(data.frame(Latitude = HR_CORE.adult_scaled$Latitude, Longitude = HR_CORE.adult_scaled$Longitude))

# Step 4: Test spatial autocorrelation on aggregated residuals
testSpatialAutocorrelation(sims_aggregated, x = unique_locations$Latitude, y = unique_locations$Longitude, plot = FALSE)


### dharma residuals plot

plot(simulationOutput)

par(mfrow = c(2, 2)) 
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$npp)
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$cr)
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$fr)
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$ps)
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$Sex)
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$log_abm_sex)
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$hrm)
plotResiduals(simulationOutput, form = HR_CORE.adult_scaled$tm)
par(mfrow = c(1, 1)) 

## predicted responses 

HR_CORE.adult_scaled$hrm <- factor(HR_CORE.adult_scaled$hrm,levels = c("KDE", "MCP"))
vars <- c("log_abm_sex","Sex","hrm","tm","npp","fr","cr","ps")

# Labels
var_labels <- list(
  log_abm_sex = "ABM",
  Sex = "SEX",
  npp = "NPP",
  hrm = "HRM",
  tm = "TM",
  fr = "FR",
  cr = "CR",
  ps = "PS"
)


# Create an empty list to store the plots
plots_list <- list()

# Fit the GLMM model
best.glmm_gamma <- glmmTMB(
  formula = hr ~ (1|Species/ID)
  +(0+ps|Species)
  +(0+npp|Species)
  +(0+cr|Species)
  +(0+fr|Species)
  +npp
  +cr
  +fr
  +ps
  +Sex
  +log_abm_sex
  +hrm
  +tm,
  data = HR_CORE.adult_scaled,
  na.action = na.fail,
  family = Gamma(link = log)
)

# Loop through each fixed effect
for (var in vars) {
  # Generate predicted response values and prediction intervals
  mydf <- predict_response(best.glmm_gamma, terms = var, type = "random")
  
  # Ensure predictions are in log scale
  mydf$predicted <- log(mydf$predicted)
  mydf$conf.low <- log(mydf$conf.low)
  mydf$conf.high <- log(mydf$conf.high)
  
  # Plot observed data with jitter and model predictions
  if (var %in% c("Sex", "hrm", "tm")) {
    p <- ggplot() +
      geom_jitter(data = HR_CORE.adult_scaled, aes(x = !!sym(var), y = log(hr)), 
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
    
    # Special formatting for "tm"
    if (var == "tm") {
      p <- p +
        scale_x_discrete(labels = c("Radio tracking" = "RT", "Satellite tracking (GPS)" = "SAT")) +
        scale_y_continuous(
          trans = "log", 
          labels = scales::number_format(accuracy = 1)
        ) +
        theme(axis.text.x = element_text(size = 18))
    }
    
  } else {
    p <- ggplot() +
      geom_jitter(data = HR_CORE.adult_scaled, aes(x = !!sym(var), y = log(hr)), 
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

