# Face pop-out power analysis
# Last updated, 15 Nov 2019
# Parameters from Mercure et al 2018

## REQUIRED FOR 80%
  # attention capture: n = 68; N = 136
  # attention maintenence: n = 80; N = 160

## REQUIRED FOR 90%
  # attention capture: n = 90; N = 180
  # attention maintenence: n = 110; N = 220

## PROBABLE FOR PHD
  # attention capture: n = 40 (N = 80); 58.4% power
  # attention maintenence: n = 40 (N = 80); 50.6% power

# Dependencies ------------------------------------------------------------
# devtools::install_github("Lakens/ANOVApower", force = TRUE)
require("ANOVApower")
# https://github.com/Lakens/ANOVApower

# Attention capture --------------------------------------------------------------

# Calculating the power required to detect effect 
# reported in Mercure et al. (2018)
# at 7- to 1o-months
# Main effect of group on fixation latency to faces: 
# F(1) = 6.2; p = 0.014; η2 = 0.057

# Setting up --------------------------------------------------------------

# design = between-factor (group) analysis
# n = sample size for between subject condition (per group)
# mu = vector with means for each condition
# sd = population standard deviation
# r = correlation for within designs or 0 for between
# labelnames = factor/level names
# plot = logical 

#### Specific parameters for paper
# between-factor
# b = group (monolingual vs bilingul)
design <- "2b"
n <- 68
mu <- c(1.3669, 1.8690)
sd <- c(0.79476, 1.20293) 
labelnames <- c("group", "monolingual", "bilingual")

design_result <- ANOVA_design(design = design,
                              n = n,
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)

# Simulation-based power calculations -------------------------------------

### Repeatedly simulate data for each condition based on means,
# sample size, standard deviation, and correlation
# using ANOVA_power. This performs power analyses based on 
# repeatedly simulating normally distributed data.

ANOVA_power(design_result, alpha = 0.05, nsims = 1000)

### Simulate a dataset that has exactly the desired properties
# every cell of the design has n datapoints that have the 
# desired mean and standard deviation, and correlation between groups 
# (for a within design). By performing an ANOVA on this dataset, 
# we can calculate the required statistics from the ANOVA result 
# used to calculate the statistical power. 

ANOVA_exact(design_result)

### Visual representation of power and sample size relationship
plot_power(design_result, min_n = 8, max_n = 50)


# Attention maintenance --------------------------------------------------------------

# Calculating the power required to detect effect 
# reported in Mercure et al. (2018)
# at 17- to 10-months
# Main effect of group on fixation latency to faces: 
# F(1) = 4.7; p = 0.032; η2 = 0.044

# Setting up --------------------------------------------------------------

# design = between-factor (group) analysis
# n = sample size for between subject condition (per group)
# mu = vector with means for each condition
# sd = population standard deviation
# r = correlation for within designs or 0 for between
# labelnames = factor/level names
# plot = logical 

#### Specific parameters for paper
# between-factor
# b = group (monolingual vs bilingul)

design <- "2b"
n <- 43
mu <- c(6.3862, 7.5096)
sd <- c(2.60361, 2.40046) 
labelnames <- c("group", "monolingual", "bilingual")

design_result <- ANOVA_design(design = design,
                              n = n,
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)

# Simulation-based power calculations -------------------------------------

### Repeatedly simulate data for each condition based on means,
# sample size, standard deviation, and correlation
# using ANOVA_power. This performs power analyses based on 
# repeatedly simulating normally distributed data.

ANOVA_power(design_result, alpha = 0.05, nsims = 1000)

### Simulate a dataset that has exactly the desired properties
# every cell of the design has n datapoints that have the 
# desired mean and standard deviation, and correlation between groups 
# (for a within design). By performing an ANOVA on this dataset, 
# we can calculate the required statistics from the ANOVA result 
# used to calculate the statistical power. 

ANOVA_exact(design_result)

### Visual representation of power and sample size relationship
plot_power(design_result, min_n = 8, max_n = 50)
