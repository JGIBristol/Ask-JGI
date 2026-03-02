# AskJGI analyses for archaeological chemistry
library(dirichletprocess)
library(ggplot2)
library(dplyr)
library(patchwork) 
library(tidyr)
library(reshape2)
library(MixSIAR)

# --- DATA SETUP (Using your provided structure) ---
ds <- read.csv('sustainlipids.csv', header = TRUE)
balk <-  subset(ds, grouping == "Balkans")
# write as csv for use later
write.csv(balk, 'balk.csv', row.names = FALSE)
groups <- split(balk$D13C, balk$bin)

# Load reference source data
ref_sources <- read.csv('reference_fats.csv', header = TRUE)

# --- EXPLORATORY VISUALISATION ---
# Boxplot of Δ13C by bin
# Does in fact seem like Δ13C decreases as we move north for the most part
ggplot(balk, aes(x = bin, y = D13C)) +
  geom_boxplot() +
  labs(title = "Boxplot of Δ13C by Bin", x = "Bin", y = "Δ13C") +
  theme_minimal()

# --- ALTERNATIVE BAYESIAN APPROACH ---
# Each pot is composed of a weighted sum of different food sources, each of which has a different mean
# We will start with 3 food sources (ruminant adipose, non-ruminant adipose, and dairy)
# The weights for each pot are drawn from a Dirichlet distribution, which can differ by region (bin)

# Load the mixture data
mix <- load_mix_data(
  filename = 'balk.csv',
  iso_names = c("d13C16", "d13C18"),
  factors = "bin",
  fac_random = TRUE, # including a random effect for region
  fac_nested = FALSE, # we don't have a nested structure in this case
  cont_effects = NULL # column headings of continuous effects -- N/A in this case
)

# Load the source data
# Excel file shared by Isabel:
# source <- load_source_data(
#   filename = 'reference_fats1.csv',
#   source_factors = NULL,
#   conc_dep = FALSE, # do not have concentration dependence
#   data_type = "raw",
#   mix = mix
# ) # doesn't include dairy, so not using this for now
# Mean data from supplement shared by Isabel:
source <- load_source_data(
  filename = 'reference_fats2.csv',
  source_factors = NULL,
  conc_dep = FALSE, # do not have concentration dependence
  data_type = "means",
  mix = mix
)

# Load the discrimination data
# we aren't looking at tissue of a consumer, so we just set TDF to 0
# Made a discrimination file following this format:
# https://github.com/brianstock/MixSIAR/blob/master/inst/extdata/wolves_discrimination.csv
# discr <- load_discr_data(
#   filename = 'discrimination1.csv',
#   mix = mix
# )
discr <- load_discr_data(
  filename = 'discrimination2.csv',
  mix = mix
)

# # Plot sources
# plot(mix$data$d13C16, mix$data$d13C18,
#      col = "grey",
#      pch = 16,
#      xlab = "d13C16",
#      ylab = "d13C18")
# 
# points(ref_sources$d13C16, ref_sources$d13C18,
#        col = "red",
#        pch = 19)

# Make an isospace plot
plot_data(filename="isospace_plot",
          plot_save_pdf = FALSE,
          plot_save_png = TRUE,
          mix,
          source,
          discr)

# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
plot_prior(alpha.prior=1,source)

# Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

# Run the model
# test run to check if the model is working as expected
jags.test <- run_model(run="test", mix, source, discr, model_filename)

output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE,
                       diag_save_ggmcmc = FALSE)

output_JAGS(jags.test, mix, source, output_options)
