library(dirichletprocess)
library(ggplot2)
library(dplyr)
library(patchwork) # Excellent for arranging grids of plots
library(tidyr)
library(reshape2)

# --- DATA SETUP (Using your provided structure) ---
# Assuming 'groups' and 'dp_models' are already created as per your existing code.
# Ensure names(dp_models) corresponds to the bin names.

ds <- read.csv('C:/Users/iw12499/OneDrive - University of Bristol/Documents/Documents/Postdoc/R/csv/sustainlipids.csv', header=TRUE)
balk <-  subset(ds, grouping == "Balkans")
#balk <-  subset(balk, bin != "b")
groups <- split(balk$D13C, balk$bin)

##PART 1##

# Function to fit DP Gaussian #
fit_dp_gaussian <- function(data, n_iter = 3000, alpha_prior = c(1, 2), bin_name = NULL) {
  
  # 1. Initialize the DP with raw data
  dp <- DirichletProcessGaussian(data)
  
  # 2. Set g0Priors (Normal-Inverse-Gamma)
  # mu = overall mean of your data
  # tau = spread of the cluster centers
  # alpha/beta = shape/rate for the cluster variances
  #dp$g0Priors <- c(mean(data, na.rm=TRUE), 0.01, 10, 10)
  
  # 3. Set your cluster concentration prior (as we discussed)
  dp$alphaPriorParameters <- alpha_prior
  
  # 4. Fit the model
  dp <- Fit(dp, n_iter, progressBar = TRUE)
  
  # Basic diagnostics
  cat("=== bin:", bin_name, "===\n")
  cat("Number of clusters found:", dp$numberClusters, "\n")
  cat("Cluster weights:", round(dp$weights, 3), "\n\n")
  
  return(dp)
}

set.seed(123)
dp_models <- lapply(names(groups), function(bin_name) {
  fit_dp_gaussian(groups[[bin_name]], bin_name = bin_name)
})
names(dp_models) <- names(groups)

# 1. Get the min and max from your original combined data
all_values <- balk$D13C # The column you split into groups
buffer <- 2             # Add a little room on the edges

x_min <- min(all_values, na.rm = TRUE) - buffer
x_max <- max(all_values, na.rm = TRUE) + buffer

# 2. Create the grid using these real-world values
xGrid <- seq(x_min, x_max, by = 0.1)

## --- NEW: DIAGNOSTICS (Insert here) --- ##
# Extract alpha chains from all models
alpha_chains <- lapply(names(dp_models), function(bin_name) {
  data.frame(
    Iteration = 1:length(dp_models[[bin_name]]$alphaChain),
    AlphaValue = dp_models[[bin_name]]$alphaChain,
    Bin = bin_name
  )
}) %>% bind_rows()

# Calculate the mean posterior alpha for each group
alpha_summary <- alpha_chains %>%
  group_by(Bin) %>%
  summarise(MeanAlpha = mean(AlphaValue))

print(alpha_summary)

# Trace Plot
p_trace <- ggplot(alpha_chains, aes(x = Iteration, y = AlphaValue, color = Bin)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~Bin, scales = "free_y") +
  theme_minimal() +
  labs(title = "Alpha Trace Plot (Convergence Check)",
       subtitle = "Looking for a 'fuzzy caterpillar' shape",
       y = "Alpha Value")

# Density Plot
p_density <- ggplot(alpha_chains, aes(x = AlphaValue, fill = Bin)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Posterior Alpha Distribution",
       x = "Alpha Value",
       y = "Density")

# Show them together
p_trace / p_density

## PART 2: Loop for Diagnostics and Summary Data ##

# We will store the diagnostic plots in a list, and the summary data in a dataframe
diagnostic_plot_list <- list()
summary_data_list <- list()

# Loop through every group in your model list
for(bin_name in names(dp_models)) {
  
  message(paste("Processing bin:", bin_name))
  
  # 1. Get the specific model and original data for this bin
  dp_obj <- dp_models[[bin_name]]
  original_data <- data.frame(val = dp_obj$data)
  
  # 2. Generate the 100 Posterior/Prior Draws (The Loop)
  # We use lapply here as it's cleaner than a for-loop for lists
  draws_list <- lapply(1:100, function(i) {
    priorF <- dirichletprocess:::PriorFunction(dp_obj)
    postF <- PosteriorFunction(dp_obj)
    data.frame(x = xGrid, 
               Posterior = postF(xGrid), 
               Prior = priorF(xGrid), 
               Iter = i)
  })
  allRes <- bind_rows(draws_list)
  
  # 3. Create the Diagnostic Figures for THIS group
  # Posterior Plot
  p1 <- ggplot(allRes, aes(x=x, y=Posterior, group=Iter)) + 
    geom_histogram(data=original_data, aes(x=val, y=..density..), 
                   inherit.aes = F, binwidth = 0.1, fill="grey80", alpha=0.5) + 
    geom_line(alpha = 0.2, color = "blue") + 
    theme_minimal() +
    labs(x = NULL, y = NULL, title = paste(bin_name, "- Posterior"))
  
  # Prior Plot
  p2 <- ggplot(allRes, aes(x=x, y=Prior, group=Iter)) + 
    geom_histogram(data=original_data, aes(x=val, y=..density..), 
                   inherit.aes = F, binwidth = 0.1, fill="grey80", alpha=0.5) + 
    geom_line(alpha = 0.2, color = "red") + 
    theme_minimal() +
    labs(x = NULL, y = NULL, title = paste(bin_name, "- Prior"))
  
  # Combine them side-by-side and store in our list
  diagnostic_plot_list[[bin_name]] <- (p2 + p1)
  
  # 4. Calculate Summary Stats for the Stacked Plot
  # We summarize the 100 draws into Mean, Lower CI, Upper CI
  bin_summary <- allRes %>%
    group_by(x) %>%
    summarise(
      mean_posterior = mean(Posterior),
      lower_bound = quantile(Posterior, 0.025),
      upper_bound = quantile(Posterior, 0.975),
      .groups = 'drop'
    ) %>%
    mutate(bin = bin_name) # Tag this data with the group name
  
  summary_data_list[[bin_name]] <- bin_summary
}

# --- VISUALIZATION 2A: The Grid of Diagnostics ---
# wrap_plots automatically creates a grid from our list of plots
combined_diagnostics <- wrap_plots(diagnostic_plot_list, ncol = 2) 
print(combined_diagnostics)


# --- VISUALIZATION 2B: The Stacked Summary Plot ---
# Combine all summaries into one big dataframe
all_groups_summary <- bind_rows(summary_data_list)

# We also need to combine original data for the background histograms
all_original_data <- do.call(rbind, lapply(names(dp_models), function(n) {
  d <- data.frame(val = dp_models[[n]]$data)
  d$bin <- n
  return(d)
}))

stacked_plot <- ggplot() +
  # 1. Background Histograms (faceted by bin)
  geom_histogram(data = all_original_data, aes(x = val, y = ..density..),
                 bins = 30, fill = "grey90", color = "grey80") +
  
  # 2. Shaded CI Ribbons (Colored by bin)
  geom_ribbon(data = all_groups_summary, 
              aes(x = x, ymin = lower_bound, ymax = upper_bound, fill = bin), 
              alpha = 0.4) +
  
  # 3. Mean Lines (Colored by bin)
  geom_line(data = all_groups_summary, 
            aes(x = x, y = mean_posterior, color = bin), 
            size = 1) +
  
  # 4. Layout: Stack them vertically
  facet_grid(bin ~ .) + 
  theme_minimal() +
  labs(title = "Posterior Densities by Group",
       subtitle = "Stacked comparison of all bins",
       y = "Density", x = "Value")

print(stacked_plot)


## PART 3: Hellinger Distance Matrix ##

# 1. Pivot the data so columns are bins, rows are x-values
# We only need the 'mean_posterior' for this calculation
density_matrix <- all_groups_summary %>%
  select(x, bin, mean_posterior) %>%
  pivot_wider(names_from = bin, values_from = mean_posterior) %>%
  select(-x) # Remove the x column for calculation

# 2. Initialize an empty matrix to store results
n_bins <- ncol(density_matrix)
bin_names <- colnames(density_matrix)
hellinger_mat <- matrix(0, nrow = n_bins, ncol = n_bins, dimnames = list(bin_names, bin_names))

# 3. Calculate step size (dx)
dx <- 0.1 # Based on your xGrid definition

# 4. Nested loop to calculate distance between every pair
for(i in 1:n_bins) {
  for(j in 1:n_bins) {
    if(i <= j) {
      # Extract densities
      f <- density_matrix[[i]]
      g <- density_matrix[[j]]
      
      # Normalize (Safeguard to ensure area = 1)
      f <- f / sum(f * dx)
      g <- g / sum(g * dx)
      
      # Compute Hellinger Distance
      h_dist <- sqrt(0.5 * sum((sqrt(f) - sqrt(g))^2 * dx))
      
      # Fill Matrix (It's symmetric)
      hellinger_mat[i, j] <- h_dist
      hellinger_mat[j, i] <- h_dist
    }
  }
}

# 5. View the Matrix
print(round(hellinger_mat, 4))

# Optional: Visualize the Matrix as a Heatmap
melted_cormat <- melt(hellinger_mat)
heatmap_plot <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = round(value, 2))) +
  theme_minimal() +
  labs(title = "Hellinger Distance Matrix", x = "", y = "")


# --- 4. Dendrogram from Hellinger distances ---
hellinger_dist <- as.dist(hellinger_mat)
hc <- hclust(hellinger_dist, method = "average")
dend <- as.dendrogram(hc)

# Convert dendrogram to a ggplot-compatible object
library(ggdendro)
dend_data <- dendro_data(dend)
datalabels <- dend_data[["labels"]][["label"]]

dend_plot <- ggplot(segment(dend_data)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_x_continuous(breaks = 1:n_bins, labels = datalabels) +
  theme_minimal(base_size = 14) +
  labs(title = "Dendrogram of bins") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

# --- 5. Arrange side by side ---
grid.arrange(heatmap_plot, dend_plot, stacked_plot, ncol = 3)

# Calculate means and standard deviations from raw data
summary_stats <- all_original_data %>%
  group_by(bin) %>%
  summarise(
    mean_val = mean(val),
    sd_val = sd(val)
  )

ggplot(summary_stats, aes(x = reorder(bin, mean_val), y = mean_val, color = bin)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.2) +
  coord_flip() + # Makes it easier to read group names
  theme_minimal() +
  labs(title = "Average D13C by Group",
       x = "Group (Bin)",
       y = expression(delta^{13}*C* " (per mil)"))


library(ggridges)

ggplot(all_original_data, aes(x = val, y = bin, fill = bin)) +
  geom_density_ridges(alpha = 0.6, scale = 1.2) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(title = "Isotopic Shifts Across Groups",
       x = expression(delta^{13}*C),
       y = "Bin")

# Create a data frame of all pairs
pairs <- expand.grid(bin1 = bin_names, bin2 = bin_names)

# Calculate difference in means for every pair
pairs$mean_diff <- sapply(1:nrow(pairs), function(i) {
  abs(mean(groups[[pairs$bin1[i]]]) - mean(groups[[pairs$bin2[i]]]))
})

# Pull the Hellinger distances from your matrix
pairs$h_dist <- as.vector(hellinger_mat)

ggplot(pairs, aes(x = mean_diff, y = h_dist)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Does Mean Offset Drive Hellinger Distance?",
       x = "Absolute Difference in Mean D13C",
       y = "Hellinger Distance")
