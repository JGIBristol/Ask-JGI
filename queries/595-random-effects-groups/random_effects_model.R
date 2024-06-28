# Standard R libraries
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)

library(lme4) # libraries for Multi-level modelling
library(sjPlot) # library for plotting lme4 and producing tables of coefficients

# Generating Data ---------------------------------------------------------

# Preparing sample
N <- 3000


# Params

wellbeing_existing <- 50 # Average wellbeing score will be 50 at age 23
wellbeing_never <- 75 # Average wellbeing score will be 75 at age 23
wellbeing_new <- 30 # Average wellbeing score will be 30 at age 23

mean_intercept <- mean(c(wellbeing_existing, wellbeing_never, wellbeing_new))

standard_deviation <- 10 # Spread in wellbeing scores at 23


slope_existing <- -10 # Wellbeing will be on average 10 lower for existing parents
slope_never <- 15 # Wellbeing will be on average 15 lower for never group
slope_new <- 10 # Wellbeing will be on average 10 higher



# Creating IDs, ages, and parent groups
pid <- rep(1:N, 2) # IDs for participants
age <- c(rep(0, N),rep(1, N)) # Ages (0 represents 23 and 1 represents 26)
parent_grp <- rep(sample(c("existing", "never", "new"),N, replace = T), 2) # A sample from parent groups (existing, never, new)


# Makes sure the wellbeing scores are in the correct order
well_being_23 <- ifelse(parent_grp=='existing' & age == 0, rnorm(N, wellbeing_existing, standard_deviation),
                        ifelse(parent_grp=='never' & age == 0, rnorm(N, wellbeing_never, standard_deviation),
                               ifelse(parent_grp=='new' & age == 0, rnorm(N, wellbeing_new, standard_deviation),NA)))

well_being_23 <- well_being_23[!is.na(well_being_23)]

# Add some noise to the wellbeing score at 23
well_being_23 <- well_being_23 + rnorm(N, 0, 5)

hist(well_being_23[parent_grp=='existing' & age == 0])
hist(well_being_23[parent_grp=='never' & age == 0])
hist(well_being_23[parent_grp=='new' & age == 0])



# Make sure generated scores are lower
well_being_26 <- well_being_23 + ifelse(parent_grp=='existing' & age == 0, slope_existing,
                                         ifelse(parent_grp=='never' & age == 0, slope_never, 
                                                ifelse(parent_grp=='new' & age == 0, slope_new,NA)))
well_being_26 <- well_being_26[!is.na(well_being_26)]
                                                
# Add some noise
well_being_26 <- well_being_26 + rnorm(N, 0, 2)

# Plot wellbeing differences at 26
hist(well_being_26[parent_grp=='existing' & age == 0] - well_being_23[parent_grp=='existing' & age == 0])
hist(well_being_26[parent_grp=='never' & age == 0] - well_being_23[parent_grp=='never' & age == 0])
hist(well_being_26[parent_grp=='new' & age == 0] - well_being_23[parent_grp=='new' & age == 0])



# create final data frame
df <- tibble::as_tibble(list(
  pid = pid,
  age = age,
  parent_grp = parent_grp,
  wellbeing = c(well_being_23, well_being_26)
))


# Plotting Data -----------------------------------------------------------

# Wellbeing at 23
df |> 
  filter(age==0) |> 
  ggplot(aes(x=wellbeing, fill=parent_grp)) +
    geom_histogram(bins=30, alpha=0.5, position="identity") +
    labs(title="Wellbeing at 23", x="Wellbeing", y="Count") 

# Wellbeing at 26
df |> 
  filter(age==1) |> 
  ggplot(aes(x=wellbeing, fill=parent_grp)) +
  geom_histogram(bins=30, alpha=0.5, position="identity") +
  labs(title="Wellbeing at 26", x="Wellbeing", y="Count") 

df |> 
  pivot_wider(names_from = age, values_from = wellbeing) |>
  mutate(diff = `1` - `0`) |> 
  ggplot(aes(x=diff, fill=parent_grp)) +
  geom_histogram(bins=30, alpha=0.5, position="identity") +
  labs(title="Wellbeing Difference", x="Wellbeing Difference", y="Count")

# Scatter plot of scores
df |> 
  ggplot(aes(x=age, y=wellbeing, color=parent_grp)) +
  geom_jitter() +
  labs(title="Wellbeing by Age", x="Age", y="Wellbeing", color="Parent Group")


# Modelling ---------------------------------------------------------------

m1 <- lmer(wellbeing ~ parent_grp + parent_grp:age + (1 | pid), data=df)
summary(m1)

plot_model(m1) # Library from SJPlot
tab_model(m1) # Print the table, SJPlot

plot(m1) # Plot Residuals

newdata <- with(df, expand.grid(age=unique(age), parent_grp=unique(parent_grp)))

newdata$prediction <- predict(m1, newdata=newdata, re.form=NA)

df |> 
  mutate(predicted = predict(m1)) |>
  ggplot() +
  geom_jitter(aes(x=age, y=wellbeing, color=parent_grp), alpha=0.2) +
  
  geom_line(data = newdata,aes(x=age, y = prediction, color=parent_grp),size=1) 
  labs(title="Wellbeing by Age", x="Age", y="Wellbeing", color="Parent Group")

# Notes -------------------------------------------------------------------
# Need to take care when interpretating the coefficients
# (see below)

# If you wanted to compare the coefficients you 
# could look into "contrast coding". However there
# you would need to be extremely careful with your
# interpretation. 

# Output
# Fixed effects:
#                         Estimate
# (Intercept)             50.09133 <- Average wellbeing score at 23 for reference category (existing)
# parent_grpnever         24.74609 <- Average wellbeing score at 23 for never, compared to reference category (existing)
# parent_grpnew          -20.39208 <- Average wellbeing score at 23 for new, compared to reference category (existing)
# parent_grpexisting:age -10.04945 <- Average increase in wellbeing for group existing, compared to their wellbeing at 23
# parent_grpnever:age     14.91641 <- Average increase in wellbeing for group never, compared to their wellbeing at 23
# parent_grpnew:age        9.94495 <- Average increase in wellbeing for group new, compared to their wellbeing at 23




