library("dplyr")
library("tidyr")
library("ggplot2")
library("lemon")
library("viridis")
library("brms")
library("loo")
library("corrr")
library("tools")
library("gghalves")
library("ggbeeswarm")
library("ggdist")
library("ggrepel")
library("patchwork")
library("ggthemes")
library("scales")

## Clear everything ##
rm(list=ls()) 
gc() 

# Set randoms #
set.seed(1234)

######################################################################################################################
## Data stuff
######################################################################################################################
setwd("C:/Users/nelso/OneDrive/BNelson/Collabs/Peter W/chinook-straying")
df<- read.csv(
  "C:/Users/nelso/OneDrive/BNelson/Collabs/Peter W/Attractiveness_2010_2015_update_ver_3.csv", 
  stringsAsFactors = FALSE
)

# Source helper functions
source("helper-funs-brms.r")
source("model-list-final.r")

# Format col names
names(df)<- tolower(names(df))

# Add new features
cleaned_df<- df %>%
  # create new variable for interaction random effects
  #mutate(pop_year = paste0(population,"-", year)) %>%
  # Cull years with NAs
  filter(!is.na(donor)) %>%
  filter(!is.na(recipient)) %>%
  select(-category)

# Add population code
populations<- unique(cleaned_df$hatcheryname)
cleaned_df$hatcheryname<- factor(cleaned_df$hatcheryname)
cleaned_df$population<- cleaned_df$pop_id
cleaned_df<- cleaned_df %>% arrange(population, year)
# Add density covatiate
cleaned_df$density<- cleaned_df$escape / cleaned_df$area
# Add sample code
cleaned_df$sample<- 1:nrow(cleaned_df)

######################################################################################################################
## Summary stats, plotting, etc.
######################################################################################################################
# Range of recipient rates
print(min(cleaned_df$recipient))
print(max(cleaned_df$recipient))
# Range of donor rates
print(min(cleaned_df$donor))
print(max(cleaned_df$donor))

par(mfrow=c(2,1))
hist(cleaned_df$recipient, xlim = c(0,1))
hist(cleaned_df$donor, xlim = c(0,1))

par(mfrow=c(1,1))
plot(cleaned_df$recip, cleaned_df$donor, ann = FALSE, las=1, pch=16, col=alpha("purple", 0.25), xlim=c(0, 1), ylim=c(0,1))
abline(v=0, h=0, col="gray75")
abline(a = 0, b = 1.0, lwd = 1.25, col="grey50", lty = "dashed")
mtext(side = 1, outer = FALSE, "Recipient", font=2, line=2.5)
mtext(side = 2, outer = FALSE, "Donor", font=2, line=2.5)

## [violin] Plot data distribution, by population
# Colors
plot_col<- viridis(5)
rate_cols<- c("donor"=plot_col[1], "recipient"=plot_col[3])
rate_cols_fill<- c("donor"=alpha(plot_col[1], 0.25), "recipient"=alpha(plot_col[3], 0.25))
# Labels
rate_labs<- c("donor"="Donor Rate", "recipient"="Recipient Rate")
pop_labs<- as.character(1:19)

donor_data<- cleaned_df %>%
  select(donor, population) %>%
  mutate(rate_type = "donor") %>%
  rename("rate" = "donor")
recipient_data<- cleaned_df %>%
  select(recipient, population)  %>%
  mutate(rate_type = "recipient") %>%
  rename("rate" = "recipient")
all_data<- rbind(donor_data, recipient_data)

# Plot rates, by type and population
# prop_plot()
prop_plot_combined()

#####################################################################################################################
## Preliminary modeling ##
#####################################################################################################################
# Model dataset
model_data<- cleaned_df %>%
  select(sample, donor, recipient, difftemp, density, localtemp, contemp, area, localflow, distance, distsource, escape, population, year) %>%
  mutate(escape = log(escape),
         area = log(area),
         distsource = log(distsource),
         density = log(density),
         distance = log(distance)) %>%
  mutate(
    difftemp = (difftemp - mean(difftemp)) / sd(difftemp),
    localtemp = (localtemp - mean(localtemp)) / sd(localtemp),
    contemp = (contemp - mean(contemp)) / sd(contemp),
    area = (area - mean(area)) / sd(area),
    localflow = (localflow - mean(localflow)) / sd(localflow),
    distsource = (distsource - mean(distsource)) / sd(distsource),
    escape = (escape - mean(escape)) / sd(escape),
    distance = (distance - mean(distance)) / sd(distance),
    density = (density - mean(density)) / sd(density),
    pop_year = as.factor(paste0(population,"-", year))
  )

# Modify data types for features
model_data$population<- as.factor(model_data$population)
model_data$year<- as.factor(model_data$year)

# run_models(final_models)
best_model_ids<- c(3, 1)
best_models<- paste0("final-model-", best_model_ids, ".rds", sep = "")
best_model_list <- lapply(best_models, readRDS)
# run_models(final_models_uncor)

# # List all .rds files in the directory
# rds_files <- list.files(pattern = "\\.rds$")
# # Read all .rds files into a list
# rds_list <- lapply(rds_files, readRDS)
# 
# ## Generate LOO/WAIC tablular data
# loo_outs<- loo_table(rds_list)
# write.csv(loo_table(), "loo-results-table.csv", row.names = FALSE)

# coefs_plot_2(best_model_list, covariates = c("area", "distsource", "escape"), rate = "donor", axis_labs = TRUE)
n_preds = 50
area_plot()

## Plot individual marginal effects of significant covariates
# Escapement
plot_coef_esc<- function(n_preds=50, pop_in = NA) {
  newdf = data.frame(id = 1:n_preds, population = pop_in,
                     localflow = mean(model_data$localflow), 
                     contemp = mean(model_data$contemp),
                     area = mean(model_data$area),
                     distance = mean(model_data$distance),
                     distsource = mean(model_data$distsource),
                     difftemp = mean(model_data$difftemp),
                     escape = seq(
                       min(model_data$escape), 
                       max(model_data$escape),
                       length.out = n_preds))
  
  pred = predict(best_model_list[[1]], newdata = newdf)
  pred = cbind(newdf, pred)
  OUTS<- NULL
  OUTS$donor = pred[ ,(ncol(pred) - 7):(ncol(pred)-4)]
  OUTS$recip =  pred[ ,(ncol(pred) - 3):ncol(pred)]
  return(OUTS)
}

test<- mapply(n_preds=50, pop_in = c(NA, 1:19), plot_coef_esc)
par(mfrow=c(1,2), mar=c(1, 1, 1, 1) + 0.1, oma = c(4, 3.5, 2, 1) + 0.1)
# Donor
scaled_x<- seq(min(model_data$escape), max(model_data$escape), length.out = n_preds)
plot(scaled_x, test[,1]$donor$Estimate.donor, type = "l", ann=FALSE, las = 1, lwd=2, ylim = c(0,1))
lines(scaled_x, test[,1]$donor$Q2.5.donor, lwd=2, lty = "dashed")
lines(scaled_x, test[,1]$donor$Q97.5.donor, lwd=2, lty = "dashed")
for(i in 2:20) {lines(scaled_x, test[,i]$donor$Estimate.donor, col = alpha("grey50", 0.25), lwd = 2)}
# axis(side = 1)
mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Donor")
# Recipient
plot(scaled_x, test[,1]$recip$Estimate.recipient, type = "l", ann=FALSE, las = 1, lwd=2, ylim = c(0,1), yaxt = "n")
lines(scaled_x, test[,1]$recip$Q2.5.recipient, lwd=2, lty = "dashed")
lines(scaled_x, test[,1]$recip$Q97.5.recipient, lwd=2, lty = "dashed")
for(i in 2:20) {lines(scaled_x, test[,i]$recip$Estimate.recipient, col = alpha("grey50", 0.25), lwd = 2)}
# axis(side = 1)
axis(side = 2, labels = FALSE, at = seq(0, 1, by = 0.2))
mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Recipient")
mtext(side = 1, line = 1.75, outer = TRUE, font = 2, cex = 1.50, "Escapement (z-scored)")
mtext(side = 2, line = 2.0, outer = TRUE, font = 2, cex = 1.50, "Rate")

## Second Escapement plot
par(mfrow=c(5, 4), mar=c(1, 1, 1, 1) + 0.1, oma = c(4, 3.5, 2, 1) + 0.1)
plot_cols<- viridis::viridis(19)
for(i in 1:19){
  temp_pop<- model_data[model_data$population==i, ]
  temp_newdf = data.frame(id = 1:n_preds, population = i,
                     localflow = mean(model_data$localflow), 
                     contemp = mean(model_data$contemp),
                     area = mean(model_data$area),
                     distance = mean(model_data$distance),
                     distsource = mean(model_data$distsource),
                     difftemp = mean(model_data$difftemp),
                     escape = seq(
                       min(temp_pop$escape), 
                       max(temp_pop$escape),
                       length.out = n_preds))
  temp_pred = predict(best_model_list[[1]], newdata = temp_newdf)
  temp_pred = cbind(temp_newdf, temp_pred)
  # # Donor rates
  # y_max<- 1.25 * max(c(temp_pred$Estimate.donor, temp_pop$donor))
  # plot(model_data$escape, model_data$donor, ann = FALSE, col="white", pch=16, cex=0.01, las = 1, ylim=c(0, y_max), xlim=c(-5, 2))
  # points(temp_pop$escape, temp_pop$donor, pch=16, col=alpha(plot_cols[i], 0.25))
  # lines(temp_pred$escape, temp_pred$Estimate.donor, col=alpha(plot_cols[i], 1.0), lwd=3)
  # Recipient rates
  y_max<- 1.25 * max(c(temp_pred$Estimate.recipient, temp_pop$recipient))
  plot(model_data$escape, model_data$recipient, ann = FALSE, col="white", pch=16, cex=0.01, las = 1, ylim=c(0, y_max), xlim=c(-5, 2))
  points(temp_pop$escape, temp_pop$recipient, pch=16, col=alpha(plot_cols[i], 0.25))
  lines(temp_pred$escape, temp_pred$Estimate.recipient, col=alpha(plot_cols[i], 1.0), lwd=3)
}

# Confluence temp
plot_coef_contemp<- function(n_preds=50, pop_in = NA) {
  newdf = data.frame(id = 1:n_preds, population = pop_in,
                     localflow = mean(model_data$localflow), 
                     escape = mean(model_data$escape),
                     area = mean(model_data$area),
                     distance = mean(model_data$distance),
                     distsource = mean(model_data$distsource),
                     difftemp = mean(model_data$difftemp),
                     contemp = seq(
                       min(model_data$contemp), 
                       max(model_data$contemp),
                       length.out = n_preds))
  
  pred = predict(best_model_list[[1]], newdata = newdf)
  pred = cbind(newdf, pred)
  OUTS<- NULL
  OUTS$donor = pred[ ,(ncol(pred) - 7):(ncol(pred)-4)]
  OUTS$recip =  pred[ ,(ncol(pred) - 3):ncol(pred)]
  return(OUTS)
}

test<- mapply(n_preds=50, pop_in = c(NA, 1:19), plot_coef_contemp)
par(mfrow=c(1,2), mar=c(1, 1, 1, 1) + 0.1, oma = c(4, 3.5, 2, 1) + 0.1)
# Donor
scaled_x<- seq(min(model_data$contemp), max(model_data$contemp), length.out = n_preds)
plot(scaled_x, test[,1]$donor$Estimate.donor, type = "l", ann=FALSE, las = 1, lwd=2, ylim = c(0,1))
lines(scaled_x, test[,1]$donor$Q2.5.donor, lwd=2, lty = "dashed")
lines(scaled_x, test[,1]$donor$Q97.5.donor, lwd=2, lty = "dashed")
for(i in 2:20) {lines(scaled_x, test[,i]$donor$Estimate.donor, col = alpha("grey50", 0.25), lwd = 2)}
# axis(side = 1)
mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Donor")
# Recipient
plot(scaled_x, test[,1]$recip$Estimate.recipient, type = "l", ann=FALSE, las = 1, lwd=2, ylim = c(0,1), yaxt = "n")
lines(scaled_x, test[,1]$recip$Q2.5.recipient, lwd=2, lty = "dashed")
lines(scaled_x, test[,1]$recip$Q97.5.recipient, lwd=2, lty = "dashed")
for(i in 2:20) {lines(scaled_x, test[,i]$recip$Estimate.recipient, col = alpha("grey50", 0.25), lwd = 2)}
# axis(side = 1)
axis(side = 2, labels = FALSE, at = seq(0, 1, by = 0.2))
mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Recipient")
mtext(side = 1, line = 1.75, outer = TRUE, font = 2, cex = 1.50, "Confluence Temperature (z-scored)")
mtext(side = 2, line = 2.0, outer = TRUE, font = 2, cex = 1.50, "Rate")


# Local temp
plot_coef_localtemp<- function(n_preds=50, pop_in = NA) {
  newdf = data.frame(id = 1:n_preds, population = pop_in,
                     localflow = mean(model_data$localflow), 
                     escape = mean(model_data$escape),
                     area = mean(model_data$area),
                     distance = mean(model_data$distance),
                     distsource = mean(model_data$distsource),
                     difftemp = mean(model_data$difftemp),
                     localtemp = seq(
                       min(model_data$localtemp), 
                       max(model_data$localtemp),
                       length.out = n_preds))
  
  pred = predict(best_model_list[[2]], newdata = newdf)
  pred = cbind(newdf, pred)
  OUTS<- NULL
  OUTS$donor = pred[ ,(ncol(pred) - 7):(ncol(pred)-4)]
  OUTS$recip =  pred[ ,(ncol(pred) - 3):ncol(pred)]
  return(OUTS)
}

test<- mapply(n_preds=50, pop_in = c(NA, 1:19), plot_coef_localtemp)
par(mfrow=c(1,2), mar=c(1, 1, 1, 1) + 0.1, oma = c(4, 3.5, 2, 1) + 0.1)
# Donor
scaled_x<- seq(min(model_data$localtemp), max(model_data$localtemp), length.out = n_preds)
plot(scaled_x, test[,1]$donor$Estimate.donor, type = "l", ann=FALSE, las = 1, lwd=2, ylim = c(0,1))
lines(scaled_x, test[,1]$donor$Q2.5.donor, lwd=2, lty = "dashed")
lines(scaled_x, test[,1]$donor$Q97.5.donor, lwd=2, lty = "dashed")
for(i in 2:20) {lines(scaled_x, test[,i]$donor$Estimate.donor, col = alpha("grey50", 0.25), lwd = 2)}
# axis(side = 1)
mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Donor")
# Recipient
plot(scaled_x, test[,1]$recip$Estimate.recipient, type = "l", ann=FALSE, las = 1, lwd=2, ylim = c(0,1), yaxt = "n")
lines(scaled_x, test[,1]$recip$Q2.5.recipient, lwd=2, lty = "dashed")
lines(scaled_x, test[,1]$recip$Q97.5.recipient, lwd=2, lty = "dashed")
for(i in 2:20) {lines(scaled_x, test[,i]$recip$Estimate.recipient, col = alpha("grey50", 0.25), lwd = 2)}
# axis(side = 1)
axis(side = 2, labels = FALSE, at = seq(0, 1, by = 0.2))
mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Recipient")
mtext(side = 1, line = 1.75, outer = TRUE, font = 2, cex = 1.50, "Local Temperature (z-scored)")
mtext(side = 2, line = 2.0, outer = TRUE, font = 2, cex = 1.50, "Rate")

# Panel plot of escapements vs. rates
test<- mapply(n_preds=50, pop_in = c(NA, 1:19), plot_coef_esc)
par(mfrow=c(5, 4), mar=c(1, 2, 2, 1) + 0.1, oma = c(4, 4, 2, 1) + 0.1)
y_axis_plots<- c(1, 5, 9, 13, 17)
for(i in 1:20) {
  scaled_x<- seq(min(model_data$escape), max(model_data$escape), length.out = n_preds)
  plot(scaled_x, test[,i]$donor$Estimate.donor, type = "l", ann=FALSE, col = alpha("purple", 0.90), las = 1, lwd=2, yaxt = "n", ylim = c(0,1))
  if(i %in% y_axis_plots) {axis(side = 2, las = 2)}
  lines(scaled_x, test[,i]$recip$Estimate.recipient, lwd=2, col = alpha("blue", 0.90))
  if(i==1) mtext(side = 3, line = 0, font = 2, "Unsampled", cex = 0.50) else {
    mtext(side = 3, line = 0, font = 2, unique(cleaned_df$hatcheryname)[i-1], cex = 0.50)
  }
}
mtext(side = 1, line = 2.5, font =2, "Escapement (1000s)", outer = TRUE)
mtext(side = 2, line = 2.5, font = 2, "Rate", outer = TRUE)

# Panel plot of Contemp
test<- mapply(n_preds=50, pop_in = c(NA, 1:19), plot_coef_contemp)
par(mfrow=c(5, 4), mar=c(1, 2, 2, 1) + 0.1, oma = c(4, 4, 2, 1) + 0.1)
y_axis_plots<- c(1, 5, 9, 13, 17)
for(i in 1:20) {
  scaled_x<- seq(min(model_data$contemp), max(model_data$contemp), length.out = n_preds)
  plot(scaled_x, test[,i]$donor$Estimate.donor, type = "l", ann=FALSE, col = alpha("purple", 0.90), las = 1, lwd=2, yaxt = "n", ylim = c(0,1))
  if(i %in% y_axis_plots) {axis(side = 2, las = 2)}
  lines(scaled_x, test[,i]$recip$Estimate.recipient, lwd=2, col = alpha("blue", 0.90))
  if(i==1) mtext(side = 3, line = 0, font = 2, "Unsampled", cex = 0.50) else {
    mtext(side = 3, line = 0, font = 2, unique(cleaned_df$hatcheryname)[i-1], cex = 0.50)
  }
}
mtext(side = 1, line = 2.5, font =2, "Confluence Temp. (C)", outer = TRUE)
mtext(side = 2, line = 2.5, font = 2, "Rate", outer = TRUE)

# Panel plot of Local temp
test<- mapply(n_preds=50, pop_in = c(NA, 1:19), plot_coef_localtemp)
par(mfrow=c(5, 4), mar=c(1, 2, 2, 1) + 0.1, oma = c(4, 4, 2, 1) + 0.1)
y_axis_plots<- c(1, 5, 9, 13, 17)
for(i in 1:20) {
  scaled_x<- seq(min(model_data$localtemp), max(model_data$localtemp), length.out = n_preds)
  plot(scaled_x, test[,i]$donor$Estimate.donor, type = "l", ann=FALSE, col = alpha("purple", 0.90), las = 1, lwd=2, yaxt = "n", ylim = c(0,1))
  if(i %in% y_axis_plots) {axis(side = 2, las = 2)}
  lines(scaled_x, test[,i]$recip$Estimate.recipient, lwd=2, col = alpha("blue", 0.90))
  if(i==1) mtext(side = 3, line = 0, font = 2, "Unsampled", cex = 0.50) else {
    mtext(side = 3, line = 0, font = 2, unique(cleaned_df$hatcheryname)[i-1], cex = 0.50)
  }
}
mtext(side = 1, line = 2.5, font =2, "Local Temp. (C)", outer = TRUE)
mtext(side = 2, line = 2.5, font = 2, "Rate", outer = TRUE)