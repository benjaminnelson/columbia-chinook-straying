########## NON-GRAPHICAL FUNCTIONS #########
zoib_model<- function(mu_formula, zi_formula, chains = 8, iter = 4000, adpt_delta = 0.99) {
  zoib_model_1 <- bf(
    noquote(paste0("mvbind(donor, recipient) ~", mu_formula)),
    phi ~ 1,
    zi ~ zi_formula, noquote(paste0("zi ~", zi_formula)),
    family = zero_inflated_beta()
  )
  
  brms_model<- brm(
    zoib_model_1,
    data = model_data,
    cores = chains,
    chains = chains,
    iter = iter,
    warmup = iter / 2,
    control = list(adapt_delta = adpt_delta),
    seed = 1234
  )
}

run_models<- function(model_list = final_models, first = 1, last = length(model_list), chains = 8, iter = 4000) {
  for(i in first:last) {
    temp_model<- brm(
      model_list[[i]],
      data = model_data,
      cores = chains,
      chains = chains,
      iter = iter,
      warmup = iter / 2,
      control = list(adapt_delta = 0.99),
      seed = 1234
    )
    saveRDS(temp_model, paste0("final-model-", i, ".rds", sep=""))
    print(summary(temp_model))
    print(loo(temp_model))
  }
}

# Function for extracting LOOICs from saved models
loo_table<- function(start = 1, end = length(final_models)) {
  n_models<- length(seq(from = start, to = end, by = 1))
  loo_mat<- matrix(NA, nrow = n_models, ncol = 6)
  for(i in 1:n_models) {
    temp_model<- readRDS(paste0("final-model-", i, ".rds", sep = ""))
    temp_loo<- loo(temp_model)
    temp_ests<- temp_loo$estimates  
    loo_mat[i,]<- c(round(temp_ests[1,]), round(temp_ests[2,]), round(temp_ests[3,])) 
  }
  OUTS<- as.data.frame(loo_mat)
  names(OUTS)<- c("elpd_loo", "elpd_loo_se", "p_loo", "p_loo_se", "looic", "looic_se")
  return(OUTS)
}

## Function that tests whether coefs are 'significant' or not; can accomodate a df or two vals/bounds (lower, upper)
sig_test<- function(df = NULL, lower, upper) {
  base_func<- function(lower, upper) {
    result = FALSE
    if(lower > 0.0 & upper > 0.0){result = TRUE}
    if(lower < 0.0 & upper < 0.0){result = TRUE}
    return(result)
  }
  
  if(is.null(df)) {base_func(lower, upper)} else {
    result <- mapply(base_func, lower, upper)
    return(result)
  }
}

########## GRAPHICAL FUNCTIONS #########
# Violin plot of rates by type and populations
# Build plot
prop_plot<- function() { 
  cols<- viridis(2)
  prop_plot<- ggplot(all_data, aes(x = population, y = rate)) + 
    geom_jitter(aes(fill=rate_type, color=rate_type, alpha=0.25), shape=16, width = 0.25) +   
    geom_violin(aes(group = population, fill=rate_type, color=rate_type), trim = TRUE, scale = "area") +
    labs(x = "Population", y = "Rate") + 
    facet_grid(rate_type ~ .) +
    scale_fill_manual(values = rate_cols_fill) + 
    scale_color_manual(values = rate_cols) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          strip.text.x = element_text(size=12, color="black", 
          face = "bold"), strip.text.y = element_text(size=12, color="black", face="bold"), 
          legend.position = "none",
          axis.title.x = element_text(face = "bold", size=14, hjust = 0.5, vjust = 0.00),
          axis.title.y = element_text(face = "bold", size=14, hjust = 0.5, vjust = 2.50),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.spacing = unit(1.15, "lines")) +
    scale_x_discrete(labels = pop_labs, breaks = rep(1:19))
  prop_plot
}

prop_plot_combined<- function() { 
  cols<- viridis(2)
  # Plot aggregated zoib all_data
  quota_halves <- ggplot(all_data, aes(x = rate_type, y = rate)) +
    geom_half_point(aes(color = rate_type), 
                    color = rate_cols,
                    transformation = position_quasirandom(width = 0.1),
                    side = "l", size = 0.5, alpha = 0.5) +
    geom_half_boxplot(aes(fill = rate_type), side = "r", color = rate_cols, fill = rate_cols_fill) + 
    scale_y_continuous(labels = scales::label_percent()) +
    scale_fill_viridis_d(option = "plasma", end = 0.8) +
    scale_color_viridis_d(option = "plasma", end = 0.8) +
    guides(color = "none", fill = "none") +
    labs(y = "Rate", x = "Rate Type") +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5), # Add solid black axes
      axis.title.x = element_text(face = "bold", size=12, hjust = 0.5, vjust = 0.00),
      axis.title.y = element_text(face = "bold", size=12, hjust = 0.5, vjust = 2.50),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  quota_densities <- ggplot(all_data, aes(x = rate, fill = rate_type)) +
    geom_density(alpha = 0.6) +
    scale_x_continuous(labels = label_percent()) +
    scale_fill_viridis_d(option = "plasma", end = 0.8) +
    labs(x = "Rate", y = "Density", fill = "Rate") +
    scale_fill_manual(values = rate_cols) +  # Use manual color scale
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.5), # Add solid black axes
          axis.title.x = element_text(face = "bold", size=12, hjust = 0.5, vjust = 0.00),
          axis.title.y = element_text(face = "bold", size=12, hjust = 0.5, vjust = 2.50),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12)
    )
  
  prop_plot<- ggplot(all_data, aes(x = population, y = rate)) + 
    geom_jitter(aes(fill=rate_type, color=rate_type, alpha=0.25), shape=16, width = 0.25) +   
    geom_violin(aes(group = population, fill=rate_type, color=rate_type), trim = TRUE, scale = "area") +
    labs(x = "Population", y = "Rate") + 
    facet_grid(rate_type ~ .) +
    scale_fill_manual(values = rate_cols_fill) + 
    scale_color_manual(values = rate_cols) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          strip.text.x = element_text(size=12, color="black", 
          face = "bold"), strip.text.y = element_text(size=12, color="black", face="bold"), 
          legend.position = "none",
          axis.title.x = element_text(face = "bold", size=12, hjust = 0.5, vjust = 0.00),
          axis.title.y = element_text(face = "bold", size=12, hjust = 0.5, vjust = 2.50),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.spacing = unit(1.15, "lines")) +
    scale_y_continuous(labels = scales::label_percent()) +
    # scale_x_discrete(labels = as.character(unique(all_data$population)))
    scale_x_continuous(breaks = seq(1, 19, 1), labels = seq(1, 19, 1))
    # scale_x_discrete(labels = pop_labs, breaks = rep(1:19))
  # Panel dimensions via 'patchwork'
  (quota_halves | quota_densities) / prop_plot
}

model_summary<- function(model, shiny=FALSE) {
  summary(model)
  brms::pp_check(model, resp = "donor", ndraws = 100)
  brms::pp_check(model, resp = "recipient", ndraws = 100)
  if(shiny==TRUE) {brms::launch_shinystan(model)}
}

## Coefs plot for brms models
coefs_plot<- function(models, covariates, axis_labs = FALSE, probs = c(0.90, 0.95), rate = "donor", zi = FALSE, y_min = -3, y_max = 3) {

  TICK_SPACE = 0.15

  # Set colors
  plot_cols<- viridis(length(models))
  # Loop over models in the list
  for(i in 1:length(models)) {
    temp_model<- models[i]
    temp_data<- temp_model$data
    temp_coefs<- as.data.frame(temp_model)
    if(i==1) {
      plot(1:length(covariates), 1:length(covariates), las = 1, type="n", ylim=c(y_min, y_max), xlim=c(0, length(covariates)), ann = FALSE, xaxt="n")
      if(axis_labs == TRUE){
        axis(1, at = 0:(length(covariates) - 1), labels = tools::toTitleCase(covariates)) }
      abline(h=0, lty = "dashed", col="grey50")
    }
    # Loop over covariates for plotting
    for(j in 1:length(covariates)) {
      # Subset the year effects for both donor and recipent rates
      cols_to_keep <- grep(paste0(rate, "_", covariates[j], sep=""), names(temp_coefs), value = TRUE)
      df_subset <- data.frame(temp_coefs[, cols_to_keep])
      if(ncol(df_subset) > 1){zi_col = TRUE}
      df_subset_col<- ifelse(zi == TRUE & zi_col == TRUE, 2, 1) 
      # Add plotting features for each covariate
      segments((j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.025), (j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.975), lwd=1, col="black")
      segments((j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.05), (j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.95), lwd=3, col="grey50")
      points((j - 1) + (TICK_SPACE * (i-1)), median(df_subset[, df_subset_col]), cex=1.50, pch=16, col=plot_cols[i])
    }
  }
}

plot_year_effects<- function(model) {
  ## Make df of coefs
  model_df<- as.data.frame(model)
  # Subset the year effects for both donor and recipent rates
  donor_cols_to_keep <- grep("b_donor_as.factoryear", names(model_df), value = TRUE)
  recipient_cols_to_keep <- grep("b_recipient_as.factoryear", names(model_df), value = TRUE)
  df_subset_donor <- model_df[, donor_cols_to_keep]
  df_subset_recipient <- model_df[, recipient_cols_to_keep]
  # Plot the year effects for the donor and recipient rates on the same figure
  par(mfrow=c(1,1), mar=c(1, 1, 1, 1) + 0.1, oma = c(5, 4, 2, 1) + 0.1)
  plot(1994:2015, as.numeric(matrixStats::colQuantiles(as.matrix(df_subset_donor), probs = c(0.50, 0.025, 0.975))[ ,"50%"]), ylim = c(-2.5, 2.5), type = "l", lwd=2, col="purple")
  abline(h = 0, col="grey50", lty= "dashed")
  lines(1994:2015, as.numeric(matrixStats::colQuantiles(as.matrix(df_subset_donor), probs = c(0.50, 0.025, 0.975))[ ,"97.5%"]), col="purple", lty="dashed")
  lines(1994:2015, as.numeric(matrixStats::colQuantiles(as.matrix(df_subset_donor), probs = c(0.50, 0.025, 0.975))[ ,"2.5%"]), col="purple", lty="dashed")
  lines(1994:2015, as.numeric(matrixStats::colQuantiles(as.matrix(df_subset_recipient), probs = c(0.50, 0.025, 0.975))[ ,"97.5%"]), col="green", lty="dashed")
  lines(1994:2015, as.numeric(matrixStats::colQuantiles(as.matrix(df_subset_recipient), probs = c(0.50, 0.025, 0.975))[ ,"2.5%"]), col="green", lty="dashed")
  lines(1994:2015, as.numeric(matrixStats::colQuantiles(as.matrix(df_subset_recipient), probs = c(0.50, 0.025, 0.975))[ ,"50%"]), col="green", lwd=2)
}

## Coefs plot for brms models
coefs_plot_2<- function(models, covariates, legend_on = TRUE, axis_labs = FALSE, probs = c(0.90, 0.95), rate = "donor", zi = FALSE, y_min = -3, y_max = 3) {
  
  TICK_SPACE = 0.15
  legend_lab = c("ConTemp", "LocalTemp")
  # Set colors
  plot_cols<- viridis(length(models))
  # Loop over models in the list
  for(i in 1:length(models)) {
    temp_model<- models[i]
    temp_data<- temp_model$data
    temp_coefs<- as.data.frame(temp_model)
    if(i==1) {
      plot(1:length(covariates), 1:length(covariates), las = 1, type="n", ylim=c(y_min, y_max), xlim=c(0, length(covariates)), ann = FALSE, xaxt="n")
      if(axis_labs == TRUE){
        axis(1, at = 0:(length(covariates) - 1), labels = tools::toTitleCase(covariates)) }
      abline(h=0, lty = "dashed", col="grey50")
    }
    # Loop over covariates for plotting
    for(j in 1:length(covariates)) {
      # Subset the year effects for both donor and recipent rates
      cols_to_keep <- grep(paste0(rate, "_", covariates[j], sep=""), names(temp_coefs), value = TRUE)
      df_subset <- data.frame(temp_coefs[, cols_to_keep])
      if(ncol(df_subset) > 1){zi_col = TRUE}
      df_subset_col<- ifelse(zi == TRUE & zi_col == TRUE, 2, 1) 
      # Add plotting features for each covariate
      # Mu
      segments((j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.025), (j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.975), lwd=1, col="black")
      segments((j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.05), (j - 1) + (TICK_SPACE * (i-1)), quantile(df_subset[, df_subset_col], probs = 0.95), lwd=3, col="grey50")
      points((j - 1) + (TICK_SPACE * (i-1)), median(df_subset[, df_subset_col]), cex=1.50, pch=16, col=plot_cols[i])
      # # Zi
      # segments((j - 1) + (TICK_SPACE * (i-1)) + 0.50, quantile(df_subset[, df_subset_col], probs = 0.025), (j - 1) + (TICK_SPACE * (i-1)) + 0.5, quantile(df_subset[, df_subset_col], probs = 0.975), lwd=1, col="black")
      # segments((j - 1) + (TICK_SPACE * (i-1)) + 0.50, quantile(df_subset[, df_subset_col], probs = 0.05), (j - 1) + (TICK_SPACE * (i-1)) + 0.5, quantile(df_subset[, df_subset_col], probs = 0.95), lwd=3, col="grey50")
      # points((j - 1) + (TICK_SPACE * (i-1)) + 0.50, median(df_subset[, df_subset_col]), cex=1.50, pch=17, col=plot_cols[i])
    }
    # Add legend
    if(legend_on == TRUE) {
      # text(length(covariates) - 0.5, y_max - (i * 0.45), paste0("Model ", i, sep=""), font=2, col=plot_cols[i], cex = 1.25)
      text(length(covariates) - 0.5, y_max - (i * 0.45), legend_lab[i], font=2, col=plot_cols[i], cex = 1.25)
    }
  }
}

plot_fit<- function(model, model_data) {
  ## Get ypreds
  brms_ypreds<- brms::posterior_predict(model)
  brms_ypreds_y1<- matrixStats::colQuantiles(brms_ypreds[,,1], probs = c(0.50, 0.025, 0.975))
  brms_ypreds_y2<- matrixStats::colQuantiles(brms_ypreds[,,2], probs = c(0.50, 0.025, 0.975))
  # Plot model fit
  par(mfrow = c(4,5), oma=c(4, 3, 2, 1) + 0.1, mar=c(1, 2, 1, 1) + 0.1)
  for(i in 1:length(unique(model_data$population))) {
    temp_pop<- model_data[model_data$population==i, ]
    temp_rows<- temp_pop$sample
    temp_y_max<- 1.25 * max(brms_ypreds_y1[temp_rows,3])
    plot(temp_pop$year, temp_pop$donor, pch=16, col=alpha("black", 0.50), ann = FALSE, cex=1.1, las=1, ylim = c(0, temp_y_max))
    lines(temp_pop$year, brms_ypreds_y1[temp_rows,1], col="purple")
    lines(temp_pop$year, brms_ypreds_y1[temp_rows,3], lty="dashed", col="purple")
    lines(temp_pop$year, brms_ypreds_y1[temp_rows,2], col="purple", lty="dashed")
  }
}

## Area coef plot
plot_coef_2<- function(n_preds=n_preds, pop_in = NA) {
  # n_preds = 50
  newdf = data.frame(id = 1:n_preds, population = pop_in,
                     localflow = mean(model_data$localflow), 
                     localtemp = mean(model_data$localtemp),
                     escape = mean(model_data$escape),
                     distance = mean(model_data$distance),
                     distsource = mean(model_data$distsource),
                     difftemp = mean(model_data$difftemp),
                     area = seq(
                       min(model_data$area), 
                       max(model_data$area),
                       length.out = n_preds))
  
  pred = predict(best_model_list[[2]], newdata = newdf)
  pred = cbind(newdf, pred)
  OUTS<- NULL
  OUTS$donor = pred[ ,(ncol(pred) - 7):(ncol(pred)-4)]
  OUTS$recip =  pred[ ,(ncol(pred) - 3):ncol(pred)]
  return(OUTS)
}

# Area coef plot w/ modified args
plot_coef_3<- function(n_preds=n_preds, pop_in = NA, min_in = NULL, max_in = NULL) {
  # n_preds = 50
  newdf = data.frame(id = 1:n_preds, population = pop_in,
                     localflow = mean(model_data$localflow), 
                     localtemp = mean(model_data$localtemp),
                     escape = mean(model_data$escape),
                     distance = mean(model_data$distance),
                     distsource = mean(model_data$distsource),
                     difftemp = mean(model_data$difftemp),
                     area = seq(
                       min(min_in), 
                       max(max_in),
                       length.out = n_preds))
  
  pred = predict(best_model_list[[2]], newdata = newdf)
  pred = cbind(newdf, pred)
  OUTS<- NULL
  OUTS$donor = pred[ ,(ncol(pred) - 7):(ncol(pred)-4)]
  OUTS$recip =  pred[ ,(ncol(pred) - 3):ncol(pred)]
  return(OUTS)
}

# Plot for area
area_plot<- function() {
  n_preds = 50
  test<- mapply(n_preds=n_preds, pop_in = c(NA, 1:19), plot_coef_2)
  # Donor
  par(mfrow=c(1,2), mar=c(1, 1, 1, 1) + 0.1, oma = c(4, 3.5, 2, 1) + 0.1)
  x_scaled_pre<- seq(min(model_data$area), max(model_data$area), length.out = n_preds)
  x_scaled<- exp(x_scaled_pre * sd(log(cleaned_df$area))+ mean(log(cleaned_df$area))) / 1000
  plot(x_scaled, test[,1]$donor$Estimate.donor, type = "l", ann=FALSE, las = 1, lwd=1, ylim = c(0,1), xaxt="n")
  polygon(c(x_scaled, rev(x_scaled)), c(test[,1]$donor$Q97.5.donor, rev(test[,1]$donor$Q2.5.donor)), col = alpha(plot_col[1], 0.20), border = NA)
  lines(x_scaled, test[,1]$donor$Q2.5.donor, lwd=1, lty = "dashed")
  lines(x_scaled, test[,1]$donor$Q97.5.donor, lwd=1, lty = "dashed")
  axis(side = 1)
  mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Donor")
  # Recipient
  plot(x_scaled, test[,1]$recip$Estimate.recipient, type = "l", ann=FALSE, las = 1, lwd=1, ylim = c(0,1), xaxt="n", yaxt = "n")
  polygon(c(x_scaled, rev(x_scaled)), c(test[,1]$recip$Q97.5.recipient, rev(test[,1]$recip$Q2.5.recipient)), col = alpha(plot_col[2], 0.20), border = NA)
  lines(x_scaled, test[,1]$recip$Q2.5.recipient, lwd=1, lty = "dashed")
  lines(x_scaled, test[,1]$recip$Q97.5.recipient, lwd=1, lty = "dashed")
  axis(side = 1)
  axis(side = 2, labels = FALSE, at = seq(0, 1, by = 0.2))
  mtext(side = 3, line = 0, outer = FALSE, font = 2, cex = 1.20, "Recipient")
  mtext(side = 1, line = 1.75, outer = TRUE, font = 2, cex = 1.50, "Sq. km (1000s)")
  mtext(side = 2, line = 2.0, outer = TRUE, font = 2, cex = 1.50, "Rate")
}

# Generate area table
area_table<- function(outs=NULL){
  obs_areas = rep(NA, length=19)
  for(i in 1:19) {obs_areas[i]<- model_data[model_data$population==i, ]$area[1]}
  obs_areas_orig = rep(NA, length=19)
  for(i in 1:19) {obs_areas_orig[i]<- cleaned_df[cleaned_df$population==i, ]$area[1]}
  area_data = data.frame(population = 1:19,
                         localflow = mean(model_data$localflow), 
                         localtemp = mean(model_data$localtemp),
                         escape = mean(model_data$escape),
                         distance = mean(model_data$distance),
                         distsource = mean(model_data$distsource),
                         difftemp = mean(model_data$difftemp),
                         area = obs_areas)
  area_preds<- predict(best_model_list[[2]], newdata = area_data)
  # Create df
  area_table<- round(data.frame(
    donor_mean = area_preds[,1,1],
    donor_low = area_preds[,3,1],
    donor_up = area_preds[,4,1],
    recipient_mean = area_preds[,1,2],
    recipient_low = area_preds[,3,2],
    recipient_up = area_preds[,4,2]
  ), 2)
  # Final df
  donor_ints<- paste0(area_table$donor_mean, " (", area_table$donor_low, "-", area_table$donor_up, ")", sep = "")
  recip_ints<- paste0(area_table$recipient_mean, " (", area_table$recipient_low, "-", area_table$recipient_up, ")", sep = "")
  final_area_table<- data.frame(
    population = unique(cleaned_df$hatcheryname),
    area = obs_areas_orig,
    donor_rate = donor_ints,
    recipient_rate = recip_ints)
  if(outs == TRUE) {write.csv(final_area_table, "area_table.csv", row.names = FALSE)}
}