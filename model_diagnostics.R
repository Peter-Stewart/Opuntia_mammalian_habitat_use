# Occupancy ####
# Reload post and diagnostics lists using pars=NULL to get all parameters
for(i in 1:12){
  print(sum(diagnostics_list_grid1[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(diagnostics_list_grid2[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(diagnostics_list_fine1[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(diagnostics_list_fine2[[i]]$divergent__))
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(post_list_grid1[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(post_list_grid2[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(post_list_fine1[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(post_list_fine2[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}


# Total detections ####
# Reload post and diagnostics lists using pars=NULL to get all parameters
for(i in 1:12){
  print(sum(total_activity_diagnostics_list_grid1[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(total_activity_diagnostics_list_grid2[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(total_activity_diagnostics_list_fine1[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(total_activity_diagnostics_list_fine2[[i]]$divergent__))
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(total_activity_post_list_grid1[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(total_activity_post_list_grid2[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(total_activity_post_list_fine1[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(total_activity_post_list_fine2[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}


# Day-night detections ####
# Reload post and diagnostics lists using pars=NULL to get all parameters
for(i in 1:12){
  print(sum(day_night_diagnostics_list_grid1[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(day_night_diagnostics_list_grid2[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(day_night_diagnostics_list_fine1[[i]]$divergent__))
}
for(i in 1:12){
  print(sum(day_night_diagnostics_list_fine2[[i]]$divergent__))
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(day_night_post_list_grid1[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(day_night_post_list_grid2[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(day_night_post_list_fine1[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}

pars_diagnostics <- list()
for(i in 1:12){
  print(i)
  pars_diagnostics[[i]] <- summarise_draws(day_night_post_list_fine2[[i]], "rhat", "ess_bulk", "ess_tail")
}
par(mfrow=c(4,3))
for(i in 1:12){
  print(i)
  print(summary(pars_diagnostics[[i]]$rhat))
  print(summary(pars_diagnostics[[i]]$ess_bulk))
  print(summary(pars_diagnostics[[i]]$ess_tail))
  plot(pars_diagnostics[[i]]$rhat ~ pars_diagnostics[[i]]$ess_bulk, xlab = "ess_bulk", ylab = "Rhat")
  abline(h = 1)
  abline(h = 1.05, lty = 2, col = "red")
}
