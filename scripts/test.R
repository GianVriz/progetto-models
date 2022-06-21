# Source all files in /src
for (f in list.files("src", pattern="*.R")) {
    cat("f", "\n")
    source(paste0("src/", f))
}

dataset = "men"
dat = read.csv(ffData("open_bandit_dataset", "random", dataset, paste0(dataset, ".csv")))

horizon     <- NROW(dat)
simulations <- 1

# Set up formula:       y      ~ z    | x1 + x2 + ..
# In bandit parlance:   reward ~ arms | covariates

# Simple setup without covariate information
f = click ~ item_id | 1

# Instantiate Replay Bandit (Li, 2010) for i.i.d. sampled data 
bandit  <- OfflineReplayEvaluatorBandit$new(formula = f, data = dat)
# bandit  <- OfflineBootstrappedReplayBandit$new(formula = f, data = dat)


# Agent = Bandit scheme + arm Sampling policy
agents  <- list(
                Agent$new(ThompsonSamplingPolicy$new(), bandit, "TS"),
                Agent$new(UCB1Policy$new(), bandit, "UCB1"),
                Agent$new(UCB2Policy$new(), bandit, "UCB2"),
                Agent$new(RandomPolicy$new(), bandit, "Random")
                Agent$new(LinUCBDisjointOptimizedPolicy$new(0.01), bandit, "LinUCB 0.01"),
                Agent$new(LinUCBDisjointOptimizedPolicy$new(0.05), bandit, "LinUCB 0.05"),
                Agent$new(LinUCBDisjointOptimizedPolicy$new(0.1),  bandit, "LinUCB 0.1"),
                Agent$new(LinUCBDisjointOptimizedPolicy$new(1.0),  bandit, "LinUCB 1.0"),
                )


# Instantiate a Simulator
# t_over_sims: if default, randomly splits the data into portions to use for simulations
simulation <- Simulator$new(agents,
                            horizon = horizon,
                            simulations = simulations,
                            do_parallel = TRUE
                            )

# Run the simulation.
history    <- simulation$run()

# Find appropriate y lims
cumulative_data = history$get_cumulative_data()
ylim = c(0, max(cumulative_data$cum_reward_rate, na.rm=TRUE))

# Plot the results
save_png(filename=ffPlots("cumulative-reward.png"))
plot(history, type = "cumulative", regret = FALSE, rate = TRUE,
     legend_position = "topright", ylim = ylim, xlab = "time")
dev.off()