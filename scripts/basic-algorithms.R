# Source all files in /src
for (f in list.files("src", pattern="*.R")) {
    cat("f", "\n")
    source(paste0("src/", f))
}

dataset = "men"
dat = read.csv(ffData("open_bandit_dataset", "random", dataset, paste0(dataset, ".csv")))

# Set up formula:       y      ~ z    | x1 + x2 + ..
# Bandit notation:      reward ~ arms | covariates
# Simple setup without covariate information
f <- click ~ item_id | 1

# change type from "simulated" to "real" to run the algorithms on the dataset
#! Warning: "real" takes hours to run

type = "simulated"              # type of analysis to run
savedir = NULL                  # subdirectory to save results
horizon = NULL                  # horizon to run the algorithms
simulations = NULL              # number of simulations of each algorithm
bandit = NULL                   # type of bandit

if(type == "simulated"){
    savedir = "simulated-basic-algorithms"
    horizon = 50000
    simulations = 100

    # Simulate data by sampling rewards using the marginal probabilities of the observed item_id's
    tab = table(dat$item_id, dat$click)
    prop = tab[, 2] / rowSums(tab)
    bandit = BasicBernoulliBandit$new(prop)
} else if (type == "real") {
    #! Warning: takes a long time
    savedir = "basic-algorithms"
    horizon = NROW(data)
    simulations = 100

    # Instantiate Replay Bandit (Li, 2010) for i.i.d. sampled data
    bandit <- OfflineReplayEvaluatorBandit$new(formula = f, data = dat)
}


# Agent = Bandit scheme + arm Sampling policy
agents <- list(
    Agent$new(RandomPolicy$new(), bandit, "Random"),
    Agent$new(ThompsonSamplingPolicy$new(), bandit, "TS"),
    Agent$new(EpsilonGreedyPolicy$new(0.05), bandit, "EG 0.05"),
    Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EG 0.1"),
    Agent$new(EpsilonGreedyPolicy$new(0.2), bandit, "EG 0.2"),
    Agent$new(Exp3Policy$new(0.05), bandit, "Exp3 0.05"),
    Agent$new(Exp3Policy$new(0.1), bandit, "Exp3 0.1"),
    Agent$new(Exp3Policy$new(0.2), bandit, "Exp3 0.2"),
    Agent$new(SoftmaxPolicy$new(0.05), bandit, "Softmax 0.05"),
    Agent$new(SoftmaxPolicy$new(0.1), bandit, "Softmax 0.1"),
    Agent$new(UCB1Policy$new(), bandit, "UCB1"),
    Agent$new(UCB2Policy$new(), bandit, "UCB2")
)


# Instantiate a Simulator
# OfflineReplay (for "real" simulations):
#            t_over_sims: if left to default, randomly splits the dataset into smaller portions
#            to use for simulations

simulation <- Simulator$new(agents,
    horizon = horizon,
    simulations = simulations,
    do_parallel = TRUE
)

# Run the simulation
history <- simulation$run()

# Save results
sims_directory = ffData("sims", savedir)
if (!dir.exists(sims_directory)){
    dir.create(sims_directory, recursive=T)
} else {
    print("Dir already exists!")
}
history$save(ffData("sims", savedir, paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".RData")))
print("Saved data.")

# TODO: plot history results. Perhaps add pointwise confidence bands?...
plots_directory = ffPlots(savedir)
if (!dir.exists(plots_directory)){
    dir.create(plots_directory, recursive = T)
} else {
    print("Dir already exists!")
}
save_png(filename = ffPlots(savedir, "cumulative-reward.png"))
plot(history,
    type = "cumulative", regret = FALSE, rate = TRUE,
    legend_position = "topright", ylim = c(0,0.01), xlab = "time")
dev.off()
