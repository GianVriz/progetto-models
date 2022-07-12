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
    horizon = 30000
    simulations = 100

    # Simulate data by sampling rewards using the marginal probabilities of the observed item_id's
    tab = table(dat$item_id, dat$click)
    prop = tab[, 2] / rowSums(tab)
    bandit = BasicBernoulliBandit$new(prop)
} else if (type == "real") {
    #! Warning: takes a long time
    savedir = "basic-algorithms"
    horizon = NROW(dat)
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

done = list.files(ffData("sims", savedir), pattern="history*")

if(length(done) < simulations){
    for(i in 1:(simulations - length(done))){
        seed = sample(1:10^6, size=1)
        # Instantiate a Simulator
        # OfflineReplay (for "real" simulations):
        #            t_over_sims: if left to default, randomly splits the dataset into smaller portions
        #            to use for simulations
        simulation <- Simulator$new(agents,
            horizon = horizon,
            simulations = 1,
            do_parallel = TRUE,
            set_seed = seed
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
        savename = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
        save(history, file = ffData("sims", savedir, paste0("history-", savename, ".RData")))
        save(seed, file = ffData("sims", savedir, paste0("seed-", savename, ".Rdata")))
        print("Saved data.")
    }
}

done = list.files(ffData("sims", savedir), pattern="history*")
out = NULL
for(i in 1:length(done)){
    load(ffData("sims", savedir, done[i]))
    tmp = history$get_data_frame()
    out = cbind(out, tmp$cum_reward_rate)
}
i = 1
load(ffData("sims", savedir, done[i]))
df = history$get_data_frame()
df$cum_reward_rate = NULL
cbind(df, out)

# TODO: plot history results. We should plot the means and perhaps add pointwise confidence bands
plots_directory = ffPlots(savedir)
if (!dir.exists(plots_directory)){
    dir.create(plots_directory, recursive = T)
} else {
    print("Dir already exists!")
}
save_png(filename = ffPlots(savedir, "cumulative-reward.png"))

#! Example of plot for a single history file. We should plot using the `out` data frame instead.
#! Maybe copy the function that does the plot in the package repository to start?
plot(history,
    type = "cumulative", regret = FALSE, rate = TRUE,
    legend_position = "topright", ylim = c(0,0.01), xlab = "time")
dev.off()
