# Source all files in /src
for (f in list.files("src", pattern="*.R")) {
    cat("f", "\n")
    source(paste0("src/", f))
}

dat = read.csv(ffData("open_bandit_dataset", "random", "all", "all.csv"))

