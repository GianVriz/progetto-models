#libraries.

for (f in list.files("src", pattern="*.R")) {
  cat("f", "\n")
  source(paste0("src/", f))
}

require("devtools")
devtools::install_github('Nth-iteration-labs/contextual')
library(contextual)
library(ggplot2)
library(useful)
memory.limit(size = 50000)

savedir = "simulated-basic-algorithms2"
done = list.files(ffData("sims", savedir), pattern="history*")
out = NULL
for(i in 1:length(done)){
  cat("i =", i, "\n")
  load(ffData("sims", savedir, done[i]))
  tmp = history$get_data_frame()
  out = cbind(out, tmp$cum_reward_rate)
}

out = data.frame(out)
save(out, file="cumulative-reward.RData")

df = history$get_data_frame()
df$agent = as.character(df$agent)

cum_rew = cbind("agent" = df$agent, "t" = df$t, out)

# Standardized plot dimensions.
save_png = function(filename, ...) {
  png(filename,
      width = 1280,
      height = 768,
      res = 120,
      ...)
}

#Mean function.
mean_2 <- function(x) {
  {
    A <- apply(x[, 3:NCOL(x)], 1, mean)
  }
  return(A)
}

#C.I function.
quantile_2 <- function(x, p) {
  y = x[, 3:NCOL(x)]
  A <- apply(y, 1, function(y) quantile(y, p))
  return(A)
}

alpha = 0.05

#Dataset
df_plot = cum_rew[, c(1,2)]
df_plot$med = quantile_2(cum_rew, 0.5)
df_plot$lb = quantile_2(cum_rew, alpha/2)
df_plot$ub = quantile_2(cum_rew, 1-alpha/2)
save(df_plot,file="dfplot.RData")

# Overall Plot
ggplot(data = df_plot, aes(
  x = t,
  y = med,
  group = agent,
  colour = agent
)) + geom_line()+labs(y = "Cumulative Reward Rate", x = "Time") + ggtitle("Simulation")

last = df_plot$med[df_plot$t == max(df_plot$t)]

best = order(last, decreasing=TRUE)[1:4]
top = unique(df_plot$agent)[best]

#Plot best 4
library(magrittr)
plts = vector(mode="list", length = length(top))
for (i in 1:length(top)) {
  col = c("darkblue", "darkorange", "darkgreen", "purple")
  plts[[i]] = df_plot[df_plot$agent == top[i], ] %>%
    ggplot(aes(x = t, y = med)) + 
    geom_line(colour=col[i]) + 
    geom_ribbon(aes(ymin = lb, ymax = ub), fill=col[i], alpha = 0.2)+
    labs(y = "Cumul. Reward", x = "Time") +
    ggtitle(top[i]) +
    theme(plot.title = element_text(hjust = 0.5))
}

library(gridExtra)
do.call("grid.arrange", c(plts, ncol=2))
