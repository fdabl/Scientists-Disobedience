library(brms)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(doParallel)
library(RColorBrewer)
source('helpers.R')
#### Runs the Bayesian power analysis

n_pro <- 2
n_sci <- 3
n_pol <- 3

mu <- 2
tau_pro <- c(0, 0)
tau_pol <- c(0, 0, 0)
tau_sci <- c(0, 0, 0)
tau_int <- array(0, c(n_pro, n_sci, n_pol))

grid <- expand.grid(
  n = c(100, 125, 150, 175, 200),
  effect = c(0, seq(0.10, 0.30, 0.025)), # Cohen's \delta on latent level
  times = seq(400)
)

fit_all <- readRDS('models/models.RDS')

# Runs the Bayes factor power analysis for the t-test
if (!file.exists('results/power_results_two_groups.RDS')) {

  registerDoParallel(cores = detectCores() - 3)

  start <- Sys.time()
  simres <- foreach(i = seq(nrow(grid)), .combine = 'rbind') %dopar% {
    n <- grid[i, 'n']
    times <- grid[i, 'times']
    effect <- grid[i, 'effect']

    N <- n * n_pro * n_sci * n_pol
    tau_pro <- c(effect / 2, -effect / 2)
    group_means <- get_group_means(mu, tau_pro, c(0, 0, 0), c(0, 0, 0), tau_int)
    df <- sim_dat(n, group_means, group_sd = 1)

    fit_null <- fit_all[[1]]
    fit_full <- fit_all[[3]]

    x <- get_logml_all(df, list('null' = fit_null, 'full' = fit_full), cores = 1, chains = 2, iter = 1000)
    bfs <- c(
      get_bfs(x$fit_updated),
      log(get_directed_bf(x$fit_updated$full, groups = 2, pars = 'Protest'))
    )
    bfs <- unname(c(bfs[c(2, 3)], log(exp(bfs[2]) * exp(bfs[3]))))

    c(n, times, effect, bfs)
  }

  end <- Sys.time()
  print(end - start)

  saveRDS(simres, 'results/power_results_two_groups.RDS')
} else {
  simres <- readRDS('results/power_results_two_groups.RDS')
}

simres_prep <- prepare_simres(simres)
p2 <- plot_simres(simres_prep, title = 'Statistical power analysis for two groups')

# Runs the Bayes factor power analysis for the one-way ANOVA
if (!file.exists('results/power_results_three_groups.RDS')) {

  registerDoParallel(cores = detectCores() - 3)

  start <- Sys.time()
  simres <- foreach(i = seq(nrow(grid)), .combine = 'rbind') %dopar% {
    n <- grid[i, 'n']
    times <- grid[i, 'times']
    effect <- grid[i, 'effect']

    N <- n * n_pro * n_sci * n_pol
    tau_engagement <- rev(get_effects(effect, type = c(1, 2, 3)))
    group_means <- get_group_means(mu, c(0, 0), c(0, 0, 0), tau_engagement, tau_int)
    df <- sim_dat(n, group_means, group_sd = 1)

    fit_null <- fit_all[[1]]
    fit_full <- fit_all[[2]]

    x <- get_logml_all(df, list('null' = fit_null, 'full' = fit_full), cores = 1, chains = 2, iter = 1000)
    bfs <- c(
      get_bfs(x$fit_updated),
      log(get_directed_bf(x$fit_updated$full, groups = 3, pars = 'Engagement'))
    )
    bfs <- unname(c(bfs[c(2, 3)], log(exp(bfs[2]) * exp(bfs[3]))))

    c(n, times, effect, bfs)
  }

  end <- Sys.time()
  print(end - start)

  saveRDS(simres, 'results/power_results_three_groups.RDS')
} else {
  simres <- readRDS('results/power_results_three_groups.RDS')
}

simres_prep <- prepare_simres(simres)
p3 <- plot_simres(simres_prep, title = 'Statistical power analysis for three groups')

pdf('figures/power_analysis.pdf', width = 14, height = 7)
grid.arrange(p2, p3, ncol = 2)
dev.off()
