library(brms)
library(dplyr)
source('helpers.R')

###
# This script shows how we will test our hypotheses on example data
#
# - policy support
# - support of activists
# - perceived radicalness of protest
# - credibility of environmental scientists in general
# - credibility of the protesting individual environmental scientist in particular
# - donation behaviour
#
# Everything is measured on 5-point Likert scales, except donation behaviour, which
# is measured in dollars (ranging from 0 to 100). We use linear rather than ordinal
# for this outcome. Here we only demonstrate ordinal regression.
###

###
# All 19 models
###
formulas <- c(

  # A: Engagement form (No engagement x Support only x Engagement)
  # B: Protest form (March vs. Civil disobedience)
  # C: Political orientation (Democrat x Independent x Republican)

  # Size: 0
  '1',

  # Size: 1
  'A',
  'B',
  'C',

  # Size: 2
  'A + B',
  'A + C',
  'B + C',

  # Size: 3
  'A + B + C',
  'A + B + A:B',
  'A + C + A:C',
  'B + C + B:C',

  # Size: 4
  'A + B + C + A:C',
  'A + B + C + B:C',
  'A + B + C + A:B',

  # Size: 5
  'A + B + C + A:C + B:C',
  'A + B + C + A:B + A:C',
  'A + B + C + A:B + B:C',

  # Size: 6
  'A + B + C + A:B + A:C + B:C',

  # Size: 7
  'A + B + C + A:B + A:C + B:C + A:B:C'
)

formulas_named <- str_replace_all(
  str_replace_all(
    str_replace_all(formulas, 'A', 'Engagement'), 'B', 'Protest'
  ), 'C', 'Politics'
)

###
# Prior model specification
###

cohen_d <- function(m1, m2, sd1, sd2, n1, n2) {
  sd_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  (m1 - m2) / sd_pooled
}

# Friedman (2024)
n1 <- 41 + 42
n2 <- 54 + 53
cohen_d(7.13, 6.38, 2.05, 2.19, n1, n2) # Risk to humans
cohen_d(5.70, 4.89, 2.44, 2.25, n1, n2) # Personal worry

###
# Pre-compile model so that estimation on 'actual' data is quicker
###

if (!file.exists('models/models_scaled.RDS')) {
  # Data generated below only used to estimate all models, which can
  # then be updated with newly generated data in the power analysis below
  # this avoids recompiling the model each time

  # Define the number of levels for each factor and number of replications per cell
  n_protest <- 2
  n_engagement <- 3
  n_politics <- 3
  n <- 100
  N <- n * n_protest * n_engagement * n_politics

  mu <- 2
  tau_protest <- c(0, 0)
  tau_politics <- c(0, 0, 0)
  tau_engagement <- c(0, 0, 0)
  tau_int <- array(rep(0, N), c(n_protest, n_engagement, n_politics))
  group_means <- get_group_means(mu, tau_protest, tau_politics, tau_engagement, tau_int)
  df <- sim_dat(n, group_means, group_sd = 1)

  numCores <- detectCores() - 3
  cl <- makeCluster(numCores)
  registerDoParallel(cl)

  fit_all <- foreach(i = seq(length(formulas_named)), .packages = 'brms') %dopar% {

    x <- formulas_named[i]
    form <- as.formula(paste0('Y ~ ', x))

    if (x == '1') {
      m <- brm(
        formula = form,
        data = df, family = brms::cumulative('probit'),
        cores = 4, chains = 4, iter = 1000, save_pars = save_pars(all = TRUE)
      )
    } else {

      # Assumed a certain order of formulas_named and priors below
      engagement_ix <- c(2, 3)
      protest_ix <- c(1)
      politics_ix <- c(4, 5)

      engagement_protest_ix <- c(10, 11)
      engagement_politics_ix <- c(6, 7, 8, 9)
      protest_politics_ix <- c(12, 13)

      all_three_ix <- c(14, 15, 16, 17)

      prior_index_map <- list(
        '2' = engagement_ix,
        '3' = protest_ix,
        '4' = politics_ix,

        '5' = c(engagement_ix, protest_ix),
        '6' = c(engagement_ix, politics_ix),
        '7' = c(protest_ix, politics_ix),

        '8' = c(engagement_ix, protest_ix, politics_ix),

        '9' = c(engagement_ix, protest_ix, engagement_protest_ix),
        '10' = c(engagement_ix, politics_ix, engagement_politics_ix),
        '11' = c(protest_ix, politics_ix, protest_politics_ix),

        '12' = c(engagement_ix, protest_ix, politics_ix, engagement_politics_ix),
        '13' = c(engagement_ix, protest_ix, politics_ix, protest_politics_ix),
        '14' = c(engagement_ix, protest_ix, politics_ix, engagement_protest_ix),

        '15' = c(engagement_ix, protest_ix, politics_ix, engagement_politics_ix, protest_politics_ix),
        '16' = c(engagement_ix, protest_ix, politics_ix, engagement_protest_ix, engagement_politics_ix),
        '17' = c(engagement_ix, protest_ix, politics_ix, engagement_protest_ix, protest_politics_ix),

        '18' = c(engagement_ix, protest_ix, politics_ix, engagement_protest_ix, engagement_politics_ix, protest_politics_ix),
        '19' = c(engagement_ix, protest_ix, politics_ix, engagement_protest_ix, engagement_politics_ix, protest_politics_ix, all_three_ix)
      )

      prior_ix <- prior_index_map[[as.character(i)]]

      s1 <- 2
      s2 <- 4

      prior_width <- 0.15
      priors <- c(
        # Priors for main effects
        set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Protest1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Engagement1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Engagement2'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Politics1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Politics2'),

        # Priors for two-way interactions
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Engagement1:Politics1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Engagement1:Politics2'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Engagement2:Politics1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Engagement2:Politics2'),

        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Engagement1:Protest1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Engagement2:Protest1'),

        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Protest1:Politics1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s1), class = 'b', coef = 'Protest1:Politics2'),

        # Priors for three-way interactions
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s2), class = 'b', coef = 'Engagement1:Protest1:Politics1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s2), class = 'b', coef = 'Engagement1:Protest1:Politics2'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s2), class = 'b', coef = 'Engagement2:Protest1:Politics1'),
        set_prior(sprintf('student_t(1, 0, %f)', prior_width / s2), class = 'b', coef = 'Engagement2:Protest1:Politics2')
      )

      priors <- priors[prior_ix, ]

      print(x)
      print(priors)

      m <- brm(
        formula = form,
        data = df, family = brms::cumulative('probit'),
        prior = priors, cores = 4, chains = 4, iter = 1000, save_pars = save_pars(all = TRUE)
        # prior = c(
        #   prior(student_t(1, 0, 0.15), class = b),
        #   prior(student_t(1, 0, 2.5), class = Intercept)
        # ), cores = 4, chains = 4, iter = 1000, save_pars = save_pars(all = TRUE)
      )
      m
    }
  }

  saveRDS(fit_all, 'models/models_scaled.RDS')
} else {
  fit_all <- readRDS('models/models_scaled.RDS')
}

#######################
# Confirmatory Analyses
#######################

# H1: No effect of protest form on policy support [Main effect, only look at March versus CD]
# H2: More activist support for protest than civil disobedience [Main effect, only look at March versus CD]
# H3: Less perceived radicalness for protest than civil disobedience [Main effect, only look at March versus CD]
# H4: Highest policy support for scientists joining, then scientists supporting, then no engagement
# H5: Highest activist support for scientists joining, then scientists supporting, then no engagement
# H6: Protest perceived as least radical when scientists engage in it, then scientists supporting, then no engagement
# H7: Civil disobedience perceived as least radical when scientists engage in it, then scientists supporting, then no engagement
# H8: No effect of engagement type on science credibility (source, only compare March versus CD, control does not have source)
# H9: No effect of engagement type on science credibility (general)

# In sum, there are six different types of statistical specifications on the means:
# - Comparing two groups against each other:
#     - Null (\mu1 = \mu2)
#     - Full (\mu1 != \mu2)
#     - Constrained (\mu1 > \mu2)
# - Comparing three groups against each other:
#     - Null (\mu1 = \mu2 = \mu3)
#     - Full (\mu1 != \mu2 != \mu3)
#     - Constrained (\mu1 > \mu2 > \mu3)

# Below, we show how to test our confirmatory hypotheses using policy support as an example
# Following that, we show how to run the exploratory Bayesian model-averaged ordinal ANOVA
# We assume that the relevant hypotheses are true

# Define the number of levels for each factor and number of replications per cell
n_protest <- 2
n_engagement <- 3
n_politics <- 3
n <- 150
N <- n * n_protest * n_engagement * n_politics

## Test hypotheses related to policy support, assume that all hypotheses are true
# H1: No effect of protest form on policy support
# H4: Highest policy support for scientists joining, then scientists supporting, then no engagement
set.seed(2024)

mu <- 2
effect <- 0.30
tau_protest <- c(0, 0)
tau_politics <- c(0, 0, 0)
tau_engagement <- rev(get_effects(effect, type = c(1, 2, 3)))
tau_int <- array(rep(0, N), c(n_protest, n_engagement, n_politics))
group_means <- get_group_means(mu, tau_protest, tau_politics, tau_engagement, tau_int)
df_policy <- sim_dat(n, group_means, group_sd = 1)

# No difference of protest form on policy support
df_policy %>%
  group_by(Protest) %>%
  summarize(
    Y_mean_likert = mean(Y),
    Y_mean_cont = mean(Y_cont),
    Y_sd_likert = sd(Y),
    Y_sd_cont = sd(Y_cont)
  )

# Engagement has an increasing effect
df_policy %>%
  group_by(Engagement) %>%
  summarize(
    Y_mean_likert = mean(Y),
    Y_mean_cont = mean(Y_cont),
    Y_sd_likert = sd(Y),
    Y_sd_cont = sd(Y_cont)
  )

#####################################
## To test H1, we compare the models
## H_0: Protest = Civil disobedience
## H_1: Protest != Civil disobedience
#####################################
null_model <- fit_all[[1]]
full_model <- fit_all[[3]]
h1_test <- get_logml_all(df_policy, list('null' = null_model, 'full' = full_model), cores = 4)
exp(h1_test$logmls['null'] - h1_test$logmls['full'])

##############################################
## To test H4, we compare the models
## H_0: Joining = Supporting = No Engagement
## H_1: Joining != Supporting != No Engagement
## H_r: Joining > Supporting > No Engagement
##############################################
null_model <- fit_all[[1]]
full_model <- fit_all[[2]]

h4_test <- get_logml_all(
  df_policy,
  list('H0' = null_model, 'H1' = full_model)
)

# All marginal likelihoods
logmls_all <- c(
  h4_test$logmls,
  'H_r1' = unname(get_directed_bf(full_model) + h4_test$logmls[2])
)

logmls_all # ordinal model does best (but only slightly compared to full model)

##########################
# Model-averaged inference
##########################
logmls <- get_logml_all(df_policy, fit_all, iter = 1000, cores = 1, chains = 2)
bfs <- get_bfs(logmls$fit_updated)

## Calculate posterior
uniform_prior <- rep(1/19, 19)
model_posterior <- (exp(logmls$logmls - max(logmls$logmls)) * uniform_prior) /
  sum(exp(logmls$logmls - max(logmls$logmls)) * uniform_prior)

# Inclusion Bayes factors
inclusion_bfs <- c(
  'A' = get_inclusion_bf_main(formulas, 'A', uniform_prior, model_posterior),
  'B' = get_inclusion_bf_main(formulas, 'B', uniform_prior, model_posterior),
  'C' = get_inclusion_bf_main(formulas, 'C', uniform_prior, model_posterior),
  'A:B' = get_inclusion_bf_int(formulas, 'A:B', uniform_prior, model_posterior),
  'A:C' = get_inclusion_bf_int(formulas, 'A:C', uniform_prior, model_posterior),
  'B:C' = get_inclusion_bf_int(formulas, 'B:C', uniform_prior, model_posterior),
  'A:B:C' = get_inclusion_bf_int(formulas, 'A:B:C', uniform_prior, model_posterior)
)
