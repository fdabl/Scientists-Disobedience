library(stringr)
library(doParallel)

get_bf_matrix <- function(logmls_all) {
  logmls <- logmls_all - max(logmls_all)
  bfs_favour_H0 <- exp(logmls)

  bf_matrix <- matrix(nrow = 3, ncol = 3)
  rownames(bf_matrix) <- c('H0  vs ', 'H1  vs ', 'H1r vs ')
  colnames(bf_matrix) <- c('H0', 'H1', 'H1r')

  for (i in 1:3) {
    for (j in 1:3) {
      bf_matrix[i, j] <- exp(logmls[i] - logmls[j])
    }
  }

  bf_matrix
}

create_prop <- function(
    df, varname,
    lo = 'Strongly disagree', hi = 'Strongly agree',
    political = FALSE
  ) {
  varname <- sym(varname)

  if (political) {
    df_prop <- df %>%
      group_by(Engagement, Protest, political_affiliation, !!varname) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      group_by(Engagement, Protest, political_affiliation) %>%
      mutate(
        proportion = count / sum(count),
        varname = quo_name(varname),
      ) %>%
      ungroup()

  } else {
    df_prop <- df %>%
      group_by(Engagement, Protest, !!varname) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      group_by(Engagement, Protest) %>%
      mutate(
        proportion = count / sum(count),
        varname = quo_name(varname),
      ) %>%
      ungroup()
  }

  df_prop %>%
    rename(
      value_name = quo_name(varname),
    ) %>%
    mutate(
      value_name = factor(
        ifelse(
          value_name == 1, paste0('1: ', lo),
          ifelse(value_name == 5, paste0('5: ', hi), value_name)
        ),
        levels = rev(c(paste0('1: ', lo), '2', '3', '4', paste0('5: ', hi)))
      )
    )
}

create_mean <- function(df, varname, political = FALSE) {
  varname <- sym(varname)

  if (political) {
    df_mean <- df %>%
      group_by(Engagement, Protest, political_affiliation) %>%
      summarize(
        estimate = mean(!!varname),
        ysd = sd(!!varname),
        yse = ysd / sqrt(n()),
        varname = quo_name(varname),
        conf.low = estimate - 1.96 * yse,
        conf.high = estimate + 1.96 * yse
      )
  } else {
    df_mean <- df %>%
      group_by(Engagement, Protest) %>%
      summarize(
        estimate = mean(!!varname),
        ysd = sd(!!varname),
        yse = ysd / sqrt(n()),
        conf.low = estimate - 1.96 * yse,
        conf.high = estimate + 1.96 * yse,
        varname = quo_name(varname)
      )
  }

  df_mean
}

# Create a nice figure visualizing the Likert responses and either
# the means of the Likert responses or the latent effect
create_plot <- function(
    df, varname, title, subtitle = NULL,
    political = FALSE, df_effects = NULL,
    lo = 'Strongly disagree', hi = 'Strongly agree'
  ) {

  df_prop <- create_prop(df, varname, political = political, lo = lo, hi = hi)

  if (is.null(df_effects)) {
    df_effects <- create_mean(df, varname, political = political)
    div <- 5
    offset <- 0
    sec_axis_title <- 'Mean Likert score'
    trans_func <- function(x) x * 5
  } else {
    div <- 1
    offset <- 0
    sec_axis_title <- 'Latent mean'

    # Have larger effects for political affiliation
    if (political) {
      trans_func <- function(x) 3 * x - 1.5
      trans_func_inv <- function(x) (x + 1.5) / 3
      div <- 2.5
      df_effects <- df_effects %>%
        rename(political_affiliation = Politics)
    } else {
      trans_func <- function(x) 1.2 * x - 0.60
      trans_func_inv <- function(x) (x + 0.60) / 1.2
    }
  }

  palette <- c('#E67C73', '#F2A489', '#F6E8A7', '#A3C4D4', '#5D92C7')

  # Unify 'Support / Endorse' condition name with text in manuscript
  if (varname != 'SourceCredibility') {
    labels <- c('Control', 'Endorsed', 'Engaged')
  } else {
    labels <- c('Endorsed', 'Engaged')
  }

  df_prop <- df_prop %>%
    mutate(Engagement = factor(Engagement, labels = labels))

  df_effects <- df_effects %>%
    mutate(Engagement = factor(Engagement, labels = labels))

  if (political) {
    p <- ggplot(
      df_prop,
      aes(x = Engagement, y = proportion, fill = as.factor(value_name))
    ) +
      geom_bar(stat = 'identity', position = 'stack') +
      geom_point(
        data = df_effects,
        aes(x = Engagement, y = trans_func_inv(estimate)),
        inherit.aes = FALSE, size = 2.5
      ) +
      geom_errorbar(
        data = df_effects,
        aes(
          x = Engagement,
          ymin = trans_func_inv(conf.low), ymax = trans_func_inv(conf.high)
        ),
        inherit.aes = FALSE, width = 0.30, linewidth = 1
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = '',
        y = 'Proportion'
      ) +
      scale_fill_manual(values = palette) +
      scale_y_continuous(
        limits = c(0, 1.01),
        sec.axis = sec_axis(transform = trans_func, name = sec_axis_title)
      ) +
      theme_minimal() +
      facet_wrap(~ Protest + political_affiliation) +
      theme(
        legend.position = 'top',
        legend.title = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.50),
        plot.subtitle = element_text(size = 14, hjust = 0.50)
      ) +
      guides(fill = guide_legend(reverse = TRUE))

  } else {

    p <- ggplot(
      df_prop,
      aes(x = Engagement, y = proportion, fill = as.factor(value_name))
    ) +
      geom_bar(stat = 'identity', position = 'stack') +
      geom_point(
        # data = df_effects, aes(x = Engagement, y = estimate / div + offset),
        data = df_effects, aes(x = Engagement, y = trans_func_inv(estimate)),
        inherit.aes = FALSE, size = 2.5
      ) +
      geom_errorbar(
        data = df_effects, aes(x = Engagement, ymin = trans_func_inv(conf.low), ymax = trans_func_inv(conf.high)),
        inherit.aes = FALSE, width = 0.30, linewidth = 1
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = '',
        y = 'Proportion'
      ) +
      scale_fill_manual(values = palette) +
      scale_y_continuous(
        # limits = c(0, 1.01),
        sec.axis = sec_axis(transform = trans_func, name = sec_axis_title)
      ) +
      theme_minimal() +
      facet_wrap(~ Protest) +
      theme(
        legend.position = 'top',
        legend.title = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.50),
        plot.subtitle = element_text(size = 14, hjust = 0.50)
      ) +
      guides(fill = guide_legend(reverse = TRUE))
  }

  p
}


get_directed_bf <- function(full_model, groups = 3, pars = 'Engagement', type = 'increasing') {
  post <- posterior_samples(full_model, pars = pars)

  if (groups == 3) {
    # engaged <- post[, 1]
    # support <- post[, 2]
    # control <- -(engaged + support)

    control <- post[, 1]
    support <- post[, 2]
    engaged <- -(control + support)

    constrained_prior <- 1 / 6

    if (type == 'increasing') {
      constrained_post <- mean(engaged > support & support > control)
    } else {
      constrained_post <- mean(engaged < support & support < control)
    }

    res <- constrained_post / constrained_prior
  }

  if (groups == 2) {
    constrained_prior <- 1 / 2
    cd <- post[, 1]

    if (type == 'increasing') {
      constrained_post <- mean(cd > 0)
    } else {
      constrained_post <- mean(cd < 0)
    }
    res <- constrained_post / constrained_prior
  }

  res
}


get_logml_all <- function(
    df, fit_all,
    prior_width = 0.15, recompile = FALSE,
    refresh = 0, ...
  ) {

  print(prior_width)

  set.seed(1)
  fit_updated <- lapply(fit_all, function(fit) {
    # priors <- c(
    #   # Priors for main effects
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Protest'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Engagement'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Politics'),
    #
    #   # Priors for two-way interactions
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 2), class = 'b', coef = 'Engagement:Protest'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 2), class = 'b', coef = 'Engagement:Politics'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 2), class = 'b', coef = 'Protest:Politics'),
    #
    #   # Priors for three-way interactions
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 4), class = 'b', coef = 'Engagement:Protest:Politics')
    # )

    s1 <- 2
    s2 <- 4
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

    update(
      fit, newdata = df, recompile = recompile, refresh = refresh,
      # prior = eval(parse(text = sprintf("prior(student_t(1, 0, %f), class = 'b')", prior_width))), ...
      prior = priors, ...
    )
  })

  logmls <- sapply(fit_updated, function(fit) {
    bridge_sampler(fit)$logml
  })

  list(
    'logmls' = logmls,
    'fit_updated' = fit_updated
  )
}

run_sensitivity <- function(
    df, fit_all, prior_widths,
    includes_ordered = FALSE,
    groups = NULL, pars = NULL, type = NULL
  ) {

  numCores <- detectCores() - 6
  cl <- makeCluster(numCores)
  registerDoParallel(cl)

  set.seed(1)
  res <- foreach(
    prior_width = prior_widths, .packages = 'brms',
    .export = c('get_logml_all', 'get_bf_matrix', 'get_directed_bf')
  ) %dopar% {

    fit <- get_logml_all(df, fit_all, recompile = TRUE, prior_width = prior_width, refresh = 1)

    if (includes_ordered) {
      bf_full_restricted <- get_directed_bf(
        fit$fit_updated$full, groups = groups, pars = pars, type = type
      )

      logml_restricted <- fit$logmls[2] + log(bf_full_restricted)
      logmls <- c(fit$logmls, logml_restricted)

    } else {
      logmls <- fit$logmls
    }

    get_bf_matrix(logmls)

  }

  res
}


run_sensitivity_inclusion <- function(
    df, fit_all, prior_widths, filename = NULL
) {

  res <- c()
  n_priors <- length(prior_widths)

  if (file.exists(filename)) {
    res <- readRDS(filename)
  } else {

    for (prior_width in prior_widths) {
      fit <- get_logml_all_parallel(
        df, fit_all, recompile = TRUE,
        prior_width = prior_width, refresh = 1
      )

      res <- rbind(res, c(get_inclusion_bayes_factors(fit)))
    }

    res <- cbind(res, prior_widths)
    colnames(res) <- c(
      'Engagement', 'Protest', 'Politics',
      'Engagement:Protest', 'Engagement:Politics', 'Protest:Politics',
      'Engagement:Protest:Politics', 'prior_widths'
    )
    rownames(res) <- NULL
    saveRDS(res, filename)
  }

  res
}


run_sensitivity_inclusion_donation <- function(
    df, prior_widths, filename = NULL
) {

  res <- c()
  n_priors <- length(prior_widths)

  if (file.exists(filename)) {
    res <- readRDS(filename)

  } else {

    numCores <- detectCores() - 5
    cl <- makeCluster(numCores)
    registerDoParallel(cl)

    res <- foreach(
      prior_width = prior_widths,
      .combine = 'rbind',
      .packages = 'BayesFactor',
      .export = c(
        'anovaBF', 'get_bf_inclusion_donation', 'get_size_order', 'sort_formulas',
        'normalize_probs', 'find_interaction', 'find_main', 'get_post', 'str_trim'
      )
    ) %dopar% {

      fit_donation <- anovaBF(
        Donation ~ Protest + Engagement + Politics, data = df,
        rscaleEffects = c(
          'Engagement' = prior_width, 'Protest' = prior_width, 'Politics' = prior_width
        )
      )

      inclusion_bfs_donation <- c(
        # Main effects
        get_bf_inclusion_donation(fit_donation, 'Engagement')[1],
        get_bf_inclusion_donation(fit_donation, 'Protest')[1],
        get_bf_inclusion_donation(fit_donation, 'Politics')[1],

        # Two-way interaction effects
        get_bf_inclusion_donation(fit_donation, 'Engagement', varname2 = 'Protest')[2],
        get_bf_inclusion_donation(fit_donation, 'Engagement', varname2 = 'Politics')[2],
        get_bf_inclusion_donation(fit_donation, 'Protest', varname2 = 'Politics')[2],

        # Tree-way interaction effects
        get_bf_inclusion_donation(fit_donation, 'Engagement')[3]
      )

      c(inclusion_bfs_donation, prior_width)
    }

    colnames(res) <- c(
      'Engagement', 'Protest', 'Politics',
      'Engagement:Protest', 'Engagement:Politics', 'Protest:Politics',
      'Engagement:Protest:Politics', 'prior_widths'
    )
    saveRDS(res, filename)
  }

  res
}

get_logml_all_parallel <- function(
    df, fit_all,
    prior_width = 0.15,
    recompile = FALSE,
    refresh = 0, ...
  ) {

  numCores <- detectCores() - 5
  cl <- makeCluster(numCores)
  registerDoParallel(cl)

  # Update fits in parallel
  fit_updated <- foreach(fit = fit_all, .packages = 'brms') %dopar% {

    # priors <- c(
    #   # Priors for main effects
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Protest'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Engagement'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width), class = 'b', coef = 'Politics'),
    #
    #   # Priors for two-way interactions
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 2), class = 'b', coef = 'Engagement:Protest'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 2), class = 'b', coef = 'Engagement:Politics'),
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 2), class = 'b', coef = 'Protest:Politics'),
    #
    #   # Priors for three-way interactions
    #   set_prior(sprintf('student_t(1, 0, %f)', prior_width / 4), class = 'b', coef = 'Engagement:Protest:Politics')
    # )

    s1 <- 2
    s2 <- 4
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

    update(
      fit, newdata = df, recompile = recompile, refresh = refresh,
      # prior = eval(parse(text = sprintf("prior(student_t(1, 0, %f), class = 'b')", prior_width))), ...
      prior = priors, ...
    )
  }

  # Calculate logmls in parallel
  logmls <- foreach(fit = fit_updated, .packages = 'bridgesampling') %dopar% {
    brms::bridge_sampler(fit)$logml
  }

  # Stop the parallel cluster
  stopCluster(cl)

  # Return results in the same order as input
  list(
    'logmls' = unlist(logmls),
    'fit_updated' = fit_updated
  )
}

get_effects <- function(d, type = c(1, 2, 2)) {

  if (all(type == c(1, 2, 2))) {
    b <- -d / 3
    a <- 2 * d / 3

    res <- c(a, b, b)
  } else if (all(type == c(1, 1, 2))) {

    a <- -d / 3
    b <- -2 * a
    res <- c(a, a, b)
  } else if (all(type == c(1, 2, 3))) {
    a <- d / 2
    res <- c(-a, 0, a)
  } else {
    stop('Error: Wrong type')
  }

  res
}


# Make sure that the effect sizes here are not standardized, so we get 'raw' group means
get_group_means <- function(mu, tau_pro, tau_pol, tau_sci, tau_int) {
  n_pro <- length(tau_pro)
  n_pol <- length(tau_pol)
  n_sci <- length(tau_sci)

  group_means <- numeric(length = n_pro * n_pol * n_sci)

  index <- 1

  for (i in seq_along(tau_pro)) {
    for (j in seq_along(tau_pol)) {
      for (k in seq_along(tau_sci)) {
        group_means[index] <- mu + tau_pro[i] + tau_pol[j] + tau_sci[k] + tau_int[i, j, k]
        index <- index + 1
      }
    }
  }

  group_means
}


sim_dat <- function(n_per_group, group_means, group_sd = 1) {

  # Simulating continuous scores
  scores <- c()
  k <- length(group_means)

  for (i in seq(k)) {
    scores <- c(scores, rnorm(n_per_group, group_means[i], group_sd))
  }

  # Mapping scores to Likert scale (1 to 5)
  # This example uses quantile-based discretization
  breaks <- quantile(scores, probs = seq(0, 1, length.out = 6))
  y_likert <- cut(scores, breaks = breaks, labels = 1:5, include.lowest = TRUE)
  y_likert <- as.numeric(y_likert)

  df <- expand.grid(
    id = seq(n_per_group),
    A = factor(
      c('Engaged', 'Support', 'Control'),
      levels = c('Engaged', 'Support', 'Control')
    ),
    C = factor(
      c('Democrat', 'Independent', 'Republican'),
      levels = c('Democrat', 'Independent', 'Republican')
    ),
    B = factor(
      c('Civil Disobedience', 'Legal March'),
      levels = c('Civil Disobedience', 'Legal March')
    )
  )

  # Sum coding
  contrasts(df$A) <- contr.sum(levels(df$A))
  contrasts(df$B) <- contr.sum(levels(df$B))
  contrasts(df$C) <- contr.sum(levels(df$C))

  normalize <- function(y) {
    y[y > 5] <- 5
    y[y < 0] <- 0
    y
  }

  colnames(df) <- c('id', 'Engagement', 'Politics', 'Protest')
  df$Y <- y_likert
  df$Y_cont <- scores
  df %>% select(id, Engagement, Protest, Politics, Y, Y_cont)
}

get_bfs <- function(models, ...) {
  # Assume first one is null model
  mls <- sapply(models, function(m) bridge_sampler(m, ...)$logml)

  ml0 <- mls[1]
  bfs <- sapply(mls, function(ml) ml - ml0)
  bfs
}

get_bfs <- function(logmls, ...) {
  # Assume first one is null model
  ml0 <- logmls[1]
  bfs <- sapply(logmls, function(ml) ml - ml0)
  bfs
}


prepare_simres <- function(res) {
  rownames(res) <- NULL
  colnames(res) <- c('n', 'times', 'effect', 'bf10', 'bfr1', 'bfr0')

  simres <- data.frame(res) %>%
    mutate(
      n = factor(n),
      effect = factor(effect)
    ) %>%
    group_by(n, effect) %>%
    summarize(
      undirected = mean(ifelse(effect == 0, exp(-bf10) >= 10, exp(bf10) >= 10)),
      directed = mean(ifelse(effect == 0, NA, exp(bfr0) >= 10))
    ) %>%
    ungroup() %>%
    data.frame %>%
    pivot_longer(cols = -(1:2)) %>%
    mutate(
      name = factor(name, levels = c('undirected', 'directed'))
    )

  simres
}

plot_simres <- function(simres_prep, title) {
  cols <- brewer.pal(12, 'Set3')
  p <- ggplot(
    simres_prep,
    aes(
      x = n, y = value, linetype = name, color = effect, group = interaction(name, effect)
    )
  ) +
    geom_point(size = 3) +
    geom_line(linewidth = 1.5) +
    ylab('Propotion BF > 10') +
    xlab('Sample size per group') +
    scale_color_manual(name = expression('Effect size ' ~ delta), values = cols) +
    scale_linetype_manual(
      name = 'Hypothesis',
      values = c('solid', 'dotted'),
      labels = c('undirected', 'directed')
    ) +
    scale_y_continuous(breaks = seq(0, 1, 0.10), limits = c(0, 1)) +
    theme_minimal() +
    labs(title = title) +
    theme(
      legend.position = 'top',
      # legend.title.align = 0.5,
      strip.text = element_text(size = 14),
      plot.title = element_text(hjust = 0.50, size = 18)
    ) +
    guides(color = guide_legend(nrow = 2, order = 1), linetype = guide_legend(nrow = 2, keywidth = 2, order = 2))
  p
}

get_prob <- function(delta, thresholds = c(-2, -1, 1, 2), group = 1) {
  K <- 5
  ys <- matrix(NA, nrow = length(delta), ncol = K)

  tr <- thresholds
  ys[, 1] <- pnorm(tr[1] + group * delta)
  ys[, 2] <- pnorm(tr[2] + group * delta) - pnorm(tr[1] + group * delta)
  ys[, 3] <- pnorm(tr[3] + group * delta) - pnorm(tr[2] + group * delta)
  ys[, 4] <- pnorm(tr[4] + group * delta) - pnorm(tr[3] + group * delta)
  ys[, 5] <- 1 - pnorm(tr[4] + group * delta)

  ys
}


# Compute inclusion Bayes factor for main effects
get_inclusion_bf_main <- function(formulas, effect, model_prior, model_posterior) {

  # Step 1: Identify models that include the effect without any interactions involving the effect
  include_effect <- sapply(formulas, function(formula) {
    # Include the effect but exclude models with interactions involving the effect
    includes_main_effect <- grepl(effect, formula)
    has_interaction <- grepl(paste0(effect, ':'), formula) || grepl(paste0(':', effect), formula)
    return(includes_main_effect && !has_interaction)
  })

  # Step 2: Identify models that do NOT include the effect and have no interactions with the effect
  exclude_effect <- sapply(formulas, function(formula) {
    includes_main_effect <- grepl(effect, formula)
    has_interaction <- grepl(paste0(effect, ':'), formula) || grepl(paste0(':', effect), formula)
    return(!includes_main_effect && !has_interaction)
  })

  # Step 3: Remove models with interactions involving the effect from both groups
  valid_models <- include_effect | exclude_effect

  # Normalize the prior and posterior probabilities for the valid models
  normalized_prior <- model_prior[valid_models] / sum(model_prior[valid_models])
  normalized_posterior <- model_posterior[valid_models] / sum(model_posterior[valid_models])

  # Step 4: Calculate Prior Odds
  prior_with_effect <- sum(normalized_prior[include_effect[valid_models]])
  prior_without_effect <- sum(normalized_prior[exclude_effect[valid_models]])
  prior_odds <- prior_with_effect / prior_without_effect

  # Step 5: Calculate Posterior Odds
  posterior_with_effect <- Rmpfr::mpfr(sum(normalized_posterior[include_effect[valid_models]]), prec = 200)
  posterior_without_effect <- Rmpfr::mpfr(sum(normalized_posterior[exclude_effect[valid_models]]), prec = 200)
  posterior_odds <- as.numeric(posterior_with_effect / posterior_without_effect)

  # Step 6: Calculate Inclusion Bayes Factor
  IBF <- posterior_odds / prior_odds
  IBF
}

# Compute inclusion Bayes factor for interaction effects
get_inclusion_bf_int <- function(formulas, effect, model_prior, model_posterior) {

  with_ix <- c(3, 4)
  without_ix <- c(1, 2)

  if (effect == 'Engagement:Protest') {
    without <- c('Engagement + Protest', 'Engagement + Protest + Politics')
    with <- c('Engagement + Protest + Engagement:Protest', 'Engagement + Protest + Politics + Engagement:Protest')
  }

  if (effect == 'Engagement:Politics') {
    without <- c('Engagement + Politics', 'Engagement + Protest + Politics')
    with <- c('Engagement + Politics + Engagement:Politics', 'Engagement + Protest + Politics + Engagement:Politics')
  }

  if (effect == 'Protest:Politics') {
    without <- c('Protest + Politics', 'Engagement + Protest + Politics')
    with <- c('Protest + Politics + Protest:Politics', 'Engagement + Protest + Politics + Protest:Politics')
  }

  if (effect == 'Engagement:Protest:Politics') {
    without <- c('Engagement + Protest + Politics + Engagement:Protest + Engagement:Politics + Protest:Politics')
    with <- c('Engagement + Protest + Politics + Engagement:Protest + Engagement:Politics + Protest:Politics + Engagement:Protest:Politics')
    with_ix <- 2
    without_ix <- 1
  }

  valid_models <- c(with, without)
  valid_models_ix <- sort(sapply(valid_models, function(m) which(formulas == m)))

  # Step 3: Normalize the prior and posterior probabilities for the relevant models
  normalized_prior <- model_prior[valid_models_ix] / sum(model_prior[valid_models_ix])
  normalized_posterior <- model_posterior[valid_models_ix] / sum(model_posterior[valid_models_ix])

  # Step 4: Calculate Prior Odds
  prior_with_effect <- sum(normalized_prior[with_ix])
  prior_without_effect <- sum(normalized_prior[without_ix])
  prior_odds <- prior_with_effect / prior_without_effect

  # Step 5: Calculate Posterior Odds
  posterior_with_effect <- sum(normalized_posterior[with_ix])
  posterior_without_effect <- sum(normalized_posterior[without_ix])
  posterior_odds <- posterior_with_effect / posterior_without_effect

  # Step 6: Calculate Inclusion Bayes Factor
  IBF <- posterior_odds / prior_odds
  IBF
}

get_inclusion_bayes_factors <- function(fit_obj) {

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

  formulas <- str_replace_all(
    str_replace_all(
      str_replace_all(formulas, 'A', 'Engagement'), 'B', 'Protest'
    ), 'C', 'Politics'
  )

  logmls <- fit_obj$logmls
  bfs <- get_bfs(logmls)

  ## Calculate posterior
  uniform_prior <- rep(1/19, 19)
  model_posterior <- (exp(logmls - max(logmls)) * uniform_prior) /
    sum(exp(logmls - max(logmls)) * uniform_prior)

  # Inclusion Bayes factors
  inclusion_bfs <- c(
    'Engagement' = get_inclusion_bf_main(formulas, 'Engagement', uniform_prior, model_posterior),
    'Protest' = get_inclusion_bf_main(formulas, 'Protest', uniform_prior, model_posterior),
    'Politics' = get_inclusion_bf_main(formulas, 'Politics', uniform_prior, model_posterior),
    'Engagement:Protest' = get_inclusion_bf_int(formulas, 'Engagement:Protest', uniform_prior, model_posterior),
    'Engagement:Politics' = get_inclusion_bf_int(formulas, 'Engagement:Politics', uniform_prior, model_posterior),
    'Protest:Politics' = get_inclusion_bf_int(formulas, 'Protest:Politics', uniform_prior, model_posterior),
    'Engagement:Protest:Politics' = get_inclusion_bf_int(formulas, 'Engagement:Protest:Politics', uniform_prior, model_posterior)
  )

  inclusion_bfs
}


# Get latent effect sizes
get_all_predictions <- function(
    fit, outcome,
    variables = c('Politics', 'Protest', 'Engagement'), ...
  ) {

  df <- data.frame(
    avg_predictions(
      fit, variables = variables,
      type = 'link',
      # balanced grid
      newdata = datagrid(Politics = unique, Protest = unique, Engagement = unique),
      ...
    )
  ) %>%
    mutate(outcome = !!outcome)

  # Unify 'Support / Endorse' condition name with text in manuscript
  if (outcome %in% c('Source credibility', 'SourceCredibility')) {
    labels <- c('Endorsed', 'Engaged')
  } else {
    labels <- c('Control', 'Endorsed', 'Engaged')
  }

  df %>%
    mutate(Engagement = factor(Engagement, labels = labels))
}

######
## Model-averaging for ANOVA model for donations
######
sort_formulas <- function(formulas) {
  formulas[get_size_order(formulas)]
}

#' Returns the index of models that include an interaction with varname
find_interaction <- function(varname, model_formulas, type = 'exclude') {
  if (model_formulas[1] != '1') {
    model_formulas <- c('1', model_formulas)
  }

  sel_int_included <- grepl(paste0(varname, ':'), model_formulas) | grepl(paste0(':', varname), model_formulas)

  if (type == 'exclude') {
    res <- which(!sel_int_included)
  } else {
    res <- which(sel_int_included)
  }

  res
}

#' Returns the index of the models that include varname1 and varname2
find_main <- function(varname1, model_formulas, varname2 = NULL, type = 'include') {

  model_formulas <- lapply(
    strsplit(model_formulas, '\\+'), str_trim
  )

  sel_incl <- sapply(model_formulas, function(split_formula) {
    included <- varname1 %in% split_formula

    if (!is.null(varname2)) {
      included <- included & (varname2 %in% split_formula)
    }

    included
  })

  if (type == 'include') {
    res <- which(sel_incl)
  } else {
    res <- which(!sel_incl)
  }

  res
}


normalize_probs <- function(models_post, sel) {
  models_post[sel] / sum(models_post[sel])
}


#' Function to compute posterior model probabilities
get_post <- function(fit_all) {
  log_bayes_factors <- c(0, fit_all@bayesFactor[, 1]) # add intercept-only model

  # Convert the log Bayes factors to numeric
  log_bf <- as.numeric(log_bayes_factors)

  # Find the maximum log Bayes factor for numerical stability
  max_log_bf <- max(log_bf)

  # Compute the exponentials and adjust by subtracting the maximum value
  exp_adjusted <- exp(log_bf - max_log_bf)

  # Compute the posterior model probabilities
  posterior_probs <- exp_adjusted / sum(exp_adjusted)

  posterior_probs
}

get_size_order <- function(formulas) {
  size <- sapply(formulas, function(form) length(strsplit(form, '\\+')[[1]]))
  order(size)
}

#' Computes the inclusion Bayes factors for Figure 3 (policy analysis)
#' Takes a BayesFactor object that includes all 19 models
get_bf_inclusion_donation <- function(
    fits, varname, varname2 = NULL, shape1 = 1, shape2 = 1
) {

  models_prior <- rep(1 / 19, 19)

  model_formulas <- c('1', rownames(fits@bayesFactor))
  order_size <- get_size_order(model_formulas)
  model_formulas <- sort_formulas(model_formulas)
  models_post <- Rmpfr::mpfr(get_post(fits), prec = 500)[order_size]

  ####
  # Main effect: Protest / Engagement / Politics
  ####

  ## Find all models that exclude any interaction with bp, renormalize posterior accordingly
  var_sel_int_excl <- find_interaction(varname, model_formulas, type = 'exclude')
  models_post_var_int_rm <- normalize_probs(models_post, var_sel_int_excl)
  models_prior_var_int_rm <- normalize_probs(models_prior, var_sel_int_excl)

  ## Find all models from those models that include the main effect bp
  var_sel_incl <- find_main(varname, model_formulas[var_sel_int_excl], type = 'include')

  ## Find all models from those models that exclude the main effect bp
  var_sel_excl <- find_main(varname, model_formulas[var_sel_int_excl], type = 'exclude')

  ## Calculate prior odds
  prior_var_included <- sum(models_prior_var_int_rm[var_sel_incl])
  prior_odds_var_included <- prior_var_included / (1 - prior_var_included)

  ## Sum the posterior of the models that include the main effect of bp
  posterior_var_included <- sum(models_post_var_int_rm[var_sel_incl])
  posterior_odds_var_included <- posterior_var_included / (1 - posterior_var_included)

  ## Calculate the inclusion Bayes factor
  bf_inclusion_main <- posterior_odds_var_included / prior_var_included

  ####
  # Two-way Interaction effect
  ####

  ## Get all models that include both varname and varname2 as a main effect, renormalize posterior accordingly
  var_x1_x2 <- find_main(varname, model_formulas, varname2 = varname2, type = 'include')
  models_post_both <- normalize_probs(models_post, var_x1_x2)
  models_prior_both <- normalize_probs(models_prior, var_x1_x2)

  ## From that set of models, get those that include the interaction of the two
  var_x1_x2_incl <- find_main(paste0(varname, paste0(':', varname2)), model_formulas[var_x1_x2], type = 'include')

  if (length(var_x1_x2_incl) == 0) {
    # Change how we search for varname:varname2 to be varname2:varname
    var_x1_x2_incl <- find_main(paste0(varname2, paste0(':', varname)), model_formulas[var_x1_x2], type = 'include')
  }

  var_x1_x2_incl <- var_x1_x2_incl[-length(var_x1_x2_incl)] # need to remove the model with the three way interaction

  ## Calculate prior odds
  prior_twoway <- sum(models_prior_both[var_x1_x2_incl])
  prior_odds_toway <- prior_twoway / (1 - prior_twoway)

  ## Sum the posterior of the models that include the interaction of bp and income
  posterior_twoway <- sum(models_post_both[var_x1_x2_incl])
  posterior_odds_twoway <- posterior_twoway / (1 - posterior_twoway)

  bf_inclusion_twoway <- posterior_odds_twoway / prior_odds_toway

  ####
  # Three-way Interaction effect
  ####

  # We only compare two models here, so that's just the Bayes factor
  bf_inclusion_threeway <- as.numeric((fits[18] / fits[17])@bayesFactor[, 1])

  # Behavioral plasticity x income x country
  as.numeric(c(bf_inclusion_main, bf_inclusion_twoway, exp(bf_inclusion_threeway)))
}


# Make predictions using the best donation model
get_df_preds_donation <- function(fit_best, df) {

  df_preds <- c()
  post <- posterior(fit_best, iterations = 500)

  for (i in seq(nrow(df))) {

    row <- df[i, ]
    politics <- as.character(row[['political_affiliation']])
    protest <- as.character(row[['Protest']])
    engagement <- as.character(row[['Engagement']])

    intercept <- post[, 'mu']

    # Main effects
    beta_protest <- post[, paste0('Protest-', protest)]
    beta_engagement <- post[, paste0('Engagement-', engagement)]
    beta_politics <- post[, paste0('Politics-', politics)]

    # Two-way interactions
    beta_protest_engagement <- post[, paste0('Protest:Engagement-', paste0(protest, '.&.', engagement))]
    beta_protest_politics <- post[, paste0('Protest:Politics-', paste0(protest, '.&.', politics))]
    beta_engagement_politics <- post[, paste0('Engagement:Politics-', paste0(engagement, '.&.', politics))]

    # Three-way interaction
    bp_threeway_name <- paste0('Protest:Engagement:Politics-', paste0(protest, '.&.', engagement, '.&.', politics))
    beta_protest_engagement_politics <- post[, bp_threeway_name]

    # Let's make predictions
    ypred <- (
      intercept + beta_protest + beta_engagement + beta_politics +
      beta_protest_engagement + beta_protest_politics + beta_engagement_politics + beta_protest_engagement_politics
    )

    d <- data.frame(
      times = seq(nrow(post)), ypred = as.numeric(ypred),
      protest = protest, engagement = engagement, politics = politics
    )

    df_preds <- rbind(df_preds, d)
  }

  colnames(df_preds) <- c('times', 'ypred', 'Protest', 'Engagement', 'Politics')
  df_preds
}
