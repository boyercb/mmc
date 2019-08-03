# Load helper functions ---------------------------------------------------

get_data <- function(path) {
  paste0("../../../../", path)
}

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

# a ggplot theme for Stellar plots
mmc_theme <-
  function() {
    theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        strip.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5)
      )
  }

plot_treatment_effects <- function(
    fit,
    outcome,
    data,
    type = "individual",
    color = NULL,
    color_name = NULL,
    color_values = NULL,
    color_labels = NULL,
    title = NULL,
    ylabel = outcome,
    xlabel = NULL
  ) {
  
    plot_df <-
      data %>%
      group_by(treatment) %>%
      summarise(n = n()) %>%
      ungroup()
  
    predictions <-
      predict(fit,
              newdata = plot_df,
              interval = "confidence",
              alpha = 0.05)
  
    plot_df <-
      plot_df %>%
      mutate(
        pred = predictions$fit[, 1],
        conf95_low = predictions$fit[, 2],
        conf95_high = predictions$fit[, 3],
        label = specd(pred, 3)
      )
  
    p <- ggplot(plot_df, aes(
      x = factor(treatment, labels = c("Control", "Treatment")),
      y = pred, 
    )) 
    
    if (type == "cluster") {
      if (is.null(color)) {
        p <- p + geom_jitter(
          aes(
            x = factor(treatment, labels = c("Control", "Treatment")),
            y = get(outcome),
            size = n
          ),
          data = data,
          alpha = 0.30,
          width = 0.2,
          height = 0
        ) 
      } else {
        p <- p + geom_jitter(
          aes(
            x = factor(treatment, labels = c("Control", "Treatment")),
            y = get(outcome),
            size = n,
            color = factor(get(color))
          ),
          data = data,
          alpha = 0.30,
          width = 0.2,
          height = 0
        ) 
      }
    } else {
      if (is.null(color)) {
        p <- p + geom_jitter(
          aes(
            x = factor(treatment, labels = c("Control", "Treatment")),
            y = get(outcome)
          ),
          data = data,
          alpha = 0.30,
          width = 0.2,
          height = 0.1
        ) 
      } else {
        p <- p + geom_jitter(
          aes(
            x = factor(treatment, labels = c("Control", "Treatment")),
            y = get(outcome),
            color = factor(get(color))
          ),
          data = data,
          alpha = 0.30,
          width = 0.2,
          height = 0.1
        ) 
      }
    }
    
    p <- p +
      geom_point() +
      geom_text(aes(label = label), nudge_x = 0.075, size = 3) +
      geom_errorbar(aes(ymin = conf95_low, ymax = conf95_high), width = 0) +
      labs(
        title = title,
        x = xlabel,
        y = ylabel
      ) +
      mmc_theme() +
      scale_color_manual(
        name = color_name,
        values = color_values,
        labels = color_labels
      ) + 
      theme(
        legend.position = "bottom",
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 10)
      )
    
    return(p)
}


plot_coefs <- function(plot_data, levels){
  plot_data$outcome <- factor(plot_data$outcome, levels = levels)
  ggplot(plot_data, aes(y = outcome, x = estimate)) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
    geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome), 
                 alpha = .3) + 
    facet_grid(adjusted~blocks) + 
    labs(
      x = "Treatment Effect",
      y = ""
    ) +
    mmc_theme()
}

plot_balance <- function(plot_data, levels){
  plot_data$outcome <- factor(plot_data$outcome, levels = levels)
  ggplot(plot_data, aes(y = outcome, x = estimate)) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
    geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome), 
                 alpha = .3) + 
    facet_grid(~blocks) + 
    labs(
      x = "Treatment Effect",
      y = ""
    ) +
    mmc_theme()
}

main_estimator <-
  function (outcome,
            covariates = NULL,
            treatment = "treatment",
            data,
            clusters = NULL,
            se_type = "HC2") {
    
    if (is.na(covariates) | is.null(covariates)) {
      f <- reformulate(treatment, outcome)
    } else {
      f <- reformulate(
        termlabels = c(treatment, covariates, paste0(treatment, ":", covariates)), 
        response = outcome
      )
    }
    if (!is.null(clusters)) {
      se_type <- "CR2" 
      clusters <- sym(clusters)
    }
    
    lm_robust(
      formula = f,
      data = data,
      clusters = !!clusters,
      se_type = se_type
    )
  }

# source("00_packages_and_helpers/helpers_analysis_functions.R")
# source("00_packages_and_helpers/helpers_codebook.R")
# source("00_packages_and_helpers/helpers_p_value_functions.R")
# source("00_packages_and_helpers/helpers_plot_functions.R")
# source("00_packages_and_helpers/helpers_table_functions.R")
