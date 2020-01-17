# ggplot theme ------------------------------------------------------------

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


# Plotting function -------------------------------------------------------

plot_coefs <- function(plot_data, outcome_levels, type_levels){
  plot_data$outcome <- factor(plot_data$outcome, levels = outcome_levels)
  plot_data$type <- factor(plot_data$type, levels = type_levels)
  ggplot(plot_data, aes(y = fct_reorder(outcome, estimate), x = estimate, shape = index)) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
    geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome), 
                 alpha = .3) +
    geom_text(data = filter(plot_data, index == "Yes"), 
              aes(label = round(estimate, 3)), nudge_y = 0.5, family = "Palatino", size = 2.5
    ) + 
    facet_grid(type~adjusted, scales = "free_y", space = "free_y") + 
    labs(
      x = "Treatment Effect",
      y = ""
    ) +
    # scale_y_discrete(expand = c(.2, 0)) +
    coord_cartesian(clip = "off") + 
    scale_shape_manual(values = c(19, 5), guide = FALSE) +
    mmc_theme() +
    theme()
}



# plot treatment effects --------------------------------------------------

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
      pred = predictions[, 1],
      conf95_low = predictions[, 2],
      conf95_high = predictions[, 3],
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


# plot balance ------------------------------------------------------------


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

