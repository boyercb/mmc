# Prevalence subitems -----------------------------------------------------

subitem_labels <- c(
  "controls seeing friends",
  "restricts contact with family",
  "insists on knowing whereabouts",
  "ignores or treats indifferently",
  "gets angry about contact w men",
  "suspicious of infidelity",
  "controls healthcare access",
  "insulted once",
  "insulted a few times",
  "insulted many times",
  "humiliate once",
  "humiliate a few times",
  "humiliate many times",
  "scare/intimidate once",
  "scare/intimidate a few times",
  "scare/intimidate many times",
  "threatened verbally once",
  "threatened verbally a few times",
  "threatened verbally many times",
  "slapped once",
  "slapped a few times",
  "slapped many times",
  "pushed once",
  "pushed a few times",
  "pushed many times",
  "punched once",
  "punched a few times",
  "punched many times",
  "kicked once",
  "kicked a few times",
  "kicked many times",
  "choked once",
  "choked a few times",
  "choked many times",
  "threatened w weapon once",
  "threatened w weapon a few times",
  "threatened w weapon many times",
  "forced sex once",
  "forced sex a few times",
  "forced sex many times",
  "coerced sex once",
  "coerced sex a few times",
  "coerced sex many times",
  "forced other sex once",
  "forced other sex a few times",
  "forced other sex many times"
)

subitem_tidy <- 
  lapply(X = subitem_models, get, x = "robust") %>%
  lapply(X = ., FUN = tidy)

subitem_conf <- 
  lapply(X = subitem_models, get, x = "robust") %>%
  lapply(X = ., FUN = function (x) {
    df <- as.data.frame(confint(x))
    colnames(df) <- c("conf.low", "conf.high")
    df
    })

subitem_plot_data <- 
  map2(subitem_tidy, subitem_conf, bind_cols) %>% 
  do.call(what = rbind, args = .) 

index_tidy <- 
  lapply(X = main_models, get, x = "robust") %>%
  lapply(X = ., FUN = tidy)

index_conf <- 
  lapply(X = main_models, get, x = "robust") %>%
  lapply(X = ., FUN = function (x) {
    df <- as.data.frame(confint(x))
    colnames(df) <- c("conf.low", "conf.high")
    df
  })

index_plot_data <- 
  map2(index_tidy, index_conf, bind_cols) %>% 
  do.call(what = rbind, args = .) 

plot_data <-
  bind_rows(subitem_plot_data, index_plot_data) %>%
  filter(term == "treatment") %>% 
  mutate(
    outcome = c(
      rep(subitem_labels, each = 2),
      rep("\nIPV Index\n", each = 2),
      rep("\nPhys./Sex. Violence Index\n", each = 2),
      rep("\nEmotional Violence Index\n", each = 2)
    ),
    type = c(
      rep("Controlling\nBehaviors", 14),
      rep("Emotional Violence", 24),
      rep("Physical Violence", 36),
      rep("Sexual Violence", 18),
      rep("IRT\nIndices", 6)
    ),
    adjusted = rep(c("Adjusted", "Unadjusted"), length(subitem_labels) + 3),
    index = c(rep("No", length(subitem_labels) * 2), rep("Yes", 6))
  )

pdf("08_memo/figures/subitem_plot.pdf", width = 10, height = 10)
plot_coefs(
  plot_data, 
  rev(
    c(
      subitem_labels,
      "\nIPV Index\n",
      "\nPhys./Sex. Violence Index\n",
      "\nEmotional Violence Index\n"
    )
  ),
    c(
      "Controlling\nBehaviors",
      "Emotional Violence",
      "Physical Violence",
      "Sexual Violence",
      "IRT\nIndices"
    )) %>% print()
dev.off()

pdf("08_memo/figures/densities.pdf", width = 10, height = 6)
p <- 
  el_imputed %>%
  select(violence_outcomes, treatment) %>%
  gather(key, value, -treatment) %>%
ggplot(., aes(x = value, fill = factor(treatment))) +
  geom_density(position = "identity", alpha = 0.5) +
  facet_grid(~key) + 
  scale_fill_manual(name = "",
                    labels = c("Control", "MMC"),
                    values = c("black", "grey90")) +
  mmc_theme()
print(p)
dev.off()


