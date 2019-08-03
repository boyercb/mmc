# Prevalence subitems -----------------------------------------------------

ipv_labels <- c(
  "slapped at least once",
  "pushed at least once",
  "punched at least once",
  "kicked at least once",
  "choked at least once",
  "threatened w weapon at least once",
  "forced sex at least once",
  "coerced sex at least once",
  "forced other sex at least once"
)

subitem_plot_data <- 
  lapply(X = subitem_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>%
  filter(term == "treatment") %>% 
  mutate(
    outcome = rep(ipv_labels, each = 2),
    blocks = "All blocks (N = 16)",
    adjusted = rep(c("Unadjusted", "Adjusted"), length(ipv_labels))
  )

subitem_plot_data_ra <- 
  lapply(X = subitem_models_ra, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>%
  filter(term == "treatment") %>% 
  mutate(
    outcome = rep(ipv_labels, each = 2),
    blocks = "Randomized blocks (N = 10)",
    adjusted = rep(c("Unadjusted", "Adjusted"), length(ipv_labels))
  )

subitem_plot_data <-
  bind_rows(subitem_plot_data, subitem_plot_data_ra)

pdf("08_memo/figures/subitem_plot.pdf", width = 9, height = 5)
plot_coefs(subitem_plot_data, rev(ipv_labels)) %>% print()
dev.off()


# Frequency subitems ------------------------------------------------------

freq_labels <- c(
  "how often slapped",
  "how often pushed",
  "how often punched",
  "how often kicked",
  "how often choked",
  "how often threatened w weapon",
  "how often forced sex",
  "how often coerced sex",
  "how often forced other sex"
)

subitem_freq_plot_data <- 
  lapply(X = subitem_freq_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>%
  filter(term == "treatment") %>% 
  mutate(
    outcome = rep(freq_labels, each = 2),
    blocks = "All blocks (N = 16)",
    adjusted = rep(c("Unadjusted", "Adjusted"), length(freq_labels))
  )

subitem_freq_plot_data_ra <- 
  lapply(X = subitem_freq_models_ra, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>%
  filter(term == "treatment") %>% 
  mutate(
    outcome = rep(freq_labels, each = 2),
    blocks = "Randomized blocks (N = 10)",
    adjusted = rep(c("Unadjusted", "Adjusted"), length(freq_labels))
  )

subitem_freq_plot_data <-
  bind_rows(subitem_freq_plot_data, subitem_freq_plot_data_ra)

pdf("08_memo/figures/subitem_freq_plot.pdf", width = 9, height = 5)
plot_coefs(subitem_freq_plot_data, rev(freq_labels)) %>% print()
dev.off()

violence_labels <- c(
  "Any IPV",
  "IPV breadth and frequency",
  "Any physical violence",
  "Breadth and frequency of physical violence",
  "Any sexual violence",
  "Breadth and frequency of sexual violence"
)


# Pre-treatment balance ---------------------------------------------------

subitem_balance_plot_data <- 
  lapply(X = subitem_pre_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>%
  filter(term == "treatment") %>% 
  mutate(
    outcome = freq_labels,
    blocks = "All blocks (N = 16)"
  )

subitem_balance_plot_data_ra <- 
  lapply(X = subitem_pre_models_ra, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>%
  filter(term == "treatment") %>% 
  mutate(
    outcome = freq_labels,
    blocks = "Randomized blocks (N = 10)"
  )

subitem_balance_plot_data <-
  bind_rows(subitem_balance_plot_data, subitem_balance_plot_data_ra)

pdf("08_memo/figures/subitem_balance_plot.pdf", width = 9, height = 5)
plot_balance(subitem_balance_plot_data, rev(freq_labels)) %>% print()
dev.off()


p1 <- plot_treatment_effects(
  fit = main_models[[1]],
  outcome = "ipv",
  data = el,
  type = "individual",
  color = "manual_block",
  color_name = "Manually assigned",
  color_values = c("black", "red"),
  color_labels = c("No", "Yes"),
  title = "All blocks (N = 16)",
  ylabel = "Any IPV"
)

p2 <-plot_treatment_effects(
  fit = main_models_ra[[1]],
  outcome = "ipv",
  data = filter(el, manual_block == 0),
  type = "individual",
  title = "Randomized blocks (N = 10)",
  ylabel = "Any IPV"
)

p3 <- plot_treatment_effects(
  fit = main_models[[3]],
  outcome = "ipv_freq",
  data = el,
  type = "individual",
  color = "manual_block",
  color_name = "Manually assigned",
  color_values = c("black", "red"),
  color_labels = c("No", "Yes"),
  title = "All blocks (N = 16)",
  ylabel = "Any IPV"
)

p4 <- plot_treatment_effects(
  fit = main_models_ra[[3]],
  outcome = "ipv_freq",
  data = filter(el, manual_block == 0),
  type = "individual",
  title = "Randomized blocks (N = 10)",
  ylabel = "Any IPV"
)

pdf("08_memo/figures/ipv_plot.pdf", width = 9, height = 5, onefile=FALSE)
ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom") %>% print()
dev.off()

pdf("08_memo/figures/ipv_freq_plot.pdf", width = 9, height = 5, onefile=FALSE)
ggarrange(p3, p4, nrow = 1, common.legend = T, legend = "bottom") %>% print()
dev.off()

pooled_p1 <- plot_treatment_effects(
  fit = main_pooled_models[[1]],
  outcome = "ipv",
  data = pooled_el,
  type = "cluster",
  color = "manual_block",
  color_name = "Manually assigned",
  color_values = c("black", "red"),
  color_labels = c("No", "Yes"),
  title = "All blocks (N = 16)",
  ylabel = "Any IPV"
)

pooled_p2 <-plot_treatment_effects(
  fit = main_pooled_models_ra[[1]],
  outcome = "ipv",
  data = filter(pooled_el, manual_block == 0),
  type = "cluster",
  title = "Randomized blocks (N = 10)",
  ylabel = "Any IPV"
)

pooled_p3 <- plot_treatment_effects(
  fit = main_pooled_models[[3]],
  outcome = "ipv_freq",
  data = pooled_el,
  type = "cluster",
  color = "manual_block",
  color_name = "Manually assigned",
  color_values = c("black", "red"),
  color_labels = c("No", "Yes"),
  title = "All blocks (N = 16)",
  ylabel = "Any IPV"
)

pooled_p4 <- plot_treatment_effects(
  fit = main_pooled_models_ra[[3]],
  outcome = "ipv_freq",
  data = filter(pooled_el, manual_block == 0),
  type = "cluster",
  title = "Randomized blocks (N = 10)",
  ylabel = "Any IPV"
)

pdf("08_memo/figures/pooled_ipv_plot.pdf", width = 9, height = 5, onefile=FALSE)
ggarrange(pooled_p1, pooled_p2, nrow = 1, common.legend = T, legend = "bottom") %>% print()
dev.off()

pdf("08_memo/figures/pooled_ipv_freq_plot.pdf", width = 9, height = 5, onefile=FALSE)
ggarrange(pooled_p3, pooled_p4, nrow = 1, common.legend = T, legend = "bottom") %>% print()
dev.off()

# map2(
#   violence_outcomes,
#   violence_labels,
#   function (x, y) {
#     plot_treatment_effects(
#       outcome = x,
#       data = filter(pooled_el, manual_block == 0),
#       type = "cluster",
#       title = "Cluster, Random Only",
#       ylabel = y
#     )
#   }
# )