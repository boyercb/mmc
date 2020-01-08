outcome_labels <- tibble(
  key = c(
    "ipv_control_2pl_irt_index_w",
    "phys_sex_violence_2pl_index_w",
    "emo_violence_2pl_index_w",
    "jv_index_m",
    "otherbeliefs_index_m",
    "dm_index_m",
    "goodfeeling_index_m",
    "nosex_index_m",
    "valuewomansex_index_m",
    "commfreq_index_m",
    "goodtalk_index_m",
    "expression_index_m",
    "coupleenjoy_index_m",
    "easeconflictresolve_index_m",
    "freqargue_index_m",
    "womanresolve_index_m",
    "emoresponse_index_m",
    "jv_index_w",
    "otherbeliefs_index_w",
    "dm_index_w",
    "goodfeeling_index_w",
    "nosex_index_w",
    "commfreq_index_w",
    "goodtalk_index_w",
    "expression_index_w",
    "coupleenjoy_index_w",
    "easeconflictresolve_index_w",
    "freqargue_index_w",
    "partnerresolve_index_w"
  ),
  order = factor(
    x = 1:length(key),
    labels = c(
      "IPV Index",
      "Phys./Sex. Violence Index",
      "Emo. Violence Index", 
      "Justifies Violence",
      "Perceptions of Violence",
      "Decision-making",
      "Man feels supported",
      "Neg. response to no consent",
      "Values woman's pleasure",
      "Discuss relationship",
      "Good talk",
      "Freq. good expressions",
      "Enjoy mutual acts",
      "Resolve conflicts",
      "Freq. argue",
      "Woman's resolution skills",
      "Man's emotional reg.",
      "Justifies Violence",
      "Perceptions of Violence",
      "Decision-making",
      "Woman feels supported",
      "Neg. response to no consent",
      "Discuss relationship",
      "Good talk",
      "Freq. good expressions",
      "Enjoy mutual acts",
      "Resolve conflicts",
      "Freq. argue",
      "Man's resolution skills"
    )
  )
)

pdf("08_memo/figures/distribution_violence.pdf", width = 8, height = 4, onefile = FALSE)
el_imputed %>%
  select(violence_outcomes) %>%
  gather() %>%
  left_join(outcome_labels, by = "key") %>%
  ggplot(., aes(x = value)) + 
  geom_histogram(bins = 50) +
  facet_wrap(~order, scales = "free", labeller = label_wrap_gen()) +
  mmc_theme() 
dev.off()

pdf("08_memo/figures/distribution_mens.pdf", width = 8, height = 6, onefile = FALSE)
el_imputed %>%
  select(mens_outcomes) %>%
  gather() %>%
  left_join(outcome_labels, by = "key") %>%
  ggplot(., aes(x = value)) + 
  geom_histogram(bins = 50) +
  facet_wrap(~order, scales = "free", labeller = label_wrap_gen()) +
  mmc_theme()
dev.off()

pdf("08_memo/figures/distribution_womens.pdf", width = 8, height = 6, onefile = FALSE)
el_imputed %>%
  select(womens_outcomes) %>%
  gather() %>%
  left_join(outcome_labels, by = "key") %>%
  ggplot(., aes(x = value)) + 
  geom_histogram(bins = 50) +
  facet_wrap(~order, scales = "free", labeller = label_wrap_gen()) +
  mmc_theme()
dev.off()