# All blocks (N = 16) -----------------------------------------------------

subitems <- rep(violence_subitems, each = 2)
covariates <- rep(paste0("pre_", violence_subitems, "_c"), each = 2)
covariates[c(TRUE, FALSE)] <- NA

subitem_models <-
  map2(subitems,
       covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = y,
           data = el,
           clusters = "block_m"
         )
       })

freq_subitems <- rep(violence_freq_subitems, each = 2)
freq_covariates <- rep(paste0("pre_", violence_freq_subitems, "_c"), each = 2)
freq_covariates[c(TRUE, FALSE)] <- NA

subitem_freq_models <-
  map2(freq_subitems,
       freq_covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = y,
           data = el,
           clusters = "block_m"
         )
       })

subitem_pre_models <-
  map(pre_violence_subitems,
      function (x) {
        lm_robust(
          formula = reformulate("treatment", x),
          data = el,
          clusters = block_m
        )
      })


# Only randomized blocks (N = 10) -----------------------------------------

subitem_models_ra <-
  map2(subitems,
       covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = y,
           data = filter(el, manual_block == 0),
           clusters = "block_m"
         )
       })

subitem_freq_models_ra <-
  map2(freq_subitems,
       freq_covariates,
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = y,
           data = filter(el, manual_block == 0),
           clusters = "block_m"
         )
       })

subitem_pre_models_ra <-
  map(pre_violence_subitems,
      function (x) {
        lm_robust(
          formula = reformulate("treatment", x),
          data = filter(el, manual_block == 0),
          clusters = block_m
        )
      })


# Pooled ------------------------------------------------------------------

# All blocks (N = 16) -----------------------------------------------------

# subitem_pooled_models <-
#   map(violence_subitems,
#       function (x) {
#         lm_robust(
#           formula = reformulate("treatment", x),
#           data = pooled_el
#         )
#       })
# 
# subitem_freq_pooled_models <-
#   map(violence_freq_subitems,
#       function (x) {
#         lm_robust(
#           formula = reformulate("treatment", x),
#           data = pooled_el
#         )
#       })
# 
# subitem_pre_pooled_models <-
#   map(pre_violence_subitems,
#       function (x) {
#         lm_robust(
#           formula = reformulate("treatment", x),
#           data = pooled_el
#         )
#       })


# Only randomized blocks (N = 10) -----------------------------------------

# subitem_pooled_models_ra <-
#   map(violence_subitems,
#       function (x) {
#         lm_robust(
#           formula = reformulate("treatment", x),
#           data = filter(pooled_el, manual_block == 0)
#         )
#       })
# 
# subitem_freq_pooled_models_ra <-
#   map(violence_freq_subitems,
#       function (x) {
#         lm_robust(
#           formula = reformulate("treatment", x),
#           data = filter(pooled_el, manual_block == 0)
#         )
#       })
# 
# subitem_pre_pooled_models_ra <-
#   map(pre_violence_subitems,
#       function (x) {
#         lm_robust(
#           formula = reformulate("treatment", x),
#           data = filter(pooled_el, manual_block == 0)
#         )
#       })