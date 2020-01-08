# All blocks (N = 16) -----------------------------------------------------

subitems <- rep(violence_subitems, each = 2)

subitem_covariates <- rep(c("Yes", "No"), length(violence_subitems))

subitem_models <-
  pmap(
    list(
      x = subitems,
      y = subitem_covariates
    ),
       function (x, y) {
         main_estimator(
           outcome = x,
           covariates = if (y == "Yes") get_covariates("ipv_control_2pl_irt_index_w", selected_covariates),
           data = el_imputed,
           cluster = "block_id", 
           se_type = "wild",
           sims = sims
         )
       })





