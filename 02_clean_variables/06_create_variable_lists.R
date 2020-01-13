
# intimate partner violence -----------------------------------------------

violence_outcomes <-
  c(
    "ipv_control_2pl_irt_index_w",
    "phys_sex_violence_2pl_index_w",
    "emo_violence_2pl_index_w"
  )

pre_violence_outcomes <-
  c(
    "phys_sex_prechr2018_2pl_index_w",
    "emo_prechr2018_2pl_index_w"
  )

violence_subitems <-
  c(
    "see_friends_w", 
    "contact_family_w", 
    "know_loc_w", 
    "ignore_w", 
    "angry_talk_w",
    "suspicious_cheat_w",
    "permission_doc_w",
    "feel_bad3_1_i_w",
    "feel_bad3_2_i_w",
    "feel_bad3_3_i_w",
    "humiliate3_1_i_w",
    "humiliate3_2_i_w",
    "humiliate3_3_i_w",
    "scare3_1_i_w",
    "scare3_2_i_w",
    "scare3_3_i_w",
    "threaten3_1_i_w",
    "threaten3_2_i_w",
    "threaten3_3_i_w",
    "slap3_1_i_w",
    "slap3_2_i_w",
    "slap3_3_i_w",
    "push3_1_i_w",
    "push3_2_i_w",
    "push3_3_i_w",
    "fist3_1_i_w",
    "fist3_2_i_w",
    "fist3_3_i_w",
    "kick3_1_i_w",
    "kick3_2_i_w",
    "kick3_3_i_w",
    "choke3_1_i_w",
    "choke3_2_i_w",
    "choke3_3_i_w",
    "gun3_1_i_w",
    "gun3_2_i_w",
    "gun3_3_i_w",
    "forcesex3_1_i_w",
    "forcesex3_2_i_w",
    "forcesex3_3_i_w",
    "pressuresex3_1_i_w",
    "pressuresex3_2_i_w",
    "pressuresex3_3_i_w",
    "degrade3_1_i_w",
    "degrade3_2_i_w",
    "degrade3_3_i_w"
  )

pre_violence_subitems <-
  c(
    "feel_bad4_1_i_w",
    "feel_bad4_2_i_w",
    "feel_bad4_3_i_w",
    "humiliate4_1_i_w",
    "humiliate4_2_i_w",
    "humiliate4_3_i_w",
    "scare4_1_i_w",
    "scare4_2_i_w",
    "scare4_3_i_w",
    "threaten4_1_i_w",
    "threaten4_2_i_w",
    "threaten4_3_i_w",
    "slap4_1_i_w",
    "slap4_2_i_w",
    "slap4_3_i_w",
    "push4_1_i_w",
    "push4_2_i_w",
    "push4_3_i_w",
    "fist4_1_i_w",
    "fist4_2_i_w",
    "fist4_3_i_w",
    "kick4_1_i_w",
    "kick4_2_i_w",
    "kick4_3_i_w",
    "choke4_1_i_w",
    "choke4_2_i_w",
    "choke4_3_i_w",
    "gun4_1_i_w",
    "gun4_2_i_w",
    "gun4_3_i_w",
    "forcesex4_1_i_w",
    "forcesex4_2_i_w",
    "forcesex4_3_i_w",
    "pressuresex4_1_i_w",
    "pressuresex4_2_i_w",
    "pressuresex4_3_i_w",
    "degrade4_1_i_w",
    "degrade4_2_i_w",
    "degrade4_3_i_w"
  )


# secondary outcomes ------------------------------------------------------

mens_outcomes <-
  c(
    "jv_index_m",
    "nosex_index_m",
    "otherbeliefs_index_m",
    "dm_index_m",
    "goodtalk_index_m",
    "goodfeeling_index_m",
    "commfreq_index_m",
    "expression_index_m",
    "coupleenjoy_index_m",
    "easeconflictresolve_index_m",
    "freqargue_index_m",
    "womanresolve_index_m",
    "emoresponse_index_m",
    "valuewomansex_index_m"
  )

womens_outcomes <-
  c(
    "jv_index_w",
    "nosex_index_w",
    "otherbeliefs_index_w",
    "dm_index_w",
    "goodtalk_index_w",
    "goodfeeling_index_w",
    "commfreq_index_w",
    "expression_index_w",
    "coupleenjoy_index_w",
    "easeconflictresolve_index_w",
    "freqargue_index_w",
    "partnerresolve_index_w"
  )


outcomes <- c(
  violence_outcomes,
  mens_outcomes,
  womens_outcomes
)


# covariates --------------------------------------------------------------

mens_invariant_covariates <- 
  c(
    "age_m",
    "literacy_score_m",
    "childhoodabuse_index_m",
    "hscl_index_m",
    "married_m",
    "married_multiple_m",
    "engaged_m",
    "girlfriend_m",
    "father_m"
  )

womens_invariant_covariates <- 
  c(
    "age_w",
    "hscl_index_w",
    "relationship_years_w",
    "mother_w"
  )

baseline_covariates <-
  names(el)[grepl("_bl$", names(el))]

invariant_covariates <- c(
  mens_invariant_covariates,
  womens_invariant_covariates,
  baseline_covariates,
  pre_violence_outcomes
)

