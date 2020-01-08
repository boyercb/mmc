
# verify there are no duplicate entries
stopifnot(sum(duplicated(elm$uuid_roster)) == 0)
stopifnot(sum(duplicated(elw$man_uuid)) == 0)

drop_m <- c(
  "p_trusts_r1",
  "p_trusts_r2",
  "shocks_theftamount",
  "converted_toislam",
  "calm_method_o_num"
)

drop_w <- c(
  "intro_meat",
  "intro_veg",
  "intro_rice",
  "intro_understand",
  "out1_treatment",
  "out2_treatment",
  "out3_treatment",
  "out4_treatment",
  "out5_treatment",
  "out1_control1",
  "out1_control2",
  "out1_control",
  "out2_control1",
  "out2_control2",
  "out2_control",
  "out3_control1",
  "out3_control2",
  "out3_control",
  "out4_control1",
  "out4_control2",
  "out4_control",
  "out5_control1",
  "out5_control2",
  "out5_control",
  "expend_intro",
  "cereal_items",
  "cereal_expend",
  "cerealid",
  "cerealname",
  "cerealvalue",
  "cereal_expend",
  "currentlypartnered",
  "relationship_name1",
  "relationship_name1_o",
  "relationship_name2",
  "man_relationshipend_dec",
  "relationship_atall",
  "relationship_ever",
  "mostrecent_relationship_ex",
  "relationship_sincedeccatch",
  "emergency",
  "emergency_response",
  "emergency_phone",
  "emergency_phonenum",
  "emergency_phoneintro",
  "girl_questions_consent",
  "shocks_theftamount",
  "shocks_theftamount_unit"
)

drop_bl <- c(
  "rent_time",
  "rent_period",
  "rent_currency",
  "expend_food",
  "expend_food_currency",
  "expend_school",
  "expend_school_currency",
  "expend_health",
  "expend_health_currency",
  "expend_energy",
  "expend_energy_currency",
  "expend_clothing",
  "expend_clothing_currency",
  "expend_transport",
  "expend_transport_currency",
  "expend_LD",
  "expend_USD",
  "expend_PPP",
  "ownhouse2"
)

# drop variables 
bl <- bl[, !names(bl) %in% names(elm)]
bl <- bl[, !names(bl) %in% drop_bl]
elm <- elm[, !names(elm) %in% drop_m]
elw <- elw[, !names(elw) %in% drop_w]
