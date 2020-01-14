
comm_vars <- c(
  "comm_herdailyevents",
  "comm_hisdailyevents",
  "comm_herworries",
  "comm_hisworries",
  "comm_familyissues"
)

arg_vars <- c(
  "arg_resp",
  "arg_prov",
  "arg_money",
  "arg_drink",
  "arg_infidelity",
  "arg_sex",
  "arg_children",
  "arg_anything_else"
)

how_arg_vars <- c(
  "how_arg_calm",
  "how_arg_listened",
  "how_arg_yell",
  "how_arg_threat",
  "how_arg_third_party",
  "how_arg_left"
)


# men's recodes -----------------------------------------------------------

# recode those who are in a relationship now as in a relationship last oct
elm$inrelationship_oct[elm$inrelationship == 1] <- 1

# recode to reflect those not pre-loaded as signing quiz didn't sign
elm$quiz_signed[is.na(elm$quiz_signed)] <- 0

# recode live on same block indicator to zero if live together
elm$live_sameblock[is.na(elm$live_sameblock)] <- 1

# combine randomized order trust questions
elm$p_trusts_r1[!is.na(elm$p_trusts_r2)] <- 0
elm$p_trusts_r2[!is.na(elm$p_trusts_r1)] <- 0
elm$p_trusts_r <- elm$p_trusts_r1 + elm$p_trusts_r2

# recode comm vars for those not in relationship
elm[elm$inrelationship == 0 | is.na(elm$inrelationship), comm_vars] <- 0

# recode how resolve questions for those who had no arguments
elm$no_arguments <- as.numeric(rowSums(elm[,arg_vars], na.rm = TRUE) == 0)
elm[elm$no_arguments == 1, how_arg_vars] <- 0

# recode child dependent questions for those without children
elm$arg_children[elm$father == 0] <- 0
elm$time_carechildren[elm$father == 0] <- 0
elm$time_togetherchildren[elm$father == 0] <- 0

# recode relationship questions for dependencies
elm$married_multiple[elm$married == 0] <- 0
elm$engaged[elm$married == 1] <- 0
elm$girlfriend[elm$married == 1 | elm$engaged == 1] <- 0
elm$girlfriend_multiple[elm$girlfriend == 0] <- 0

# recode mmc questions for control and non-compliers
elm$mmc_join[elm$treatment == 0 | elm$complier == 0] <- 0
elm$mmc_receive[elm$treatment == 0 | elm$complier == 0] <- 0
elm$mmc_read[elm$mmc_receive == 0] <- 0
elm$mmc_read_others[elm$mmc_receive == 1 | elm$treatment == 0 | elm$complier == 0] <- 0
elm$mmc_dochallenges[elm$treatment == 0 | elm$complier == 0] <- 0

# recode abuse follow up questions
elm$att_abuse_disobeys_yes[elm$jv_disobeys == 0] <- 0
elm$att_abuse_disobeys_no[elm$jv_disobeys == 1] <- 0


# women's recodes ---------------------------------------------------------

# create woman's relationship indicators
elw$inrelationship <- as.numeric(elw$currentlypartnered)

elw$partner[elw$relationship_name1 == 4 | (elw$relationship_name1 %in% c(-66, -77) & elw$relationship_name2 == 0)] <- "former partner"
elw$currently_w_man <- as.numeric(elw$partner != "former partner")

# combine randomized order trust questions
elw$p_trusts_r1[!is.na(elw$p_trusts_r2)] <- 0
elw$p_trusts_r2[!is.na(elw$p_trusts_r1)] <- 0
elw$p_trusts_r <- elw$p_trusts_r1 + elw$p_trusts_r2

# recode how resolve questions for those who had no arguments
elw$no_arguments <- as.numeric(rowSums(elw[,arg_vars], na.rm = TRUE) == 0)
elw[elw$no_arguments == 1, how_arg_vars] <- 0

# recode mmc questions for control and non-compliers
elw$w_mmc_knowabout[elw$treatment == 0 | elw$participant == 0] <- 0
elw$w_mmc_join[elw$treatment == 0 | elw$participant == 0] <- 0
elw$w_mmc_receive[elw$treatment == 0 | elw$participant == 0] <- 0
elw$w_mmc_receive[elw$w_mmc_join == 0] <- 0
elw$w_mmc_read[elw$w_mmc_receive == 0] <- 0
elw$w_mmc_dochallenges[elw$w_mmc_join == 0] <- 0

# recode abuse follow up questions
elw$att_abuse_disobeys_yes[elw$jv_disobeys == 0] <- 0
elw$att_abuse_disobeys_no[elw$jv_disobeys == 1] <- 0

# recode relationship lengths
elw$relationship_years[!is.na(elw$relationship_months)] <- 
  elw$relationship_months[!is.na(elw$relationship_months)] / 12
elw$relationship_months[!is.na(elw$relationship_years)] <- 
  elw$relationship_years[!is.na(elw$relationship_years)] * 12

# recode child dependent questions for those without children
elw$arg_children[elw$mother == 0] <- 0
elw$time_carechildren[elw$mother == 0] <- 0
elw$time_together[elw$inrelationship == 0] <- 0
elw$time_wanttogether[elw$inrelationship == 0] <- 0

# recode income sep variables
elw$income_r_sep_amt[elw$inrelationship == 0 | elw$income_r_sep == 0] <- 0
elw$income_r_sep_amt_unit[elw$inrelationship == 0 | elw$income_r_sep == 0] <- 1

# recode violence variables
elw <-
  elw %>%
  mutate_at(vars(slap1:degrade4), ~replace(., . %in% c(-77, -88, -99), NA)) %>%
  mutate(
    # replace since Xmas indicators with 0 if reported never experiencing act
    feel_bad2 = replace(feel_bad2, feel_bad1 == 0, 0),
    humiliate2 = replace(humiliate2, humiliate1 == 0, 0),
    scare2 = replace(scare2, scare1 == 0, 0),
    threaten2 = replace(threaten2, threaten1 == 0, 0),
    slap2 = replace(slap2, slap1 == 0, 0),
    push2 = replace(push2, push1 == 0, 0),
    fist2 = replace(fist2, fist1 == 0, 0),
    kick2 = replace(kick2, kick1 == 0, 0),
    choke2 = replace(choke2, choke1 == 0, 0),
    gun2 = replace(gun2, gun1 == 0, 0),
    forcesex2 = replace(forcesex2, forcesex1 == 0, 0),
    pressuresex2 = replace(pressuresex2, pressuresex1 == 0, 0),
    degrade2 = replace(degrade2, degrade1 == 0, 0),
    
    # replace act frequency since Xmas with 0 if reported not experiencing act since Xmas
    feel_bad3 = replace(feel_bad3, feel_bad2 == 0, 0),
    humiliate3 = replace(humiliate3, humiliate2 == 0, 0),
    scare3 = replace(scare3, scare2 == 0, 0),
    threaten3 = replace(threaten3, threaten2 == 0, 0),
    slap3 = replace(slap3, slap2 == 0, 0),
    push3 = replace(push3, push2 == 0, 0),
    fist3 = replace(fist3, fist2 == 0, 0),
    kick3 = replace(kick3, kick2 == 0, 0),
    choke3 = replace(choke3, choke2 == 0, 0),
    gun3 = replace(gun3, gun2 == 0, 0),
    forcesex3 = replace(forcesex3, forcesex2 == 0, 0),
    pressuresex3 = replace(pressuresex3, pressuresex2 == 0, 0),
    degrade3 = replace(degrade3, degrade2 == 0, 0),
    
    # replace pre Xmas frequency with 0 if reported never experiencing act
    feel_bad4 = replace(feel_bad4, feel_bad1 == 0, 0),
    humiliate4 = replace(humiliate4, humiliate1 == 0, 0),
    scare4 = replace(scare4, scare1 == 0, 0),
    threaten4 = replace(threaten4, threaten1 == 0, 0),
    slap4 = replace(slap4, slap1 == 0, 0),
    push4 = replace(push4, push1 == 0, 0),
    fist4 = replace(fist4, fist1 == 0, 0),
    kick4 = replace(kick4, kick1 == 0, 0),
    choke4 = replace(choke4, choke1 == 0, 0),
    gun4 = replace(gun4, gun1 == 0, 0),
    forcesex4 = replace(forcesex4, forcesex1 == 0, 0),
    pressuresex4 = replace(pressuresex4, pressuresex1 == 0, 0),
    degrade4 = replace(degrade4, degrade1 == 0, 0),
    
    # create pre Xmas 0/1 indicators
    pre_feel_bad2 = as.numeric(feel_bad4 > 0),
    pre_humiliate2 = as.numeric(humiliate4 > 0),
    pre_scare2 = as.numeric(scare4 > 0),
    pre_threaten2 = as.numeric(threaten4 > 0),
    pre_slap2 = as.numeric(slap4 > 0),
    pre_push2 = as.numeric(push4 > 0), 
    pre_fist2 = as.numeric(fist4 > 0), 
    pre_kick2 = as.numeric(kick4 > 0), 
    pre_choke2 = as.numeric(choke4 > 0),
    pre_gun2 = as.numeric(gun4 > 0),
    pre_forcesex2 = as.numeric(forcesex4 > 0), 
    pre_pressuresex2 = as.numeric(pressuresex4 > 0),
    pre_degrade2 = as.numeric(degrade4 > 0),
    
    # create pre Xmas 0/1 frequency copies
    pre_feel_bad3 = feel_bad4,
    pre_humiliate3 = humiliate4,
    pre_scare3 = scare4,
    pre_threaten3 = threaten4,
    pre_slap3 = slap4,
    pre_push3 = push4, 
    pre_fist3 = fist4, 
    pre_kick3 = kick4, 
    pre_choke3 = choke4,
    pre_gun3 = gun4,
    pre_forcesex3 = forcesex4, 
    pre_pressuresex3 = pressuresex4,
    pre_degrade3 = degrade4,
    
    # violence in pregnancy questions
    screen_pregnant = replace(screen_pregnant, slap1 == 0 & fist1 == 0 & kick1 == 0 & choke1 == 0 & gun1 == 0 & forcesex1 == 0, 0),
    xpregnant = replace(xpregnant, screen_pregnant == 0, 0),
    preg_violence = replace(preg_violence, screen_pregnant == 0, 0),

    # injury/fight questions
    ever_injure = replace(ever_injure, slap1 == 0 & fist1 == 0 & kick1 == 0 & choke1 == 0 & gun1 == 0 & forcesex1 == 0, 0),
    xinjure = replace(xinjure, ever_injure == 0, 0),
    injure_year = replace(injure_year, ever_injure == 0, 0),
    ever_fight = replace(ever_fight, slap1 == 0 & fist1 == 0 & kick1 == 0 & choke1 == 0 & gun1 == 0 & forcesex1 == 0, 0),
    xfight = replace(xfight, ever_fight == 0, 0),
    ever_perp = replace(ever_perp, slap1 == 0 & fist1 == 0 & kick1 == 0 & choke1 == 0 & gun1 == 0 & forcesex1 == 0, 0),
    xperp = replace(xperp, ever_perp == 0, 0)
  )

elw <-
  elw %>%
  mutate(
    # ipv defined as physical + sexual
    ipv = as.numeric(slap2 + push2 + fist2 + kick2 + choke2 + gun2 + 
                       forcesex2 + pressuresex2 + degrade2 > 0),
    ipv_freq = slap3 + push3 + fist3 + kick3 + choke3 + gun3 + 
      forcesex3 + pressuresex3 + degrade3,
    pre_ipv = as.numeric(slap4 + push4 + fist4 + kick4 + choke4 + gun4 + 
                           forcesex4 + pressuresex4 + degrade4 > 0),
    pre_ipv_freq = slap4 + push4 + fist4 + kick4 + choke4 + gun4 + 
      forcesex4 + pressuresex4 + degrade4,
    
    # physical violence 
    physical = as.numeric(slap2 + push2 + fist2 + kick2 + choke2 + gun2 > 0),
    physical_freq = slap3 + push3 + fist3 + kick3 + choke3 + gun3,
    pre_physical = as.numeric(slap4 + push4 + fist4 + kick4 + choke4 + gun4 > 0),
    pre_physical_freq = slap4 + push4 + fist4 + kick4 + choke4 + gun4,
    
    # sexual violence
    sexual = as.numeric(forcesex2 + pressuresex2 + degrade2 > 0),
    sexual_freq = forcesex3 + pressuresex3 + degrade3,
    pre_sexual = as.numeric(forcesex4 + pressuresex4 + degrade4 > 0),
    pre_sexual_freq = forcesex4 + pressuresex4 + degrade4
  )

# baseline household survey -----------------------------------------------

bl <- replace(bl, bl == -99, NA)
bl <- replace(bl, bl == -88, NA)
bl <- replace(bl, bl == -77, 0)

X_RATE <- 0.0053

bl$expend_food_USD <- bl$expend_food
bl$expend_food_USD <- ifelse(bl$expend_food_currency == 1, bl$expend_food_USD * X_RATE, bl$expend_food_USD)

bl$expend_school_USD <- bl$expend_school
bl$expend_school_USD <- ifelse(bl$expend_school_currency == 1, bl$expend_school_USD * X_RATE, bl$expend_school_USD)

bl$expend_health_USD <- bl$expend_health
bl$expend_health_USD <- ifelse(bl$expend_health_currency == 1, bl$expend_health_USD * X_RATE, bl$expend_health_USD)

# bl$expend_energy_USD <- bl$expend_energy
# bl$expend_energy_USD <- ifelse(bl$expend_energy_currency == 1, bl$expend_energy_USD * X_RATE, bl$expend_energy_USD)

bl$expend_clothing_USD <- bl$expend_clothing
bl$expend_clothing_USD <- ifelse(bl$expend_clothing_currency == 1, bl$expend_clothing_USD * X_RATE, bl$expend_clothing_USD)

bl$expend_transport_USD <- bl$expend_transport
bl$expend_transport_USD <- ifelse(bl$expend_transport_currency == 1, bl$expend_transport_USD * X_RATE, bl$expend_transport_USD)

bl$ownhouse[bl$renter == 1] <- 0

bl$dwelling_units[is.na(bl$dwelling_units)] <- 1
