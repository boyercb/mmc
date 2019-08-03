el <-
  el %>%
  mutate_at(vars(slap1:degrade4), ~replace(., . %in% c(-77, -88, -99), NA)) %>%
  mutate(
    # replace since Xmas indicators with 0 if reported never experiencing act
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
    pre_slap3 = slap4,
    pre_push3 = push4, 
    pre_fist3 = fist4, 
    pre_kick3 = kick4, 
    pre_choke3 = choke4,
    pre_gun3 = gun4,
    pre_forcesex3 = forcesex4, 
    pre_pressuresex3 = pressuresex4,
    pre_degrade3 = degrade4
  )


el <-
  el %>%
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

# create centered versions of pre-treatment outcomes
el[, paste0(names(el)[grepl("pre_", names(el))], "_c")] <- 
  scale(el[, grepl("pre_", names(el))], scale = FALSE)
