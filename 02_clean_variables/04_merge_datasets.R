
# create wide dataset -----------------------------------------------------

# create master list of men's and women's ids
unique_ids <- 
  c(
    elm$uuid_roster,
    elw$man_uuid
  )

unique_ids <- unique(unique_ids)

# drop one id where man was ineligible but we interviewed his partner
drop_ids <- "041449a5-379d-4d3b-8129-38eb7981d86a"
unique_ids <- unique_ids[!unique_ids %in% drop_ids]

# initialize wide dataset
el <- data.frame(
  uuid_roster = unique_ids,
  stringsAsFactors = FALSE
)

# convert all ids to strings
elm$uuid_roster <- as.character(elm$uuid_roster)
elw$man_uuid <- as.character(elw$man_uuid)
telerivet$uuid_roster <- as.character(telerivet$uuid_roster)

# variables in both elm and elw that don't need suffix
key_vars <- c(
  "treatment",
  "block_id",
  "block_rand"
)

# variables to merge from telerivet data
telerivet_vars <- c(
  "respondent_numsent",
  "respondent_numsuccesssent",
  "challenges_successsent",
  "kids",
  "score",
  "steps"
)

# prepare merge datasets
key_merge <- select(elm, uuid_roster, key_vars)
elm_merge <- select(elm, -key_vars)
elw_merge <- select(elw, -key_vars)
telerivet_merge <- select(telerivet, uuid_roster, telerivet_vars)

# add suffixes
names(elm_merge) <- paste0(names(elm_merge), "_m")
names(elw_merge) <- paste0(names(elw_merge), "_w")

names(elm_merge)[names(elm_merge) == "uuid_roster_m"] <- "uuid_roster" 
names(elw_merge)[names(elw_merge) == "man_uuid_w"] <- "man_uuid" 

# merge key data
el <- left_join(el, key_merge, by = "uuid_roster")

# merge men's data
el <- left_join(el, elm_merge, by = "uuid_roster")

# merge women's data
el <- left_join(el, elw_merge, by = c("uuid_roster" = "man_uuid"))

# merge telerivet data
el <- left_join(el, telerivet_merge, by = "uuid_roster")

# create indicator variable for missingness
el$man_only <- as.numeric(is.na(el$endline_uuid_w))


# create wide imputed dataset ---------------------------------------------

# create master list of men's and women's ids
unique_ids_imp <- 
  c(
    elm_imputed$uuid_roster,
    elw_imputed$man_uuid
  )

unique_ids_imp <- unique(unique_ids_imp)

# drop one id where man was ineligible but we interviewed his partner
drop_ids <- "041449a5-379d-4d3b-8129-38eb7981d86a"
unique_ids_imp <- unique_ids_imp[!unique_ids_imp %in% drop_ids]
unique_ids_imp <- unique_ids_imp[!unique_ids_imp %in% elm$uuid_roster[elm$inrelationship == 0]]
unique_ids_imp <- unique_ids_imp[!unique_ids_imp %in% elw$man_uuid[elw$currently_w_man == 0]]

# initialize wide dataset
el_imputed <- data.frame(
  uuid_roster = unique_ids_imp,
  stringsAsFactors = FALSE
)

# convert all ids to strings
elm_imputed$uuid_roster <- as.character(elm_imputed$uuid_roster)
elw_imputed$man_uuid <- as.character(elw_imputed$man_uuid)

# prepare merge datasets
key_merge_imp <- select(elm_imputed, uuid_roster, key_vars)
elm_merge_imp <- select(elm_imputed, -key_vars)
elw_merge_imp <- select(elw_imputed, -key_vars)

# add suffixes
names(elm_merge_imp) <- paste0(names(elm_merge_imp), "_m")
names(elw_merge_imp) <- paste0(names(elw_merge_imp), "_w")

names(elm_merge_imp)[names(elm_merge_imp) == "uuid_roster_m"] <- "uuid_roster" 
names(elw_merge_imp)[names(elw_merge_imp) == "man_uuid_w"] <- "man_uuid" 

# merge key data
el_imputed <- left_join(el_imputed, key_merge_imp, by = "uuid_roster")

# merge men's data
el_imputed <- left_join(el_imputed, elm_merge_imp, by = "uuid_roster")

# merge women's data
el_imputed <- left_join(el_imputed, elw_merge_imp, by = c("uuid_roster" = "man_uuid"))

# merge telerivet data
el_imputed <- left_join(el_imputed, telerivet_merge, by = "uuid_roster")

# create indicator variable for missingness
el_imputed$man_only <- as.numeric(is.na(el_imputed$endline_uuid_w))

# create long dataset -----------------------------------------------------


