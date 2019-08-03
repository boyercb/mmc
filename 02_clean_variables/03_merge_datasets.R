# create master list of men's surveys and blocks for 
# merge with women's data
elm_merge <- 
  elm %>%
  select(
    partner_track_id, 
    block
  ) 

# master tracking merge
ra_merge <-
  ra %>%
  select(
    partner_track_id, 
    treatment,
    participant,
    block
  ) %>%
  rename(
    block_m = block
  )

# merge man's block info into women's data
el <- left_join(
  select(elw, -treatment, -participant),
  ra_merge, 
  by = c("track_id" = "partner_track_id")
)

# missing_matches <- c(
#   "W401c_20093_1001", 
#   "W401c_20096_1001", 
#   "W401c_20172_2001", 
#   "W402e_20041_1001", 
#   "W404B_50112_1001", 
#   "w407B_50072_1001" 
# )
# 
# missing_blocks <- c(
#   "W404B",
#   "W401C-1",
#   "W401C-1",
#   "W401C-1",
#   "W402E",
#   "W407B"
# )
# 
# el$block_m[el$track_id %in% missing_matches] <- missing_blocks

# construct treatment indicator (manual for now later should merge)
# el <-
#   el %>%
#   mutate(
#     treatment = case_when(
#       block_m == "C303A" ~ 0,
#       block_m == "C303B" ~ 1,
#       block_m == "L207A" ~ 0,
#       block_m == "L207B" ~ 1,
#       block_m == "L209D" ~ 1,
#       block_m == "W402E" ~ 0,
#       block_m == "W403B" ~ 0,
#       block_m == "W404B" ~ 1,
#       block_m == "W406C" ~ 1,
#       block_m == "W407B" ~ 0,
#       block_m == "C305C-1" ~ 1,
#       block_m == "C305C-2" ~ 0,
#       block_m == "L201D-1" ~ 1,
#       block_m == "L201D-2" ~ 0,
#       block_m == "W401C-1" ~ 1,
#       block_m == "W401C-2" ~ 0
#     ),
#     manual_block = as.numeric(grepl("-", block_m))
#   ) %>%
#   filter(!is.na(block_m))

el$manual_block <- as.numeric(grepl("-", el$block_m))

el <-
  el %>% 
  mutate(
    block_m = replace(block_m, is.na(block_m), block),
    treatment = case_when(
            block_m == "C303A" ~ 0,
            block_m == "C303B" ~ 1,
            block_m == "L207A" ~ 0,
            block_m == "L207B" ~ 1,
            block_m == "L209D" ~ 1,
            block_m == "W402E" ~ 0,
            block_m == "W403B" ~ 0,
            block_m == "W404B" ~ 1,
            block_m == "W406C" ~ 1,
            block_m == "W407B" ~ 0,
            block_m == "C305C-1" ~ 1,
            block_m == "C305C-2" ~ 0,
            block_m == "L201D-1" ~ 1,
            block_m == "L201D-2" ~ 0,
            block_m == "W401C-1" ~ 1,
            block_m == "W401C-2" ~ 0
          )
    )
