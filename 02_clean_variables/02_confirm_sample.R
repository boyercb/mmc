# subset to complete surveys
elw <- filter(elw, surveystatus == 1)
elm <- filter(elm, surveystatus == 1)

# check for duplicates
# drop pure duplicates
elw <- distinct(elw)
elm <- distinct(elm)

# find duplicate IDs
duplicate_ids_w <-
  elw %>%
  group_by(track_id) %>%
  arrange(track_id) %>%
  add_count() %>%
  filter(n > 1) 

write.csv(duplicate_ids_w, "duplicates_w.csv")

duplicate_ids_m <-
  elm %>%
  group_by(track_id) %>%
  arrange(track_id) %>%
  add_count() %>%
  filter(n > 1) 

write.csv(duplicate_ids_m, "duplicates_m.csv")

keys <- c(
  "uuid:ddfebb3d-2a7d-46b4-a13f-b063dad43cf7",
  "uuid:1b19eed9-c27f-4106-a88b-f8b167ed3a50",
  "uuid:15c5367e-5982-4541-9b0b-9b33405b139c"
)

# keep duplicates that have responses for physical items
elw <- 
  elw %>%
  group_by(track_id) %>%
  arrange(track_id) %>%
  add_count() %>%
  filter(n == 1 | (n > 1 & KEY %in% keys)) %>%
  select(-n)

elm <- 
  elm %>%
  group_by(track_id) %>%
  arrange(track_id) %>%
  add_count() %>%
  filter(n == 1 | (n > 1 & row_number() == 1)) %>%
  select(-n)

ra <-
  ra %>%
  group_by(partner_track_id) %>%
  arrange(partner_track_id) %>%
  add_count() %>%
  filter(n == 1 | (n > 1 & row_number() == 1)) %>%
  select(-n)

