
# read endline data -------------------------------------------------------

# men's survey data
elm <- read_stata(
  get_data("endline_mens_windices.dta")
)

elm <- zap_formats(elm)

# women's survey data
elw <- read_stata(
  get_data("endline_women_windices.dta")
)

elw <- zap_formats(elw)

# create codebooks
elm_stata_cb <- map(elm, ~attr(., "label"))
elw_stata_cb <- map(elw, ~attr(., "label"))

# el_labs <- map(el, ~attr(., "labels"))
elm_stata_labs <- map(elm, ~attr(., "labels"))
elw_stata_labs <- map(elw, ~attr(., "labels"))


# load endline questionnaires ---------------------------------------------

# men's questionnaire
elm_survey <- read.csv(
  get_data("surveys/mmc_mens_endline_survey.csv"),
  stringsAsFactors = FALSE
)

elm_choices <- read.csv(
  get_data("surveys/mmc_mens_endline_choices.csv"),
  stringsAsFactors = FALSE
)

# women's questionnaire
elw_survey <- read.csv(
  get_data("surveys/mmc_womens_endline_survey.csv"),
  stringsAsFactors = FALSE
)

elw_choices <- read.csv(
  get_data("surveys/mmc_womens_endline_choices.csv"),
  stringsAsFactors = FALSE
)


# create ODK codebooks ----------------------------------------------------

# men's questionnaire codebook
elm_cb <- make_ODK_codebook(
  choices_sheet = elm_choices,
  choices_label_name = "label",
  questions_sheet = elm_survey,
  Qs_label_name = "label",
  remove_non_questions = TRUE
)

# women's questionnaire codebook
elw_cb <- make_ODK_codebook(
  choices_sheet = elw_choices,
  choices_label_name = "label",
  questions_sheet = elw_survey,
  Qs_label_name = "label",
  remove_non_questions = TRUE
)
