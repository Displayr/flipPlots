# Downloaded from https://en.wikipedia.org/wiki/List_of_German_states_by_GDP 2016-02-12

german.state.gdp <- read.csv(file.path("inst", "extdata", "German_state_GDP.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8")

devtools::use_data(german.state.gdp, internal = FALSE, overwrite = TRUE)
