globalVariables(c("map.coordinates", "admin1.coordinates", "ISO_3166_1", "ISO_3166_2"))

#' Make a list mapping between alternative names of countries
#'
#' Look at the admin-0 level data and ISOcodes package to find alternative names
#' of countries. The parameter \code{column} chooses the primary attribute (the
#' names of the list).
#'
#' @param column Usually either \code{"admin"} or \code{"name"}.
#'
#' @return A list of string vectors. The names are the primary country name and
#'   the vectors contain alternatives to those names.
makeNameMap <- function(column)
{
    data("ISO_3166_1", package = "ISOcodes", envir = environment())
    data("map.coordinates", package = packageName(), envir = environment())
    map.coordinates <- data.frame(map.coordinates)

    name.map <- list()
    for (i in seq_len(nrow(map.coordinates)))
    {
        country.name <- as.character(map.coordinates[[column]][i])

        columns <- c("admin", "adm0_a3", "geounit", "gu_a3", "subunit", "su_a3",
            "name", "name_long", "brk_a3", "brk_name", "abbrev", "postal",
            "formal_en", "formal_fr", "name_sort", "name_alt",
            "iso_a2", "iso_a3", "wb_a2", "wb_a3", "adm0_a3_is", "adm0_a3_us")

        all.names <- as.character(unlist(map.coordinates[i, columns]))
        all.names[all.names == "-99"] <- NA
        all.names <- all.names[!is.na(all.names)]
        all.names <- unique(all.names)
        all.names <- all.names[all.names != country.name]

        iso.match <- match(country.name, ISO_3166_1$Name)
        if (is.na(iso.match))
        {
            iso.match <- match(all.names, ISO_3166_1$Name)
            if (any(!is.na(iso.match)))
                iso.match <- iso.match[!is.na(iso.match)]

            iso.match <- iso.match[1]
        }

        if (!is.na(iso.match))
        {
            columns <- c("Alpha_2", "Alpha_3", "Name", "Official_name", "Common_name")
            iso.names <- unlist(ISO_3166_1[iso.match, columns])
            iso.names <- iso.names[!is.na(iso.names)]
            all.names <- unique(c(all.names, iso.names))
        }

        all.names <- all.names[all.names != country.name]

        if (length(all.names) == 0)
            next

        name.map[[country.name]] <- all.names
    }

    name.map
}

admin0.name.map.by.name <- makeNameMap("name")
admin0.name.map.by.admin <- makeNameMap("admin")

admin1.name.map <- local(
{
    data("ISO_3166_1", "ISO_3166_2", package = "ISOcodes", envir = environment())
    data("admin1.coordinates", package = packageName(), envir = environment())
    admin1.coordinates <- data.frame(admin1.coordinates)

    ISO_3166_1$Country <- rep(NA, nrow(ISO_3166_1))
    for (i in seq_len(nrow(ISO_3166_1)))
    {
        country <- ISO_3166_1$Name[i]
        if (country %in% names(admin0.name.map.by.admin))
        {
            ISO_3166_1$Country[i] <- country
        }
        else
        {
            for (admin in names(admin0.name.map.by.admin))
            {
                alt <- admin0.name.map.by.admin[[admin]]
                if (country %in% alt)
                {
                    ISO_3166_1$Country[i] <- admin
                    break
                }
            }
        }
    }

    iso.3166 <- merge(ISO_3166_1[, c("Alpha_2", "Country")], ISO_3166_2,
        by.x = "Alpha_2", by.y = "Country")

    iso.3166$Code <- substring(iso.3166$Code, 4)
    iso.3166$Code[!grepl("^[[:alpha:]]+$", iso.3166$Code)] <- NA

    name.map <- list()
    for (i in seq_len(nrow(admin1.coordinates)))
    {
        state <- as.character(admin1.coordinates$name[i])
        if (is.na(state))
            next

        country <- as.character(admin1.coordinates$admin[i])
        country.name.map <- name.map[[country]]
        if (is.null(country.name.map))
            country.name.map <- list()

        columns <- c("abbrev", "postal", "woe_name", "gn_name")
        all.names <- as.character(unlist(admin1.coordinates[i, columns]))
        all.names[all.names == "-99"] <- NA

        name.alt <- as.character(admin1.coordinates$name_alt[i])
        all.names <- c(all.names, strsplit(name.alt, "|", fixed = TRUE)[[1]])
        name.local <- as.character(admin1.coordinates$name_local[i])
        all.names <- c(all.names, strsplit(name.local, "|", fixed = TRUE)[[1]])

        iso.code <- iso.3166$Code[iso.3166$Country == country & iso.3166$Name == state]
        all.names <- c(all.names, iso.code)

        all.names <- all.names[!is.na(all.names)]
        all.names <- unique(all.names)
        all.names <- all.names[all.names != state]

        if (length(all.names) == 0)
            next

        country.name.map[[state]] <- all.names
        name.map[[country]] <- country.name.map
    }

    name.map
})
