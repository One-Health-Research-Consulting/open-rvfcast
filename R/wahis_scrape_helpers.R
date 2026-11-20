## WAHIS scrape helpers used by update_wahis_rvf_tables(), copied from the local
 ## ecohealthalliance/wahis clone (R/transform_outbreak_reports.R and R/ingest_reports.R) so the
 ## scrape runs inside this project without installing wahis. Changes from the originals: the
 ## roxygen import/export tags are dropped, and transform_outbreak_reports() takes the disease
 ## lookup path as an argument instead of finding it with system.file(package = "wahis")

#' Convert scraped outbreak reports into formatted data tables
#'
#' Accepts lists produced by \code{safe_ingest_general_info} and
#' \code{safe_ingest_outbreak_detail}, plus the expanded report metadata tibble
#' from the processing script. Returns a named list of tibbles that match the
#' schema of the legacy \code{rvf_outbreak_data_raw.csv} as closely as possible.
#'
#' Returns a named list with up to three tibbles:
#' \itemize{
#'   \item \code{outbreak_reports_events}    — one row per report
#'   \item \code{outbreak_reports_outbreaks} — one row per report × outbreak × species
#'   \item \code{outbreak_reports_diseases_unmatched} — diseases outside ANDO
#' }
#'
#' @param general_info_resps   list from \code{safe_ingest_general_info()}
#' @param outbreak_details_resps list from \code{safe_ingest_outbreak_detail()}
#' @param report_list          tibble from the processing script (one row per report)
#' @return A named list of tibbles
#' @param disease_lookup_path  path to the ANDO disease lookup workbook (copied from wahis inst/diseases)
transform_outbreak_reports <- function(general_info_resps,
                                       outbreak_details_resps,
                                       report_list,
                                       disease_lookup_path = "data/WAHIS/disease_lookup.xlsx") {

    message("Transforming outbreak reports")

    # ── helpers ──────────────────────────────────────────────────────────────
    null_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
    null_lgl <- function(x) if (is.null(x)) NA            else as.logical(x)
    null_int <- function(x) if (is.null(x)) NA_integer_   else as.integer(x)
    null_dbl <- function(x) if (is.null(x)) NA_real_      else as.double(x)

    # ── normalise list structure ──────────────────────────────────────────────
    # When fetch_batch is wrapped in a for-loop (for fault tolerance), the caller
    # accumulates a list-of-batches rather than a flat list.  Detect this by
    # checking whether the first element is itself a batch (i.e. it has neither
    # $outbreak nor $ingest_status at the top level) and flatten one level if so.
    if (length(outbreak_details_resps) > 0) {
        first <- outbreak_details_resps[[1]]
        if (is.null(first$outbreak) && is.null(first$ingest_status)) {
            outbreak_details_resps <- purrr::flatten(outbreak_details_resps)
        }
    }

    # ── discard ingestion errors ──────────────────────────────────────────────
    general_info <- purrr::discard(general_info_resps,    function(x) !is.null(x$ingest_status))
    detail_ok    <- purrr::discard(outbreak_details_resps, function(x) !is.null(x$ingest_status))

    if (!length(general_info)) return(NULL)

    # ── Events table ──────────────────────────────────────────────────────────
    outbreak_reports_events <- map_dfr(general_info, function(x) {
        ev <- x$event
        rp <- x$report

        tibble(
            report_id      = null_int(rp$reportId),
            url_report_id  = null_int(x$report_info_id),
            country        = null_chr(ev$country$name),
            country_iso3c_api = null_chr(ev$country$isoCode),
            country_area_id   = null_int(ev$country$areaId),
            disease_category        = null_chr(ev$disease$category),
            disease                 = null_chr(ev$disease$name),
            is_aquatic              = null_lgl(ev$isAquatic),
            report_date             = null_chr(rp$reportedOn),
            report_nat_ref          = null_chr(rp$reportReference),
            reason_for_notification = null_chr(ev$reason$translation),
            date_of_confirmation_of_the_event = null_chr(ev$confirmOn),
            date_of_start_of_the_event        = null_chr(ev$startedOn),
            date_event_resolved               = null_chr(ev$endedOn),
            date_of_previous_occurrence       = null_chr(ev$lastOccurrence),
            casual_agent = null_chr(ev$causalAgent$name),
            serotype     = null_chr(ev$subType),
            future_reporting = NA_character_  # not in public API
        )
    })

    # Join report-level metadata from expanded report list
    reports_meta <- report_list |>
        select(report_info_id,
               report_type,
               outbreak_thread_id = event_id_oie_reference,
               report_number,
               event_status,
               is_last_report_unchanged)

    # Map event → initial-notification report ID (canonical event identifier)
    lookup_thread_url <- report_list |>
        filter(report_type == "IN") |>
        select(outbreak_thread_id     = event_id_oie_reference,
               url_outbreak_thread_id = report_info_id)

    reports_meta <- left_join(reports_meta, lookup_thread_url, by = "outbreak_thread_id")

    outbreak_reports_events <- outbreak_reports_events |>
        left_join(reports_meta, by = c("url_report_id" = "report_info_id")) |>
        mutate(
            follow_up_count = if_else(report_type == "IN", 0L, as.integer(report_number)),
            is_final_report = (
                tolower(event_status) == "resolved" &
                    isTRUE(is_last_report_unchanged)
            ),
            is_endemic = NA  # not in public API
        ) |>
        mutate(country = case_when(
            country == "Central African (Rep.)" ~ "Central African Republic",
            country == "Dominican (Rep.)"       ~ "Dominican Republic",
            country == "Ceuta"                  ~ "Morocco",
            country == "Melilla"                ~ "Morocco",
            TRUE                                ~ country
        )) |>
        mutate_if(is.character, tolower) |>
        mutate(country_iso3c = coalesce(
            toupper(country_iso3c_api),
            countrycode::countrycode(
                country, origin = "country.name", destination = "iso3c",
                warn = FALSE
            )
        )) |>
        select(-country_iso3c_api) |>
        mutate_at(vars(contains("date")), ~ lubridate::as_datetime(.))

    # ── Disease standardisation ───────────────────────────────────────────────
    ando_disease_lookup <- readxl::read_xlsx(
        disease_lookup_path
    ) |>
        mutate(disease = textclean::replace_non_ascii(disease)) |>
        rename(disease_class = class_desc) |>
        filter(report == "animal") |>
        select(-report, -no_match_found) |>
        mutate_at(.vars = c("ando_id", "preferred_label", "disease_class"),
                  ~ na_if(., "NA"))

    outbreak_reports_events <- outbreak_reports_events |>
        mutate(disease = trimws(disease)) |>
        mutate(disease = textclean::replace_non_ascii(disease)) |>
        mutate(disease = ifelse(disease == "", casual_agent, disease)) |>
        mutate(disease = str_remove_all(disease, "\\s*\\([^\\)]+\\)")) |>
        mutate(disease = str_remove(disease, "virus")) |>
        mutate(disease = trimws(disease)) |>
        left_join(ando_disease_lookup, by = "disease") |>
        mutate(disease = coalesce(preferred_label, disease)) |>
        select(-preferred_label) |>
        distinct()

    diseases_unmatched <- outbreak_reports_events |>
        filter(is.na(ando_id)) |>
        distinct(disease) |>
        mutate(table = "outbreak_animal")

    # ── Outbreaks table (one row per report × outbreak × species) ────────────
    #
    # Built from outbreak_details_resps, which come from the per-report endpoint:
    #   GET /review/report/{reportId}/outbreak/{outbreakId}/all-information
    #
    # Each element has:
    #   $outbreak         — location metadata (isCluster, epiUnitType, lat/lng, etc.)
    #   $adminDivisions   — [{areaId, name, adminLevel, parentAreaId}, ...]
    #   $quantityUnit     — unit of measurement
    #   $speciesQuantities — [{totalQuantities: {speciesName, susceptible, cases, ...},
    #                          speciesType: {wildType, waterType, ...}}, ...]

    if (length(detail_ok) == 0) {
        outbreak_reports_detail <- tibble()
    } else {
        outbreak_reports_detail <- map_dfr(detail_ok, function(x) {
            ob         <- x$outbreak
            report_id  <- null_int(x$report_info_id)
            outbreak_id <- null_int(x$url_outbreak_id)

            # Admin division hierarchy (levels 1-3)
            ad <- x$adminDivisions
            get_level <- function(lvl) {
                hit <- Filter(function(a) identical(a$adminLevel, lvl), ad)
                if (length(hit)) hit[[1]] else list(areaId = NULL, name = NULL)
            }
            lev1 <- get_level(1L)
            lev2 <- get_level(2L)
            lev3 <- get_level(3L)

            qty_unit <- null_chr(x$quantityUnit$keyValue)

            sq <- x$speciesQuantities
            if (!length(sq)) {
                return(tibble(
                    report_id           = report_id,
                    outbreak_id         = outbreak_id,
                    outbreak_nat_ref    = null_chr(ob$nationalReference),
                    outbreak_start_date = null_chr(ob$startDate),
                    outbreak_end_date   = null_chr(ob$endDate),
                    is_cluster          = null_lgl(ob$isCluster),
                    n_outbreaks_cluster = null_int(ob$clusterCount),
                    epi_unit            = null_chr(ob$epiUnitType$keyValue),
                    longitude           = null_dbl(ob$longitude),
                    latitude            = null_dbl(ob$latitude),
                    location_approx     = null_lgl(ob$isLocationApprox),
                    location_name       = null_chr(ob$location),
                    ob_area_id          = null_int(ob$areaId),
                    level1_area_id      = null_int(lev1$areaId),
                    level1_name         = null_chr(lev1$name),
                    level2_area_id      = null_int(lev2$areaId),
                    level2_name         = null_chr(lev2$name),
                    level3_area_id      = null_int(lev3$areaId),
                    level3_name         = null_chr(lev3$name),
                    quantitative_unit   = qty_unit,
                    species             = NA_character_,
                    is_wild             = NA,
                    wild_type           = NA_character_,
                    water_type          = NA_character_,
                    susceptible         = NA_integer_,
                    cases               = NA_integer_,
                    dead                = NA_integer_,
                    killed_disposed     = NA_integer_,
                    slaughtered         = NA_integer_,
                    vaccinated          = NA_integer_
                ))
            }

            map_dfr(sq, function(s) {
                tq <- s$totalQuantities
                st <- s$speciesType
                tibble(
                    report_id           = report_id,
                    outbreak_id         = outbreak_id,
                    outbreak_nat_ref    = null_chr(ob$nationalReference),
                    outbreak_start_date = null_chr(ob$startDate),
                    outbreak_end_date   = null_chr(ob$endDate),
                    is_cluster          = null_lgl(ob$isCluster),
                    n_outbreaks_cluster = null_int(ob$clusterCount),
                    epi_unit            = null_chr(ob$epiUnitType$keyValue),
                    longitude           = null_dbl(ob$longitude),
                    latitude            = null_dbl(ob$latitude),
                    location_approx     = null_lgl(ob$isLocationApprox),
                    location_name       = null_chr(ob$location),
                    ob_area_id          = null_int(ob$areaId),
                    level1_area_id      = null_int(lev1$areaId),
                    level1_name         = null_chr(lev1$name),
                    level2_area_id      = null_int(lev2$areaId),
                    level2_name         = null_chr(lev2$name),
                    level3_area_id      = null_int(lev3$areaId),
                    level3_name         = null_chr(lev3$name),
                    quantitative_unit   = qty_unit,
                    species             = null_chr(tq$speciesName),
                    is_wild             = null_lgl(tq$isWild),
                    wild_type           = null_chr(st$wildType),
                    water_type          = null_chr(st$waterType),
                    susceptible         = null_int(tq$susceptible),
                    cases               = null_int(tq$cases),
                    dead                = null_int(tq$deaths),
                    killed_disposed     = null_int(tq$killed),
                    slaughtered         = null_int(tq$slaughtered),
                    vaccinated          = null_int(tq$vaccinated)
                )
            })
        })
    }

    if (nrow(outbreak_reports_detail) > 0) {
        # Bring in country, report-type, and composite key from the events table
        events_meta <- outbreak_reports_events |>
            select(url_report_id, country_name = country, country_iso3c,
                   country_area_id, report_type, report_number, report_nat_ref,
                   epi_event_id_unique = outbreak_thread_id)

        outbreak_reports_detail <- outbreak_reports_detail |>
            left_join(events_meta, by = c("report_id" = "url_report_id")) |>
            mutate(
                in_fur = tolower(coalesce(report_type, "fur")),
                report_outbreak_species_id_unique = paste(
                    report_id, outbreak_id,
                    gsub("[^[:alnum:]]", "_", tolower(coalesce(species, "unknown"))),
                    sep = "_"
                ),
                morbidity = if_else(
                    !is.na(cases) & !is.na(susceptible) & susceptible > 0L,
                    cases / susceptible, NA_real_
                ),
                mortality = if_else(
                    !is.na(dead) & !is.na(cases) & cases > 0L,
                    dead / cases, NA_real_
                )
            ) |>
            mutate_if(is.character, tolower) |>
            mutate_at(vars(contains("date")), ~ lubridate::as_datetime(.))
    }

    # ── Assemble output ───────────────────────────────────────────────────────
    wahis_joined <- list(
        outbreak_reports_events      = outbreak_reports_events,
        outbreak_reports_outbreaks   = outbreak_reports_detail,
        outbreak_reports_diseases_unmatched = diseases_unmatched
    )
    keep(wahis_joined, ~ nrow(.) > 0)
}



#' Parse a general-information API response
#' @param resp curl API response from .../report/{id}/general-information
ingest_general_info <- function(resp) {
    out <- fromJSON(rawToChar(resp$content))
    report_info_id <- suppressWarnings(
        as.integer(gsub(".*/report/(\\d+)/.*", "\\1", resp$url))
    )
    if (!is.na(report_info_id)) out$report_info_id <- report_info_id
    return(out)
}

#' Safe wrapper for ingest_general_info
#' @param resp curl API response
safe_ingest_general_info <- function(resp) {
    out <- safely(ingest_general_info)(resp)
    if (!is.null(out$result)) out$result
    else list(ingest_status = paste("ingestion error:", out$error))
}

#' Parse an outbreaks list API response
#' Returns the list of outbreak IDs associated with this report.
#' Used only to discover which outbreakIds exist per report before calling
#' safe_ingest_outbreak_detail for each (reportId, outbreakId) pair.
#' @param resp curl API response from .../report/{id}/outbreaks
ingest_outbreaks <- function(resp) {
    outbreaks <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    report_info_id <- suppressWarnings(
        as.integer(gsub(".*/report/(\\d+)/.*", "\\1", resp$url))
    )
    list(outbreaks = outbreaks, report_info_id = report_info_id)
}

#' Safe wrapper for ingest_outbreaks
#' @param resp curl API response
safe_ingest_outbreaks <- function(resp) {
    out <- safely(ingest_outbreaks)(resp)
    if (!is.null(out$result)) out$result
    else list(ingest_status = paste("ingestion error:", out$error))
}

#' Parse a per-outbreak all-information API response
#'
#' Fetches the full details for one outbreak as reported in one specific report,
#' including admin division hierarchy and per-species cumulative case counts.
#'
#' @param resp curl response from
#'   .../report/\{reportId\}/outbreak/\{outbreakId\}/all-information
ingest_outbreak_detail <- function(resp) {
    d <- fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    d$report_info_id  <- suppressWarnings(
        as.integer(gsub(".*/report/(\\d+)/.*", "\\1", resp$url))
    )
    d$url_outbreak_id <- suppressWarnings(
        as.integer(gsub(".*/outbreak/(\\d+)/.*", "\\1", resp$url))
    )
    return(d)
}

#' Safe wrapper for ingest_outbreak_detail
#' @param resp curl response
safe_ingest_outbreak_detail <- function(resp) {
    out <- safely(ingest_outbreak_detail)(resp)
    if (!is.null(out$result)) out$result
    else list(ingest_status = paste("ingestion error:", out$error))
}

