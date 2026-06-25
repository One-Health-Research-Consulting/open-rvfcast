#' Update WAHIS RVF outbreak tables (full or incremental)
#'
#' Loads an existing saved output file, compares it against the current WAHIS
#' endpoint, and fetches only new reports and outbreaks. If no saved WAHIS case 
#' RDS file exists, the full pipeline is executed.
#'
#' The comparison key is report_number per event: if the API reports a
#' higher follow-up number than the maximum already in the file, the new chain
#' of reports (starting from the API's latest report_info_id) is fetched
#' via BFS, terminating the moment a previousReportId already present
#' in the existing data is encountered.
#'
#' @param output_path  Path to the .Rds file to read from (if present) and write to. Directory is created automatically if needed.
#' @param disease_pattern  Regex passed to grepl() to filter diseases; default "rift valley".
#' @param host_con  Number of parallel curl connections (default 8).
#' @param delay     Delay in seconds between requests (default 0.5).
#' @param retry     Number of retries per failed request (default 2).
#' @return  Named list of tibbles: outbreak_reports_events, outbreak_reports_outbreaks, and optionally outbreak_reports_diseases_unmatched
#' @author Morgan Kain (with groundwork by Emma Mendelsohn)
#' @export

update_wahis_rvf_tables <- function(
    output_path     = "data/WAHIS/outbreak_report_tables.Rds"
  , disease_pattern = "rift valley"
  , host_con        = 8L
  , delay           = 0.5
  , retry           = 2L
) {
  
    base_url  <- "https://wahis.woah.org/api/v1/pi/review/report/"
    curl_args <- list(
      .host_con       = host_con
    , .delay          = delay
    , .handle_opts    = list(low_speed_limit = 100, low_speed_time = 300)
    , .retry          = retry
    , .handle_headers = list(`Accept-Language` = "en"))

    
#### Load existing data or prepare for a full run -----------------------------------

    output_dir <- dirname(output_path)
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (!file.exists(output_path)) {
      
        message("No existing file at '", output_path, "' — running full pipeline")
        existing             <- NULL
        existing_report_ids  <- integer(0)
        existing_event_state <- tibble(
          event_id_oie_reference = integer(0)
        , max_report_number      = integer(0))
        
    } else {
      
        message("Loading existing tables from '", output_path, "'")
        existing   <- readRDS(output_path)
        ev         <- existing$outbreak_reports_events
        existing_report_ids  <- unique(as.integer(ev$url_report_id))
        existing_event_state <- ev |>
            filter(!is.na(outbreak_thread_id)) |>
            group_by(outbreak_thread_id) |>
            summarise(max_report_number = max(report_number, na.rm = TRUE), .groups = "drop") |>
            transmute(
              event_id_oie_reference = as.integer(outbreak_thread_id)
            , max_report_number      = as.integer(max_report_number))
        
    }

    
#### Fetch current WAHIS report list and compute diff -------------------------------

    message("Fetching current WAHIS report list")
    current_list <- scrape_outbreak_report_list() |>
        filter(grepl(disease_pattern, disease, ignore.case = TRUE)) |>
        mutate(
          event_id_oie_reference = as.integer(event_id_oie_reference)
        , report_info_id         = as.integer(report_info_id)
        , report_number          = as.integer(report_number))

    message("Found ", nrow(current_list), " matching event(s) on WAHIS")

    needs_update <- current_list |>
        left_join(existing_event_state, by = "event_id_oie_reference") |>
        filter(is.na(max_report_number) | report_number > max_report_number)

    if (nrow(needs_update) == 0) {
      message("Already up to date — no new reports found")
      return(output_path)
    }

    message(nrow(needs_update), " event(s) have new or unseen report(s)")

    
#### BFS: collect general-info for all new reports ---------------------------------
    
## Seeded from the latest report_info_id per updated event; the BFS stops
## the moment it sees a previousReportId already in existing_report_ids.

    new_gi_resps <- bfs_general_info(
      seed_ids  = needs_update$report_info_id
    , known_ids = existing_report_ids)
    
    message("Collected general-info for ", length(new_gi_resps), " new report(s)")

    new_report_list <- build_report_list(new_gi_resps) |>
        left_join(
          current_list |> select(report_info_id, is_last_report_unchanged)
        , by     = "report_info_id"
        , suffix = c("", ".api")) |>
        mutate(is_last_report_unchanged = coalesce(is_last_report_unchanged.api, is_last_report_unchanged)) |>
        select(-is_last_report_unchanged.api)

    
#### Fetch outbreak details for the new report set ---------------------------------

    new_ids          <- unique(new_report_list$report_info_id)
    message("Fetching outbreak details for ", length(new_ids), " new report(s)")
    new_detail_resps <- fetch_outbreak_details(new_ids)

    
#### Transform new batch ------------------------------------------------------------

    message("Transforming new data")
    new_tables <- transform_outbreak_reports(
      general_info_resps     = new_gi_resps
    , outbreak_details_resps = new_detail_resps
    , report_list            = new_report_list)

    if (is.null(new_tables)) {
      message("Transform produced no output — nothing to merge")
      return(invisible(existing))
    }

    
#### Merge new rows into existing data ---------------------------------------------
    
    ## distinct() on the natural key is a safety net; in practice the BFS
    ## termination ensures no report is processed twice.

    if (is.null(existing)) {
      
        merged <- new_tables
        
    } else {
      
        merged <- list(
            outbreak_reports_events = bind_rows(
              existing$outbreak_reports_events
            , new_tables$outbreak_reports_events) |>
                distinct(url_report_id, .keep_all = TRUE)
            , outbreak_reports_outbreaks = bind_rows(
                existing$outbreak_reports_outbreaks
              , new_tables$outbreak_reports_outbreaks) |>
                distinct(report_id, outbreak_id, species, .keep_all = TRUE))

        unmatched <- bind_rows(
           existing$outbreak_reports_diseases_unmatched
         , new_tables$outbreak_reports_diseases_unmatched) |> 
          distinct()

        if (nrow(unmatched) > 0) {
          
          merged$outbreak_reports_diseases_unmatched <- unmatched
          
        }
        
    }

    saveRDS(merged, output_path)
    
    n_ev  <- nrow(merged$outbreak_reports_events)
    n_ob  <- nrow(merged$outbreak_reports_outbreaks)
    
    message("Saved updated tables to '", output_path, "' ",
            "(", n_ev, " reports, ", n_ob, " outbreak-species rows)")

    output_path
    
}


#### Helper Functions -------------------------------------------------------------
## Many of these are updated functions from https://github.com/ecohealthalliance/wahis

scrape_outbreak_report_list <- function() {
  scrape_report_list("https://wahis.woah.org/api/v1/pi/event/filtered-list")
}

scrape_report_list <- function(post_url) {
  
  page_size <- 1000000L
  
  body_data <- list(
    pageNumber = 0L
    , pageSize   = page_size
    , sortColumn = NULL
    , sortOrder  = NULL)
  
  report_list_response <- POST(
    post_url
    , body   = body_data
    , encode = "json")
  
  response_content <- content(report_list_response)
  report_list      <- response_content$list
  
  # otherwise not all results retrieved
  assertthat::assert_that(length(report_list) < page_size)
  
  reports <- purrr::map_dfr(report_list, function(item) {
    as_tibble(lapply(item, function(v) if (is.null(v)) NA else v))
  }) |>
    janitor::clean_names() |>
    dplyr::rename(
      report_info_id = report_id
      , event_id_oie_reference = event_id)
  
  reports
  
}

fetch_batch <- function(urls, ingest_fn) {
  if (!length(urls)) return(list())
  split(urls, (seq_along(urls) - 1L) %/% 100L) |>
    map(function(batch) {
      do.call(map_curl, c(list(urls = batch, .f = ingest_fn), curl_args))
    }) |>
    reduce(c)
}

## BFS over previousReportId chain; terminates when it hits known_ids
bfs_general_info <- function(seed_ids, known_ids = integer(0)) {
  all_resps <- list()
  fetched   <- as.integer(known_ids)
  pending   <- setdiff(as.integer(seed_ids), fetched)
  
  while (length(pending) > 0) {
    message("  BFS wave: fetching general-info for ", length(pending), " report(s)")
    resps     <- fetch_batch(
      paste0(base_url, pending, "/general-information")
    , safe_ingest_general_info)
    all_resps <- c(all_resps, resps)
    fetched   <- c(fetched, pending)
    pending   <- resps |>
      discard(~ !is.null(.$ingest_status)) |>
      map(~ .$report$previousReportId) |>
      compact() |>
      as.integer() |>
      setdiff(fetched)
  }
  all_resps
}

## Build expanded report list from a set of general-info responses
build_report_list <- function(gi_resps) {
  gi_resps |>
    discard(~ !is.null(.$ingest_status)) |>
    map_dfr(function(x) {
      tibble(
        report_info_id           = as.integer(x$report_info_id)
      , event_id_oie_reference   = as.integer(x$event$eventId)
      , report_number            = as.integer(x$report$reportNumber)
      , report_type              = if_else(as.integer(x$report$reportNumber) == 0L, "IN", "FUR")
      , event_status             = x$event$eventStatus$keyValue
      , disease                  = x$event$disease$name
      , country                  = x$event$country$name
      , is_last_report_unchanged = NA)
    })
}

## Fetch per-outbreak all-information for all outbreaks in a set of reports
fetch_outbreak_details <- function(report_ids) {
  if (!length(report_ids)) return(list())
  
  ob_resps <- fetch_batch(
    paste0(base_url, report_ids, "/outbreaks")
  , safe_ingest_outbreaks)
  
  pairs <- ob_resps |>
    discard(~ !is.null(.$ingest_status)) |>
    map_dfr(function(x) {
      if (!length(x$outbreaks)) return(NULL)
      rid <- as.integer(x$report_info_id)
      map_dfr(x$outbreaks, function(ob) {
        tibble(
          report_id   = rid
        , outbreak_id = if (!is.null(ob$outbreakId))
            as.integer(ob$outbreakId) else NA_integer_
        )
      })
    }) |>
    filter(!is.na(outbreak_id)) |>
    distinct()
  
  if (nrow(pairs) == 0) return(list())
  
  fetch_batch(
    paste0(base_url, pairs$report_id, "/outbreak/", pairs$outbreak_id, "/all-information")
  , safe_ingest_outbreak_detail
  )
  
}
