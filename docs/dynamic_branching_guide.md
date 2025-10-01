# Dynamic Branching Implementation Guide

## Overview

This pipeline uses a three-target pattern for each dataset with two key goals:

1. **Minimize unnecessary re-runs** when new data is added
2. **Facilitate collaboration** by storing processed data in centralized S3-compatible storage

The core design principle: **never re-run all dynamic branches when adding new data.**

With 20+ years of daily data across all of Africa (250-7,000+ branches per dynamically branched target), this design enables adding new days or months in hours rather than weeks, while allowing multiple researchers to share processed results.

## The Three-Target Pattern

Each dataset follows this structure:

### 1. S3 Storage Check Target (`*_AWS`)

```r
tar_target(ndvi_transformed_AWS,
  AWS_get_folder(
    ndvi_transformed_directory,
    skip_fetch = Sys.getenv("SKIP_FETCH") == "TRUE",
    sync_with_remote = TRUE
  ),
  error = "null",
  cue = tar_cue("always")
)
```

**Purpose:** Always runs to check and download existing files from S3-compatible storage.

**Key settings:**
- `cue = tar_cue("always")` - Forces execution every pipeline run
- `error = "null"` - Pipeline continues even if S3 storage is unavailable
- Returns character vector of downloaded file paths

**Note:** Despite the `AWS` naming, this works with any S3-compatible storage (AWS S3, Cloudflare R2, MinIO, etc.).

### 2. Processing Target (`*_transformed`, `*_anomalies`, etc.)

```r
tar_target(ndvi_transformed,
  transform_ndvi(
    ndvi_transformed_sources,
    ndvi_transformed_directory,
    basename_template = "ndvi_transformed_{.y}_{.m}.parquet",
    overwrite = parse_flag(c("OVERWRITE_MODIS_NDVI", "OVERWRITE_SENTINEL_NDVI", "OVERWRITE_NDVI_TRANSFORMED")),
    ndvi_transformed_AWS  # Enforce dependency via ... parameter
  ),
  pattern = map(ndvi_transformed_sources),
  format = "file",
  error = "null",
  repository = "local"
)
```

**Purpose:** Creates parquet files only if they don't already exist (or if overwrite is set to TRUE).

**Key settings:**
- `format = "file"` - Returns file paths (enables branching)
- `pattern = map(...)` - Creates one branch per input item
- `error = "null"` - Failed branches don't stop the pipeline
- `repository = "local"` - Only paths tracked, not file contents
- S3 check target passed via `...` enforces execution order

**File-level caching:** Even if a branch invalidates, the function checks if the output file exists:

```r
# Inside processing functions
existing_dataset <- error_safe_read_parquet(save_filename)

if(!is.null(existing_dataset) & !overwrite) {
  row_count <- existing_dataset |> count() |> collect() |> pull(n)
  if(row_count > 0) {
    return(save_filename)  # Skip processing, return existing file
  }
}
```

### 3. S3 Storage Upload Target (`*_AWS_upload`)

```r
tar_target(ndvi_transformed_AWS_upload,
  AWS_put_files(
    ndvi_transformed,
    ndvi_transformed_directory,
    overwrite = parse_flag(c("OVERWRITE_MODIS_NDVI", "OVERWRITE_SENTINEL_NDVI", "OVERWRITE_NDVI_TRANSFORMED"))
  ),
  error = "null"
)
```

**Purpose:** Syncs new/updated files to S3-compatible storage. Compares schema and row counts to avoid redundant uploads.

## Source Pairing Targets

**What they are:** Intermediary targets that create lightweight mappings between dates/months and the specific input files needed to process them. They return simple tibbles pairing processing units with their required input file paths.

### Why They're Necessary

When combining multiple upstream datasets (e.g., MODIS + Sentinel → NDVI):
- Need to map which input files are needed for each output date/month
- Must maintain branch continuity when upstream files change
- Can't afford to invalidate all branches when one upstream file is added

**The solution:** These lightweight targets:
- Create tibble mappings in seconds (just tibble operations, no data processing)
- Preserve branch identity so only affected branches re-run
- Invalidate frequently but rebuild quickly

### Pattern

```r
tar_target(ndvi_transformed_sources,
  create_ndvi_transformed_sources(
    modis_ndvi_transformed,
    sentinel_ndvi_transformed,
    months_to_process
  ) |>
    group_by(month) |>  # Group by branching key
    tar_group(),         # Add tar_group column
  iteration = "group"
)
```

**Key principle:** Function returns plain tibble. The `group_by() |> tar_group()` happens in the target definition, separating data creation from branching logic.

**Result:**
```
# A tibble: 12 × 4
   month   modis_files    sentinel_files  tar_group
   <chr>   <list>         <list>          <int>
 1 2024-01 <chr [2]>      <chr [3]>       1
 2 2024-02 <chr [2]>      <chr [3]>       2
```

Each row = one branch. Processing target receives single-row tibble per branch.

**Variations:**
- Date-level branching: `group_by(date)` - one branch per date
- Month-level branching: `group_by(month)` - one branch per month

**Without sources targets:** Adding one date could invalidate ALL 7,000+ branches, forcing weeks of recomputation.

## Incremental Updates: What Runs When?

When `dates_to_process` expands from 2005-2024 to 2005-2025 (adding 65 dates):

**What re-runs:**
- `dates_to_process` / `months_to_process` ⚡ *instant*
- S3 storage check targets ⚡ *seconds* (always run)
- Source creation targets ⚡ *seconds* (rebuild tibble mappings)
- Processing branches 🐌 *hours* (ONLY new 2025 branches)
  - 2005-2024 branches: **CACHED** ✅
  - 2025 branches: **NEW** 🔄
- Upload targets ⚡ *minutes*

**What doesn't re-run:**
- ✅ Any processing for 2005-2024 data
- ✅ Static datasets (soil, elevation, etc.)
- ✅ Historical means (unless 180+ days old)

## Dependency Management: The Ellipsis Pattern

**Problem:** Processing targets need to wait for S3 check targets, but don't need their returned data.

**Solution:** Pass S3 check target to function's `...` parameter:

```r
# Function signature
transform_ndvi <- function(ndvi_transformed_sources,
                           ndvi_transformed_directory,
                           basename_template,
                           overwrite = FALSE,
                           ...) {  # <-- Accepts S3 target (unused)
  # Function body never references ...
}

# Target usage
tar_target(ndvi_transformed,
  transform_ndvi(
    ...,
    ndvi_transformed_AWS  # Passed to ... - creates dependency
  ),
  pattern = map(ndvi_transformed_sources)
)
```

Benefits: Enforces execution order (S3 check → processing), files pre-downloaded before processing starts.

## Environment Flags

### SKIP_FETCH

```bash
SKIP_FETCH=FALSE  # Default
```

Controls whether to download from S3 storage before processing.

- `FALSE` (default) - Download from S3, skip regenerating what exists (enables collaboration)
- `TRUE` - Skip S3 download, process locally (use when overwriting or testing)

### OVERWRITE_* Flags

```bash
OVERWRITE_STATIC_DATA=FALSE
OVERWRITE_SENTINEL_NDVI=FALSE
OVERWRITE_MODIS_NDVI=FALSE
OVERWRITE_NDVI_TRANSFORMED=FALSE
OVERWRITE_NASA_WEATHER=FALSE
OVERWRITE_ECMWF_FORECASTS=FALSE
OVERWRITE_WEATHER_ANOMALIES=FALSE
OVERWRITE_FORECAST_ANOMALIES=FALSE
OVERWRITE_NDVI_ANOMALIES=FALSE
OVERWRITE_AFRICA_FULL_PREDICTOR_DATA=FALSE
```

**General rule:** Keep at `FALSE`. New data processes automatically regardless.

**Multiple flag checking:**
```r
overwrite = parse_flag(c(
  "OVERWRITE_MODIS_NDVI",      # Upstream source
  "OVERWRITE_SENTINEL_NDVI",   # Upstream source
  "OVERWRITE_NDVI_TRANSFORMED" # Direct flag
))
# Returns TRUE if ANY flag is "TRUE"
```

### Common Scenarios

| Scenario | SKIP_FETCH | OVERWRITE_* | Behavior |
|----------|------------|-------------|----------|
| **Normal run** | FALSE | FALSE | Download from S3, process only new/missing |
| **Reprocessing** | TRUE | TRUE | Skip download, force reprocess all |
| **Schema changes** | FALSE | TRUE | Download, detect schema mismatch, upload new |
| **Testing** | TRUE | FALSE | No S3, process only missing local files |

### Overwrite Cascades

Setting upstream flags triggers downstream reprocessing:

```
OVERWRITE_MODIS_NDVI=TRUE
  ↓
NDVI transformed sees OVERWRITE_MODIS_NDVI=TRUE → reprocesses
  ↓
NDVI anomalies reprocesses
```

### Important Notes

- **Trailing spaces break parsing** - watch for spaces after `FALSE`
- **Changes require R restart** - run `source(".Rprofile")`
- **Overwrite only affects existing data** - new dates/months always process

## Key Takeaways

The pattern achieves incremental updates and collaboration through:

1. **Always-running S3 checks** - Download existing data, avoid regeneration
2. **Source pairing targets** - Lightweight mappings preserve branch identity
3. **Dynamic branching** - Independent computation units (dates/months)
4. **File-level caching** - Functions check existence even if branch invalidates
5. **Ellipsis dependencies** - Clean dependency enforcement
6. **S3-compatible storage** - Works with AWS S3, Cloudflare R2, MinIO, etc.

**Result:** A pipeline with 20+ years of daily data across all of Africa can add a new month in hours instead of weeks. Historical branches (potentially thousands) stay cached. Multiple researchers collaborate without duplicating expensive computations.
