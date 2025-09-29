# Incremental Processing with tar_group() - Implementation Plan

## Context
Working on optimizing the targets pipeline to avoid unnecessary re-runs. The current issue is that `dates_to_process` runs daily with `cue = tar_cue("always")`, which invalidates all downstream targets even when the actual dates/months don't change. This causes expensive re-processing of data that's already been computed.

## Current Status
- ✅ Modified `months_to_process` to use `tar_group()` approach
- ✅ Updated downstream targets to use `months_to_process$month`
- 🔄 **Testing in progress**: Verifying that only new months trigger new processing

## Next Steps (After Testing Months)

### 1. Test Results Analysis
- [ ] Verify that adding a new month only processes that new month's branches
- [ ] Confirm existing months remain stable and skip processing
- [ ] Check that file aggregation works correctly without `unlist()`

### 2. Apply Same Pattern to `dates_to_process`
```r
# Current (problematic):
tar_target(dates_to_process, set_model_dates(...), cue = tar_cue("always"))

# New (incremental):
tar_target(dates_to_process,
  tibble(
    date = set_model_dates(
      start_year = 2005,
      end_year = lubridate::year(Sys.time()),
      n_per_month = NULL,
      seed = 212
    )
  ) |>
  mutate(month = format(date, "%Y-%m")) |>  # Add month column
  group_by(date) |>
  tar_group(),
  iteration = "group")
```

### 3. Update `months_to_process` Definition
Since `dates_to_process` will now be grouped and include a month column:
```r
tar_target(months_to_process,
  dates_to_process |>
    select(-date) |>           # Remove date column
    distinct(month) |>         # Get unique months
    group_by(month) |>
    tar_group(),
  iteration = "group")
```

### 4. Update All Downstream Targets Using `dates_to_process`
- [ ] **CRITICAL**: Find all targets that use `dates_to_process` directly
- [ ] Change to `dates_to_process$date` in function parameters (was previously just a vector, now a tibble)
- [ ] Keep `{dates_to_process}` in glue templates (this will need careful testing)
- [ ] Remove any remaining `iteration = "list"` declarations
- [ ] **Check carefully**: Some functions may expect a vector of dates, not a tibble

### 5. Expected Behavior
- **First run**: Full rebuild (expected due to target changes)
- **Subsequent runs**: Only new dates/months trigger processing
- **File aggregation**: Should work cleanly with default `iteration = "vector"`

## AWS Upload Function Analysis

**Current `AWS_put_files()` function behavior:**
- Takes `transformed_file_list` (vector of all files) and `local_folder`
- Loops through ALL files in `local_folder`
- Only uploads files that are in `transformed_file_list` AND meet upload criteria
- Already has incremental logic: compares schema/row count, skips if unchanged
- Has `clean_remote` option to delete dangling AWS files

**Good news:** The function already supports incremental uploads! It only uploads files that have actually changed (different schema/row count) or don't exist on AWS.

**For our incremental approach:**
- ✅ Function can handle single files or file vectors
- ✅ Already has built-in change detection
- ❌ `clean_remote = TRUE` becomes problematic with incremental processing
- 🔄 May need to switch AWS targets to `pattern = map()` for per-file branching

### 6. AWS Upload Target Updates
- [ ] Convert AWS upload targets to use `pattern = map()` for individual file uploads
- [ ] Set `clean_remote = FALSE` to avoid deleting files from other branches
- [ ] Each file gets its own upload branch, only changed files trigger re-upload

**Example change:**
```r
# Current:
tar_target(ndvi_transformed_AWS_upload, AWS_put_files(
    ndvi_transformed,  # All files
    ndvi_transformed_directory,
    clean_remote = TRUE  # Problematic with incremental
))

# New:
tar_target(ndvi_transformed_AWS_upload,
    AWS_put_files(
        ndvi_transformed,  # Single file per branch
        ndvi_transformed_directory,
        clean_remote = FALSE  # Don't delete other files
    ),
    pattern = map(ndvi_transformed)
)
```

## AWS External State Problem & Tradeoff

**The fundamental limitation:** Targets cannot detect external changes to AWS that don't affect local files.

**Problem scenario:**
1. AWS files get deleted/corrupted externally (outside pipeline)
2. Local files remain unchanged and valid
3. Upstream targets (e.g., `ndvi_transformed`) appear up-to-date
4. **AWS upload targets never run** - no opportunity to detect/fix AWS issues
5. Missing/corrupted AWS data persists with no automatic recovery

**This is an inherent tradeoff:**
- **Speed**: Incremental processing (risk missing external AWS changes)
- **Reliability**: Always-run AWS sync (slow but catches all AWS issues)

**Mitigation strategies:**
1. **AWS download validation**: `AWS_get_folder()` validates downloaded files (schema, row count) and removes corrupted files
2. **Manual recovery**: `OVERWRITE_*=true` flags to force re-upload when issues detected
3. **Accept risk**: Most AWS corruption/deletion is rare vs. daily processing needs
4. **Hybrid approach**: Daily incremental + periodic full sync

**Current AWS download validation in `AWS_get_folder()`:**
- Downloads missing files from AWS
- Validates each file: readable, >0 rows
- Removes corrupted/empty local files
- Optionally removes corrupted files from AWS (`sync_with_remote = TRUE`)
- Provides first line of defense against AWS corruption

**Decision: Accept incremental approach** with understanding that external AWS changes require manual intervention.

## AWS Corruption/Deletion Recovery Procedures

If you suspect AWS data corruption or deletion:

### 1. Force Target Invalidation (when local files are fine)
```bash
# Invalidate specific AWS upload target to force check of all branches
tar_invalidate(ndvi_transformed_AWS_upload)
tar_make()
```
This forces re-evaluation of all branches and will re-upload any files that fail AWS validation.

### 2. Delete Local Files (when local files may be corrupted)
```bash
# Delete suspect local files to force re-download/re-processing
rm data/ndvi_transformed/suspect_file.parquet
tar_make()
```
Pipeline will re-process missing local files and re-upload to AWS.

### 3. Force Complete Re-upload (systemic corruption)
```bash
# Set overwrite flag to force re-upload of all files regardless of validation
OVERWRITE_NDVI_TRANSFORMED=true tar_make()
# Or for all data types:
OVERWRITE_ALL=true tar_make()
```

### 4. Nuclear Option (complete rebuild)
```bash
# Delete all local processed data to force complete rebuild
rm -rf data/ndvi_transformed/ data/nasa_weather_transformed/
tar_make()
```

### 5. Check AWS State
- Review AWS console/CLI to confirm file presence
- Use `AWS_get_folder()` with validation to test download integrity
- Monitor pipeline logs for AWS upload/download failures

## Files to Watch
- `predictor_data_processing_targets.R` - main targets file
- `R/AWS_get_folder.R` - contains `AWS_put_files()` and `AWS_get_folder()` functions
- Look for `pattern = map(dates_to_process)` and `pattern = map(months_to_process)` usage
- Look for `clean_remote = TRUE` in AWS upload targets

This approach should provide true incremental processing where adding new dates only computes what's actually new.