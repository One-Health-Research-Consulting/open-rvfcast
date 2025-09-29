# RVF Forecasting Pipeline Manual

## Overview

This pipeline processes satellite and weather data to generate Rift Valley Fever (RVF) forecasting models. It uses the {targets} R package for reproducible data processing with incremental updates.

## Quick Start

```bash
# Daily processing run
tar_make()

# Check pipeline status
tar_visnetwork()
tar_outdated()
```

## Pipeline Architecture

The pipeline uses incremental processing with `tar_group()` to avoid reprocessing unchanged data:

- **`dates_to_process`**: Grouped by individual dates for fine-grained processing
- **`months_to_process`**: Grouped by months for efficient monthly aggregations
- **Dynamic branching**: Only new dates/months trigger processing of downstream targets

## Data Processing Stages

### 1. Date Management
- `dates_to_process`: Generates processing dates from 2005 to current year
- `months_to_process`: Extracts unique months from processing dates
- Both use `tar_group()` for incremental processing

### 2. NDVI Data Processing
- **MODIS NDVI**: Historical data (2005-2018)
- **Sentinel NDVI**: Recent data (2018-present)
- **Combined processing**: Merges both sources into unified dataset

### 3. Weather Data Processing
- **NASA POWER**: Daily weather data from NASA
- **Variables**: Temperature, precipitation, humidity
- **Spatial processing**: Aligned to analysis grid

### 4. AWS Synchronization
- **Upload**: Processed files uploaded to AWS S3
- **Download**: Missing files downloaded from AWS
- **Validation**: Files validated for integrity (schema, row count)

## Environment Variables

Set these in your `.env` file:

```bash
# AWS Configuration
AWS_ACCESS_KEY_ID=your_access_key
AWS_SECRET_ACCESS_KEY=your_secret_key
AWS_REGION=your_region
AWS_BUCKET_ID=your_bucket_name
AWS_S3_ENDPOINT=your_endpoint_url

# Processing Flags
OVERWRITE_NDVI_TRANSFORMED=false
OVERWRITE_NASA_WEATHER=false
OVERWRITE_ALL=false
```

## Common Operations

### Force Rebuild of Specific Data Type
```bash
OVERWRITE_NDVI_TRANSFORMED=true tar_make()
```

### Force Complete Rebuild
```bash
OVERWRITE_ALL=true tar_make()
```

### Check Which Targets Need Updates
```bash
tar_outdated()
```

### Visualize Pipeline Dependencies
```bash
tar_visnetwork()
```

## Troubleshooting

### Pipeline Runs Slowly
- Check if incremental processing is working: new runs should only process new dates
- Verify `tar_group()` approach is implemented correctly
- Use `tar_outdated()` to see which targets are marked as needing updates

### AWS Upload/Download Issues
- Verify AWS credentials in `.env` file
- Check network connectivity to AWS endpoint
- Review AWS bucket permissions
- See [AWS Corruption Recovery Procedures](incremental_plan.md#aws-corruptiondeletion-recovery-procedures)

### Data Quality Issues
- Check target error logs: `tar_meta()$error`
- Validate input data sources are accessible
- Verify file formats and schemas
- Use `tar_progress()` to monitor running targets

### Out of Memory Errors
- Process data in smaller batches
- Check available system memory
- Consider using arrow/parquet for large datasets
- Monitor memory usage during processing

## File Organization

```
project/
├── _targets/              # Targets cache and metadata
├── data/                  # Processed data files
│   ├── ndvi_transformed/
│   ├── nasa_weather_transformed/
│   └── ...
├── R/                     # R functions
├── predictor_data_processing_targets.R  # Main pipeline definition
├── .env                   # Environment variables
├── manual.md             # This manual
└── incremental_plan.md   # Implementation plan
```

## Performance Tips

1. **Use incremental processing**: Let `tar_group()` handle only changed data
2. **Monitor target status**: Use `tar_outdated()` before running
3. **Batch AWS operations**: Use environment flags to control uploads
4. **Clean old data**: Periodically remove unnecessary cached files
5. **Profile bottlenecks**: Use `tar_meta()` to identify slow targets

## Getting Help

- Check target-specific error messages: `tar_meta()$error`
- Review target dependencies: `tar_visnetwork()`
- Examine target code: `tar_manifest()`
- For AWS issues, see recovery procedures in `incremental_plan.md`