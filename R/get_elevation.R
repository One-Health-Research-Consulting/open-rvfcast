#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param 
#' @return
#' @author Whitney Bagge
#' @export
get_glw <- function() {
  
  url_elevation <- ""
  url_elevation_out <- GET(url_elevation)
  unzipped_elevation <- unzip(url_elevation_out)

}

# Install and load required packages
install.packages("aws.s3")
library(aws.s3)
install.packages("paws")
library(paws)

renv::install("ecohealthalliance/containerTemplateUtils")

library(containerTemplateUtils)

# Set the AWS region
region <- "af-south-1"

# Set the S3 bucket name and prefix
bucket_name <- "deafrica-input-datasets"
prefix <- ""  # Empty prefix to fetch all objects, or specify a specific prefix

Sys.setenv("AWS_REGION"="af-south-1")
aws_s3_download(path="data", bucket=bucket_name, key="")




