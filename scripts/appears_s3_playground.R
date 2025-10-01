
secret <- base64_enc(paste(Sys.getenv("APPEEARS_USERNAME"), Sys.getenv("APPEEARS_PASSWORD"), sep = ":"))
credentials_response <- POST("https://appeears.earthdatacloud.nasa.gov/api/s3credentials", 
                             add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                                         "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                             body = "") |> content()

url_parts <- str_split(modis_ndvi_bundle_request$s3_url, "/")[[1]]
bucket_name <- url_parts[3]  # The third element is the bucket name
object_key <- paste(url_parts[4:length(url_parts)], collapse = "/")  # The rest is the key name

# # Create an S3 client
# s3 <- paws::s3(
#   paws::config(
#     paws::credentials(
#       paws::creds(
#         access_key_id = credentials$accessKeyId,
#         secret_access_key = credentials$secretAccessKey,
#         session_token = credentials$sessionToken,
#         expiration = as.POSIXct(credentials$expiration)
#       )
#     ),
#     region = "us-west-2"  # Replace with the correct region
#   )
# )

# Sys.setenv(AWS_ACCESS_KEY_ID = credentials_response$accessKeyId)
# Sys.setenv(AWS_SECRET_ACCESS_KEY = credentials_response$secretAccessKey)
# Sys.setenv(AWS_SESSION_TOKEN = credentials_response$sessionToken)

# Create an S3 client
s3 <- paws::s3()

s3 <- arrow::s3_bucket(bucket_name, 
                       region = 'us-west-2')

test <- arrow::open_dataset(s3$path(object_name))

# Download the file from S3
s3_download <- s3$get_object(
  Bucket = bucket_name,
  Key = object_name)



# Request temporary S3 credentials from AρρEEARS API
temp_creds <- httr::POST(
  url = "https://appeears.earthdatacloud.nasa.gov/api/s3credentials",
  httr::authenticate(Sys.getenv("APPEEARS_USERNAME"), Sys.getenv("APPEEARS_PASSWORD"))) |> 
  httr::stop_for_status() |> 
  httr::content()

# Create the signed request with temporary credentials
# Use aws.s3 functions with temporary credentials
aws.s3::save_object(
  object = object_key,
  bucket = bucket_name,
  file = "local_file.test",  # Local file path where the content should be saved
  overwrite = TRUE,
  key = temp_creds$access_key,
  secret = temp_creds$secret_key,
  session_token = temp_creds$session_token,
  region = "us-west-2"  # Correct region for the bucket
)

aws.iam::set_credentials(temp_creds)
aws.iam::get_caller_identity()

aws.s3::save_object()

s3_fs <- arrow::s3_bucket(bucket = dirname(modis_ndvi_bundle_request$s3_url[[1]]),
                          secret_key = temp_creds$accessKeyId,
                          access_key = temp_creds$secretAccessKey,
                          session_token = temp_creds$sessionToken)

# Copy the desired MODIS file from the AρρEEARS S3 bucket to a local folder using
# the temporary S3 credentials provided above.
arrow::copy_files(from = modis_ndvi_bundle_request$s3_url[[1]], to = ".",)


aws.iam::assume_role(role = "arn:aws:iam::[SECOND_ACCOUNT_ID]:role/r_myRole", session = "mySession", use = TRUE)
