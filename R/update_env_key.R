update_env_key <- function(key, value, file_path = ".env") {

  # Read the file
  lines <- readLines(file_path)
  
  # Check if the key exists
  key_exists <- FALSE
  for (i in seq_along(lines)) {
    # Split the line into key-value pairs
    if (grepl(paste0("^", key, "="), lines[i])) {
      lines[i] <- paste0(key, "=", value)  # Modify the existing key
      key_exists <- TRUE
      break
    }
  }
  
  # If the key doesn't exist, append it
  if (!key_exists) {
    lines <- c(lines, paste0(key, "=", value))
  }
  
  # Write the modified lines back to the file
  writeLines(lines, file_path)
}
