
# Slick but very slow
parse_gdalinfo <- function(lines, nested_list = list()) {
  
  while(length(lines) > 0) {
    
    current_line <- lines[1]
    next_line <- lines[2]
    
    next_indent <- stringr::str_count(next_line, "\\G ")
    
    # Get indentation levels
    current_indent <- stringr::str_count(current_line, "\\G ")
    
    if(grepl("Upper|Lower|Center", current_line)) current_line <- paste0("  ", current_line)
    if(grepl("Upper|Lower|Center", next_line)) next_line <- paste0("  ", next_line)

    # Get element names and values.
    key_value_pair <- stringr::str_split(current_line, "=| is|:")[[1]] |> stringr::str_squish()
    key <- key_value_pair[1]
    value <- key_value_pair[2]
    
    if(grepl("^Band", key)) key <- current_line
    
    lines <- tail(lines, -1)
    
    # Check if next_indent is going deeper
    going_down <- next_indent > current_indent
    
    # Use recursion if the next indent is greater
    recursion <- !is.na(going_down) && going_down
    
    # Also use recursion if we're on nest level 0 and there is a key but no value
    recursion <- recursion || (!is.na(value) && value == "" && current_indent == 0)
    
    # Use recursion to drill deeper
    if(recursion) {
      sublist <- parse_gdalinfo(lines = lines)
      lines <- sublist$lines
      value <- list(sublist$nested_list)
      
      next_line <- lines[1]
      next_indent <- stringr::str_count(next_line, "\\G ")
    }
    
    nested_list = append(nested_list, value |> setNames(key))
    
    # Next key
    next_key_value_pair <- stringr::str_split(next_line, "=| is|:")[[1]] |> stringr::str_squish()
    next_key <- next_key_value_pair[1]
    next_value <- next_key_value_pair[2]
    
    # If we're done walk back up the recursion
    # is.na(value) means no delimiter
    if(!is.na(next_indent) && current_indent > next_indent || grepl("GEOGCRS", key)) {
      return(list(lines = lines, nested_list = nested_list))
    }
    
  }
  
  return(list(lines = lines, nested_list = nested_list))
}
