# Method 1: Get schema information using DBI
table_info <- DBI::dbGetQuery(con, "PRAGMA table_info(data)")
print(table_info)

# Method 2: Show schema in a more readable format
schema_query <- "SELECT column_name, data_type 
                 FROM information_schema.columns 
                 WHERE table_name = 'data'"
schema_info <- DBI::dbGetQuery(con, schema_query)
print(schema_info)

# Method 3: Shorter command that just lists columns
cols <- DBI::dbListFields(con, "data")
print(cols)

# Method 4: Get the first few rows to see the structure
head_data <- DBI::dbGetQuery(con, "SELECT * FROM data ORDER BY x, y, date LIMIT 20")
str(head_data)
