#' Read and translate DBF-IV files, tailored for the 'Turtle database'
#'
#' @description
#' This function reads DBF-IV files and converts them into R data frames. It's specifically
#' designed for handling turtle data files, with support for character encoding conversion
#' and proper data type casting.
#'
#' @param dbf_file Path to the DBF file to read
#' @param include_deleted_rows Logical. If TRUE, includes rows marked as deleted in the DBF file
#'
#' @return A data frame containing the DBF file contents with appropriate data types:
#' \itemize{
#'   \item Numeric fields (type 'N') are converted to numeric
#'   \item Date fields (type 'D') are converted to Date objects
#'   \item Logical fields (type 'L') are converted to boolean values
#'   \item Character fields are converted to strings with proper encoding
#' }
#'
#' @examples
#' # Read a DBF file
#' data <- read_dbf("path/to/your/file.dbf")
#'
#' # Read a DBF file including deleted rows
#' data <- read_dbf("path/to/your/file.dbf", include_deleted_rows = TRUE)
#'
#' @export

read_dbf <- function(dbf_file, include_deleted_rows = FALSE) {
  # --- Load raw header ---
  con <- file(dbf_file, "rb")
  on.exit(close(con))

  # --- Read header size from DBF bytes 9–10 ---
  seek(con, 8, rw = "read")  # 0-based offset, so byte 9
  header_size <- readBin(con, "integer", size = 2, endian = "little", 
                        signed = FALSE)

  seek(con, 0, rw = "read")
  raw_data <- readBin(con, what = "raw", n = header_size + 1024, size = 1)

  # --- Scan field names and types ---
  extract_fields_custom <- function(raw_data, start_offset = 68, 
                                  descriptor_size = 48, max_fields = 100) {
    fields <- list()
    
    # Find terminator position (0x0D) from start_offset onwards
    terminator_index <- which(raw_data[start_offset:length(raw_data)] == 
                            as.raw(0x0D))[1]
    
    # Compute max valid field descriptors based on where terminator appears
    if (!is.na(terminator_index)) {
      n_fields <- floor((terminator_index - 1) / descriptor_size)
    } else {
      n_fields <- max_fields  # fallback if terminator not found
    }
    
    for (i in 0:(n_fields - 1)) {
      offset <- start_offset + i * descriptor_size
      if (offset + descriptor_size > length(raw_data)) break
      
      block <- raw_data[offset:(offset + descriptor_size - 1)]
      
      # Check for terminator
      if (block[1] == as.raw(0x0D)) {
        break
      }
      
      # Field name: bytes 1–32
      name_raw <- block[1:32]
      chars <- sapply(name_raw, function(b) 
        if (b >= 32 && b <= 126) rawToChar(as.raw(b)) else "")
      name <- gsub("[[:space:]\\.]+$", "", paste(chars, collapse = ""))
      
      if (nchar(name) == 0) {
        next
      }
      
      # Field type: byte 33
      field_type <- tryCatch({
        rawToChar(block[34])
      }, error = function(e) "?")
      
      # Field length and decimal count: bytes 34–35
      field_length <- as.integer(block[35])
      decimal_count <- as.integer(block[36])
      
      fields[[length(fields) + 1]] <- list(
        name = name,
        type = field_type,
        width = field_length,
        offset = offset,
        index = i + 1
      )
    }
    
    return(fields)
  }

  # --- Run extraction ---
  field_headers <- extract_fields_custom(raw_data)

  # --- Read number of records (bytes 5–8) ---
  seek(con, 4, rw = "read")
  n_records <- readBin(con, "integer", size = 4, endian = "little")

  # --- Compute field widths and total record size ---
  field_widths <- sapply(field_headers, `[[`, "width")
  record_size <- sum(as.integer(field_widths)) + 1  # +1 for deletion flag

  # --- Read actual data records ---
  seek(con, header_size, rw = "read")
  data_raw <- readBin(con, what = "raw", n = n_records * record_size, size = 1)

  # Correctly decode data using known field widths
  records <- vector("list", n_records)
  for (i in 1:n_records) {
    start <- (i - 1) * record_size + 1
    end <- start + record_size - 1
    
    record_bytes <- data_raw[start:end]
    
    # skip deleted record (0x2A is * in ASCII)
    delete_flag <- record_bytes[1]
    if (!include_deleted_rows && identical(delete_flag, as.raw(0x2A))) next
    
    record <- record_bytes[-1]  # drop the delete flag for active record
    
    row <- character(length(field_headers))
    offset <- 1
    for (j in seq_along(field_headers)) {
      width <- as.integer(field_headers[[j]]$width)
      field_bytes <- record[offset:(offset + width - 1)]
      
      # Decode using CP437 and fallback
      field_text <- tryCatch({
        iconv(rawToChar(field_bytes), from = "CP437", to = "UTF-8")
      }, error = function(e) rawToChar(field_bytes))
      
      row[j] <- trimws(field_text)
      offset <- offset + width
    }
    records[[i]] <- row
  }

  # Handle circumstances where the data is non-existent or incomplete
  valid_records <- Filter(Negate(is.null), records)

  # Check for partial final row
  expected_len <- length(field_headers)
  valid_records <- lapply(valid_records, function(x) {
    if (length(x) < expected_len) {
      c(x, rep("", expected_len - length(x)))
    } else {
      x
    }
  })

  # If no records, return empty data frame with headers only
  if (length(valid_records) == 0) {
    df <- data.frame(matrix(ncol = length(field_headers), nrow = 0))
    colnames(df) <- sapply(field_headers, `[[`, "name")
  } else {
    df <- as.data.frame(do.call(rbind, valid_records), 
                       stringsAsFactors = FALSE)
    colnames(df) <- sapply(field_headers, `[[`, "name")
  }

  # --- Cast data types based on DBF field type ---
  for (i in seq_along(field_headers)) {
    field <- field_headers[[i]]
    field_type <- field$type
    col_name <- field$name
    
    if (field_type == "N") {
      df[[col_name]] <- suppressWarnings(as.numeric(df[[col_name]]))
    } else if (field_type == "D") {
      df[[col_name]] <- suppressWarnings(as.Date(df[[col_name]], 
                                                format = "%Y%m%d"))
    } else if (field_type == "L") {
      df[[col_name]] <- toupper(df[[col_name]]) %in% c("Y", "T")
    } else {
      df[[col_name]] <- as.character(df[[col_name]])
    }
  }

  return(df)
} 