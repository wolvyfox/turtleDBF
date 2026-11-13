# Header helpers ---------------------------------------------------------------

dbf_little_endian <- function(bytes) {
  sum(as.integer(bytes) * 256^(seq_along(bytes) - 1))
}


# Descriptor text is NUL-terminated. Keep printable ASCII bytes.
dbf_header_text <- function(bytes) {
  values <- as.integer(bytes)
  first_zero <- match(0L, values)

  if (!is.na(first_zero)) {
    values <- utils::head(values, first_zero - 1L)
  }

  values <- values[values >= 32L & values <= 126L]
  if (length(values) == 0L) "" else intToUtf8(values)
}


dbf_encoding_from_driver <- function(driver_id, fallback = "CP437") {
  known_encodings <- c(
    `1` = "CP437",
    `2` = "CP850",
    `3` = "windows-1252",
    `4` = "macintosh",
    `27` = "CP437",
    `87` = "windows-1252"
  )

  encoding <- unname(known_encodings[as.character(driver_id)])
  if (length(encoding) == 0L || is.na(encoding)) fallback else encoding
}


dbf_read_structure <- function(
    connection,
    dbf_file,
    encoding,
    fallback_encoding
) {
  # The supported layouts share the first 32 bytes of the header.
  fixed_header <- readBin(connection, what = "raw", n = 32L)

  if (length(fixed_header) < 32L) {
    stop("The file is too short to contain a DBF header.")
  }

  version_byte <- as.integer(fixed_header[1])
  supported_versions <- c(0x03, 0x83, 0x04, 0x8C, 0x30)

  if (!version_byte %in% supported_versions) {
    stop(
      "Unsupported or damaged DBF version byte: ",
      sprintf("0x%02X", version_byte)
    )
  }

  number_of_records <- dbf_little_endian(fixed_header[5:8])
  header_length <- dbf_little_endian(fixed_header[9:10])
  record_length <- dbf_little_endian(fixed_header[11:12])
  language_driver_id <- as.integer(fixed_header[30])

  # Level 7 adds 36 header bytes before its 48-byte field descriptors.
  # The other supported layouts use 32-byte descriptors at offset 32.
  # Descriptor offsets are zero-based file positions.
  level_7 <- version_byte %in% c(0x04, 0x8C)
  descriptor_start <- if (level_7) 68L else 32L
  descriptor_size <- if (level_7) 48L else 32L
  file_size <- file.info(dbf_file)$size

  if (header_length <= descriptor_start || record_length < 1L) {
    stop("The DBF header contains invalid header or record lengths.")
  }

  if (header_length > file_size) {
    stop("The declared DBF header is longer than the file.")
  }

  seek(connection, where = 0, origin = "start")
  header_bytes <- readBin(connection, what = "raw", n = header_length)

  if (length(header_bytes) != header_length) {
    stop("The complete DBF header could not be read.")
  }

  # Check for 0x0D only at descriptor boundaries. Within a descriptor,
  # the same byte can represent a field width of 13.
  possible_offsets <- seq(
    from = descriptor_start,
    to = header_length - 1L,
    by = descriptor_size
  )
  terminator_offsets <- possible_offsets[
    as.integer(header_bytes[possible_offsets + 1L]) == 0x0D
  ]

  if (length(terminator_offsets) == 0L) {
    stop("The DBF field-descriptor terminator was not found.")
  }

  field_offsets <- possible_offsets[
    possible_offsets < terminator_offsets[1]
  ]

  field_rows <- lapply(field_offsets, function(offset) {
    descriptor <- header_bytes[(offset + 1L):(offset + descriptor_size)]

    if (level_7) {
      data.frame(
        name = dbf_header_text(descriptor[1:32]),
        type = dbf_header_text(descriptor[33]),
        width = as.integer(descriptor[34]),
        decimals = as.integer(descriptor[35]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        name = dbf_header_text(descriptor[1:11]),
        type = dbf_header_text(descriptor[12]),
        width = as.integer(descriptor[17]),
        decimals = as.integer(descriptor[18]),
        stringsAsFactors = FALSE
      )
    }
  })

  if (length(field_rows) > 0L) {
    fields <- do.call(rbind, field_rows)
  } else if (number_of_records == 0L && record_length == 1L) {
    # Accept a descriptor-free header for an empty table.
    fields <- data.frame(
      name = character(),
      type = character(),
      width = integer(),
      decimals = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    stop("The DBF header contains no field descriptors.")
  }

  # Record width includes the one-byte deletion marker.
  calculated_record_length <- 1L + sum(fields$width)

  if (calculated_record_length != record_length) {
    warning(
      "The declared record length does not equal the field widths.",
      call. = FALSE
    )
  }

  if (is.null(encoding)) {
    encoding <- dbf_encoding_from_driver(
      language_driver_id,
      fallback = fallback_encoding
    )
  }

  list(
    version_byte = version_byte,
    number_of_records = number_of_records,
    header_length = header_length,
    record_length = record_length,
    language_driver_id = language_driver_id,
    encoding = encoding,
    fields = fields
  )
}


# Record-decoding helpers -------------------------------------------------------

dbf_empty_column <- function(field_type, timezone) {
  switch(
    field_type,
    N = numeric(),
    F = numeric(),
    D = as.Date(character()),
    L = logical(),
    `@` = as.POSIXct(character(), tz = timezone),
    character()
  )
}


dbf_empty_result <- function(fields, timezone, add_deleted_flag) {
  columns <- Map(
    dbf_empty_column,
    fields$type,
    MoreArgs = list(timezone = timezone)
  )
  names(columns) <- fields$name

  if (add_deleted_flag) {
    columns <- c(list(.deleted = logical()), columns)
  }

  as.data.frame(
    columns,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


# Level 7 timestamps store the Julian day and milliseconds since midnight
# as two little-endian 32-bit integers.
dbf_decode_timestamp <- function(data_raw, starts, timezone) {
  # Each matrix column contains one record's timestamp bytes.
  byte_indices <- outer(0:7, starts, `+`)
  bytes <- matrix(as.integer(data_raw[byte_indices]), nrow = 8L)

  julian_day <- colSums(bytes[1:4, , drop = FALSE] * 256^(0:3))
  milliseconds <- colSums(bytes[5:8, , drop = FALSE] * 256^(0:3))

  valid <- julian_day >= 1721426 & julian_day <= 5373484 &
    milliseconds >= 0 & milliseconds < 86400000

  # Julian day 2440588 is the Unix epoch (1970-01-01).
  seconds <- rep(NA_real_, length(julian_day))
  seconds[valid] <- (julian_day[valid] - 2440588) * 86400 +
    milliseconds[valid] / 1000

  as.POSIXct(seconds, origin = "1970-01-01", tz = timezone)
}


dbf_decode_field <- function(
    field,
    field_start,
    record_starts,
    data_raw,
    record_text,
    encoding,
    timezone
) {
  # Add the one-based field position to each zero-based record offset.
  starts <- record_starts + field_start

  if (field$type == "@") {
    return(dbf_decode_timestamp(data_raw, starts, timezone))
  }

  values <- substring(
    record_text,
    first = starts,
    last = starts + field$width - 1L
  )

  # Transcode before trimming so trimws() receives valid UTF-8.
  values <- iconv(values, from = encoding, to = "UTF-8", sub = "")
  values <- trimws(values)

  # Text-based memo fields return the stored pointer.
  if (field$type %in% c("C", "M")) {
    return(values)
  }

  values[values == ""] <- NA_character_

  if (field$type %in% c("N", "F")) {
    return(suppressWarnings(as.numeric(values)))
  }

  if (field$type == "D") {
    return(suppressWarnings(as.Date(values, format = "%Y%m%d")))
  }

  if (field$type == "L") {
    logical_value <- rep(NA, length(values))
    logical_value[toupper(values) %in% c("Y", "T")] <- TRUE
    logical_value[toupper(values) %in% c("N", "F")] <- FALSE
    return(logical_value)
  }

  # Fall back to text for fields without a dedicated decoder.
  values
}


dbf_add_attributes <- function(result, structure, dbf_file) {
  attr(result, "dbf_version_byte") <- structure$version_byte
  attr(result, "dbf_language_driver_id") <- structure$language_driver_id
  attr(result, "dbf_encoding") <- structure$encoding
  attr(result, "dbf_declared_records") <- structure$number_of_records
  attr(result, "dbf_source_file") <- dbf_file
  result
}


# Public reader ----------------------------------------------------------------

#' Read a DBF file into a data frame
#'
#' @param dbf_file Path to a DBF file.
#' @param include_deleted_rows Include records marked as deleted in the DBF.
#' @param add_deleted_flag Add a logical `.deleted` column.
#' @param encoding Character encoding for text fields. `NULL` uses the DBF
#'   language-driver byte where recognised.
#' @param fallback_encoding Encoding used when the language driver is absent or
#'   unknown. Defaults to CP437.
#' @param timezone Time zone used to display decoded dBASE Level 7 timestamps.
#'
#' @return A base data frame whose names and basic types follow the DBF header.
#'   Attributes record the version byte, language driver, encoding, declared
#'   record count and source path.
#'
#' @details
#' The reader recognises version bytes `0x03` and `0x83` (dBASE III+
#' compatible), `0x04` and `0x8C` (dBASE Level 7), and `0x30`
#' (Visual FoxPro). It supports character, numeric, floating-point, date,
#' logical and Level 7 timestamp fields. Memo fields return the stored
#' pointer for text-based memo fields. The reader does not open memo sidecar
#' files or decode specialised Visual FoxPro binary field types.
#'
#' Deleted records are omitted by default. Missing or unrecognised logical
#' values become `NA`. If a file ends before the declared record count, the
#' reader warns and returns the complete records it found.
#'
#' The record block is loaded into memory. Field offsets are used to extract
#' and convert a whole column at a time.
#'
#' @examples
#' \dontrun{
#' data <- read_dbf("path/to/your/file.dbf")
#' data_with_deleted <- read_dbf(
#'   "path/to/your/file.dbf",
#'   include_deleted_rows = TRUE,
#'   add_deleted_flag = TRUE
#' )
#' }
#' @export
read_dbf <- function(
    dbf_file,
    include_deleted_rows = FALSE,
    add_deleted_flag = FALSE,
    encoding = NULL,
    fallback_encoding = "CP437",
    timezone = "UTC"
) {
  dbf_file <- normalizePath(dbf_file, winslash = "/", mustWork = TRUE)
  connection <- file(dbf_file, open = "rb")
  on.exit(close(connection), add = TRUE)

  structure <- dbf_read_structure(
    connection,
    dbf_file,
    encoding,
    fallback_encoding
  )

  empty_result <- function() {
    result <- dbf_empty_result(
      structure$fields,
      timezone,
      add_deleted_flag
    )
    dbf_add_attributes(result, structure, dbf_file)
  }

  if (structure$number_of_records == 0L) {
    return(empty_result())
  }

  # Some layouts have metadata after the descriptor terminator.
  # Use the declared header length to locate the first record.
  seek(connection, where = structure$header_length, origin = "start")
  data_raw <- readBin(
    connection,
    what = "raw",
    n = structure$number_of_records * structure$record_length
  )

  complete_records <- floor(length(data_raw) / structure$record_length)

  if (complete_records < structure$number_of_records) {
    warning(
      "The file ended before every declared record could be read; ",
      "only complete records were retained.",
      call. = FALSE
    )
    data_raw <- utils::head(
      data_raw,
      complete_records * structure$record_length
    )
  }

  if (complete_records == 0L) {
    return(empty_result())
  }

  # Record offsets are relative to data_raw. Deleted rows start with 0x2A.
  record_starts <- (seq_len(complete_records) - 1L) *
    structure$record_length
  deleted <- data_raw[record_starts + 1L] == as.raw(0x2A)
  keep <- include_deleted_rows | !deleted

  record_starts <- record_starts[keep]
  deleted <- deleted[keep]

  if (length(record_starts) == 0L) {
    return(empty_result())
  }

  # Replace NULs one-for-one so rawToChar() can build a string with the
  # same byte offsets. Slice by byte position before transcoding and keep
  # the original bytes for binary timestamps.
  text_raw <- data_raw
  text_raw[text_raw == as.raw(0x00)] <- as.raw(0x20)
  record_text <- rawToChar(text_raw)
  Encoding(record_text) <- "bytes"

  # One-based field positions start at 2, after the deletion marker.
  field_starts <- 2L + c(
    0L,
    utils::head(cumsum(structure$fields$width), -1L)
  )

  # Decode one field across all retained rows to avoid per-cell R calls.
  columns <- lapply(seq_len(nrow(structure$fields)), function(index) {
    dbf_decode_field(
      field = structure$fields[index, , drop = FALSE],
      field_start = field_starts[index],
      record_starts = record_starts,
      data_raw = data_raw,
      record_text = record_text,
      encoding = structure$encoding,
      timezone = timezone
    )
  })
  names(columns) <- structure$fields$name

  if (add_deleted_flag) {
    columns <- c(list(.deleted = deleted), columns)
  }

  result <- as.data.frame(
    columns,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  dbf_add_attributes(result, structure, dbf_file)
}
