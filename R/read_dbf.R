# A DBF stores a header followed by fixed-width records (rows).
# The header tells us where the records begin, how long each record is and
# how many the file declares. It also describes each field (column) by name,
# type and width. Every record starts with a deletion marker.
#
# A raw byte holds a value from 0 to 255. R prints raw bytes in hexadecimal.
# For example, 31 means 0x31 (49 in decimal). In an ASCII text field, that
# byte stands for the character "1". The DBF layout tells us how to interpret
# each byte. Field widths count bytes, so a character that takes more than
# one byte in the file's encoding uses more than one position.


# Header helpers ---------------------------------------------------------------

# Multi-byte header numbers are stored in little-endian order. The first
# byte counts ones, the next counts 256s and the next counts 256^2s.
# For example, 02 01 represents 2 * 256^0 + 1 * 256^1 = 258.
dbf_little_endian <- function(bytes) {
  sum(as.integer(bytes) * 256^(seq_along(bytes) - 1))
}


# A field name occupies a fixed slot in its descriptor. The first 00 byte
# (NUL) ends the name. For example, 41 4C 49 00 00 represents "ALI" with
# padding. Keep printable ASCII bytes from the name.
dbf_header_text <- function(bytes) {
  values <- as.integer(bytes)
  first_zero <- match(0L, values)

  if (!is.na(first_zero)) {
    values <- utils::head(values, first_zero - 1L)
  }

  values <- values[values >= 32L & values <= 126L]
  if (length(values) == 0L) "" else intToUtf8(values)
}


# The language driver is a header code for the file's text encoding.
# For example, driver 3 selects Windows-1252. A zero or unrecognised code
# uses the fallback. An explicit encoding supplied by the caller takes
# precedence over this lookup.
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


# Read the header and return the layout that read_dbf() needs to locate and
# interpret the record bytes.
dbf_read_structure <- function(
    connection,
    dbf_file,
    encoding,
    fallback_encoding
) {
  # The supported layouts share these first 32 bytes. Reading them as raw
  # leaves each byte available for interpretation according to its position.
  fixed_header <- readBin(connection, what = "raw", n = 32L)

  if (length(fixed_header) < 32L) {
    stop("The file is too short to contain a DBF header.")
  }

  # A file offset counts how many bytes to skip from the start of the file.
  # The first byte has offset 0 and is at R index 1. A value of 04 here
  # becomes the integer 4, which matches the hexadecimal constant 0x04.
  version_byte <- as.integer(fixed_header[1])
  supported_versions <- c(0x03, 0x83, 0x04, 0x8C, 0x30)

  if (!version_byte %in% supported_versions) {
    stop(
      "Unsupported or damaged DBF version byte: ",
      sprintf("0x%02X", version_byte)
    )
  }

  # These are R indices. For instance, bytes 9 and 10 (file offsets 8
  # and 9) together store the header length.
  number_of_records <- dbf_little_endian(fixed_header[5:8])
  header_length <- dbf_little_endian(fixed_header[9:10])
  record_length <- dbf_little_endian(fixed_header[11:12])
  language_driver_id <- as.integer(fixed_header[30])

  # Each field descriptor is a block of metadata for one column.
  # Layout                 First descriptor offset   Bytes per descriptor
  # dBASE III / FoxPro                           32                     32
  # dBASE Level 7                                68                     48
  # Level 7 has 36 extra bytes before its first descriptor (32 + 36 = 68).
  # A descriptor can use 48 header bytes to describe a field that takes
  # only 4 bytes in each record.
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

  # Two 32-byte descriptors start at offsets 32 and 64. The field list
  # then ends with a 0D byte at offset 96. Check for that marker only
  # where another descriptor could start. A 0D inside a descriptor can
  # have another meaning, such as a field width of 13 bytes.
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
    # Convert the file offset to an R index. The name, type and width
    # then sit at different positions within each descriptor layout.
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
    # A descriptor terminator immediately after the fixed header can
    # describe an empty table with no columns.
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

  # The first byte of each record holds the deletion marker.
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


# A DBF with no active records should still return correctly typed columns.
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


# A Level 7 timestamp stores a day number and milliseconds since midnight
# as two four-byte binary integers. Zero bytes inside these integers are
# meaningful, so use the original record bytes.
dbf_decode_timestamp <- function(data_raw, starts, timezone) {
  # Add offsets 0 through 7 to each timestamp start. Each matrix column
  # then holds the eight original bytes for one record.
  byte_indices <- outer(0:7, starts, `+`)
  bytes <- matrix(as.integer(data_raw[byte_indices]), nrow = 8L)

  julian_day <- colSums(bytes[1:4, , drop = FALSE] * 256^(0:3))
  milliseconds <- colSums(bytes[5:8, , drop = FALSE] * 256^(0:3))

  valid <- julian_day >= 1721426 & julian_day <= 5373484 &
    milliseconds >= 0 & milliseconds < 86400000

  # Julian day 2440588 is 1970-01-01, the POSIXct origin used below.
  # A timestamp on that day with 1000 milliseconds becomes one second
  # after the origin. Invalid day or time values stay NA.
  seconds <- rep(NA_real_, length(julian_day))
  seconds[valid] <- (julian_day[valid] - 2440588) * 86400 +
    milliseconds[valid] / 1000

  as.POSIXct(seconds, origin = "1970-01-01", tz = timezone)
}


# Decode one fixed-width field for every retained record.
dbf_decode_field <- function(
    field,
    field_start,
    record_starts,
    data_raw,
    record_text,
    encoding,
    timezone
) {
  # Worked example with two records of 8 bytes each. The header describes
  # NAME as a 3-byte character field and LENGTH as a 4-byte numeric field.
  #
  #                    Marker   NAME       LENGTH
  # Record 1 (hex)     20       41 4C 49   31 32 2E 35
  # Record 2 (hex)     20       42 4F 42   20 39 2E 30
  # NAME reads as "ALI" and "BOB". LENGTH reads as "12.5" and " 9.0".
  #
  # record_starts is c(0, 8), measured from the start of the record block.
  # LENGTH starts at R position 5 within either record (after the marker
  # and NAME). Adding 5 to c(0, 8) gives c(5, 13) in the record block.
  starts <- record_starts + field_start

  if (field$type == "@") {
    return(dbf_decode_timestamp(data_raw, starts, timezone))
  }

  # For LENGTH, the start positions are c(5, 13) and the inclusive end
  # positions are c(8, 16). One substring() call extracts both values.
  # The same call can take a vector of positions for thousands of rows.
  values <- substring(
    record_text,
    first = starts,
    last = starts + field$width - 1L
  )

  # Decode using the file's encoding before trimming the padding.
  # iconv() drops bytes it cannot convert, so trimws() receives valid text.
  values <- iconv(values, from = encoding, to = "UTF-8", sub = "")
  values <- trimws(values)

  # C is ordinary text. In a text-based memo field (M), these bytes
  # identify a memo in a separate file. Return the stored pointer.
  if (field$type %in% c("C", "M")) {
    return(values)
  }

  values[values == ""] <- NA_character_

  # Numeric fields store text digits. In the LENGTH example, trimming
  # gives c("12.5", "9.0") and as.numeric() gives c(12.5, 9).
  if (field$type %in% c("N", "F")) {
    return(suppressWarnings(as.numeric(values)))
  }

  if (field$type == "D") {
    # A date such as "20261113" has year, month and day in that order.
    return(suppressWarnings(as.Date(values, format = "%Y%m%d")))
  }

  if (field$type == "L") {
    # Y/T and N/F become TRUE and FALSE. Blank or unknown values stay NA.
    logical_value <- rep(NA, length(values))
    logical_value[toupper(values) %in% c("Y", "T")] <- TRUE
    logical_value[toupper(values) %in% c("N", "F")] <- FALSE
    return(logical_value)
  }

  # Use a text fallback for fields without a type-specific decoder.
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
  # "rb" opens the file for reading in binary mode. readBin() can then
  # read the bytes used for both header metadata and field values.
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

  # Skip exactly the declared number of header bytes to reach the records.
  # Some layouts have extra information after the descriptor terminator.
  # For example, two 8-byte records ask readBin() for 16 bytes.
  seek(connection, where = structure$header_length, origin = "start")
  data_raw <- readBin(
    connection,
    what = "raw",
    n = structure$number_of_records * structure$record_length
  )

  # If only 14 bytes arrive for two 8-byte records, there is one complete
  # record. Warn and discard the six bytes from the incomplete record.
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

  # Record starts are offsets within data_raw. With 8-byte records the
  # offsets are 0, 8, 16, and so on. Add one for R indices 1, 9, 17.
  # A 0x20 (space) marks an active record. A 0x2A (asterisk) marks deletion.
  record_starts <- (seq_len(complete_records) - 1L) *
    structure$record_length
  deleted <- data_raw[record_starts + 1L] == as.raw(0x2A)
  keep <- include_deleted_rows | !deleted

  # Apply the same filter to the offsets and flags so the optional
  # .deleted column stays aligned with the rows we decode.
  record_starts <- record_starts[keep]
  deleted <- deleted[keep]

  # Deleted records are normal DBF behaviour. If every record is deleted, the
  # usual result is simply a correctly typed data frame with zero rows.
  if (length(record_starts) == 0L) {
    return(empty_result())
  }

  # rawToChar() needs a NUL-free buffer for a single string. Replacing
  # each NUL with one space preserves every position in the text copy.
  # Keep the original bytes for timestamps. Mark the text copy as "bytes"
  # so substring() counts bytes before iconv() converts fields to UTF-8.
  text_raw <- data_raw
  text_raw[text_raw == as.raw(0x00)] <- as.raw(0x20)
  record_text <- rawToChar(text_raw)
  Encoding(record_text) <- "bytes"

  # The deletion marker takes position 1 within a record. Field starts
  # are 2 plus the widths of the preceding fields. In the worked example,
  # NAME starts at 2 and LENGTH starts at 2 + 3 = 5.
  field_starts <- 2L + c(
    0L,
    utils::head(cumsum(structure$fields$width), -1L)
  )

  # lapply() visits each field once. The original reader called its
  # extraction and conversion code for each cell in nested R loops.
  # Here each call works on a column's vector of values. R still processes
  # each value internally, with fewer calls from the reader's R code.
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
