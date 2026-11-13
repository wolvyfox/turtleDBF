# turtleDBF

An R package for reading DBF files from the Queensland turtle database into typed data frames.

## Installation

You can install the package directly from GitHub.
If you have 'devtools' installed, use devtools. Otherwise, use 'remotes' as it is lighter weight:

```r
# If you have 'devtools' installed
devtools::install_github("wolvyfox/turtleDBF")

# Otherwise, using 'remotes' package (lighter weight)
install.packages("remotes")
remotes::install_github("wolvyfox/turtleDBF")
```

## Usage

The package provides one main function, `read_dbf()`.

```r
library(turtleDBF)

# Help from within R after loading package
?turtleDBF
?read_dbf

# Read a DBF file
data <- read_dbf("path/to/your/file.dbf")

# Include deleted records and identify them in a logical .deleted column
data_with_deleted <- read_dbf(
  "path/to/your/file.dbf",
  include_deleted_rows = TRUE,
  add_deleted_flag = TRUE
)
```

## Supported files

| Layout | Version bytes |
| --- | --- |
| dBASE III compatible | `0x03`, `0x83` |
| dBASE Level 7 | `0x04`, `0x8C` |
| Visual FoxPro | `0x30` |

The reader uses the header's language driver where recognised, with CP437
as the default fallback. Pass `encoding` if you know the file uses another
encoding. It converts standard text, numeric, date and logical fields as
well as dBASE Level 7 timestamps. Memo sidecars and specialised Visual
FoxPro binary fields are outside its scope.

## How reading works

The version byte tells the reader where to find the field descriptors and
how long each one is. Each descriptor gives a field's name, type and width
in bytes. The record length tells the reader how many bytes to step over to
reach the next row. For example, if records are eight bytes wide and a field
starts at position five in the first record, that field starts at positions
5, 13, 21 and so on in the record block.

The reader loads the record block into memory and uses those positions to
extract a vector of values for each field. It then converts that vector
to the column's R type. If the file ends early, it warns and returns the
complete records it could read.

## License

MIT 
