# turtleDBF

An R package for reading and translating DBF-IV files, specifically designed for turtle data within Threatened Species Operations of the Queensland Department of Environment, Tourism and Science and Innovation.

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

The package provides a single main function `read_dbf()` that reads the DBF-IV files and returns a data frame:

```r
library(turtleDBF)

# Help from within R after loading package
?turtleDBF
?read_dbf

# Read a DBF file
data <- read_dbf("path/to/your/file.dbf")

# Read a DBF file including deleted rows
data_with_deleted <- read_dbf("path/to/your/file.dbf", include_deleted_rows = TRUE)
```

## Features

- Reads DBF files and converts them to R data frames
- Automatically detects and converts field types (numeric, date, logical, character)
- Option to include or exclude deleted rows
- Handles character encoding (CP437 to UTF-8)

## License

MIT 
