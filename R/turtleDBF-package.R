#' @title turtleDBF: A translator for the DBF-IV files of the turtle database
#' @name turtleDBF
#' @aliases turtleDBF-package
#' @docType package
#' @importFrom utils citation
#'
#' @description
#' A package for reading and translating DBF-IV files, specifically designed for 'turtle data' 
#' within Threatened Species Operations of the Queensland Department of Environment, Tourism 
#' and Science and Innovation (DETSI).
#'
#' For citation information regarding turtleDBF, run \code{citation("turtleDBF")}.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{read_dbf}}}{Read and translate DBF-IV files into R data frames}
#' }
#'
#' @section Usage:
#' To use this package:
#' \enumerate{
#'   \item Install the package: \code{remotes::install_github("wolvyfox/turtleDBF")}
#'   \item Load the package: \code{library(turtleDBF)}
#'   \item Read a DBF file: \code{data <- read_dbf("path/to/your/file.dbf")}
#' }
#'
#' @section Features:
#' \itemize{
#'   \item Reads DBF files and converts them to R data frames
#'   \item Automatically detects and converts field types (numeric, date, logical, character)
#'   \item Option to include or exclude deleted rows
#'   \item Handles character encoding (CP437 to UTF-8)
#' }
#'
#' @author
#' \strong{Maintainer:} Andrew P. Colefax \email{andrew.colefax@des.qld.gov.au}
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/wolvyfox/turtleDBF}
#'   \item Report bugs at \url{https://github.com/wolvyfox/turtleDBF/issues}
#' }
#'
#' @keywords internal
"_PACKAGE" 