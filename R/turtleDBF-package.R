#' @title turtleDBF: A reader for the DBF files of the turtle database
#' @name turtleDBF
#' @aliases turtleDBF-package
#' @docType package
#' @importFrom utils citation
#'
#' @description
#' A package for reading DBF files, specifically designed for 'turtle data'
#' within Threatened Species Operations of the Queensland Department of Environment, Tourism 
#' and Science and Innovation (DETSI).
#'
#' For citation information regarding turtleDBF, run \code{citation("turtleDBF")}.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{read_dbf}}}{Read supported DBF layouts into typed R data frames}
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
#'   \item Reads dBASE III-compatible, dBASE Level 7 and Visual FoxPro layouts
#'   \item Converts supported DBF fields to typed R columns
#'   \item Optionally includes deleted records and marks them with a logical column
#'   \item Converts text to UTF-8 using a recognised language driver or an explicit encoding
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
