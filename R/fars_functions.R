#' Make a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} file name
#'
#' This function construct a name for a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file give a year.
#'
#' @param year An \code{integer}, or any other type could be coerced to an integer, represents the year of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file.
#'
#' @return A \code{string} for a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file for the given year.
#'
#' @details The function constructs a string for a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file for the given year. It addes the prefix "accident_" to the given year then adds ".csv.bz2" suffix to the string.
#'
#' @examples
#' \dontrun{
#' # input year as an intger
#' make_filename(2013)
#'
#' # input year as a character
#' make_filename("2014")
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' Read a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file
#'
#' This is a custom \code{\link[readr]{read_csv}} function that reads a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file given its name.
#'
#' @param filename A string, such as returned by \code{\link{make_filename}}, represent a name of a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data in the current working directory.
#'
#' @return A \code{data.frame} of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file.
#'
#' @details The function returns an error \code{file does not exist} if a file with the name from the input exist in the current working
#' directory, other wise return a \code{data.fram}.
#'
#' @examples
#' \dontrun{
#' # construct a file name using `make_filename`
#' file_name <- make_filenam(2013)
#' fars_read(file_name)
#'
#' # supply file name explicitly
#' fars_read("accident_2013.csv.bz2")
#' }
#'
#' @import readr
#' @import dplyr
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Read \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data files.
#'
#' This is a custom \code{readr_csv} function that reads a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file given their corresponding years.
#'
#' @param years A vector of \code{integr}s, or other types coerced to intgers, represent the years of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data files.
#'
#' @return A \code{list} of \code{data.frame}s, each of its elements correspond to one of the inpute years.
#'
#' @details The function returns a warning \code{invalid year: year} if the input year does not correspond to one of the files in the current working directory.
#' othewise construct a vecotr of file names using \code{\link{make_filename}} and reads them in a list using \code{\link{fars_read}}.
#'
#' @examples
#' \dontrun{
#' # invalid input, year doesn't exist
#' fars_read_years(c(2013, 2014, 1900))
#'
#' # valid input
#' fars_read_years(2013:2015)
#' }
#'
#' @import dplyr
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}



#' Summarize \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data
#'
#' This function returns a summary of the number of accidents per month for each year of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data
#'
#' @param years fars_read_years
#'
#' @return A \code{tibble} of number of accidents with columns represent the years and rows of the months.
#'
#' @details The function returns a numerical summary of the number of accidents per month for each year provided in the input.
#'
#' @examples
#' \dontrun{
#' years <- 2013:2015
#' fars_summarize_years(years)
#' }
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data to a state map
#'
#' This function plots individual \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} accidents' coordinates as points on a stat map
#'
#' @param state.num An \code{intger} represents the identifier of a state.
#' @inheritParams make_filename
#'
#' @return A plot of a state from \code{\link[maps]{map}} and points correspond to the coordinates of recorded accidents.
#'
#' @details The function returns an error \code{invalid STATE number: state.num} if the provided number does not correspond to a STATE in the data of this particular year and a message \code{no accidents to plot} if the number of accident for this year is 0.
#' otherwise it constructs a file name using the provided year, read the data file and plot points of individual accidents on the STATE map.
#'
#' @examples
#' \dontrun{
#' fars_map_state(50, 2013)
#' }
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
