#' Import data from given filename
#'
#' This function reads a csv file defined by \code{filename} if the file
#' can be found in the directory and returns a data frame. If the file
#' cannot be found, the function halts and throws "file does not exist" error
#' message.
#'
#' @param filename The name of the csv file to be read.
#'
#' @return This function returns a data frame of the data read.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
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

#' Create a file according to year
#'
#' This is a simple function that creates a file according to the \code{year} that
#' the accidents happened
#'
#' @param year A string that specifies the year to be included in the filename
#'
#' @return This function returns and prints a string that defines the filename
#' according to the year that accidents happened
#'
#' @examples
#' \dontrun{make_filename("1996")}
#' \dontrun{make_filename("2014")}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read files from given year and extracts month information in that year for a list
#' of years.
#'
#' This is a function that reads csv files according to given years and then extracts
#' month and year information from data. The function throws a warning if accident file for
#' a year cannot be found.
#'
#' @param years A list of string of years that will be used to find the files.
#'
#' @return This function returns a list of either data frame where each data frame
#' contains the MONTH and year column of the dataset or NULL if one or more files
#' corresponding to year in \code{years} do not exist.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{fars_read_years({"1996", "2013","2014"})}
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

#' Summarize data sets by No. of occurrences in a specific year.
#'
#' This function combines month and year data frames returned by
#'  \link{fars_read_years} to create a new data frame and summarizes the new
#'  data frame by No. of occurrences of each year in \code{years}.
#'
#'  @param years A list of strings of years that can be used to read files
#'
#'  @return a new data frame that has the total No. of occurrences of accidents
#'   for each year in \code{years}.
#'
#'  @importFrom dplyr bind_rows group_by summarize
#'  @importFrom tidyr spread
#'  @importFrom magrittr %>%
#'
#'  @examples
#'  \dontrun{fars_summarize_years({"2013","2014","2015})}
#'
#'  @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Generate a map of a state in a specific year of locations where accidents
#' occurred.
#'
#' This function generates a scatter plot map where x and y axes are defined by
#' longitude and latitude of a state (according to \code{state.num})
#' in a specific \code{year} to reflect the location of accidents. If the state
#' cannot be found in the dataset, the function halts and throws an error message.
#' If there is no accidents in the state for the given year, the function returns
#' NULL with a message "no accidents to plot".
#'
#' @param state.num A string that can be used to find the state of interest in
#' the data frame.
#' @param year A string that can be used to find the file of the specific year.
#'
#' @return This function returns NULL if no accidents occurred in the state
#' during the given year. Otherwise, it returns a scatter plot that highlights the
#' locations defined by longitude and latitude of accidents occurred.
#'
#' @examples
#' \dontrun{fars_map_state("13", "2014")}
#' \dontrun{fars_map_state("0", "2013")}
#'
#' @importFrom dplyr filter
#' @importFrom graphics points
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
