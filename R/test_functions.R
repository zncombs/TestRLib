#' @importFrom dplyr tbl_df mutate select summarize group_by bind_rows filter
#' @importFrom readr read_csv
#' @importFrom tidyr spread
#' @importFrom maps map
#' @importFrom graphics points

#' @title fars_read
#' @description attempts to read a file and creates a data.frame from that file if it is found to exist.
#' @param filename string name of file to be imported.
#' @return data.frame

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    read_csv(filename, progress = FALSE)
  })
  tbl_df(data)
}

#' @title make_filename
#' @description appends a year (int) to the file name as a string. will fail in case where a non-integer value or null is paaed to the year parameter.
#' @param year string name of file to be imported.
#' @return string of file name.

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title fars_read_years
#' @description accepts a list of years to get car fatality data for.
#' @param years string name of file to be imported.
#' @return string of file name.

fars_read_years <- function(years) {

  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      mutate(dat, year = year) %>%
        select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title fars_summarize_years
#' @description this function returns a summarization of fatalities grouped by the year and month, and accepts alist of years.
#' @param years int value that user wished to provide the summarization for.
#' @return data.frame provides a summarization of fatalities grouped by year, and month dimensions.
#' @export

fars_summarize_years <- function(years) {

  dat_list <- fars_read_years(years) %>%
    bind_rows(dat_list) %>%
    group_by(year, MONTH) %>%
    summarize(n = n()) %>%
    spread(year, n)
}

#' @title fars_map_state
#' @description this function returns a summarization of fatalities grouped by the year and month, and accepts alist of years.
#' @param state.num int value of the state the user wishes to filter by.
#' @param year int value that user wished to provide the summarization for.
#' @examples
#' \dontrun{
#'  fars_map_state(state.num = 1, year = 2015)
#' }
#' @return data.frame provides a summarization of fatalities grouped by year, and month dimensions.
#' @export

fars_map_state <- function(state.num, year) {

  globalVariables(c("STATE"))

  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    points(LONGITUD, LATITUDE, pch = 46)
  })
}
