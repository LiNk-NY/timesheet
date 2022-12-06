.unstr <- function(...) {
    trimws(unlist(strsplit(...)))
}

.getStudataframe <- function(dets, remove_text = " and Marcel Ramos") {
    stud <- strsplit(
        gsub(remove_text, "", dets$summary, fixed = TRUE), "\\s"
    )
    first <- vapply(stud, `[`, character(1L), 1)
    last <- mapply(function(x, y) { paste(x[2:y], collapse = " ") },
        stud, lengths(stud))
    date <- vapply(strsplit(dets$start.dateTime, "T"), `[`, character(1L), 1L)
    startTimeC <- strftime(
        ymd_hms(dets$start.dateTime, tz = "America/New_York"),
        format = "%H:%M:%S"
    )
    startTime <- ymd_hms(dets$start.dateTime, tz = "America/New_York")
    endTime <- ymd_hms(dets$end.dateTime, tz = "America/New_York")
    hours <- as.numeric((endTime - startTime)/ 60)

    data.frame(
        First_name = first, Last_name = last, Date = date,
        startTime = startTimeC, Hours = hours,
        stringsAsFactors = FALSE
    )
}

#' Create a summary data.frame of meetings within a date interval
#'
#' Query a particular calendar name to extract the event entries. The associated
#' metadata will be included, e.g., first and last names, program, class,
#' EMPLID.
#'
#' @param name character(1) The Google calendar name
#'
#' @param start_date character(1) The start date to restrict the entries in
#'   'YYYY-MM-DD' format.
#'
#' @param end_date character(1) The end date to restrict the entries
#'   `data.frame`.
#'
#' @param include_cancel logical(1) Whether or not to include entried beginning
#'   with "Canceled" (default: FALSE)
#'
#' @examples
#'
#' if (interactive()) {
#'   meeting_record(
#'     name = "Quantitative",
#'     start_date = "2022-09-25",
#'     end_date = "2022-10-08"
#'   )
#' }
#'
#' @return A `data.frame` of meeting times and student information
#'
#' @export
meeting_record <- function(
    name, start_date, end_date,
    remove_text = " and Marcel Ramos",
    include_cancel = FALSE
) {
    dets <- .preprocess_calendar(name = name)
    if (!missing(start_date) && !missing(end_date)) {
        dates <- .get_dates(dets)
        dets <- dets[.filter_by_date(dates, start_date, end_date), ]
    }
    if (!include_cancel)
        dets <- dets[!startsWith(dets$summary, "Canceled"), ]
    students <- .getStudataframe(dets)
    studata <- do.call(rbind,
        lapply(dets$description, function(desc) {
            x <- Filter(nchar, .unstr(desc, "\n"))
            emp <- .unstr(grep("CUNY EMPL", x, value = TRUE), ":")
            empl <- tail(emp, 1L)
            # mphstud <- .unstr(grep("^Are you a", x, value = TRUE), ":")[[2]]
            prde <- .unstr(grep("^Provide a brief", x, value = TRUE), ":")
            projdesc <- if (length(prde) > 1L) prde[[2]] else ""
            ## optional
            prog <- grep("your track", x, value = TRUE)
            program <- if (length(prog)) tail(.unstr(prog, ":"), 1L) else ""
            ## optional
            cla <- grep("^What class", x, value = TRUE)
            class <- if (length(cla)) .unstr(cla, ":")[[2]] else ""
            data.frame(
                Program = program, Class = class, EMPLID = empl,
                stringsAsFactors = FALSE
            )
        })
    )
    res <- cbind(students, studata)
    res[order(res[["Date"]], res[["startTime"]]), ]
}

# write.csv(sched, file = "~/data/QuantTutoringS2022.csv", row.names = FALSE)

