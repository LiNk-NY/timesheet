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

#' @examples
#'
#' meeting_record("2022-08-28", "2022-09-10")
#'
#' @export
meeting_record <- function(
    start_date, end_date,
    remove_text = " and Marcel Ramos", name = "Quantitative",
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
    cbind(students, studata)
}

# write.csv(sched, file = "~/data/QuantTutoringS2022.csv", row.names = FALSE)

