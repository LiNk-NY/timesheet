.preprocess_calendar <-
    function(dets, name = "Quantitative", maxResults = 2000L)
{
    res <- gc_ls(pattern = "")
    quantcal <- gc_summary(name)
    gc_events(quantcal, maxResults = maxResults)
}

.filter_by_date <- function(dates, start_date, end_date) {
    !is.na(dates) & dates >= ymd(start_date) & dates <= ymd(end_date)
}

.get_dates <- function(cal) {
    ymd(
        vapply(
            strsplit(cal$start.dateTime, "T"), `[`, character(1L), 1L
        )
    )
}

#' Tally of hours per day within a given period from Google calendar
#'
#' Provides the user a small table of total hours per day, within a given
#' Google Calendar name.
#'
#' @examples
#'
#' timesheet("Quantitative", "2022-08-28", "2022-09-10")
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#' @importFrom dplyr summarize group_by
#' @importFrom lubridate ymd ymd_hms
#' @export
timesheet <- function(
    cal_name = "Quantitative", start_date, end_date, include_cancel = FALSE,
    token = getOption("timesheet.token"), maxResults = 2000L
) {
    if (is.null(token) || !isScalarCharacter(token) || !file.exists(token))
        stop("Provide a valid token RDS file. See the 'scripts' folder.")
    googlecalendar::gc_auth(token = token)
    gc_event_ls <-
        .preprocess_calendar(name = cal_name, maxResults = maxResults)
    dates <- .get_dates(gc_event_ls)
    time_filter <- .filter_by_date(dates, start_date, end_date)

    if (!include_cancel)
        time_filter <- time_filter &
            !startsWith(gc_event_ls$summary, "Canceled")

    student <- vapply(
        strsplit(gc_event_ls$summary[time_filter], " and "),
        `[`, character(1L), 1L
    )
    timespan <- ymd_hms(gc_event_ls$end.dateTime[time_filter]) -
        ymd_hms(gc_event_ls$start.dateTime[time_filter])
    starttime <- ymd_hms(gc_event_ls$start.dateTime[time_filter])
    timetable <- data.frame(student,
        date = dates[time_filter], hours = as.numeric(timespan/60))
    group_by(timetable, date) |> dplyr::summarize(total_hours = sum(hours))
}

payrollDates <- function(
        prStart = 15, prEnd = 28, weekspan = 2L, startDate = "2022-01-02"
) {
    prs <- seq(prStart, prEnd)
    multipliers <- (seq_along(prs) - 1L) * 2L
    starts <- lubridate::as_date(
        vapply(
            multipliers,
            function(x)
                ymd(startDate) + weeks(x), ymd(startDate)
        )
    )
    ints <- lapply(starts, function(st)
        interval(st, st + weeks(weekspan))
    )
    ist <- vapply(ints,
        function(x) as.character(ymd(int_start(x))), character(1L)
    )
    est <- vapply(ints,
        function(x) as.character(ymd(int_end(x))), character(1L)
    )
    data.frame(PR = prs, startDate = ist, endDate = est)
}
