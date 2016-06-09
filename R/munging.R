#' @importFrom lubridate floor_date
#' @importFrom utils head

globalVariables(c(
    "day",
    "visitor_id",
    "Visitors",
    "Source",
    "referer_name",
    "referer_type",
    ".",
    "visit_first_action_time",
    "Page"
))

#' Compute a table of visitors from a table of actions.
#'
#' @param actions Table of actions.
#' @export
compute_visitors <- function(actions) {
    visitors <- actions %>%
        mutate(day = floor_date(actions$datetime, "day")) %>%
        group_by(visitor_id) %>%
        summarise(day_of_first_visit = min(day))

    return(visitors)
}

#' Compute a table of pages from a table of actions.
#'
#' @param actions Table of actions.
#' @export
compute_pages <- function(actions) {
    pages <- actions %>%
        group_by(url) %>%
        summarise(n = length(unique(visitor_id))) %>%
        arrange(desc(n)) %>%
        filter(grepl("amarder.github.io", url)) %>%
        mutate(Page = sub("amarder.github.io", "", url), Visitors = n) %>%
        select(Page, Visitors)

    return(pages)
}

#' Compute a table of days from a table of actions.
#'
#' @param actions Table of actions.
#' @export
compute_days <- function(actions) {
    visitors <- compute_visitors(actions)

    date_range <- function(x) seq(min(x), max(x), by = "days")
    grid <- data.frame(
        day_of_first_visit = date_range(visitors$day_of_first_visit)
    )

    days <- visitors %>%
        group_by(day_of_first_visit) %>%
        summarise(new_visitors = n()) %>%
        right_join(grid, by = "day_of_first_visit") %>%
        mutate(new_visitors = ifelse(is.na(new_visitors), 0, new_visitors))

    return(days)
}

#' Summarise visits by the source of traffic.
#'
#' @param visits Table of visits.
#' @export
compute_sources <- function(visits) {
    visitors <- visits %>%
        group_by(idvisitor) %>%
        arrange(visit_first_action_time) %>%
        do(head(., 1)) %>%
        ungroup()

    sources <- visitors %>%
        mutate(Source = ifelse(referer_type == 1, "(direct)", referer_name)) %>%
        group_by(Source) %>%
        summarise(Visitors = n()) %>%
        arrange(desc(Visitors))

    return(sources)
}
