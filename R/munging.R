#' Compute a table of visitors from a table of actions.
#'
#' @param actions Table of actions.
#' @export
#' @importFrom lubridate floor_date
#'
compute_visitors <- function(actions) {
    visitors <- actions %>%
        group_by_("visitor_id") %>%
        summarise_(day_of_first_visit = "min(day)")

    return(visitors)
}

#' Compute a table of pages from a table of actions.
#'
#' @param actions Table of actions.
#' @export
#'
compute_pages <- function(actions) {
    pages <- actions %>%
        group_by(url) %>%
        summarise(n = length(unique(visitor_id))) %>%
        arrange(desc(n)) %>%
        filter(grepl("amarder.github.io", url)) %>%
        mutate_(Page = "sub('amarder.github.io', '', url)", Visitors = "n") %>%
        select_("Page", "Visitors")

    return(pages)
}

#' Compute a table of days from a table of actions.
#'
#' @param actions Table of actions.
#' @export
#'
compute_days <- function(actions) {
    visitors <- compute_visitors(actions)

    date_range <- function(x) seq(min(x), max(x), by = "days")
    grid <- data.frame(
        day_of_first_visit = date_range(visitors$day_of_first_visit)
    )

    days <- visitors %>%
        group_by_("day_of_first_visit") %>%
        summarise_(new_visitors = "n()") %>%
        right_join(grid, by = "day_of_first_visit") %>%
        mutate_(new_visitors = "ifelse(is.na(new_visitors), 0, new_visitors)")

    return(days)
}

#' Summarise visits by the source of traffic.
#'
#' @param visits Table of visits.
#' @export
#' @importFrom utils head
#'
compute_sources <- function(visits) {
    visitors <- visits %>%
        group_by_("idvisitor") %>%
        mutate_(min_time = "min(visit_first_action_time)") %>%
        filter_("visit_first_action_time == min_time") %>%
        ungroup()

    sources <- visitors %>%
        mutate_(Source = "ifelse(referer_type == 1, '(direct)', referer_name)") %>%
        group_by_("Source") %>%
        summarise_(Visitors = "n()") %>%
        arrange_("desc(Visitors)")

    return(sources)
}
