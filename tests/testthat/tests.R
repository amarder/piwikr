db_from_env <- function() {
    keys <- c("dbname", "host", "port", "user", "password")
    values <- lapply(keys, function(x) Sys.getenv(paste0("MYSQL_", toupper(x))))
    names(values) <- keys
    values$port <- as.integer(values$port)
    my_db <- do.call(src_mysql, values)
    return(my_db)
}

db <- db_from_env()
visits <- get_visits(db)
actions <- get_actions(db)

test_that("munging code works", {
    visitors <- compute_visitors(actions)
    days <- compute_days(actions)
    pages <- compute_pages(actions)
    # sources <- compute_sources(visits)
})

test_that("graphing code works", {
    graph_visitors_vs_date(compute_days(actions))
    graph_browser_resolutions(visits)
    actions_on_big_pages <- actions %>%
        group_by(url) %>%
        mutate(visitors = n_distinct(visitor_id)) %>%
        filter(visitors > 3) %>%
        ungroup()
    graph_site_structure(actions_on_big_pages)
})
