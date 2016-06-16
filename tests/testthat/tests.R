db <- db_from_env()
base_url <- "amarder.github.io"
visits <- get_visits(db)
actions <- get_actions(db)

test_that("table prefix can be set", {
    expect_error(
        get_visits(db, table_prefix = "xxx"),
        "Table 'piwik.xxxlog_visit' doesn't exist"
    )
    expect_error(
        get_actions(db, table_prefix = "xxx"),
        "Table 'piwik.xxxlog_link_visit_action' doesn't exist"
    )
})

test_that("munging code works", {
    visitors <- compute_visitors(actions)
    days <- compute_days(actions)
    pages <- compute_pages(actions, base_url)
    sources <- compute_sources(visits)
})

test_that("graphing code works", {
    graph_visitors_vs_date(compute_days(actions))
    graph_browser_resolutions(visits)

    actions_on_big_pages <- actions %>%
        group_by(url) %>%
        mutate(visitors = n_distinct(visitor_id)) %>%
        filter(visitors > 3) %>%
        ungroup()
    graph_site_structure(actions_on_big_pages, base_url)
    file.remove("Rplots.pdf")
})
