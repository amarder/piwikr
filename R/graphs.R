#' @import ggplot2
#' @importFrom lubridate dminutes ddays
NULL

no_grid <- function() {
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )
}

my_theme <- function() theme_bw() + no_grid()

#' Graph number of visitors over time.
#'
#' @param days Table of days.
#' @export
#'
graph_visitors_vs_date <- function(days) {
    rects <- days %>%
        mutate_(
            xmin = ~ day_of_first_visit - ddays(0.5),
            xmax = ~ day_of_first_visit + ddays(0.5)
        )

    aesthetics <- aes_string(
        xmin = "xmin", xmax = "xmax",
        ymin = "0", ymax = "new_visitors"
    )

    ggplot(rects, aesthetics) +
        geom_rect() +
        ggtitle("Daily Traffic") +
        ylab("New Visitors") +
        xlab("") +
        my_theme()
}

#' Graph distribution of browser resolutions.
#'
#' @param visits Table of visits.
#' @export
#'
graph_browser_resolutions <- function(visits) {
    resolutions <- visits %>%
        group_by_("screen_width", "screen_height") %>%
        filter_(~ !is.na(screen_width), ~ !is.na(screen_height)) %>%
        summarise_(n = ~ n()) %>%
        ungroup() %>%
        mutate_(proportion = ~ n / sum(n))

    aesthetics <- aes_string(
        x = "screen_width", y = "screen_height", size = "proportion"
    )
    ggplot(resolutions, aesthetics) +
        geom_point(alpha = 0.5) +
        scale_size(range = c(0.1, 4)) +
        my_theme() +
        coord_fixed(ratio = 1) +
        ggtitle("Browser Dimensions") +
        ylab("Height") +
        xlab("Width") +
        guides(size = guide_legend(title = "Proportion\nof Visits")) +
        scale_x_continuous(breaks = seq(0, 6000, 1000), limits = c(0, NA)) +
        scale_y_continuous(breaks = seq(0, 6000, 1000), limits = c(0, NA))
}

#' @importFrom utils tail
traffic_flows <- function(actions, base_url = "amarder.github.io", n) {
    ## Focus on actions on this site.
    views <- actions %>%
        filter_(sprintf("grepl('%s', url)", base_url)) %>%
        mutate_(page = sprintf("sub('%s', '', url)", base_url))

    ## Create a column of what page this visitor viewed before this
    ## page.
    views <- views %>%
        group_by_("visit_id") %>%
        mutate_(t = ~ (datetime - min(datetime)) / dminutes(1), n = "n()") %>%
        arrange_("datetime") %>%
        mutate_(previous_page = "lag(page)") %>%
        ungroup()

    ## Create a table of vertices.
    pages <- views %>%
        group_by_("page") %>%
        summarise_(n = "n()") %>%
        ungroup()

    ## Create a table of edges.
    edges <- views %>%
        filter_("!is.na(previous_page)", "previous_page != page") %>%
        group_by_("previous_page", "page") %>%
        summarise_(n = "n()") %>%
        ungroup() %>%
        select_(i = "previous_page", j = "page", n = "n")

    ## Keep the top n pages.
    pages <- pages %>% arrange_("n") %>% tail(n = n)
    edges <- edges %>%
        right_join(pages %>% select_(i = "page"), by = "i") %>%
        right_join(pages %>% select_(j = "page"), by = "j") %>%
        filter_(~ !is.na(n))

    return(list(vertices = pages, edges = edges))
}

#' Graph structure of site based on visitor actions.
#'
#' @param actions Table of actions.
#' @param n The top `n` most-visited pages will be included in the
#'     graph.
#' @param layout igraph layout function used to arrange vertices.
#'
#' @importFrom igraph graph_from_data_frame layout_with_graphopt
#' @export
#'
graph_site_structure <- function(actions, n = 10, layout = layout_with_graphopt) {
    g <- traffic_flows(actions, n = n)

    igraph_obj <- graph_from_data_frame(
        g$edges,
        directed = TRUE,
        vertices = g$vertices
    )

    ## Use igraph to position vertices
    xy_coords <- layout(igraph_obj)
    g$vertices$x <- xy_coords[, 1]
    g$vertices$y <- xy_coords[, 2]

    ## Merge the coordinates into the edge data
    g$edges <- g$edges %>%
        left_join(g$vertices %>% select_(i = "page", i_x = "x", i_y = "y"), by = "i") %>%
        left_join(g$vertices %>% select_(j = "page", j_x = "x", j_y = "y"), by = "j")

    ggplot() +
        geom_curve(aes_string(x = "i_x", y = "i_y", xend = "j_x", yend = "j_y", alpha = "n"), data = g$edges) +
        geom_label(aes_string(x = "x", y = "y", size = "n", label = "page"), data = g$vertices) +
        theme_classic() +
        xlab("") +
        ylab("") +
        scale_x_continuous(breaks = NULL, expand = c(.1, 0)) +
        scale_y_continuous(breaks = NULL, expand = c(.1, 0)) +
        scale_alpha(range = c(0, 1), guide = guide_legend(title = "Link Traversals")) +
        scale_size(range = c(2, 7), guide = guide_legend(title = "Visitors"))
}
