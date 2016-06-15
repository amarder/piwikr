#' @import ggplot2
#' @importFrom tidyr separate_ gather_
#' @importFrom lubridate dminutes ddays
#' @importFrom igraph graph_from_data_frame E V layout.auto plot.igraph
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
traffic_flows <- function(actions, base_url = "amarder.github.io", top = 10) {
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
        ungroup() %>%
        mutate_(page_id = "row_number(n)")

    ## Create a table of edges.
    edges <- views %>%
        filter_("!is.na(previous_page)", "previous_page != page") %>%
        group_by_("previous_page", "page") %>%
        summarise_(n = "n()")

    ## Keep the top n pages.
    pages <- pages %>% arrange_("n") %>% tail(n = top)
    edges <- edges %>%
        filter_(~ (page %in% pages$page) & (previous_page %in% pages$page))

    return(list(vertices = pages, edges = edges))
}

#' Graph structure of site based on visitor actions.
#'
#' @param actions Table of actions.
#' @param vertex_size A function to transform the number a viewers a
#'     page received into the dize of the vertex to be graphed.
#'
#' @export
#'
graph_site_structure <- function(actions, vertex_size = function(n) 2 * log(n)) {
    data <- traffic_flows(actions)

    g <- graph_from_data_frame(
        data$edges,
        directed = TRUE,
        vertices = data$vertices
    )
    edge_importance <- 5 * E(g)$n / max(E(g)$n)
    layout <- layout.auto(g)
    root_vertex <- V(g)$name == "/"
    origin <- layout[root_vertex, ]
    above_x_axis <- layout[, 2] > origin[2]

    dist <- rep(2 / 5, nrow(data$vertices))
    dist[root_vertex] <- 0

    plot.igraph(
        g,
        vertex.size = vertex_size(V(g)$n),
        edge.width = 1 * edge_importance,
        edge.arrow.size = 0.1 * edge_importance,
        edge.curved = TRUE,
        layout = layout,
        vertex.label.dist = dist,
        vertex.label.degree = ifelse(above_x_axis, -pi / 2, pi / 2)
        )
}
