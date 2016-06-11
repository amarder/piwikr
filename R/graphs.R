#' @import ggplot2
#' @importFrom tidyr separate_
#' @importFrom lubridate dminutes ddays
#' @importFrom igraph graph_from_data_frame E V layout.auto plot.igraph
NULL

#' Graph number of visitors over time.
#'
#' @param days Table of days.
#' @param point_size Size of the points plotted for each day.
#' @export
#'
graph_visitors_vs_date <- function(days, point_size = 0.5) {
    g <- (
        ggplot(days, aes_string(x = "day_of_first_visit", y = "new_visitors")) +
        geom_rect(aes_string(xmin = "day_of_first_visit - ddays(0.5)", xmax = "day_of_first_visit + ddays(0.5)", ymin = "0", ymax = "new_visitors"), fill = "black", alpha = 0.25) +
        geom_point(size = point_size) +
        ggtitle("Site Traffic by Date") +
        ylab("New Visitors") +
        xlab("") +
        theme_bw() +
        scale_y_continuous(minor_breaks = NULL)
        )
    return(g)
}

#' Graph distribution of browser resolutions.
#'
#' @param visits Table of visits.
#' @export
#'
graph_browser_resolutions <- function(visits) {
    resolutions <- visits %>%
        separate_("config_resolution", c("width", "height"), sep = "x", convert = TRUE, fill = "right") %>%
        group_by_("width", "height") %>%
        filter_("!is.na(width)", "!is.na(height)") %>%
        summarise_(n = "n()") %>%
        ungroup() %>%
        mutate_(proportion = "n / sum(n)")

    (
        ggplot(resolutions, aes_string(xmin = 0, xmax = "width", ymin = 0, ymax = "height", alpha = "proportion")) +
        geom_rect(fill = NA, color = "black") +
        theme_classic() +
        guides(alpha = guide_legend(title = "Proportion\nof Visits", override.aes = list(fill = "black", color = "white"))) +
        coord_fixed(ratio = 1) +
        ggtitle("Browser Dimensions") +
        ylab("Height") +
        xlab("Width")
    )
}

#' Graph structure of site based on visitor actions.
#'
#' @param actions Table of actions.
#' @export
#'
graph_site_structure <- function(actions) {
    views <- actions %>%
        filter_("grepl('amarder.github.io', url)") %>%
        mutate_(page = "sub('amarder.github.io', '', url)")

    pages <- views %>%
        group_by_("page") %>%
        summarise_(n = "n()") %>%
        ungroup() %>%
        mutate_(page_id = "row_number(n)")

    views <- views %>%
        group_by_("visit_id") %>%
        mutate_(t = ~ (datetime - min(datetime)) / dminutes(1), n = "n()") %>%
        arrange_("datetime") %>%
        mutate_(previous_page = "lag(page)") %>%
        ungroup()

    edges <- views %>%
        filter_("!is.na(previous_page)", "previous_page != page") %>%
        group_by_("previous_page", "page") %>%
        summarise_(n = "n()")

    g <- graph_from_data_frame(edges, directed = TRUE, vertices = pages)
    edge_importance <- 5 * E(g)$n / max(E(g)$n)
    vertex_importance <- V(g)$n / max(V(g)$n)
    layout <- layout.auto(g)
    root_vertex <- V(g)$name == "/"
    origin <- layout[root_vertex, ]
    above_x_axis <- layout[, 2] > origin[2]
    vertex_diameter <- 30 * vertex_importance
    dist <- 0.7 * vertex_importance ^ (1 / 3)
    dist[dist < 0.3] <- 0.3
    dist[root_vertex] <- 0

    plot.igraph(
        g,
        vertex.size = vertex_diameter,
        edge.width = 1 * edge_importance,
        edge.arrow.size = 0.1 * edge_importance,
        edge.curved = TRUE,
        layout = layout,
        vertex.label.dist = dist,
        vertex.label.degree = ifelse(above_x_axis, -pi / 2, pi / 2)
        )
}
