#' @import ggplot2
#' @importFrom tidyr separate
#' @importFrom lubridate dminutes
#' @importFrom igraph graph_from_data_frame E V layout.auto plot.igraph

globalVariables(c(
    "previous_page",
    "datetime",
    "visit_id",
    "proportion",
    "height",
    "width",
    "config_resolution",
    "page"
))

#' Graph number of visitors over time.
#'
#' @param days Table of days.
#' @export
graph_visitors_vs_date <- function(days) {
    g <- (
        ggplot(days, aes_string(x = "day_of_first_visit", y = "new_visitors")) +
        geom_point() +
        geom_line() +
        ggtitle("Site Traffic by Date") +
        ylab("New Visitors") +
        xlab("") +
        theme_classic()
        )
    return(g)
}

#' Graph distribution of browser resolutions.
#'
#' @param visits Table of visits.
#' @export
graph_browser_resolutions <- function(visits) {
    resolutions <- visits %>%
        separate(config_resolution, c("width", "height"), sep = "x", convert = TRUE, fill = "right") %>%
        group_by(width, height) %>%
        filter(!is.na(width), !is.na(height)) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(proportion = n / sum(n))

    (
        ggplot(resolutions, aes(xmin = 0, xmax = width, ymin = 0, ymax = height, alpha = proportion)) +
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
graph_site_structure <- function(actions) {
    views <- actions %>%
        filter(grepl("amarder.github.io", url)) %>%
        mutate(page = sub("amarder.github.io", "", url))

    pages <- views %>%
        group_by(page) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(page_id = row_number(n))

    views <- views %>%
        group_by(visit_id) %>%
        mutate(t = (datetime - min(datetime)) / dminutes(1), n = n()) %>%
        arrange(datetime) %>%
        mutate(previous_page = lag(page)) %>%
        ungroup()

    edges <- views %>%
        filter(!is.na(previous_page), previous_page != page) %>%
        group_by(previous_page, page) %>%
        summarise(n = n())

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
