#' @import ggplot2
#' @importFrom tidyr separate_ gather_
#' @importFrom lubridate dminutes ddays
#' @importFrom igraph graph_from_data_frame E V layout.auto plot.igraph
NULL

#' Graph number of visitors over time.
#'
#' @param days Table of days.
#' @export
#'
graph_visitors_vs_date <- function(days) {
    lines <- days %>%
        mutate_(a = ~ day_of_first_visit - ddays(0.5),
                b = ~ day_of_first_visit + ddays(0.5)) %>%
        select_("day_of_first_visit", "new_visitors", "a", "b") %>%
        gather_("col", "x", c("a", "b"))

    g <- (
        ggplot() +
        geom_rect(aes_string(xmin = "day_of_first_visit - ddays(0.5)", xmax = "day_of_first_visit + ddays(0.5)", ymin = "0", ymax = "new_visitors"), data = days, fill = "black", alpha = 0.25) +
        geom_line(aes_string(x = "x", y = "new_visitors", group = "day_of_first_visit"), data = lines) +
        ggtitle("Daily Traffic") +
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
        separate_(
            "config_resolution", c("width", "height"), sep = "x",
            convert = TRUE, fill = "right"
        ) %>%
        group_by_("width", "height") %>%
        filter_("!is.na(width)", "!is.na(height)") %>%
        summarise_(n = "n()") %>%
        ungroup() %>%
        mutate_(proportion = "n / sum(n)")

    segments <- function(row) {
        data.frame(
            x =    c(0,         0,          row$width,  0         ),
            y =    c(0,         0,          0,          row$height),
            xend = c(row$width, 0,          row$width,  row$width ),
            yend = c(0,         row$height, row$height, row$height),
            proportion = row$proportion,
            row = row$row
        )
    }
    lines <- resolutions %>%
        mutate(row = 1:nrow(resolutions)) %>%
        group_by_("row") %>%
        do_(~ segments(.))

    (
        ggplot(
            lines,
            aes_string(x = "x", y = "y", xend = "xend", yend = "yend",
                       alpha = "proportion")
        ) +
        geom_segment(color = "black") +
        scale_alpha_continuous(range = c(0, 1)) +
        theme_bw() +
        coord_fixed(ratio = 1) +
        ggtitle("Browser Dimensions") +
        ylab("Height") +
        xlab("Width") +
        guides(alpha = guide_legend(title = "Proportion\nof Visits")) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
    )
}

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
        mutate_(keep = ~ (page %in% pages$page) & (previous_page %in% pages$page)) %>%
        filter(keep)

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

    ## TODO: Figure out how igraph places labels.
    ## https://github.com/igraph/rigraph/blob/dev/R/plot.R#L676-L677
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
