## [This page](https://developer.piwik.org/guides/persistence-and-the-mysql-backend) was useful in understanding the data stored by Piwik in MySQL.

library(dplyr)
library(lubridate)

describe_database <- function(db) {
    table_names <- dbGetQuery(db$con, 'show tables')[, 1]
    tables <- lapply(table_names, function(x) tbl(db, x) %>% as.data.frame())

    for (i in 1:length(table_names)) {
        x <- table_names[i]
        data <- tables[[i]]

        if (nrow(data) > 0) {
            print(x)
            str(data)
            cat('\n')
        }
    }
}

.get <- function(...) suppressWarnings(tbl(...) %>% as.data.frame())

.remove_empty_columns <- function(x) {
    for (k in names(x)) {
        if (all(is.na(x[, k]))) {
            x[[k]] <- NULL
        }
    }
    return(x)
}

get_actions <- function(db) {
    actions <- .get(db, 'piwik_log_link_visit_action')

    actions <- .remove_empty_columns(actions)

    ## Set up metadata on action types
    action_types <- .get(db, 'piwik_log_action')
    path <- system.file("extdata", "action_types.csv", package="piwikr")
    metadata <- read.csv(path)
    action_types <- action_types %>% left_join(metadata, by=c('type'='id'))
    action_types <- action_types %>% select(idaction, name, category_name)

    ## Merge action types back into actions
    actions <- actions %>% left_join(action_types, by=c('idaction_url'='idaction'))
    actions$idaction_name <- NULL
    actions$idaction_url <- NULL

    ## custom_float: an unspecified float field, usually used to hold the time it took the server to serve this action
    actions <- actions %>% select(
        id=idlink_va,
        visitor_id=idvisitor,
        visit_id=idvisit,
        datetime=server_time,
        url=name,
        type=category_name,
        time_to_serve=custom_float,
        time_spent_on_previous_action=time_spent_ref_action
        )
    actions$datetime <- lubridate::ymd_hms(actions$datetime)
    return(actions)
}

get_visits <- function(db) {
    visits <- .get(db, 'piwik_log_visit')
    visits <- .remove_empty_columns(visits)

    visits$visit_first_action_time <- lubridate::ymd_hms(visits$visit_first_action_time)
    return(visits)
}
