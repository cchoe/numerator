#' Pull a B&M static
#'
#' This function returns a list of users for any given 12-month B&M static
#'
#' @param conn A database connection object
#' @param static.group The static of interest. Valid values: brick_mortar (see examples)
#' @param static.end.date A SQL-formatted month-end date (see examples)
#' @return A dataframe containing users and other details
#' @examples vertica <- vertica_connect(driver.path)
#' @examples # single static
#' @examples df <- get_bm_static(vertica, static_end_date="'2017-12-31'")
#' @examples # for multiple statics
#' @examples df <- get_bm_static(vertica, static_end_date=c("'2017-12-31', '2016-12-31'")
#' @export
get_static <- function(conn, static.group = 'brick_mortar', static.end.date="'2017-12-31'"){
    sql <- paste("SELECT * FROM df.panelist_selection WHERE static_end_date IN(", static.end.date, ");", sep='')
    out <- RJDBC::dbGetQuery(conn, sql)
    return(out)
}

#' Create a Vertica Table
#'
#' This function creates a table on Vertica, while dropping any existing table if exists
#'
#' @param conn A database connection object
#' @param table.name A character string of the schema.table name
#' @param table.vars A chracter string with SQL table column definitions (see example)
#' @return A message that informs you of when the table is successfully created
#' @examples conn <- vertica_connect(driver.path)
#' @examples table.name <- 'dev.test_table'
#' @examples table.vars <- 'user_id INT, trips INT, gender VARCHAR'
#' @examples create_table(conn, table.name, table.vars)
#' @export
create_table <- function(conn, table.name, table.vars) {
    sql.drop <- paste0("DROP TABLE IF EXISTS ", table.name, ";")
    RJDBC::dbSendUpdate(conn, sql.drop)
    print(paste0("DROPPED TABLE ", table.name))
    sql.create <- paste0("CREATE TABLE ", table.name, "(", table.vars, ");")
    RJDBC::dbSendUpdate(conn, sql.create)
    return(paste0("CREATED TABLE ", table.name))
}


#' Upload local .csv to a Vertica table
#'
#' This function loads CSV-formatted data into a Vertica table, using COPY statements
#'
#' @param conn A database connection object
#' @param table.name A character string of the schema.table name
#' @param file.path A chracter string of the local file path (see example)
#' @param stack A chracter string of the database stack. Valid values: vstg, snowflake (see example)
#' @return A message that informs you of when the copy is completed
#' @examples conn <- vertica_connect(driver.path)
#' @examples table.name <- 'dev.test_table'
#' @examples file.path <- '/Users/johndoe/Desktop/test_data.csv'
#' @examples copy_local_table(conn, stack='vstg', table.name, file.path)
#' @examples TODO: allow user to upload to snowflake
#' @export
copy_local_table <- function(conn, table.name, file.path, stack='vstg') {
    sql.copy <- paste("COPY ", table.name, " FROM local '", file.path,"' delimiter E',' SKIP 1 DIRECT ABORT ON ERROR;", sep='')
    dbSendUpdate(conn, sql.copy)
    return(paste0("COPIED DATA TO TABLE ", table.name))
}
