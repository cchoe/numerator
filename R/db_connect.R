#' Connect to Vertica
#'
#' This function creates a Vertica JDBC connection object for querying InfoScout tables directly from R. 
#'
#' @param driver.path Local path to the Vertica JDBC driver (.jar)
#' @param user Database username
#' @param pw Database password
#' @return A Vertica connection object
#' @examples driver.path = '/users/me/documents/drivers/vertica-jdbc-7.2.3-0.jar'
#' @examples vertica <- vertica_connect(driver.path)
#' @examples df <- dbGetQuery(vertica, 'SELECT * FROM us_tlog.d_user LIMIT 10;')
#' @export
vertica_connect <- function(driver.path, user, pw) {
    # Creates a connection object to use when querying vertica
    #
    # Returns:
    #   A vertica connection object
    
    options( java.parameters = "-Xmx4g" )
    vDriver <- RJDBC::JDBC(driverClass="com.vertica.jdbc.Driver", classPath=driver.path)
    conn = RJDBC::dbConnect(vDriver, "jdbc:vertica://stg-dw-elb-vdbc01.infoscoutinc.net:5433/InfoScout", user, pw)
    return(conn)
}

#' Connect to Snowflake
#'
#' This function creates a Snowflake JDBC connection object for querying InfoScout tables directly from R.
#' @param driver.path Local path to the Snowflake JDBC driver (.jar)
#' @param user Database username
#' @param pw Database password
#' @return A Snowflake connection object
#' @examples driver.path = '/users/me/documents/drivers/snowflake-jdbc-3.6.6.jar'
#' @examples snowflake <- snowflake_connect(driver.path)
#' @examples df <- dbGetQuery(snowflake, 'SELECT * FROM us_tlog.d_user LIMIT 10;')
#' @export
snowflake_connect <- function(driver.path, user, pw) {
    # Creates a connection object to use when querying vertica
    #
    # Returns:
    #   A snowflake connection object
    
    options( java.parameters = "-Xmx4g" )
    vDriver <- RJDBC::JDBC(driverClass="net.snowflake.client.jdbc.SnowflakeDriver", classPath=driver.path)
    conn = RJDBC::dbConnect(vDriver, "jdbc:snowflake://infoscout.snowflakecomputing.com/?warehouse=ISC_DW_XS&db=infoscout",
                     user, pw)
    return(conn)
}