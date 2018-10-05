user <- Sys.getenv('VERTICA_USER')
pw <- Sys.getenv('VERTICA_PW')
driver.path <- Sys.getenv('VERTICA_DRIVER_PATH')
con <- infoscout::vertica_connect(driver.path, user, pw)
