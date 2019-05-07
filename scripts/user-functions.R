library(DBI)
library(odbc)
library(knitr)
library(reticulate)
library(formattable)
library(kableExtra)
library(modelr)
library(tidytext)
library(lubridate)
library(tidyverse)
library(keyring)

# include user functions
# source(file="../scripts/user-functions.R")
use_python("C:/Users/jcasey10/AppData/Local/Continuum/anaconda3/python.exe")
use_virtualenv("~/bin/base")
use_condaenv("base")

get_uid <- function() {
  rstudioapi::showPrompt(
    title = "NULOOK Username",
    message = "Username",
    default = "")
}

get_pwd <- function() {
  rstudioapi::askForPassword("Database password")
}

fix_date <- function(text) {
  answer <- ifelse(str_length(text) == 8,
                   str_c(
                     str_sub(text, 1, 4), '-',
                     str_sub(text, 5, 6), '-',
                     str_sub(text, 7, 8)
                   ),
                   NA
  )
  answer <- date(answer)
}


fix_amis_table <- function(df) {
  df.names <- colnames(df)
  
  # num.df <- df %>%
  #   select_if(is.numeric)
  
  txt.df <- df %>%
    select_if(is.character) %>%
    map(str_trim) %>%
    as_tibble
  
  txt.names <- colnames(txt.df)
  
  other.df <- df %>%
    select(-txt.names)
  
  dates.df <- txt.df %>%
    select(ends_with('_date'), ends_with("_dt")) %>%
    map(ymd) %>%
    as_tibble
  
  df <- txt.df %>%
    select(-ends_with('_date'), ends_with("_dt")) %>%
    bind_cols(., other.df, dates.df) %>%
    select(df.names)
  
  return(df)
  
}

query_amis_data_by_param <- function(query_string, query_parameters)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          'amis', 
                          UID = keyring::key_get("nu_warehouse_id"),
                          PWD = keyring::key_get("nu_warehouse_secret"))
  
  response <- dbSendQuery(connection, query_string)
  dbBind(response, query_parameters)
  
  tbl <- dbFetch(response) %>%
    rename_all(tolower) %>%
    as_tibble() %>%
    fix_amis_table()
  
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  return(tbl)
}

query_amis_data <- function(query_string)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          'amis', 
                          UID = keyring::key_get("nu_warehouse_id"),
                          PWD = keyring::key_get("nu_warehouse_secret"))
  
  response <- dbSendQuery(connection, query_string)
  tbl <- dbFetch(response)
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  names(tbl) <- tolower(names(tbl))
  
  return(as_tibble(tbl))
}

query_db <- function(db_name, query_string, myterm)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          db_name)
  
  response <- dbSendQuery(connection, query_string)
  dbBind(response, myterm)
  
  tbl <- dbFetch(response) %>%
    rename_all(tolower) %>%
    as_tibble() %>%
    fix_amis_table()
  
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  rm(connection)
  
  return(as_tibble(tbl))
}

query_table <- function(db_name, query_string)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          db_name)
  
  response <- dbSendQuery(connection, query_string)
  
  tbl <- dbFetch(response) %>%
    rename_all(tolower) %>%
    as_tibble() %>%
    fix_amis_table()
  
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  rm(connection)
  
  return(as_tibble(tbl))
}

read_db <- function(db_name, query_string)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          db_name)
  
  response <- dbSendQuery(connection, query_string)
  tbl <- dbFetch(response) %>%
    rename_all(tolower) %>%
    as_tibble() %>%
    fix_amis_table()
  
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  rm(connection)
  
  return(as_tibble(tbl))
}

read_table <- function(db_name, tab)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(), db_name)
  
  tbl <- dbReadTable(connection, tab)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  return(as_tibble(tbl))
}

read_amis_table <- function(tab)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          'amis', 
                          UID = keyring::key_get("nu_warehouse_id"),
                          PWD = keyring::key_get("nu_warehouse_secret"))
  
  tbl <- dbReadTable(connection, tab)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  return(as_tibble(tbl))
}

write_db <- function(frame, db, tab) {
  con <- dbConnect(odbc::odbc(), db)
  
  answer <- system.time({ dbWriteTable(con, tab, frame, overwrite=TRUE) })
  
  dbDisconnect(con)
  
  return(answer)
}

term_sequence <- function(begin, end) {
  for (ay in begin:end) {
    for (sem in 1:3) {
      str_c(ay, sem)
    }
  }
}