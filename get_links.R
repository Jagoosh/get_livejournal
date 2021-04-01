#! /usr/bin/env Rscript

args <- 
  # "oldadmiral"
  commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop("Blog name must be supplied (input something like oldadmiral).n", call.=FALSE)
}

library(tidyverse)
library(rvest)

get_links <- 
  function(link, pattern) {
    read_html(link) %>% 
      html_elements("a") %>% 
      html_attr("href") %>% 
      str_subset(pattern)
  }

link_years <-
  read_html(str_c("https://", args[1], ".livejournal.com/calendar")) %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  str_subset(str_c("^https://", args[1], ".livejournal.com/[:digit:]{4}/$"))

link_month <- 
  link_years %>%
  map(get_links, pattern = str_c("^https://", args[1], ".livejournal.com/[:digit:]{4}/[:digit:]{2}/$")) %>%
  flatten_chr()

# http://www.livejournal.com/view/?type=month&format=light&user=oldadmiral&y=2017&m=03

link_articles <-
  str_c(
    "http://www.livejournal.com/view/?type=month&format=light&user=", 
    args[1], "&y=", 
    link_month %>% 
      unique() %>% 
      str_extract("[:digit:]{4}/[:digit:]{2}") %>% 
      str_replace("/", "&m=")
  ) %>%
  map(get_links, pattern = str_c("^https://", args[1], ".livejournal.com/[:digit:]+\\.html$")) %>%
  flatten_chr()

link_articles %>% 
  sort() %>% unique() %>% 
  write_lines(str_c(args[1], ".txt"))
