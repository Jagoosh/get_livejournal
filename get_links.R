#! /usr/bin/env Rscript

args <- 
  # "oldadmiral"
  commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop("Blog name must be supplied (input something like oldadmiral)", call.=FALSE)
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

href <- 
  read_html(str_c("https://", args[1], ".livejournal.com/calendar")) %>% 
  html_elements("a") %>%
  html_attr("href")

link_years <- 
  href %>% 
  str_subset(str_c("^https://", args[1], ".livejournal.com/[:digit:]{4}/$"))

link_month <- 
  c(
    href %>% 
      str_subset(str_c("^https://", args[1], ".livejournal.com/[:digit:]{4}/[:digit:]{2}/$")), 
    link_years %>%
      map(get_links, pattern = str_c("^https://", args[1], ".livejournal.com/[:digit:]{4}/[:digit:]{2}/$")) %>%
      flatten_chr()
  )

link_articles <- 
  link_month %>% 
  map(get_links, pattern = str_c("^https://", args[1], ".livejournal.com/[:digit:]+\\.html$")) %>%
  flatten_chr()

link_articles <- 
  tibble(
  link = link_articles,
  itemid = str_replace(link, ".*/([:digit:]*)\\.html", "\\1") %>% as.integer()
) %>% 
  arrange(itemid)

link_articles$link %>%
  write_lines(str_c(args[1], ".txt"))
