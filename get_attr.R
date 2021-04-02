library(tidyverse)
library(rvest)
library(urltools)

get_article_attr <- 
  function(
    article_url = "https://galkovsky.livejournal.com/496.html"
  ) {
    
    html <- 
      read_html(article_url)
    
    article_attr <- 
      tibble(
        ljournal = html %>% 
          html_elements(".i-ljuser-username") %>% 
          html_attr("href") %>% 
          head(1),
        username = html %>% 
          html_elements(".i-ljuser-username") %>% 
          html_text() %>% 
          head(1),
        published = html %>% 
          html_elements(".b-singlepost-author-date.published.dt-published") %>% 
          html_text() %>% 
          head(1),
        url = html %>% 
          html_elements("li.b-linkbar-item") %>% 
          html_attr("lj-share") %>% 
          discard(is.na) %>% 
          head(1) %>% 
          str_replace(".*url: '([^']*)'.*", "\\1"),
        title = html %>% 
          html_elements("li.b-linkbar-item") %>% 
          html_attr("lj-share") %>% 
          discard(is.na) %>% 
          head(1) %>% 
          str_replace(".*title: '([^']*)'.*", "\\1") %>% 
          url_decode(),
        next_url = str_c(
          "https://www.livejournal.com/go.bml?journal=", username,
          "&itemid=", str_replace(url, "^.*/([:digit:]*)\\.html$", "\\1"),
          "&dir=next"
        ),
        prev_url = str_replace(next_url, "&dir=next", "&dir=prev")
      )
    return(article_attr)
  }

# https://www.livejournal.com/go.bml?journal=galkovsky&itemid=496&dir=next

attr <- get_article_attr("https://galkovsky.livejournal.com/496.html")
# attr <- get_article_attr("https://galkovsky.livejournal.com/689.html")

for(i in 1:3000) {
  attr <- 
    bind_rows(attr, get_article_attr(tail(attr$next_url, 1)))
}

attr

tail(attr)

write_csv(attr, "galkovsky.csv")

