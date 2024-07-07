source('functions.R')

url <- 'https://editorial.rottentomatoes.com/guide/best-movies-of-all-time/'
sess <- read_html_live(url)

seed_urls <- sess %>% html_nodes('.movie a') %>% html_attr('href')
counter <- 0

while ((counter <= 15000) && (length(seed_urls) > 0)) {
  curr_url <- seed_urls[1]
  message(curr_url)
  
  new_urls <- tryCatch({
    scrape_movie_from_url(curr_url)
  }, error = function(err) {
    message('ERROR')
    seed_urls <- seed_urls[-c(1)]
    curr_url <- seed_urls[1]
    scrape_movie_from_url(curr_url)
  })
  
  seed_urls <- c(seed_urls,new_urls)
  seed_urls <- seed_urls[-c(1)]
  seed_urls <- seed_urls[grepl('m/',seed_urls)]
  filtered_urls <- seed_urls[which(!file.exists(paste('meta_data/',name_from_url(seed_urls),sep='')))]
  message(length(filtered_urls))
  if (counter > 11200) {
    seed_urls <- filtered_urls
  }
  seed_urls <- unique(seed_urls)
  
  counter <- counter + 1
  message(counter)
}









