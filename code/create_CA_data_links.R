library(tidyverse)
links <- data.frame()
head <- NULL
query_base_file <- "data/dashboard/confirmed-cases/california.json"
page = 1
stop = FALSE
loaded_latest = FALSE
while (TRUE) {
  request <- httr::GET(
    'https://api.github.com/repos/cagov/covid-static-data/commits',
    query = list(path = query_base_file, 
                 sort = "author-date",
                 page = as.character(page)))
  
  if (request$status_code > 400 || stop) {
    break
  }
  content <- httr::content(request)
  
  if(length(content) == 0){
    break
  }
  
  for (i in 1:length(content)){
    curr_date = as.Date(sub("\\T.*", "", content[[i]]$commit$author$date))
    if (!(curr_date %in% links$date)){
      a_commit <- data.frame(date = content[[i]]$commit$author$date,
                             sha = content[[i]]$sha) %>%
        tidyr::separate(date, into = c("date", "time"), sep = "T") %>%
        dplyr::mutate(time =  sub("\\Z.*", "",time),
                      date = as.Date(date),
                      file_link = paste0(
                        "https://raw.githubusercontent.com/cagov/covid-static-data/",
                        sha,"/",query_base_file)) %>%
        dplyr::select(-sha, -time)
      
      
      links <- links %>% dplyr::bind_rows(a_commit)
      if (curr_date == max(links$date)){
        loaded_latest = TRUE
      }
    } else {
      # keep scraping until curr_date is the latest date in previous result
      if (!is.null(head)){
        if (curr_date == head) {
          sha <- content[[i]]$sha
          links[links$date == curr_date,]$file_link <- paste0(
            "https://raw.githubusercontent.com/cagov/covid-static-data/",
            sha,"/",query_base_file)
          
          stop = TRUE
          break
        }
      }
      
      # replace link for the most recent day with the newly updated link
      if (curr_date == max(links$date) && (!loaded_latest)){
        sha <- content[[i]]$sha
        links[links$date == curr_date,]$file_link <- paste0(
          "https://raw.githubusercontent.com/cagov/covid-static-data/",
          sha,"/",query_base_file)
        
        loaded_latest = TRUE
        next 
      } 
    }
  }
  
  page <- page + 1
}  

links <- write_csv(links, file = ("csv-data/CA-json-links.csv"))
