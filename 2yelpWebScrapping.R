library(tidyverse)
library(rvest)
library(rlist)
library(data.table)
library(lubridate)

# used for accumulating all reviews in one place
df_final <- list()

#Create a url object

url2 <- "https://www.yelp.com/biz/son-of-a-butcher-dallas-3?sort_by=date_asc"

page2<- read_html(url2)

# find the number of pages
pageNum <- page2 %>% 
  html_elements(xpath= '//div[@aria-label= "Pagination navigation"]') %>% 
  html_text() %>% 
  str_extract("of \\d+") %>% 
  str_remove("of ") %>% 
  as.numeric()

# create a sequence based on the number of pages
# to be used in the URL for moving from one page to the other
pageSequence <- seq(from= 0, to =(pageNum * 10)-10, by=10)

# function to be used for extracting the extra information about a review
extra_info_extract <- function(ei, butt) {
  str_extract(ei, paste0(butt, ".*")) %>% 
    .[!is.na(.)] %>% 
    str_extract("\\d+") %>% 
    str_replace_na("0") %>% 
    as.numeric()
}

# Initialize an empty data frame
df_final <- data.frame(username = character(),
                       dates = character(),
                       location = character(),
                       rating = numeric(),
                       comment = character(),
                       useful = numeric(),
                       funny = numeric(),
                       cool = numeric(),
                       stringsAsFactors = FALSE)

# beginning of for loop
for (i in pageSequence){
  
  # create a formatted url object to reference correct page(sprintf va a 
  #reemplazaren la url el %d por el valor de i, que seria el cambio de pagina de review)
  url <- sprintf("https://www.yelp.com/biz/sob-dallas?start=%d&sort_by=date_asc", i)
  
  # read the url as an html object
  page2 <- read_html(url)
  
  # collect all usernames from reviews
  usernames <- page2 %>% 
    html_elements(xpath= '//div[starts-with(@class, "user-passport")]//a[starts-with(@href, "/user_details")]') %>% 
    html_text()
  
  # collect all locations of reviews
  locations <- page2 %>% 
    html_elements(xpath= '//div[starts-with(@class, "user-passport")]//span[@class = " css-qgunke"]') %>% 
    html_text() %>% 
    .[.!="Location"]
  
  # collect the review text
  comments <- page2 %>% 
    html_elements(xpath = '//div[@class = " css-1qn0b6x"]//p[@class= "comment__09f24__D0cxf css-qgunke"]') %>% 
    html_text()
  
  # collect the review ratings
  ratings <- page2 %>% 
    html_elements(xpath = '//*[@id="reviews"]/section/div[2]/ul/li/div/div[2]//div/div[1]/span/div') %>% 
  html_attr('aria-label') %>% 
  str_remove_all(" star rating") %>% 
  as.numeric()
  
  
  # collect the review dates
  the_dates <- page2 %>% 
    html_elements(xpath ='//*[@id="reviews"]/section/div[2]/ul/li/div/div[2]//div/div[2]/span[1]') %>% 
    html_text()
    
  
  # collect the extra information about the reviews (Useful, funny, cool)
  extra_info <- page2 %>% 
    html_elements(xpath= '//div[@class = "arrange__09f24__LDfbs vertical-align-middle__09f24__zU9sE css-1qn0b6x"]//div[@class= "arrange-unit__09f24__rqHTg arrange-unit-fill__09f24__CUubG css-1qn0b6x"]') %>%
    html_elements(xpath='.//button[@type= "submit"]') %>% 
    html_text() %>% 
    .[.!=""] %>% 
    .[.!="Read more"]
  
  # assign the extra information accordingly
  useful <- extra_info_extract(extra_info, "Useful")
  funny <- extra_info_extract(extra_info, "Funny")
  cool <- extra_info_extract(extra_info, "Cool")
  
  # combine the objects into a list
  df_new <- list(username = usernames, 
                 dates = the_dates, 
                 location = locations,
                 rating = ratings,
                 comment = comments,
                 useful = useful,
                 funny = funny,
                 cool = cool)
  
  # convert the list into a data frame
  df_new_table <- as.data.frame(df_new)
  # Append the data frame to df_final
  df_final <- bind_rows(df_final, df_new_table)
  # append the data frame to the df_final object
  #df_final <- rbindlist(list(df_final, df_new_table))
  
  # random sleep time set between pages to prevent the IP address from being banned
  Sys.sleep(sample(c(15,25), 1))
}

# Convert dates to Date class

df_final_Date <- df_final %>%
  mutate(dates = mdy(dates) - days(1))

df_final_Date <- df_final_Date %>% 
  mutate(WeekDay = weekdays(dates))

df_final_Date <- df_final_Date %>%
  mutate(State= str_extract(location, "\\b[A-Z]{2}\\b"))

# write the dataset to a csv file
write_csv(df_final_Date, "SOB Yelp Review.csv", na = "")


str(df_final_Date)


#opcion 2 para seleccionar comments:
# comments<- page2 %>% 
#   html_elements(xpath = '//div[@class = " css-1qn0b6x"]') %>% 
#   html_elements(xpath = '(.//p[starts-with(@class, "comment__")])[1]') %>% 
#   html_text()


