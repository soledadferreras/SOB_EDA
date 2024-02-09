library(plotly)
library(tidyverse)
library(forcats)
library(ggplot2)
library(visdat)
library(purrr)
library(RColorBrewer)
library(readr)


file_SOB<-"C:\\Users\\chole\\Documents\\documentos personales\\educacion\\R\\Web scraping\\SOB Yelp Review.csv"


df_SOB <- read.csv(file_SOB)


df_SOB <- df_SOB %>% 
  mutate(rating_f= as.factor(rating))
#New variables Year and Month         
df_SOB <- df_SOB %>% 
  mutate(Year=year(dates),
         Month= month(dates))

#New variable Weekday_f and Change order and names of weekdays variable
df_SOB <- df_SOB %>% 
  mutate(WeekDay_f= as.factor(WeekDay))

df_SOB$WeekDay_f <- factor(df_SOB$WeekDay_f, levels=c('lunes', 'martes', 'miércoles', 'jueves', 'viernes', 'sábado', 'domingo'))

# df_SOB <- df_SOB %>% 
#   factor(df_SOB$WeekDay_f, levels=c('lunes', 'martes', 'miércoles', 'jueves', 'viernes', 'sábado', 'domingo'))

levels(df_SOB$WeekDay_f)

df_SOB$WeekDay_f <- factor(df_SOB$WeekDay_f, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"),
                           labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

levels(df_SOB$WeekDay_f)

df_SOB %>% group_by(Year) %>% 
  summarize(mean= mean(rating),
                     median= median(rating))

df_SOB %>% 
  ggplot(aes(x= rating))+
  geom_histogram()

df_SOB %>%
  group_by(Year) %>% 
  count(rating) %>% 
  summarise(total_count = sum(n))

str(df_SOB)
df_SOB %>% 
  ggplot(aes(x= rating)) +
  geom_bar()

df_SOB %>% 
  group_by(Year) %>% 
  count(rating_f)
table_cts <-table(df_SOB$Year, df_SOB$rating) 
str(table_cts )
df_SOB %>% 
  #group_by(Year) %>% 
  count(rating)
#PLOT proportions
df_SOB %>% 
  group_by(Year) %>% 
  ggplot(aes(x= Year, fill = rating_f)) +
  geom_bar(position= "fill") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80,1.00)) +
  labs(y= "Rating proportion", fill= "Rating(in Stars)") 

# same as above BUT with PLOTLY
df_SOB %>% group_by(Year) %>% 
  count(rating) %>% 
  mutate(proportions = n/sum(n)) %>% 
  plot_ly(x= ~Year, y= ~round(proportions, digits=2), color = ~factor(rating), 
          hoverinfo = "text",
          text= ~paste("Prop: ", round(proportions, digits=2), "<br>",
                       "Year: ", Year),
          colors =  "YlOrRd") %>%
  add_bars() %>% 
  layout(title = "Rating Distribution Over Years",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Proportion"),
         barmode = "stack")
df_SOB %>% 
  select(State) %>% 
  table()

df_SOB %>% 
  group_by(Year) %>% 
  summarize(Mean = mean(rating)) %>% 
  ggplot(aes(x= Year, y= Mean, fill= Mean)) +
  geom_col()+
  scale_fill_gradient(low = "#fef461", high = "#a20000") +
  ylim(0, 5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y= "Rating (in Stars)") 

df_SOB %>% 
  group_by(Year) %>% 
  summarize(Mean = weighted.mean(rating))

#plot total review counts, showing ratings per years
df_SOB %>% group_by(Year) %>% 
  count(rating) %>% 
  ggplot(aes(x= Year, fill = factor(rating))) +
  geom_bar(aes(weight = n)) +
  scale_fill_brewer(palette = "YlOrRd") +
  #ylim(0, 200)+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y= "Total reviews", fill= "Rating (Stars)") 

#plot total review counts, as above BUT with PLOTLY
df_SOB %>% group_by(Year) %>% 
  count(rating) %>% 
  plot_ly(x= ~Year, y= ~n, color = ~factor(rating),
          hoverinfo = "text",
          text= ~paste("Counts: ", n, "<br>",
                       "Year: ", Year),
          colors =  "YlOrRd") %>%
            add_bars() %>% 
            layout(title = "Total Rating reviews Over Years",
                   barmode= "stack",
                   yaxis = list(title = "Counts"))
          
  #         hoverinfo = "text",
  #         text= ~paste("Prop: ", round(proportions, digits=2), "<br>",
  #                      "Year: ", Year),
  #         colors =  "YlOrRd") %>%
  # add_bars() %>% 
  # layout(title = "Rating Distribution Over Years",
  #        xaxis = list(title = "Year"),
  #        yaxis = list(title = "Proportion"),
  #        barmode = "stack")
#FACET plot total review counts, showing ratings per years and Months
df_SOB %>% group_by(Year, Month) %>% 
  filter(Year != 2024) %>% 
  count(rating) %>% 
  ggplot(aes(x= factor(Month), fill = factor(rating))) +
  geom_bar(aes(weight = n)) +
  scale_fill_brewer(palette = "YlOrRd") +
  facet_wrap(~Year) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y= "Total reviews", x= "Month", fill= "Rating (Stars)") +
  theme(strip.text = element_text(face = "bold", color = "black"),
        strip.background = element_rect(fill = "gray90", linetype = "solid",
                                        color = "gray", linewidth = 1)) 
#FACET plot rating distribution, showing ratings per years and Months

df_SOB %>% group_by(Year, Month) %>% 
  filter(Year != 2024) %>% 
  ggplot(aes(x= factor(Month), fill = factor(rating))) +
  geom_bar(position= "fill") +
  scale_fill_brewer(palette = "YlOrRd") +
  facet_wrap(~Year) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80,1.00)) +
  labs(y= "Rating proportion", x= "Month", fill= "Rating (Stars)") +
  theme(strip.text = element_text(face = "bold", color = "black"),
        strip.background = element_rect(fill = "gray90", linetype = "solid",
                                        color = "gray", linewidth = 1)) 

# df_SOB %>% 
#   group_by(Year) %>% 
#   ggplot(aes(x= Year, fill = rating_f)) +
#   geom_bar(position= "fill") +
#   scale_fill_brewer(palette = "YlOrRd") +
#   theme_minimal() +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank()) +
#   scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80,1.00)) +
#   labs(y= "Rating proportion", fill= "Rating(in Stars)") 
#plot total review counts, showing ratings per years

# df_SOB %>% group_by(Year) %>% 
# count(rating) %>% 
# ggplot(aes(x= Year, fill = factor(rating))) +
# geom_bar(aes(weight = n)) +
# scale_fill_brewer(palette = "YlOrRd") +
# #ylim(0, 200)+
# scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175)) +
# theme_minimal() +
# theme(panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank()) +
# labs(y= "Total reviews", fill= "Rating (Stars)") 
df_transpo <- df_SOB %>% 
  mutate(Month= as.factor(Month)) %>% 
  group_by(Year, Month) %>% 
  count(rating) %>% 
  summarise(total_n = sum(n)) 

df_transpo<- df_transpo %>% 
  pivot_wider(names_from = Month,
              values_from = total_n)
df_transpo %>% 
  filter(Year != 2024) %>% 
  ggplot(aes(x=Year, y= Month)) +
  geom_col()
  
df_SOB %>% 
  mutate(Month= as.factor(Month)) %>% 
  filter(Year != 2024) %>% 
  group_by(Year, Month) %>% 
  count(rating) %>% 
  summarise(total_n = sum(n)) %>% 
  ggplot(aes(x=Month, y= total_n, fill=factor(Year))) +
  geom_col(position= "dodge")

#Summerize by month

df_SOB %>%
  filter(Year != 2024) %>% 
  group_by(Month) %>% 
  count(rating_f) %>% 
  summarize(total_reviews= sum(n)) %>% 
  ggplot(aes(x = factor(Month), y = total_reviews, fill= factor(rating))) +
  geom_col() + #color = "#f48d07",
  scale_fill_brewer(palette = "YlOrRd") +        #lwd = 1, fill = "#e4f407") +
  ylim(0, 45) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y= "Reviews counts", x= "Month")

df_SOB %>% 
  group_by(Year) %>% 
  summarize(Mean = mean(rating)) %>% 
  plot_ly(x=~ Year, y=~ Mean, color= ~Mean,
          hovertext= ~paste("Year: ", Year, "<br>",
                       "Stars: ", round(Mean, digits= 2), 
                       hoverinfo= "text")) %>% 
 add_bars(colors= "YlOrRd") %>% 
  layout(yaxis= list(title="Rating in Stars"),
         legend = list(title= list(text="Rating Stars")))


  scale_fill_gradient(low = "#fbfea6", high = "#b10016") +
  ylim(0, 5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y= "Rating (in Stars)", fill= "Rating (Stars)") 
  
  df_SOB %>%
    group_by(Year) %>%
    summarize(Mean = mean(rating),
              SD= sd(rating)) %>%
    ggplot(aes(x= Year, y= Mean, fill= Mean)) +
    geom_col() +
    geom_errorbar( aes(x=Year, ymin=Mean-SD, ymax=Mean+SD),
                   width=0.1, colour="gray", alpha=0.9, size=0.5) +
    geom_text(aes(label = round(Mean, digits = 2),
                  vjust = -0.5,
                  nudge_x = -.15,
                  fontsize = 0.25)) +
    scale_fill_gradient(low = "#fbfea6", high = "#b10016") +
    ylim(0, 6) +
    scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5))+
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y= "Rating (in Stars)", fill= "Rating (Stars)")
    
 
  # frequency histograms by weekdays facet by year
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(Year, WeekDay_f) %>%
    count(rating_f) %>% 
    ggplot(aes(x=WeekDay_f, y=n, fill= rating_f)) +
    #geom_point()+
    geom_col(position= "dodge")+
    scale_fill_brewer(palette = "YlOrRd") +
    facet_wrap(~Year) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(y= "Review numbers", x= "Weekday", fill= "Rating") +
    theme(strip.text = element_text(face = "bold", color = "black"),
          strip.background = element_rect(linetype = "solid",
                                          color = "gray", linewidth = 1),
          panel.background = element_rect(fill = 'gray92', color = 'gray'))
  
  # proportion rating by weekdays facet by year
  
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(Year, WeekDay_f) %>%
    count(rating_f) %>% 
    ggplot(aes(x=WeekDay_f, fill= rating_f)) +
    geom_bar(position= "fill")+
    scale_fill_brewer(palette = "YlOrRd") +
    facet_wrap(~Year) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(y= "Rating distributions", x= "Weekday", fill= "Rating") +
    theme(strip.text = element_text(face = "bold", color = "black"),
          strip.background = element_rect(linetype = "solid",
                                          color = "gray", linewidth = 1),
          panel.background = element_rect(fill = 'gray92', color = 'gray'))
  
  #Weekdays frequencies
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(WeekDay_f) %>% 
    count(rating_f) %>% 
    plot_ly(x= ~WeekDay_f, y=~n, color =~rating_f,
            colors =  "YlOrRd") %>% 
    add_bars() %>% 
    layout(xaxis = list(title = "Weekdays"),
           yaxis = list(title = "Rating conts"),
           barmode = "dodge",
           plot_bgcolor= toRGB("gray92")) 
  
  #Summerize ggplot
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(WeekDay_f) %>% 
    count(rating_f) %>% 
    summarize(total_reviews= sum(n)) %>% 
    ggplot(aes(x = WeekDay_f, y = total_reviews)) +
    geom_col(color = "#f48d07",
             lwd = 1, fill = "#e4f407") +
    ylim(0, 60) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y= "Reviews counts", x= "Weekday")
  
  #summarize PLOTLY
    df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(WeekDay_f) %>% 
    count(rating_f) %>% 
    summarize(total_reviews= sum(n)) %>% 
    plot_ly(x=~WeekDay_f, y=~total_reviews) %>% 
    add_bars(marker = list(color = "#e4f407",
                           
                           line = list(color = '#f48d07',
                                       
                                       width = 1.5))) %>% 
    layout(xaxis = list(title = "Weekdays"),
           yaxis = list(title = "Rating counts"),
           plot_bgcolor= toRGB("gray92"))
  
  #Weekday proportions
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(WeekDay_f) %>% 
    count(rating_f) %>% 
    mutate(proportions = n/sum(n)) %>% 
    plot_ly(x= ~WeekDay_f, y=~round(proportions, digits=2), color =~rating_f,
            colors =  "YlOrRd") %>% 
    add_bars() %>% 
    layout(xaxis = list(title = "Weekdays"),
             yaxis = list(title = "Rating Proportion"),
             barmode = "dodge",
           plot_bgcolor= toRGB("gray92"))
  
  #month
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(Month, rating_f) %>% 
    count(rating_f) %>% 
    summarize(total_reviews= sum(n)) %>% 
    ggplot(aes(x = factor(Month), y = total_reviews, fill= rating_f)) +
    geom_col() +
    scale_fill_brewer(palette = "YlOrRd") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y= "Reviews counts", x= "Month", fill= "Rating\n(Stars)")
  #ggplot Month proportions colapse
  
  df_SOB %>% group_by(Month) %>% 
    filter(Year != 2024) %>% 
    ggplot(aes(x= factor(Month), fill = factor(rating))) +
    geom_bar(position= "fill") +
    scale_fill_brewer(palette = "YlOrRd") +
    scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 8)) +
    scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80,1.00)) +
    labs(y= "Rating distribution", x= "Month", fill= "Rating\n(Stars)") +
    theme(strip.text = element_text(face = "bold", color = "black"),
          strip.background = element_rect(fill = "gray90", linetype = "solid",
                                          color = "gray", linewidth = 1))
  
  ```{r}
  library(ggplot2)
  library(plotly)
  f3a <-  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(Month, rating_f) %>% 
    count(rating_f) %>% 
    summarize(total_reviews= sum(n)) %>% 
    ggplot(aes(x = factor(Month), y = total_reviews, fill= rating_f)) +
    geom_col() +
    scale_fill_brewer(palette = "YlOrRd") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y= "Reviews counts", x= "Month", fill= "Rating\n(Stars)")
  f3b <-  df_SOB %>% group_by(Month) %>% 
    filter(Year != 2024) %>% 
    ggplot(aes(x= factor(Month), fill = factor(rating))) +
    geom_bar(position= "fill") +
    scale_fill_brewer(palette = "YlOrRd") +
    scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 8)) +
    scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80,1.00)) +
    labs(y= "Rating distribution", x= "Month", fill= "Rating\n(Stars)") +
    theme(strip.text = element_text(face = "bold", color = "black"),
          strip.background = element_rect(fill = "gray90", linetype = "solid",
                                          color = "gray", linewidth = 1))
  f33a <- ggplotly(f3a)
  f33b <- ggplotly(f3b)
  subplot(f33a, f33b)
  #layout(showlegend=FALSE)
  
  ```
  
  #Month proportions
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(Month, Year) %>% 
    count(rating_f) %>% 
    mutate(proportions = n/sum(n)) %>% 
    plot_ly(x= ~factor(Month), y=~round(proportions, digits=2), color =~rating_f,
            colors =  "YlOrRd") %>% 
    add_bars() %>% 
    plotly::facet_col(~Year) %>% 
    layout(xaxis = list(title = "Month"),
           yaxis = list(title = "Rating Proportion"),
           barmode = "dodge",
           plot_bgcolor= toRGB("gray92"))
  pf <- df_SOB %>% group_by(Year, Month) %>% 
    filter(Year != 2024) %>% 
    ggplot(aes(x= factor(Month), fill = factor(rating))) +
    geom_bar(position= "fill") +
    scale_fill_brewer(palette = "YlOrRd") +
    facet_wrap(~Year) +
    scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 8)) +
    scale_y_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80,1.00)) +
    labs(y= "Rating distribution", x= "Month", fill= "Rating\n(Stars)") +
    theme(strip.text = element_text(face = "bold", color = "black"),
          strip.background = element_rect(fill = "gray90", linetype = "solid",
                                          color = "gray", linewidth = 1))
  pf
  
  ggplotly(pf)
  # df_SOB %>% 
  #   filter(Year != 2024) %>% 
  #   group_by(Year) %>% 
  #   count(rating) %>% 
  #   mutate(proportions = n/sum(n)) %>% 
  #   plot_ly(x= ~Year, y= ~round(proportions, digits=2), color = ~factor(rating), 
  #           hoverinfo = "text",
  #           text= ~paste("Prop: ", round(proportions, digits=2), "<br>",
  #                        "Year: ", Year),
  #           colors =  "YlOrRd") %>%
  #   add_bars() %>% 
  #   layout(xaxis = list(title = "Year"),
  #          yaxis = list(title = "Proportion"),
  #          barmode = "stack")               
  df_SOB %>%
    filter(Year != 2024) %>% 
    group_by(WeekDay_f) %>% 
    count(rating_f) %>% 
    plot_ly(x= ~WeekDay_f, y=~n, color =~rating_f,
            colors =  "YlOrRd") %>% 
    add_bars() %>% 
    layout(xaxis = list(title = "Weekdays"),
           yaxis = list(title = "Rating conts"),
           barmode = "dodge",
           plot_bgcolor= toRGB("gray92"))
  

      
 str(df_SOB)
 SOB1<- df_SOB %>% 
   mutate(Year_f= as.factor(Year),
          Month_f= as.factor(Month))
 
 str(SOB1)
 
 SOB1 <- SOB1 %>%
   mutate(rating_f = recode(rating_f, `1` = "low", `2` = "low"))
 
 SOB1 %>% 
  filter(Year_f != 2024) %>% 
   group_by(Year_f, Month_f) %>% 
   count(rating_f) %>% 
   mutate(proportions = n/sum(n)) %>% 
   filter(rating_f == "low") %>% 
   ungroup() %>% 
   slice_max(proportions, n=10)
 
 
 df_SOB %>% group_by(Year, Month) %>% 
   count(rating) %>% 
   mutate(proportions = n/sum(n)) %>% 
   plot_ly(x= ~Month, y= ~round(proportions, digits=2), color = ~factor(rating)) %>% 
   add_bars()
 
 
 ```{r, fig.margin = TRUE, fig.cap = "Figure 3. Reviews count per months.", fig.width=3.5, fig.height=3.5, cache=TRUE, message=FALSE, warning=FALSE, echo=FALSE}
 df_SOB %>%
   filter(Year != 2024) %>% 
   group_by(Month) %>% 
   count(rating_f) %>% 
   summarize(total_reviews= sum(n)) %>% 
   ggplot(aes(x = factor(Month), y = total_reviews, fill= factor(rating))) +
   geom_col() +
   
   ylim(0, 45) +
   theme_minimal() +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor = element_blank()) +
   labs(y= "Reviews counts", x= "Month")
 ```
 
 ```{r echo=FALSE}
 #New variable Weekday_f and Change order and names of weekdays variable
 df_SOB <- df_SOB %>% 
   mutate(WeekDay_f= as.factor(WeekDay))
 
 df_SOB$WeekDay_f <- factor(df_SOB$WeekDay_f, levels=c('lunes', 'martes', 'miércoles', 'jueves', 'viernes', 'sábado', 'domingo'))
 df_SOB$WeekDay_f <- factor(df_SOB$WeekDay_f, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"),
                            labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
 #Summerize ggplot
 figure4<-df_SOB %>%
   filter(Year != 2024) %>% 
   group_by(WeekDay_f) %>% 
   count(rating_f) %>% 
   summarize(total_reviews= sum(n)) %>% 
   ggplot(aes(x = WeekDay_f, y = total_reviews)) +
   geom_col(color = "#f48d07",
            lwd = 1, fill = "#e4f407") +
   ylim(0, 60) +
   theme_minimal() +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   labs(y= "Reviews counts", x= "Weekday")
 
 
 ########################
library(tidyverse)
library(vosonSML)
library(SentimentAnalysis)
library(SnowballC)
library(ggpmisc)
library(ggplot2)
 
 str(df_SOB)
#Transform comment variable into a readable text format
 df_SOB$comment <- enc2utf8(df_SOB$comment)
#Run sentiment analysis
SOBsentiment <- analyzeSentiment(df_SOB$comment) 
#join SOBsentiment with df_SOB into one dataset

full_df_SOB <- bind_cols(df_SOB, SOBsentiment)
#Histogram

full_df_SOB %>% 
  ggplot(aes(x=SentimentGI))+
  geom_histogram()+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
#Scatter plot for Sentiment by date
full_df_SOB %>% 
  ggplot(aes(x=Year, y=SentimentGI))+
  geom_point()

comment_sentiments %>% 
  ggplot(aes(x=Year, y=SentimentGI, color= sentiment_category ))+
  geom_point(alpha= 0.5, position = "jitter") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())




#density plot for Sentiment by funny
full_df_SOB %>% 
  #filter(funny != 0) %>% 
  ggplot(aes(x=funny, y=SentimentGI))+
  geom_hex(bins=20) +
  scale_fill_gradient(low="lightblue", high="red")+
  geom_smooth(method= lm, formula= y~poly(x, 2)) +
  stat_poly_eq(aes(label=paste(..eq.label.., ..adj.rr.label.., sep="~~~")),
               formula= y~poly(x, 2), parse=TRUE)
#density plot for Sentiment by useful
full_df_SOB %>% 
  #filter(funny != 0) %>% 
  ggplot(aes(x=useful, y=SentimentGI))+
  geom_hex(bins=20) +
  scale_fill_gradient(low="lightblue", high="red")+
  geom_smooth(method= lm, formula= y~poly(x, 2)) +
  stat_poly_eq(aes(label=paste(..eq.label.., ..adj.rr.label.., sep="~~~")),
               formula= y~poly(x, 2), parse=TRUE)
#density plot for Sentiment by cool
full_df_SOB %>% 
  #filter(funny != 0) %>% 
  ggplot(aes(x=cool, y=SentimentGI))+
  geom_hex(bins=20) +
  scale_fill_gradient(low="lightblue", high="red")+
  geom_smooth(method= lm, formula= y~poly(x, 2)) +
  stat_poly_eq(aes(label=paste(..eq.label.., ..adj.rr.label.., sep="~~~")),
               formula= y~poly(x, 2), parse=TRUE)
# Categorize sentiments into positive, neutral, and negative
comment_sentiments <- full_df_SOB %>%
  mutate(sentiment_category = case_when(
    SentimentGI > 0 ~ "Positive",
    SentimentGI == 0 ~ "Neutral",
    SentimentGI < 0 ~ "Negative"
  ))

comment_sentiments<-comment_sentiments %>% 
  mutate(sentiment_category= as.factor(sentiment_category))
str(comment_sentiments)


#Histogram

comment_sentiments %>% 
  group_by(sentiment_category) %>% 
  count(sentiment_category) %>% 
  ggplot(aes(x=sentiment_category, y= n))+
  geom_col()+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
comment_sentiments %>% 
  ggplot(aes(x=dates, y= SentimentGI, color= sentiment_category))+
  geom_point()
###############################
library(tidytext)
library(tidyverse)
library(dplyr)
library(tm)
library(slam)
library(NLP)

# Load the BING lexicon for sentiment analysis
bing_lexicon <- get_sentiments("bing")
df_SOB_c <- df_SOB %>%
  mutate(comment = str_to_lower(comment)) %>%
  unnest_tokens(word, comment)
# Check the column names in df_SOB_c and bing_lexicon
colnames(df_SOB_c)
colnames(bing_lexicon)
# Assuming the relevant columns are "word" in df_SOB_c and "term" in bing_lexicon
# Adjust the column names if needed

# Perform inner join
bing_scores <- df_SOB_c %>%
  inner_join(bing_lexicon, by = "word") %>%
  group_by(word) %>%
  summarize(sentiment_score = sum(sentiment == "positive") - sum(sentiment == "negative")) %>%
  ungroup()
# Categorize sentiment
df_SOB_c <- df_SOB_c %>%
  left_join(bing_scores, by = "word") %>%
  mutate(sentiment_category = case_when(
    sentiment_score > 0 ~ "positive",
    sentiment_score == 0 ~ "neutral",
    sentiment_score < 0 ~ "negative"
  ))
# Identify frequent keywords in positive and negative reviews
positive_keywords <- df_SOB_c %>%
  filter(sentiment_category == "positive") %>%
  count(word, sort = TRUE)
negative_keywords <- df_SOB_c %>%
  filter(sentiment_category == "negative") %>%
  count(word, sort = TRUE)
slice_head(positive_keywords, n = 20)
#Facet top 10 by sentiment
bing_top_10<- df_SOB_c %>%
  filter(word != "shake", word != "fried", word !="butcher", word != "sob") %>% 
  drop_na(sentiment_category) %>% 
  mutate(sentiment_category = as.factor(sentiment_category)) %>% 
  group_by(sentiment_category) %>% 
  count(word, sort = TRUE) %>% 
  slice_max(order_by = n, n = 15) %>% 
  ungroup() %>% 
  mutate(word= reorder(word, n))
bing_top_10 %>% 
  ggplot(aes(word, n, fill= sentiment_category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment_category, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL)+
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# Sentiment analysis with tiidytext package using "loughran" lexicon
loughran_word_counts <- df_SOB %>%
  mutate(comment = str_to_lower(comment)) %>%
  unnest_tokens(output = word, input= comment) %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(word, sentiment, sort= TRUE)
#select top 10 words by snetiment
loughran_top_10_words_by_sentiment <- loughran_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n ))
loughran_top_10_words_by_sentiment
#bar plot showing contribution of words to sentiment
loughran_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
###################
library(paletteer)
df_SOB %>% 
  ggplot(aes(x = rating_f, y= funny)) +
  geom_point()
df_SOB %>% 
  ggplot(aes(x = rating_f, y= useful)) +
  geom_point()
df_SOB %>% 
  ggplot(aes(x = rating_f, y= cool)) +
  geom_hex(bins=20) +
  scale_fill_gradient(low="lightblue", high="red")

long_SOB<- df_SOB %>% 
  pivot_longer(cols = c("useful", "funny", "cool"), names_to = "interaction", 
               values_to = "counts")
long_SOB<-long_SOB %>% 
  mutate(interaction= as.factor(interaction))
long_SOB %>% 
  ggplot(aes(x = rating_f, y= counts, color= interaction)) +
  geom_point(position="jitter") +
  scale_color_manual(values = c("useful" = "#BF3626FF", "funny" = "#F9C53BFF", "cool" = "#AEAC4CFF")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "rating (stars)", y = "frequency")

long_SOB %>% 
  filter(counts != 0) %>% 
  count(interaction)

df_SOB %>% 
 filter(rating_f == 1) %>%  
  summarize(total= sum(useful))

#LOCATION analysis
library(tidyverse)
df_SOB %>% 
  group_by(State) %>% 
  count(comment) %>% 
  ggplot(aes(x=State, y= n)) +
  geom_col()

# df_SOB %>%
#   group_by(State) %>%
#   count(comment) %>%
#   ggplot(aes(x = State, y = forcats::fct_infreq(n))) +
#   geom_col() +
#   xlab("State") +
#   ylab("Count") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_SOB %>%
  ggplot(aes(y =  forcats::fct_rev(forcats::fct_infreq(State)))) +
  geom_bar() +
  xlab("State") +
  ylab("Count") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
library(forcats)
df_SOB %>% 
  group_by(State) %>% 
  count(comment) %>%
  plot_ly(x=~n, y=~forcats::fct_rev(forcats::fct_infreq(State))) %>% 
  add_bars() %>% 
  layout(yaxis = list(title = "State"),
         xaxis = list(title= "counts"))

##############


f12<-df_SOB %>% 
  ggplot(aes(x=State, y= rating, color= rating_f)) +
  geom_point(position = "jitter") +
    scale_color_brewer(palette = "YlOrRd") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
panel.background = element_rect(fill = 'gray80'))
ggplotly(f12)
###########################

comment_sentiments %>% 
  ggplot(aes(x = State, y= SentimentGI, color= sentiment_category)) +
  geom_point(position = "jitter") +
  scale_color_manual(values = c("Positive" = "#165D43FF", "Neutral" = "#E9851DFF", "Negative" = "#BF3626FF"))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

comment_sentiments %>% 
  ggplot(aes(x = State, y= SentimentGI, color= sentiment_category)) +
  geom_point(position = "jitter") +
  scale_color_manual(values = c("Positive" = "#165D43FF", "Neutral" = "#E9851DFF", "Negative" = "#BF3626FF"))+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())
