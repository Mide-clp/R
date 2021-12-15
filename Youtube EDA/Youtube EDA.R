##Loading librarys######
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(ggthemes)
library(corrplot)


##Importing datasets
Canada <- read_csv("~/Downloads/archive (2)/CAvideos.csv")
Germany <- read_csv("~/Downloads/archive (2)/DEvideos.csv")
France <- read_csv("~/Downloads/archive (2)/FRvideos.csv")
UK <- read_csv("~/Downloads/archive (2)/GBvideos.csv")
India <- read_csv("~/Downloads/archive (2)/INvideos.csv")
Japan <- read_csv("~/Downloads/archive (2)/JPvideos.csv")
South_Korea <- read_csv("~/Downloads/archive (2)/KRvideos.csv")
Mexico <- read_csv("~/Downloads/archive (2)/MXvideos.csv")
Russia <- read_csv("~/Downloads/archive (2)/RUvideos.csv")
US <- read_csv("~/Downloads/archive (2)/USvideos.csv")

### Combining data set into a single table######

YT_videos <- rbind(Canada,Germany,France,UK,India,Japan,South_Korea,Mexico,Russia,US) %>% 
  tibble()
YT_videos


## Analyzing duplicates

which(duplicated(YT_videos))
unique(YT_videos)

#removing duplicates, we combined all the countries video together, there are  chances of duplicates in countries

YT_videos <- unique(YT_videos) 

## formatting date

YT_videos$trending_date <- ydm(YT_videos$trending_date)
YT_videos$publish_time <- ymd(str_sub(YT_videos$publish_time,1,10))

### Changing category_id to category using their individual names instead of id

which(is.null(YT_videos)) # checking for null values
YT_videos <- within(YT_videos,{
  category_id[category_id == 1] <- "Film & Animation" 
  category_id[category_id == 2] <- "Autos & Vehicles" 
  category_id[category_id == 10] <- "Music" 
  category_id[category_id == 15] <- "Pets & Animals" 
  category_id[category_id == 17] <- "Sports"
  category_id[category_id == 19] <- "Travel & Events"
  category_id[category_id == 20] <- "Gaming"
  category_id[category_id == 22] <- "People & Blogs" 
  category_id[category_id == 23] <- "Comedy" 
  category_id[category_id == 24] <- "Entertainment"
  category_id[category_id == 25] <- "News & Politics"
  category_id[category_id == 26] <- "How-to & Style"
  category_id[category_id == 27] <- "Education"
  category_id[category_id == 28] <- "Science & Technology"
  category_id[category_id == 29] <- "Nonprofits & Activism"
  category_id[category_id == 30] <- "Movies"
  category_id[category_id == 34] <- "Comedy"
  category_id[category_id == 43] <- "Shows"
  category_id[category_id == 44] <- "Trailers"
})
colnames(YT_videos)[5] <- "category"

### saving the categorical data as a factor

YT_videos$category <- as.factor(YT_videos$category)

## information about the data set ######

glimpse(YT_videos)
summary(YT_videos)

### computing correlation for views, Likes, dislikes, and comments 

YT_videos_corr<- cor(YT_videos[, c(8:11) ])
YT_videos_corr
corrplot(corr = YT_videos_corr, method = "number", type = "upper") 

## analyzing category

YT_category <- YT_videos %>% 
  select(category, views, likes,  dislikes, comment_count) %>% 
  group_by(category) %>% 
summarise(Total_views = sum(views), Total_likes = sum(likes), 
          Total_dislikes = sum(dislikes), Total_comments = sum(comment_count),
          Like_ratio = round(Total_likes/Total_dislikes, 1)) %>% 
  arrange(-Total_views) %>% 
print()
?round
### The most viewed category

ggplot(data = YT_category)+
  geom_col(mapping = aes(x = reorder(category, +Total_views),  y = Total_views,), fill = "white")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 20 ,color = "grey", family = "mono"),
         panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Category vs views",
        subtitle = "most watched category on youtube", caption = "mide")

### The most liked category

ggplot(data = YT_category)+
  geom_col(mapping = aes(x = reorder(category, +Total_likes),  y = Total_likes), fill = "white")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 20 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Category vs likes",
        subtitle = "most liked category on youtube", caption = "mide")

### The most disliked category

ggplot(data = YT_category)+
  geom_col(mapping = aes(x = reorder(category, +Total_dislikes), y = Total_dislikes), fill = "white")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 20 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Category vs dislikes",
        subtitle = "most disliked category on youtube", caption = "mide")
  

### category with the most comment

ggplot(data = YT_category)+
  geom_col(mapping = aes(x = reorder(category, +Total_comments),  y = Total_comments), fill = "white")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 20 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Category vs comments",
        subtitle = "category with the most comments on youtube", caption = "mide")



### ratio of likes to dislikes by category

ggplot(data = YT_category)+
  geom_col(mapping = aes(x = reorder(category, +Like_ratio),  y = Like_ratio), fill = "white")+
  geom_label(mapping = aes(x = reorder(category, +Like_ratio),  y = Like_ratio, label = Like_ratio))+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 20 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Category vs like ratio",
        subtitle = "category with the highest ratio of likes to dislikes on youtube", caption = "mide")


 ## breaking by top 10 overall for each measurable variable

### selecting variable for use
Yt_individual <- YT_videos[,c(1,3,4,5,6,8,9,10,11)]
Yt_individual

###dropping duplicates of individual videos

dupe_inv <- which(duplicated(Yt_individual[, c(2,5)]))
unique(Yt_individual$title & Yt_individual$publish_time)
dupe_inv
Yt_individual <- Yt_individual[-dupe_inv,]


summary(Yt_individual)
glimpse(Yt_individual)

### Most viewed videos

Yt_individual %>% 
  filter(views > 30000000) %>%
  arrange(-views) %>% 
  ggplot()+
  geom_col(mapping = aes(x = reorder(title, +views), y = views), fill = "white")+
  geom_label(mapping = aes(x = reorder(title, +views), y = views, label = views))+
  coord_flip()+
    theme(panel.background = element_rect(fill = "#244C6C"), 
          plot.background = element_rect(fill = "#244C6C"),
          panel.grid.minor.x = element_blank(), panel.border = element_blank(),
          axis.text = element_text(size = 10 ,color = "grey", family = "mono"),
          panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
          panel.grid.major.x = element_blank(), axis.title = element_blank(),
          plot.title = element_text(size = 25, face = "bold"), 
          plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
    labs( title = "Most viewed videos ",
          subtitle = "videos with the highest views on youtube", caption = "mide")
  
    

### most liked video  

Yt_individual %>% 
  filter(likes > 1400000) %>%
  arrange(-likes) %>% 
  ggplot()+
  geom_col(mapping = aes(x = reorder(title, +likes), y = likes),  fill = "white")+
  geom_label(mapping = aes(x = reorder(title, +likes), y = likes, label = likes))+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 10 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Most liked videos",
        subtitle = "videos with the highest likes on youtube", caption = "mide")

### most disliked video
  
Yt_individual %>% 
  filter(dislikes > 145000) %>%
  arrange(-dislikes) %>% 
  ggplot()+
  geom_col(mapping = aes(x = reorder(title, +dislikes), y = dislikes),  fill = "white")+
  geom_label(mapping = aes(x = reorder(title, +dislikes), y = dislikes, label = dislikes))+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 10 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Most disliked videos",
        subtitle = "videos with the highest dislikes on youtube", caption = "mide")


### Videos with the most comment

Yt_individual %>% 
  filter(comment_count > 320000) %>%
  arrange(-comment_count) %>% 
  ggplot()+
  geom_col(mapping = aes(x = reorder(title, +comment_count), y = comment_count),  fill = "white")+
  geom_label(mapping = aes(x = reorder(title, +comment_count), y = comment_count, label = comment_count))+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 10 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Most commented on videos",
        subtitle = "videos with the highest number of comments on youtube", caption = "mide")  


  ## breaking down by channel title

### most viewed channel
YT_videos %>% 
  group_by(channel_title) %>% 
  select(channel_title, views, likes,  dislikes, comment_count) %>% 
  summarize(views = sum(views) ) %>% 
  filter(views >= 5003726538) %>% 
  arrange(-views) %>% 
  ggplot()+
  geom_col(mapping = aes(x = reorder(channel_title, +views), y = views),  fill = "white")+
  geom_label(mapping = aes(x = reorder(channel_title, +views), y = views, label = views))+
  coord_flip()+
  theme(panel.background = element_rect(fill = "#244C6C"), 
        plot.background = element_rect(fill = "#244C6C"),
        panel.grid.minor.x = element_blank(), panel.border = element_blank(),
        axis.text = element_text(size = 15 ,color = "grey", family = "mono"),
        panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 25, face = "bold"), 
        plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
  labs( title = "Most viewed  channel",
        subtitle = "channel with the highest number of views on youtube", caption = "mide")  



### most liked channel

 YT_videos %>% 
  group_by(channel_title) %>% 
  select(channel_title, views, likes,  dislikes, comment_count) %>% 
  summarize(likes = sum(likes)) %>% 
   filter(likes >= 142121545) %>% 
  arrange(-likes) %>% 
  ggplot()+
    geom_col(mapping = aes(x = reorder(channel_title, +likes), y = likes),  fill = "white")+
    geom_label(mapping = aes(x = reorder(channel_title, +likes), y = likes, label = likes))+
    coord_flip()+
    theme(panel.background = element_rect(fill = "#244C6C"), 
          plot.background = element_rect(fill = "#244C6C"),
          panel.grid.minor.x = element_blank(), panel.border = element_blank(),
          axis.text = element_text(size = 15 ,color = "grey", family = "mono"),
          panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
          panel.grid.major.x = element_blank(), axis.title = element_blank(),
          plot.title = element_text(size = 25, face = "bold"), 
          plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
    labs( title = "Most liked channel",
          subtitle = "channel with the highest number of likes on youtube", caption = "mide")  
 
 
 ### most disliked channel
 YT_videos %>% 
   group_by(channel_title) %>% 
   select(channel_title, views, likes,  dislikes, comment_count) %>% 
   summarize(dislikes = sum(dislikes)) %>% 
   filter(dislikes >= 7263910) %>% 
   arrange(-dislikes) %>% 
   ggplot()+
   geom_col(mapping = aes(x = reorder(channel_title, +dislikes), y = dislikes),  fill = "white")+
   geom_label(mapping = aes(x = reorder(channel_title, +dislikes), y = dislikes, label = dislikes))+
   coord_flip()+
   theme(panel.background = element_rect(fill = "#244C6C"), 
         plot.background = element_rect(fill = "#244C6C"),
         panel.grid.minor.x = element_blank(), panel.border = element_blank(),
         axis.text = element_text(size = 15 ,color = "grey", family = "mono"),
         panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
         panel.grid.major.x = element_blank(), axis.title = element_blank(),
         plot.title = element_text(size = 25, face = "bold"), 
         plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
   labs( title = "Most disliked channel",
         subtitle = "channel with the highest number of dislikes on youtube", caption = "mide")  
 
 ### most commented on channel
 YT_videos %>% 
   group_by(channel_title) %>% 
   select(channel_title, views, likes,  dislikes, comment_count) %>% 
   summarize(comment = sum(comment_count)) %>% 
   filter(comment >= 12405612) %>% 
   arrange(-comment) %>% 
   ggplot()+
   geom_col(mapping = aes(x = reorder(channel_title, +comment), y = comment),  fill = "white")+
   geom_label(mapping = aes(x = reorder(channel_title, +comment), y = comment, label = comment))+
   coord_flip()+
   theme(panel.background = element_rect(fill = "#244C6C"), 
         plot.background = element_rect(fill = "#244C6C"),
         panel.grid.minor.x = element_blank(), panel.border = element_blank(),
         axis.text = element_text(size = 15 ,color = "grey", family = "mono"),
         panel.grid.major.y = element_line( color = "#F4F4F4", linetype = 3, size = 1),
         panel.grid.major.x = element_blank(), axis.title = element_blank(),
         plot.title = element_text(size = 25, face = "bold"), 
         plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 15))+
   labs( title = "Most commented on channel",
         subtitle = "channel with the highest number of comments on youtube", caption = "mide")  
 
#The end