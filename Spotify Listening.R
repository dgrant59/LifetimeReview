library(rjson)
library(dplyr)
library(spotifyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(scales)
library(circular)

options(scipen = 20,
        digits = 10)

#data comes in 16 JSON files, combine and form dataframe, 
######################################################################################
#for privacy reasons I omit these, as they contain my ip address
#
######################################################################################
listening_data_df<- NULL
for(i in 1:16){
  listening_data <- fromJSON(file=paste0("C:/Users/danie/Pictures/Spotify Data/MyData/endsong_",i,".json"))
  listening_data <- lapply(listening_data, lapply, function(x)ifelse(is.null(x), NA, x))
  listening_data <- bind_rows(listening_data)
  listening_data_df <- rbind(listening_data_df,listening_data)
}

rm(listening_data)

# library("spotifyr")
# library("stringr")
#GET SONG DATA
#Store sensitive data in 2 line text file to read in
########################################################################
# client_ID <- read.csv("ClientInfo.txt",header=F)[1,1]
# client_Pass <- read.csv("ClientInfo.txt",header=F)[2,1]
# 
# acc_token <- get_spotify_access_token(client_id = client_ID,
#                                       client_secret = client_Pass)
########################################################################

#unique songs we need track info for
#remove "spotify:track:"/"spotify:episode:" from each song/podcast URI to get song ID
unique_songs <- gsub("spotify:track:","",unique(listening_data_df[is.na(listening_data_df$episode_name),]$spotify_track_uri))
unique_podcasts <- gsub("spotify:episode:","",unique(listening_data_df[!is.na(listening_data_df$episode_name),]$spotify_episode_uri))


audio_features <- get_track_audio_features(ids = unique_songs[1:99], 
                                 authorization = acc_token)
for(i in 1:floor(length(unique_songs)/100)){
  #go through list of unique songs in batches of 100 (max per API call)
  print(c(i*100,min(i*100+99,length(unique_songs)))) 
  audio_features <- rbind(audio_features, 
                          get_track_audio_features(unique_songs[(i*100):(min(i*100+99,length(unique_songs)))], 
                                                   authorization = acc_token))
  }


song_metadata <- get_tracks(ids = unique_songs[1:49], 
                             authorization = acc_token)
for(i in 1:floor(length(unique_songs)/50)){
  #go through list of unique songs in batches of 50 (max per API call)
  print(c(i*50,min(i*50+49,length(unique_songs)))) 
  song_metadata <- bind_rows(song_metadata, 
                             get_tracks(unique_songs[(i*50):(min(i*50+49,length(unique_songs)))], 
                                        authorization = acc_token))
}

#a few songs have issues with their marketing region URIs, as the URI from unique_songs 
#used as an argument for get_tracks not the same URI that is returned as the 
#official song URI by get_tracks()
#and is instead returned as a "linked" URI in an additional column.
#replace these returned URIs to match the same URIs that are found in unique_songs to allow for
#correct joins.
song_metadata[!is.na(song_metadata$linked_from.uri),]$uri <- song_metadata[!is.na(song_metadata$linked_from.uri),]$linked_from.uri
#removing irrelevant/constant columns
song_metadata <- song_metadata[,setdiff(names(song_metadata), c("is_local","type","album.type"))]

#take full listening data (with repeated songs), 
#and add associated audio features from the list of unique song audio features
listening_data_df<- left_join(listening_data_df, audio_features,by=c("spotify_track_uri"="uri"), keep=F)

listening_data_df <- left_join(listening_data_df, song_metadata,by=c("spotify_track_uri"="uri"), keep=F, suffix=c("",".y"))
listening_data_df <- listening_data_df %>% select(-ends_with(".y"))

#convert times to a useable format. 
#YYYY-MM-DDT00:00:00Z (Z denotes GMT time) needs to be in YYYY-MM-DD 00:00:00 for as.POSIXct to work
listening_data_df$time <- as.POSIXct(gsub("T"," ",gsub("Z","",listening_data_df$ts)),tz="GMT")

listening_data_df$time <- as.POSIXct(format(listening_data_df$time, tz=Sys.timezone(),usetz=TRUE))

listening_data_df <- listening_data_df %>% arrange(time)
# library(lubridate)
# library(ggplot2)
# library(magrittr)
# library(scales)

song_vs_podcast <- data.frame(Medium=c("Music", "Podcast"),
                              value =c(sum(listening_data_df[is.na(listening_data_df$episode_name),]$ms_played),
                                       sum(listening_data_df[!is.na(listening_data_df$episode_name),]$ms_played)))



podcast_data_df <- listening_data_df[!is.na(listening_data_df$episode_name),]

podcast_data_df %>% summarise(n = sum(ms_played)/1000/60/60)

#from now on, ignore podcasts, remove from listening_data_df
listening_data_df <- listening_data_df[is.na(listening_data_df$episode_name),]

# listens + time spent listening -------------------------
#total listens
nrow(listening_data_df)

#hours of content
sum(listening_data_df$ms_played)/1000/60/60

# Songs played per year --------------------------------------------------------
ggplot(listening_data_df, aes(year(time))) +
  geom_bar(fill   = "lightgreen") +
  geom_text(stat  = "bin", 
            aes(label=comma(..count..)), 
            color = "lightgreen", 
            vjust = -1,
            bins  = 8,
            size  = 7)+
  scale_x_continuous(labels = c("(2014)", 2015:2020, "(2021)"),
                     breaks = c(2014:2021))+
  scale_y_continuous(limit  = c(0, 39000),
                     expand = c(0, 2000))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 20, colour = "#f5f5f5"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)
  )
#ggsave("SongYears.png",bg="transparent",width=XXXX,height=XXXX,units="px",dpi=300,type="cairo-png")

# LISTENS BY PLATFORM ------------------------------------------------------------------------------------------
listening_data_df %<>% mutate(platform_reduced = case_when(grepl("web", platform)~"Browser",
                                                           grepl("Windows 10|Windows 7", platform) ~ "Desktop",
                                                           grepl("iOS", platform) ~ "iPhone",
                                                           grepl("Chromecast", platform) ~ "Chromecast",
                                                           grepl("harmony", platform) ~ "Browser",
                                                           grepl("Android", platform) ~ "Samsung Galaxy")) 

#https://www.r-graph-gallery.com/piechart-ggplot2.html
listening_data_df %>% 
  group_by(platform_reduced) %>% 
    summarise(listen_time = sum(ms_played)) %>%
      mutate(proportion = listen_time / sum(.$listen_time) * 100) %>% 
        arrange(desc(proportion)) %>% 
          mutate(platform_reduced=factor(platform_reduced,levels=platform_reduced)) %$%
            ggplot(., aes(x    = "", 
                          y    = listen_time,
                          fill = platform_reduced)) + 
              geom_bar(stat="identity", width = 1)+
              coord_polar("y",start = 0,clip = "off")+
              scale_fill_manual(values = c("lightgreen",
                                           "#E69F00", 
                                           "#F0E442", 
                                           "#009E73",
                                           "#56B4E9" , 
                                           "#0072B2", 
                                           "#D55E00", 
                                           "#CC79A7"))+
              theme_minimal()+
              geom_text(aes(label = paste0(signif(proportion,2),"% ",platform_reduced),x=1.7), 
                        color = "#f5f5f5", 
                        size  = 9,
                        vjust = c( 10, 1, -0.1, -1.4, -2.8), #these are set for the below ggsave, you may need to tinker
                        hjust = c( -1.4, 0,  0.1,  0.05, 0.12))+
              theme(panel.grid.major.x = element_blank(),
                    plot.margin = unit(c(5,5,5,5),"cm"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    legend.position = "none",
                    panel.background = element_rect(fill = "transparent", color = NA),
                    plot.background = element_rect(fill = "transparent", color = NA))

#ggsave("Test.png",bg="transparent",width=2800,height=2800,units="px",dpi=300,type="cairo-png")

# Reason songs ended ------------------------------------------------------------
reason_end <- listening_data_df %>% 
  group_by(reason_end) %>% 
  tally() 

#create "other" category with everything not 'found new' 'skipped' 'quit' 'song ended'
reason_end <- rbind(reason_end[c(5:7,11),],data.frame(reason_end="other",n=sum(reason_end[-c(5:7,11),]$n)))
reason_end$reason_end <- c("Found new song","Skipped","Quit","Song ended","Other")

reason_end %>% 
  mutate(n = n / sum(.$n) * 100) %>% 
  arrange(desc(n)) %>% 
  mutate(reason_end = factor(reason_end,levels=reason_end)) %>%
  ggplot(., aes(x = "", y = n, fill = reason_end)) + 
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0, clip = "off")+
  scale_fill_manual(values = c("lightgreen",
                               "#E69F00", 
                               "#F0E442", 
                               "#009E73",
                               "#56B4E9" ,
                               "#0072B2",
                               "#D55E00",
                               "#CC79A7"))+
  theme_minimal()+
  geom_text(aes(label = paste0(signif(n,2),"% ",reason_end),x=1.7), 
            color = "#f5f5f5", 
            size  = 9,
            vjust = c( 6,    4,    0.6,  0.5, -1), #may need to tinker for your own ggsave
            hjust = c(-1, -0.14, 0.05, 0.15, 0.12))+
  theme(panel.grid.major.x = element_blank(),
        plot.margin = unit(c(5,5,5,5),"cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent",color=NA), 
        plot.background = element_rect(fill = "transparent", color = NA)
  )

#ggsave("Skips.png",bg="transparent",width=2800,height=2800,units="px",dpi=300,type="cairo-png")

# Top tracks (time), not used ----------------------------------------------
top_tracks_time <- listening_data_df %>% 
                      group_by(spotify_track_uri) %>% 
                        summarise(n=sum(ms_played)/1000/60/60)%>% 
                          arrange((desc(n))) %>% 
                            top_n(10)
#associate song name to uri
top_tracks_time$name <- apply(top_tracks_time,1,function(x)song_metadata[song_metadata$uri==x[1],]$name)


# Top tracks (plays) --------------------------------------------------------
top_tracks_plays <- listening_data_df %>% 
                      group_by(spotify_track_uri) %>% 
                        tally() %>% 
                          arrange((desc(n))) 
#associate song name to uri
top_tracks_plays$name <- apply(top_tracks_plays,1,function(x)song_metadata[song_metadata$uri==x[1],]$name)
#quantiles for plotting
top_tracks_quantile <- data.frame(p=seq(0,1,0.01),n=quantile(top_tracks_plays$n,seq(0,1,0.01)))
#quantile(top_tracks_plays$n,seq(0.1,1,0.001))

ggplot(top_tracks_quantile, aes(x=p,y=n))+ 
  geom_line(color = "lightgreen")+
  geom_area(fill  = "lightgreen")+
  scale_x_continuous(expand = expansion(add = c(0, 0.01)),
                     labels = seq(0, 1, 0.1),
                     breaks = seq(0, 1, 0.1))+
  scale_y_continuous(expand = c(0,0),
                     labels = c(1,seq(100,500,100),47,85),
                     breaks = c(1,seq(100,500,100),47,85))+
  geom_segment(x = 0,xend = 0.5, y = 1, yend = 1,color="pink", size = 2)+
  geom_segment(x = 0, xend = 0.95, y = 47, yend = 47, color = "lightblue", size = 2)+
  geom_segment(x = 0, xend = 0.99, y = 85, yend = 85, color = "mediumaquamarine", size = 2)+
  geom_segment(x = 0.5, xend = 0.5, y = 0, yend = 1, color = "pink", size = 2)+
  geom_segment(x = 0.95, xend = 0.95, y = 0, yend = 47, color = "lightblue", size = 2)+
  geom_segment(x = 0.99, xend = 0.99, y = 0, yend = 85, color = "mediumaquamarine", size = 2)+
  theme_minimal()+
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=16,angle=0,vjust=0,color="#f5f5f5"),
        axis.text.y = element_text(size=16,angle=0,vjust=0,color="#f5f5f5"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size=16,angle=0,vjust=0,color="#f5f5f5",hjust=0.5),
        axis.ticks.x = element_line(colour="#f5f5f5"),
        panel.background = element_rect(fill = "transparent", color=NA), 
        plot.background  = element_rect(fill = "transparent",color=NA)) +
  ggtitle("Song Plays vs Proportion of Songs")


# Listening data grouped by what i was doing at the time generally -------------

listening_data_df %<>% mutate(time_of_life = case_when((year(time) %in% c(2016:2020) & month(time) %in% c(1:4,9:12))~"undergrad_school",
                                                       (year(time) %in% c(2016:2020) & month(time) %in% c(5:8))~"undergrad_summer",
                                                       (year(time) %in% c(2012:2016) & month(time) %in% c(1:6,9:12))~"highschool_school",
                                                       (year(time) %in% c(2012:2016) & month(time) %in% c(7:8))~"highschool_summer",
                                                       (year(time) == 2020 & month(time) %in% c(9:12)) | (year(time) == 2021 & month(time) %in% c(1:8))~"masters_school",
                                                       (year(time) == 2021 & month(time) > 8)~"graduated"
))



# Density school vs summer ------------------------------------------------------
# here we need to be careful to use the fact that midnight is adjacent to both 11pm and 1am, so need to use
# circles
# https://stackoverflow.com/questions/48407745/density-plot-based-on-time-of-the-day
# library(circular)
school <- grepl("school",listening_data_df$time_of_life)

times_in_decimal_sch = hour(listening_data_df[school,]$time) + minute(listening_data_df[school,]$time) / 60
times_in_radians_sch = 2 * pi * (times_in_decimal_sch / 24)

basic_dens_sch = density(times_in_radians_sch, from = 0, to = 2 * pi)

res_sch = density.circular(circular(times_in_radians_sch,
                                                        type = "angle",
                                                        units = "radians",
                                                        rotation = "clock"),
                                     kernel = "wrappednormal",
                                     bw = basic_dens_sch$bw)

time_pdf_sch = data.frame(time = as.numeric(24 * (2 * pi + res_sch$x) / (2 * pi)), # Convert from radians back to 24h clock
                          likelihood = res_sch$y)

times_in_decimal_no_sch = hour(listening_data_df[!school,]$time) + minute(listening_data_df[!school,]$time) / 60
times_in_radians_no_sch = 2 * pi * (times_in_decimal_no_sch / 24)


basic_dens_no_sch = density(times_in_radians_no_sch, from = 0, to = 2 * pi)

res_no_sch = density.circular(circular(times_in_radians_no_sch,
                                       type = "angle",
                                       units = "radians",
                                       rotation = "clock"),
                              kernel = "wrappednormal",
                              bw = basic_dens_no_sch$bw)

time_pdf_no_sch = data.frame(time = as.numeric(24 * (2 * pi + res_no_sch$x) / (2 * pi)), # Convert from radians back to 24h clock
                             likelihood = res_no_sch$y)

ggplot(NULL) +
  geom_area(data = time_pdf_sch,    aes(x = time, y = likelihood), fill = adjustcolor("lightgreen",alpha=0.9)) +
  geom_area(data = time_pdf_no_sch, aes(x = time, y = likelihood), fill = adjustcolor("pink",alpha=0.6))+
  scale_x_continuous("Hour of Day", labels = c(paste(c(12, seq(2, 10, 2)), "AM"),
                                               paste(c(12, seq(2, 10, 2)), "PM"),
                                               paste(12, "AM")), breaks = seq(0, 24, 2)) +
  scale_y_continuous("Likelihood of Call", expand = c(0, 0))+
  geom_vline(xintercept = 0:24, colour = "#131313")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=16, angle=0, vjust=0, color="#f5f5f5", hjust=0.5),
        axis.text.x = element_text(size=16, angle=0, vjust=0, color="#f5f5f5"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour="#f5f5f5"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent")
  ) +
  ggtitle("Listening Time Density")

#ggsave("SongsPerYear.png",bg="transparent",width=2800,height=1600,units="px",dpi=300,type="cairo-png")

# Explicit -------------------------------
table(listening_data_df$explicit)/sum(table(listening_data_df$explicit))

# most skipped ------------------------------------
#'skipped' here means forward button or finding a song manually
listening_data_df %>% 
  group_by(spotify_track_uri,name) %>% #we can group by both to allow for 'name' to tag along because name is essentially just an encoded subset of the uris, is effectively just group_by(spotify_track_uri)
  filter(length(name)>=20) %>% 
  mutate(skipped = (reason_end=="fwdbtn"|reason_end=="clickrow")) %>% 
  summarise(n=sum(skipped)/length(skipped)) %>% 
  arrange(desc(n))

# songs by release decade ------------------------------------------------

song_metadata %>% 
  mutate(release_year = floor(as.numeric(gsub("-.*","",.$album.release_date))/10)*10) %>% #convert dates (maybe with months) to decades only
  filter(release_year >= 1930) %>%
  ggplot(.,aes(x = release_year))+
  geom_bar(fill = "lightgreen") +
  scale_x_continuous(labels = c(paste0(seq(1930, 2020, 10),"s")),
                     breaks=seq(1930, 2020, 10))+
  scale_y_continuous(limits=c(0, 20000),
                     expand=c(0, 0))+
  geom_text(stat  ="bin", 
            aes(label=comma(round(..count..,0))), 
            color = "lightgreen", 
            vjust = -1,
            bins  = 10,
            size  = 7)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 20, colour = "#f5f5f5"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA), 
        plot.background = element_rect(fill = "transparent", color = NA)
  )

# top 5 artists --------------------------------------------------------
#extract artists from each song, since a song has a list of potentially multiple artists
artist_count <- as.data.frame(do.call(rbind,lapply(listening_data_df$artists, function(x) x%>%select(id,name))))

artist_count %>% 
  group_by(id, name) %>% 
    tally() %>% 
      arrange(desc(n))
#rm(artist_count)

# top 5 albums --------------------------
listening_data_df %>% 
  filter(album.album_type=="album") %>% 
    group_by(album.id,album.name,master_metadata_album_artist_name,album.images) %>% 
      summarise(n=sum(ms_played)/1000/60/60) %>% arrange(desc(n))

# holiday trends --------------------------------

holiday_df <- listening_data_df %>% 
  filter(.,grepl("x-mas|xmas|christmas|christ|santa|snow|holiday",name,ignore.case = T))
dates_in_decimal = yday(holiday_df$time)
dates_in_radians = 2 * pi * (dates_in_decimal/366)


basic_dens = density(dates_in_radians, from = 0, to = 2 * pi)

res = density.circular(circular(dates_in_radians,
                                type = "angle",
                                units = "radians",
                                rotation = "clock"),
                       kernel = "wrappednormal",
                       bw = basic_dens$bw)

date_pdf = data.frame(date = as.numeric(366 * (2 * pi + res$x) / (2 * pi)), # Convert from radians back to 24h clock
                      likelihood = res$y)



ggplot(date_pdf) +
  geom_area(aes(x = date, y = likelihood), fill = "lightgreen") +
  scale_x_continuous("Month",labels = c("Jan","Feb","Mar",
                                        "Apr","May","Jun",
                                        "Jul","Aug","Sep",
                                        "Oct","Nov","Dec"),
                     breaks = cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30))) +
  scale_y_continuous("Likelihood of Call", expand = c(0, 0))+
  geom_vline(xintercept = cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30)), colour = "#131313")+
  geom_vline(xintercept = 359.25, colour = "red", size = 2)+
  annotate("text", 
           x = 330, 
           y = 0.2, 
           label = "Christmas\n          Day", 
           size = 6,
           color = "#131313")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=16, angle=0, vjust=0, color="#f5f5f5", hjust=0.5),
        axis.text.x = element_text(size=16, angle=0, vjust=0, color="#f5f5f5"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour="#f5f5f5"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent")
  ) +
  ggtitle("Density of Songs including 'x-mas','xmas','christmas','christ','santa','snow' or 'holiday'")
#ggsave("SongsPerYear.png",bg="transparent",width=XXX,height=XXX,units="px",dpi=300,type="cairo-png")

# #mild trends for other holidays
# listening_data_df %>% filter(.,grepl("thriller|monster mash|halloween|spooky",name,ignore.case = T))
# 
# listening_data_df %>% filter(.,grepl("imagine|new york, new york|new year",name,ignore.case = T))

# most common song name -----------------------------------

#ggsave("SongsPerYear.png",bg="transparent",width=2800,height=1600,units="px",dpi=300,type="cairo-png")
song_metadata %>% 
  group_by(name) %>% 
    tally() %>% 
      arrange(desc(n))

# highest energy song -----------------

listening_data_df%>% arrange(desc(energy)) %>% 
  select(energy,name,master_metadata_album_artist_name)

# most lively song -------------------------------
listening_data_df%>% arrange(desc(liveness)) %>% 
  select(energy,name,master_metadata_album_artist_name)



#other ideas (add spide web charts to show changes in music features over time of life?)

# install.packages("ggradar")
# library(ggradar)
# listening_data_df %<>% 
#   mutate(time_of_life_alt = case_when(grepl("undergrad", time_of_life)~"undergrad",
#                                       grepl("highschool", time_of_life)~"highschool",
#                                       grepl("graduated|masters", time_of_life)~"present"
#   ))
# spider_web <- listening_data_df %>% group_by(time_of_life_alt) %>% summarize_at(c("danceability","energy","loudness","speechiness","acousticness","instrumentalness","liveness","valence","tempo"),mean,na.rm=T)
# spider_web <- spider_web[c(1,3,2),]

