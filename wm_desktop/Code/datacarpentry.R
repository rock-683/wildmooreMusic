
# For access to Spotify API go to developer.spotify.com and create an app 

# pkgs

library(spotifyr)
library(tidyverse)
library(dplyr)
library (lubridate)
library(tm)


##### GET SPOTIFY AUDIO FEATURES #####
######################################

Sys.setenv(SPOTIFY_CLIENT_ID = '8e08ea0d1ea545bea389281a5e55aacd')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '778e435700d64110afe1e5318ccde0b6')


# Connect to Spotify API

access_token <- get_spotify_access_token()


# Pull in 'wmcatch' playlist

playlist_id = "61glcHDihyvFdzYfsG4wCP"


wm_a = get_playlist_audio_features(
  username = "31ckc5hsxwhfnetyrm2waf7sbara",
  playlist_uris=playlist_id,
  authorization = get_spotify_access_token()
)


wm_as = select(wm_a,
               c("track.name","track.artists","track.id")
)

# TRACKART
# grab all track.album.images in a for loop

trackart = c()

for (x in 1:nrow(wm_a)) {trackart = c(trackart, wm_a[[48]][[x]]$url[1])}

# make trackart it's own column

wm_a$trackart = trackart



#####  WILDMOORE FANFICTION/STORY DATASET #####
###############################################


# pull in wildmoore fanfiction features (wm_f) dataset

wm_f = read.csv("wm_f.csv")


# Change date mentioned column type from character to date 

wm_f$DateMentioned = as.Date(wm_f$DateMentioned,"%m/%d/%Y")


# Create Artist column separated by commas -> Artist1, Artist2, Artist3

wm_f$Artist = c()

for (x in 1:nrow(wm_f)){
  if(wm_f$Artist2[x] == "") {
    wm_f$Artist[x] = paste0(wm_f$Artist1[x])
  } else if(wm_f$Artist2[x] != "" & wm_f$Artist3[x] == "") {
    wm_f$Artist[x] = paste0(wm_f$Artist1[x],", ",wm_f$Artist2[x])
  } else if (wm_f$Artist3[x] != "" & wm_f$Artist4[x] == "") {
    wm_f$Artist[x] = paste0(wm_f$Artist1[x],", ",wm_f$Artist2[x],", ",wm_f$Artist3[x])
  } else {
    wm_f$Artist[x] = paste0(wm_f$Artist1[x],", ",wm_f$Artist2[x],", ",wm_f$Artist3[x],", ",wm_f$Artist4[x])
  }
  
}

# Create Writer column separated by commas -> Writer1, Writer2, Writer3

wm_f$Writer = c()

for (x in 1:nrow(wm_f)){
  if(wm_f$Writer2[x] == "") {
    wm_f$Writer[x] = paste0(wm_f$Writer1[x])
  } else {
    wm_f$Writer[x] = paste0(wm_f$Writer1[x],", ",wm_f$Writer2[x])
  }
  
}

# Create SongArtist, SongWriter, SASW and fframetrack columns

wm_f$SongArtist = paste0(wm_f$Song," by ",wm_f$Artist)
wm_f$StoryWriter = paste0(wm_f$Story," by ",wm_f$Writer)
wm_f$SASW = paste0("<div><strong>",wm_f$SongArtist,"</strong></div><div>",wm_f$StoryWriter,"</div>")
wm_f$fframetrack.id = paste0(wm_f$track.id,"?utm_source=generator")

# Get rid of currently invalid rows

wm_f <- subset(wm_f, DeleteRow != 1)


#####JOIN AUDIO FEATURES WITH WILDMOORE STORY FEATURES ######
#############################################################

# Left join based on the 'track_id' column

wm_fa <- left_join(wm_f, wm_a, by = "track.id")
wm_fas = select(wm_fa,
                Song, Artist1, Artist2, Artist3, Artist4, Artist, Story, Excerpt, HTMLExcerpt, 
                Writer1, Writer2, Writer, Chapter, SongArtist, StoryWriter, SASW, DateMentioned, track.id, fframetrack.id, MusicSource, 
                trackart, danceability, energy, key, loudness,mode, speechiness, acousticness,
                Category, AO3url, track.album.release_date)

# Finding indexes for YouTube songs

JNI <- which(wm_fas$Artist1 == 'FLAME') #Instrumental
CNI <- which(wm_fas$Artist2 == 'Paolo Montalban') #Do I Love You (Cinderella)
BI <- which(wm_fas$Artist1 == 'Red Rosamond & Aaron Babs') #Better
DCG <- which(wm_fas$track.id == 'ZlfLW37_BMQ?si=gd3j7DcO5ZfOXzQu') #Girl, Destiny's Child
HDTS <- which(wm_fas$track.id == 'qNKH7q5tgh8?si=NsEXc6Nvc0zOl-YX') #Happily Divorced Theme Song
LMCYS <- which(wm_fas$MusicSource == 2) #Let Me Call You Sweetheart
MO <- which(wm_fas$track.id == 'zyx5ArXI1KY?si=qlX6yrxrhd8Ud1ir') #Moesha Theme Song


# Entering track.id and trackart for YouTube songs

wm_fas$track.id[JNI] = "EfnbquiKk5M?si=GPwZCe91xIptEJBf"
#wm_fas$fframetrack.id[LMCYS] = ".php?height=50&href=https%3A%2F%2Fwww.facebook.com%2FNetflixStrongBlackLead%2Fvideos%2F488965132190451%2F&show_text=false&width=476&t=0"
wm_fas$trackart[CNI] = "https://www.dropbox.com/scl/fi/u0w7rjd0qx04m183t0z32/cinderella1997.jpg?rlkey=zh3ofh8iymefcu2v4zrpysoj8&raw=1"
wm_fas$trackart[BI] = "https://www.dropbox.com/scl/fi/3aljflxag6j8ra5m5vdm3/better_.jpg?rlkey=o4di4h9zgabzgzonkub7sv0ut&raw=1"
wm_fas$trackart[DCG] = "https://www.dropbox.com/scl/fi/ai8ly5ghn1uh8q0dlm1rn/DCG.jpg?rlkey=pg8g3vbrheyv1dyejnbj54hha&raw=1"
wm_fas$trackart[HDTS] = "https://www.dropbox.com/scl/fi/e67eaidhxye67q8nh3nup/HD.jpg?rlkey=ac5vyc0k1hpop82xqxt6h76mq&raw=1"
wm_fas$trackart[MO] = "https://www.dropbox.com/scl/fi/t457c00ej7wwc0leum0r7/moesha.jpg?rlkey=3orgb8ajg9xx1eadqiqcvgx6o&raw=1"
wm_fas$trackart[LMCYS] = "https://www.dropbox.com/scl/fi/a7rn3kz0z016epe8lnqdu/Screenshot-2024-01-23-124205.jpg?rlkey=wvymfdi99p6mh65tf9y11skw6&raw=1"

# Creating HTML image for explore song table

wm_fas$TA = paste0(
      "<img src=\"",
      wm_fas$trackart,
      "\" height=\"50\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"",
      wm_fas$Artist,
      "\"></img>"
    )


wm_fas$rn = rownames(wm_fas)

# Creating HTML links for AO3 chapter 

wm_fas$ao3link = paste0(
  "<a href=\"",
  wm_fas$AO3url,
  "\" target=\"_blank\"
  \">ao3 link</a>"
)

wm_fas$ao3link2 = paste0(
  "<a href=\"",
  wm_fas$AO3url,
  "\" target=\"_blank\"
  \">here</a>"
)

# Saving .csv data file in app folder

write.csv(wm_fas,"Enter correct file path for mobile app/wm_mobile/wm_fas.csv", row.names=FALSE)
write.csv(wm_fas,"Enter correct file path for desktop app/wm_desktop/wm_fas.csv", row.names=FALSE)


##### TOP ARTISTS #####
#######################

# Counting how many times each artist was mentioned (artist counts)

artist_list <- c(wm_fas$Artist1, wm_fas$Artist2, wm_fas$Artist3, wm_fas$Artist4) #Combine the four columns into one list
artist_list <- artist_list[artist_list != ""] #Remove empty entries
artist_counts <- as.data.frame(table(artist_list)) #Create a df of artist counts 
artist_counts <- artist_counts[order(-artist_counts$Freq), ] #sort
colnames(artist_counts) <- c("name","counts") #rename columns
rownames(artist_counts) <- NULL #reset row numbers

# Gathering and adding id, genres, artist art and song_names_list to the artist counts data = artist_data 

# Removing nested data frame with artist information

i <- wm_a$track.artists

artist_names <- sapply(i, '[[', "name")
artist_id <- sapply(i, '[[', "id")

wm_a$artist.names <- artist_names
wm_a$artist.id <- artist_id

artist_id_list <- unique(unlist(artist_id))

# Splitting artist_id_list into chunks of 50

chunks <- split(artist_id_list, ceiling(seq_along(artist_id_list) / 50))

# Using lapply to get artists features for each chunk and then rbind the results

total_artist_list <- do.call(rbind, lapply(chunks, get_artists))
total_artist_list = total_artist_list %>% 
  select('id','name','genres','images') %>%
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>%
  ungroup
rownames(total_artist_list) <- NULL

# Number of images to filter out ones with zero (0)

num_of_images = c()

for (x in 1:nrow(total_artist_list)) {
  
  num_of_images[x] = (length(total_artist_list[[4]][[x]]))
  
}

total_artist_list$num_of_images = num_of_images
total_artist_list <- total_artist_list %>%  filter(num_of_images == 3)


# Grab all track.album.images in a for loop

artistart = c()

for (x in 1:nrow(total_artist_list)) {artistart = c(artistart, total_artist_list[[4]][[x]]$url[1])}

total_artist_list$artistart = artistart

# Left join based on the 'name' column

artist_data <- left_join(artist_counts, total_artist_list, by = "name")

ENC <- which(artist_data$name == 'Encanto Cast')
artist_data$artistart[ENC] = "https://i.scdn.co/image/ab67616d0000b273e1ac646ed6f25125e2a77229"


artist_data = select(artist_data,-images) 

# Create an empty dataframe to store the results

acombined_df <- data.frame(name = character(), counts = numeric(), id = character(), artistart = character(), song_names_list = character(), stringsAsFactors = FALSE)

# Itterate through the list of artists

for (i in seq(nrow(artist_data))) {
  name <- artist_data$name[i]
  counts <- artist_data$counts[i]
  id <- artist_data$id[i]
  artistart <- artist_data$artistart[i]
  
  # Filter expanded_df for the current artist
  
  filtered_df <- wm_fas %>% filter(grepl(name, Artist))
  
  # Extract song names for the current artist
  
  song_names <- filtered_df$Song
  
  # Create a comma-separated list with sequential numbers and a period
  
  song_names_list <- paste0(seq_along(song_names), ".", song_names, collapse = ",")
  
  # Combine artist, song_names_list, and numerical_column_value into new row
  
  new_row <- data.frame(
    name = name,
    counts = counts,
    id = id,
    artistart = artistart,
    song_names_list = song_names_list,
    stringsAsFactors = FALSE
  )
  
  # Append the new row to combined_df
  
  acombined_df <- bind_rows(acombined_df, new_row)
}

artist_data = acombined_df


# Saving .csv data file in app folder

write.csv(artist_data,"Enter correct file path for mobile app/wm_mobile/wm_artist_data.csv", row.names=FALSE)
write.csv(artist_data,"Enter correct file path for desktop app/wm_desktop/wm_artist_data.csv", row.names=FALSE)


##### TOP SONGS #####
#####################

# will do this on app (No separate datafile)
# Count how many times each song (track.id) is mentioned

song_data <- wm_fas %>% 
  count(track.id, SongArtist, trackart, MusicSource, 
        sort = TRUE) %>% 
  arrange(desc(n), SongArtist) %>%
  head(20) %>% 
  as_tibble()


##### TOP GENRE #####
#####################

# Getting genres 

genre_list <- do.call(rbind, lapply(chunks, get_artists))
genre_list <- unlist(genre_list$genres)

# Transforming the list of genres

docs <- Corpus(VectorSource(genre_list)) #Setting list of genres as a Corpus

SpaceToUnderscore <- content_transformer(function (x , pattern ) gsub(pattern, "_", x))

docs <- tm_map(docs, SpaceToUnderscore, " ")

g <- TermDocumentMatrix(docs)

# Setting up final data frame with genres

m <- as.matrix(g)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(subgenre = names(v),freq=v)

d$subgenre1 <- gsub("_", " ", d$subgenre)

# Separate genres into individual rows

genre_by_artist <- total_artist_list %>%
  separate_rows(genres, sep = ", ") %>%
  
  # Remove leading and trailing whitespaces
  
  mutate(genres = str_trim(genres))


# Create an empty dataframe to store the results

genre_data <- data.frame(genre = character(), freq = numeric(), artist_names_list = character(), stringsAsFactors = FALSE)

# Cycle through the list of genres in d

for (i in seq(nrow(d))) {
  genre <- d$subgenre1[i]
  freq <- d$freq[i]
  
  # Filter expanded_df for the current genre
  
  filtered_df <- genre_by_artist %>% filter(genres == genre)
  
  # Extract unique artist names for the current genre
  
  artist_names <- unique(filtered_df$name)
  
  # Create a comma-separated list with sequential numbers and a period
  
  artist_names_list <- paste(artist_names, collapse = ", ")
  
  # Combine genre, artist_names_list, and numerical_column_value into a new row
  
  new_row <- data.frame(
    genre = genre,
    freq = freq,
    artist_names_list = artist_names_list,
    stringsAsFactors = FALSE
  )
  
  # Append the new row to combined_df
  
  genre_data <- bind_rows(genre_data, new_row)
}

# Add genre art for top 10 genres (art made in Canva.com and uploaded to dropbox)

genre_data$genreart[1] = "https://www.dropbox.com/scl/fi/3wutj2zelwo6x305m4def/1.png?rlkey=0ek6mbo8smvaidcf7mxszseoc&raw=1"
genre_data$genreart[2] = "https://www.dropbox.com/scl/fi/h0fotzazhmzxxch18nt7m/2.png?rlkey=mk3wrtz4zgvno9lq4epsblp51&raw=1"
genre_data$genreart[3] = "https://www.dropbox.com/scl/fi/x6gz5cvdqf4on71sxavtc/3.png?rlkey=1uc56a9vpcd10hn7hh9yzonf4&raw=1"
genre_data$genreart[4] = "https://www.dropbox.com/scl/fi/0oowpy4ywu8944pv6rm2v/4.png?rlkey=bfip6phno5tmp92nah11bt7tw&raw=1"
genre_data$genreart[5] = "https://www.dropbox.com/scl/fi/pc1syf9qzfwryvspc538n/5.png?rlkey=3nh6qq4os2y69rhr7uyc6bxoo&raw=1"
genre_data$genreart[7] = "https://www.dropbox.com/scl/fi/vmn8v62yc74x21ti6j10k/6.png?rlkey=j74zvrycbgrkkcadojn3ihdcx&raw=1"
genre_data$genreart[6] = "https://www.dropbox.com/scl/fi/vijuo6mkssrree55tz26h/7.png?rlkey=jc13i7mpdqlvev1siee99vgpy&raw=1"
genre_data$genreart[8] = "https://www.dropbox.com/scl/fi/fyf4sjq274ndqn2a4ni8b/8.png?rlkey=a0ee0ipxvelhy91tny81aqi7y&raw=1"
genre_data$genreart[9] = "https://www.dropbox.com/scl/fi/upp3wu4yr333a77m196si/9.png?rlkey=792dc6n4saw16ndkvxt0xxot2&raw=1"
genre_data$genreart[10] = "https://www.dropbox.com/scl/fi/2g6vnuhrjh5a58kts51wl/genres2.png?rlkey=aszu22egr6586gthugay3mkpq&raw=1"


# Saving .csv data file in app folder

write.csv(genre_data,"Enter correct file path for mobile app/wm_mobile/wm_genre_data.csv", row.names=FALSE)
write.csv(genre_data,"Enter correct file path for desktop app/wm_desktop/wm_genre_data.csv", row.names=FALSE)



##### PLAYLISTS #####
#####################

# wildmoore's music playlist: complete

wmp_complete = select(wm_fas, track.album.release_date, rn, Song, Artist, track.id, StoryWriter, DateMentioned, Category)

# paste the genres OR categories together with an "or" | separator 
# and run that through grepl as a single regular expression.

# Wedding

Wedding <- c("First Dance", "Wedding", "Vowel Renewal", "Proposal") 

wmp_wedding = wm_fas %>%
  select(SongArtist, StoryWriter, track.id, DateMentioned, Category) %>%
  filter(grepl(paste(Wedding, collapse = "|"), Category))%>%
  as_tibble()

# Car

Car <- c("Car")

wmp_car = wm_fas %>%
  select(SongArtist, StoryWriter, track.id, DateMentioned, Category) %>%
  filter(grepl(paste(Car, collapse = "|"), Category))%>%
  as_tibble() 

# Mood Music

MoodMusic <- c("Lap Dance", "Bedroom", "Mood Music", "Love Making", "bedroom")

wmp_moodmusic = wm_fas %>%
  select(SongArtist, StoryWriter, track.id, DateMentioned, Category) %>%
  filter(grepl(paste(MoodMusic, collapse = "|"), Category))%>%
  as_tibble()


# club/Bar

Club <- c("Club", "Bar")

wmp_club = wm_fas %>%
  select(SongArtist, StoryWriter, track.id, DateMentioned, Category) %>%
  filter(grepl(paste(Club, collapse = "|"), Category))%>%
  as_tibble() 


# 90s and early 2000s r&b
# wm_fas$track.album.release_date <- as.Date(wm_fas$track.album.release_date)


rb90_early2000 <- c("Ne-Yo","Destiny's Child","Boyz II Men","SWV", 
                    "Alicia Keys", "Jagged Edge", "Next", "Xscape",
                    "Dru Hill", "Aaliyah", "Silk", "112", "Beyoncé",
                    "Jodeci","TLC", "Floetry","Mariah Carey", 
                    "Tony! Toni! Toné!", "Sade","India.Arie", "Jamie Foxx",
                    "Day26", "USHER", "Ginuwine", "Tyrese",
                    "Toni Braxton", "Brandy","Tevin Campbell","Mary J. Blige",
                    "Trey Songz", "Avant", "Booby V.", "Jaheim",
                    "Lauryn Hill", "Monica", "Michael Jackson","Brian McKnight",
                    "Janet Jackson","Faith Evans","Whitney Houston","Mýa",
                    "Tamia","Babyface", "JoJo", "John Legend",
                    "Back At One","Jon B.","Deborah Cox","T-Pain",
                    "Jill Scott","Brooke Valentine","Mario Winans","Ashanti",
                    "Kenny Lattimor","Glenn Lewis","Ciara","Case",
                    "Lloyd")


wmp_rb90 = wm_fas %>%
  select(SongArtist, track.album.release_date, StoryWriter, Artist, track.id, DateMentioned, Category) %>%
  filter(grepl(paste(rb90_early2000, collapse = "|"), Artist))%>%
  as_tibble() 

yrs <- c("1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005")

wmp_rb90_05 = wmp_rb90 %>%
  select(SongArtist, track.album.release_date, StoryWriter, Artist, track.id, DateMentioned, Category) %>%
  filter(grepl(paste(yrs, collapse = "|"), track.album.release_date))%>%
  as_tibble() 


# Other Options

Christmas <- c("Christmas")
Cooking <- c("Cooking")
KC <- c("Kid's Corner")
Date <- c("Date", "First Date", "Ask Out")

wm_fas %>%
  filter(grepl(paste(Cooking, collapse = "|"), Category)) %>%
  select(SongArtist, StoryWriter, Chapter, DateMentioned, Category) %>%
  as_tibble() %>% print(n=50)


# Create playlist data frame

wm_playlist <- data.frame (
  
  rn = c(1,8,9,10,11,2,3,5,4,6,7,12,19,20,21,22,13,14,16,15,17,18),
  
  title = c("COMPLETE",  "WEDDING", "CAR RIDE",  "CLUB/BAR", "MOOD MUSIC",  
            "90s/Early 2000s R&B", "R&B", "HIP HOP/RAP", "POP", "ROCK", "SOUL", 
            "COMPLETE (minimalistic_flag)", "WEDDING (minimalistic_flag)", "CAR RIDE (minimalistic_flag)", 
            "CLUB/BAR (minimalistic_flag)", "MOOD MUSIC (minimalistic_flag)", "90s/Early 2000s R&B (minimalistic_flag)",
            "R&B (minimalistic_flag)","HIP HOP/RAP (minimalistic_flag)", "POP (minimalistic_flag)",
            "ROCK (minimalistic_flag)", "SOUL (minimalistic_flag)"),
  
  pid = c("2QAS5OUg15FUYFp4lCeuCS", "5VrxhRzKEKvmVJcgzs7YWC", "4JkMcax8mGEPlG2zYI5faJ", "78QJVZljt6Riqht1kvR1Vr",
          "5AYG7lbvgOP6hzJZbvIl82", "2DuVHIuR4sBxqxiE1JVDkf", "6BBUcPHeuCJ1TLDeqNdB26", "5rU2yV41a3z4PZnPxEkCpr",
          "5dIeU5IeilJ4sujzRxXsQR", "3SzWqMyjzA5hLhrI58SI5k", "5YwBbyOixOlZDEBq06iXbo",
          "2AeOlPuYGrXk4haiitLloR", "5h2h2Ru1d1FBS1mdtjQMGX", "0omHuZfYOqTh5P9mQFZLU7", "3ODmj1NkLTgmLzNXx0jEQX",
          "6oxxxa5gE3xcMBEYPQyOgf", "1RYpybfPzshlhMbeEZQ9PR", "3Qbn2s9b3pbYLwANOqsFDv", "157Cnyq6nf70UsGn0aGwUS",
          "0HAebgWyRK1pK9ji2DOrpd", "4kXwIR0xeCX0l1WRNkhGWs", "3UXR86Zzj8xFy32JGIHnbS"),
  
  description = c(paste0("All ",nrow(wm_fas)," songs mentioned in a Wildmoore fanfic (songs for all playlists are in order of date mentioned)"),
                  "Songs mentioned during proposals, weddings, first dances and receptions",
                  "All the songs that played during a car ride in a Wildmoore fanfic",
                  "All the songs that played in a club or bar scene",
                  "All the songs that played during spicy/sexy scenes",
                  "All the 1990 - 2005 R&B songs mentioned in a Wildmoore story",
                  "All the R&B songs mentioned in a Wildmoore story",
                  "All the Hip Hop/Rap songs mentioned",
                  "All the Pop songs mentioned",
                  "All the rock, alt-rock, and pop rock songs mentioned",
                  "Anita Baker, The Temptations and more. Here are all the soul songs",
                  
                  paste0("All ",nrow(wm_fas)," songs mentioned in a Wildmoore fanfic (in order of date mentioned) - cover 2"),
                  "Songs mentioned during proposals, weddings, first dances and receptions - cover 2",
                  "All the songs that played during a car ride in a Wildmoore fanfic - cover 2",
                  "All the songs that played in a club or bar scene - cover 2",
                  "All the songs that played during spicy/sexy scenes - cover 2",
                  "All the 1990 - 2005 R&B songs mentioned - cover 2",
                  "All the R&B songs mentioned - cover 2",
                  "All the Hip Hop/Rap songs mentioned - cover 2",
                  "All the Pop songs mentioned - cover 2",
                  "All the rock, alt-rock, and pop rock songs mentioned - cover 2",
                  "Anita Baker, The Temptations and more. Here are all the soul songs - cover 2"),
  
  # Add playlist art for all playlist (art made in Canva.com and uploaded to dropbox)
  
                # Scene covers

  playlistart = c('https://www.dropbox.com/scl/fi/2gqrwsswbcqyc83zo8ddm/complete.png?rlkey=t7hxsg2hdpj5u9pv6r73273g9&raw=1', #complete
               'https://www.dropbox.com/scl/fi/2t1pogjblhieaa74skq0q/wedding.png?rlkey=xannwfgvg1wpgwwk4jjagmzoi&raw=1', #wedding
               "https://www.dropbox.com/scl/fi/fo7l9z06ocntkt7636wb0/carride.png?rlkey=xgyq97e5ctxhbqkzkhibk21ra&raw=1", #car ride
               "https://www.dropbox.com/scl/fi/k9jfnkn0uftjh0f7gzd42/bar.png?rlkey=qo18qospbliyzk94951cueiia&raw=1", #club/bar
               "https://www.dropbox.com/scl/fi/tfcr2xwo0kseya53nijge/moodmusic.png?rlkey=pxaqfiynqjt8ky7cqc2eg9i8e&raw=1", #mood music
               "https://www.dropbox.com/scl/fi/taliphjunddu9eebsfhk8/90srb.png?rlkey=9glocex76x17abtc3islopluf&raw=1",  #early 90s
               "https://www.dropbox.com/scl/fi/le0ogr0qrn8n4cuqchlsw/rb.png?rlkey=15ozbbv003jpv5at3x9zwoneo&raw=1",  #rb
               "https://www.dropbox.com/scl/fi/u1gvz2q6abrncq72mwnom/hiphop.png?rlkey=otpmof6id5jwbnl1hudf4nr79&raw=1", #hip hop
               "https://www.dropbox.com/scl/fi/dg74jdpvaex3jz0q4ymnz/pop.png?rlkey=9s04thsibc963dc1yz13camcu&raw=1", #pop
               "https://www.dropbox.com/scl/fi/xetd0iskbkpb5m6l1e0v8/rock.png?rlkey=p703k5vblceltk00numy9zj9k&raw=1", #rock
               "https://www.dropbox.com/scl/fi/hgca4e0fj50u2cy9voizi/soul.png?rlkey=gbol9hor1dkppiuepjq8cbfx4&raw=1", #soul
               
               # Minimalistic flag covers
               
               'https://www.dropbox.com/scl/fi/f7f63ehqg5o3y2gzlmexw/complete-2.png?rlkey=53qg22zfilcf8u82l3rt671ix&raw=1', #complete
               'https://www.dropbox.com/scl/fi/0vykm654kibk2u9jdztst/wed-3.png?rlkey=0iq3iwgvno9l79mrfepmdbd0v&raw=1', #wedding
               'https://www.dropbox.com/scl/fi/a1ek7q3m2rlryvunhyd12/carride-3.png?rlkey=6gsufzxxqq6nmiw8o25vtlljc&raw=1', #car ride
               'https://www.dropbox.com/scl/fi/f8s7ox2r3pnte0liu2eq3/club-3.png?rlkey=yh8j5ruhb9y8k4ko2fdf45aph&raw=1', #club/bar
               'https://www.dropbox.com/scl/fi/wt5c8fs3b9prcqj76n5yy/moodmusic-3.png?rlkey=7v7tfbguxavwt7codb2f1gnm3&raw=1', #mood music
               'https://www.dropbox.com/scl/fi/8awygs3gwj1ubxgvgnutr/90rb-3.png?rlkey=8uy6etwrao6fdva4mvk4u7fcg&raw=1',  #early 90s
               'https://www.dropbox.com/scl/fi/klpmy72z35if3klowik7b/rb-3.png?rlkey=s6p7q1tadescw7oaueox0wwel&raw=1', #rb
               'https://www.dropbox.com/scl/fi/3unt4ggzgazb2pvbl57hh/hiphop-3.png?rlkey=22h7iyk9ixxcakbxjlk9job8q&raw=1', #hip hop
               'https://www.dropbox.com/scl/fi/htxler86jb416498qz3gf/pop-3.png?rlkey=7whpquosepx15dldo7tz7rv6l&raw=1', #pop
               'https://www.dropbox.com/scl/fi/7nefdzj8cf407ckub5dqn/rock-3.png?rlkey=7xkwmib66xm5gjg739m0n3z0d&raw=1', #rock
               'https://www.dropbox.com/scl/fi/84vsyuox5idbd54ejdo2m/soul-3.png?rlkey=e30nzcapntekhc5v8eye4fsii&raw=1') #soul
)

wm_playlist <- wm_playlist[order(wm_playlist$rn), ] #sort

# Saving .csv data file in app folder

write.csv(wm_playlist,"Enter correct file path for mobile app/wm_mobile/wm_playlist_data.csv", row.names=FALSE)
write.csv(wm_playlist,"Enter correct file pathfor desktop app/wm_desktop/wm_playlist_data.csv", row.names=FALSE)



