
#pkgs
library(shiny)
library(shinyjs)
library(tidyverse)
library(slickR)
library(DT)
library(shinyAce)


#### Read/ Load Data, Functions and Code ####
# =================================================-

# Read in and filter data sets
# Ensure data sets are in the same folder as desktop_app.R. 

# Fan Fiction and Spotify Audio Features data (wm_fas)
wm_fas = read.csv("wm_fas.csv") 

# Top Artist data (artist_data)
artist_data_full = read.csv("wm_artist_data.csv") 
artist_data = artist_data_full[1:22,]

# Top Song data (song_data)
song_data_full <- wm_fas %>% 
  count(track.id, SongArtist, trackart, MusicSource, 
        sort = TRUE) %>% 
  arrange(desc(n), SongArtist) %>%
  as_tibble()

song_data <- song_data_full %>% 
  head(20) %>% 
  as_tibble()

# Top Genre data (genre_data)
genre_data_full = read.csv("wm_genre_data.csv")
genre_data = genre_data_full[1:10,]

# Playlist Data (playlist_data)
playlist_data = read.csv("wm_playlist_data.csv") 

# Explore tab fframe data modifications
LMCYS <- which(wm_fas$MusicSource == 2) # Let Me Call You Sweetheart
wm_fas$fframetrack.id[LMCYS] = ".php?height=125&href=https%3A%2F%2Fwww.facebook.com%2FNetflixStrongBlackLead%2Fvideos%2F488965132190451%2F&show_text=false&width=476&t=0"


# Read/load functions (rbo, uv, inpop, and shinyLink)
rCode <- paste0(readLines("R/wmFunctions.R"), collapse = "\n")
jsCode <- paste0(readLines("www/shinyLink.js"), collapse = "\n")

# Read additional code for Data & Code tab
c1Code <- paste0(readLines("Code/datacarpentry.R"), collapse = "\n")
saCode <- paste0(readLines("Code/desktop_app.R"), collapse = "\n")
cssCode <- paste0(readLines("www/wildmoore_desktop.css"), collapse = "\n")


#### UI ####
# =================================================-

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "wildmoore_desktop.css"),
    tags$title("Wildmoore's Music")
  ),
  
  navbarPage(
    title = div(img(src='wmimg1.png',
      style="margin-top: -14px; padding-right:10px;
      padding-bottom:10px", height = 60)),
    id = "navBar",
    #title = "Wildmoore's Music", 
    collapsible = TRUE, 
    inverse = TRUE,
    position = "fixed-top",
                 
    # Home tab with Top Artists, Top Songs, Top Genres and Playlists  ----
                 
    tabPanel(
        title = "Home",
        value = "home",
        tags$div(
          
          # summary 
          
          tags$p("| Wildmoore's Music |", 
                 style = "padding: 7.5% 15.5% 2.5% 15.5%;", ),
          tags$h4("Every Song Mentioned in a Wildmoore Story.",
                  style = "padding: 0% 15.5% 0.5% 15.5%; font-weight:400;"),
          tags$p(
            "There were", nrow(wm_fas),"mentions of songs in the stories under the",
            tags$a(href="https://archiveofourown.org/tags/Sophie%20Moore*s*Ryan%20Wilder/works",
                   target="_blank", "Sophie Moore/Ryan Wilder"), "(Wildmoore; Batwoman) femslash tag on 
            Archive of Our Own (AO3). Scroll down for a look at the artists, songs and genres that 
            the Wildmoore fandom mentioned most! (To view/search all", nrow(wm_fas),"song mentions, 
            and excerpts from the stories they were mentioned in, visit the", 
            shinyLink(to = "explore", "Explore"), "page.)",
            style = "padding: 0% 15.5% 16.5% 15.5%;"
          ),
          
          # top artist
          
          tags$div(
            tags$h4("Top Artists", class = "header-blk1"),
            fluidRow(
              column(
                12,slickROutput(
                  "slick_output1",
                  width='90%',height=300)
            )),
            br(),
            br(),
            htmlOutput("selected"),
            tags$div(htmlOutput("songlist"),
                     style = "padding: 0% 8.5% 2.5% 8.5%;"),
            htmlOutput("topartistsplayer"),
            textOutput("storywriter1"),
            
            # top songs
            
            tags$h4("Top Songs", class = "header-blk2"),
            fluidRow(
              column(
                12,slickROutput(
                  "slick_output2",
                  width='90%',height=300)
              )),
            br(),
            br(),
            htmlOutput("topsongsplayer"),
                   class = "color-block1 white", style = "padding: 2.5%;"),
          
          # top genres
          
          tags$div(
            tags$h4("Top Genres", class = "header-wht"),
            fluidRow(
              column(
                12,slickROutput(
                  "slick_output3",
                  width='90%',height=300)
              )),
          ),
          
          # playlists
          
          tags$div(
            tags$h4("Playlists", class = "header-blk1"),
            tags$p("spotify playlists to listen to", style ="text-align: center;"),
            fluidRow(
              column(
                12,slickROutput(
                  "slick_output4",
                  width='90%',height=300)
              )),
            br(),
            br(),
            htmlOutput("playlistplayer"), 
            class = "color-block2 white", style = "padding: 2.5%;"),
          
          # endnote
          
          tags$p(
            "To view/search all", nrow(wm_fas),"song mentions, 
            and excerpts from the stories they were mentioned in, visit the", 
            shinyLink(to = "explore", "Explore"), "page.",
            style = "padding: 10% 15.5% 5% 15.5%;"
            
            )
          )
        ),
                 
    
    # Explore tab with Song Table, Song Player and Story Box  ----
                 
    tabPanel(
      title = "Explore",
      value = "explore",
        fluidRow(
          
          # song table
          
          column(6,DT::dataTableOutput('et')),
          column(6,
                 
          # song player
          
            fluidRow(htmlOutput("exploreframe")),
            fluidRow(
              
          # story box
              
            fluidRow(
              textOutput("story"),
              textOutput("song"),
              textOutput("chapter"),
              htmlOutput("datelink"),
              br(),
              htmlOutput("excerpt_text"),
              hr(),
              htmlOutput("readfull"),
              class = "storywindow",
              style = "max-height: calc(100vh - 265px); 
                   overflow-y: auto;
                   margin-top: 10px;
                   margin-right: 5px; 
                   margin-left: 10px;
                   background-color: #333;"  
               ), style = "background-color: #333;
                           margin-right: 2.5%;
                           margin-left: 2px;
                           margin-top: -35px;
                           height:calc(100vh - 255px);
                           border-top-left-radius:3%;
                           border-top-right-radius:3%;
                           border-bottom-left-radius:3%;
                           border-bottom-right-radius:3%;"
            )
          )
        )
      
    ),
                 
    # Data & Code tab with Data (Sources, Collection, Carpentry and Sets), and Shiny App Code ----
                 
    tabPanel(
      "Data & Code",
      tagList(
        br(),
        
        # project description
        
        tags$details(open = "open",
          tags$summary("| Wildmoore's Music |", style="display:list-item"),
          tags$div(HTML("<p>#' DATE STARTED: 8/1/2023<br />#' DATE POSTED: 2/08/2024<br />#' DATE UPDATED: 7/16/2024<br />#' CODING LANGUAGES: R/html, css, javascript<br />#' PACKAGES: spotifyr, shiny, slickR, DT, dplyr, shinyAce</p>"))
        ),
        
        # data sources and data collection
        
        tags$details(open = "open",
          tags$summary("Data Sources and Data Collection", style="display:list-item"),
          br(),
          tags$div(
          tags$p(tags$strong("Archive of Our Own."),"All fanfiction/story data was gathered from Archive of Our Own, a popular 
                    fandom writing site. All stories under the",
                    tags$a(href="https://archiveofourown.org/tags/Sophie%20Moore*s*Ryan%20Wilder/works",
                    target="_blank", "Sophie Moore/Ryan Wilder"),"tag were used. 
                    Every song that was mentioned or referred to 
                    in a story was added to a datafile with the name of the song, the story and 
                    chapter it was mentioned in, the writer(s) of the story,  the date that chapter 
                    was published, an excerpt/snippet of the story where the song was mentioned, 
                    the url of that chapter, and a brief description (category) of the type of scene 
                    the song was mentioned in (e.g. “in the car”, “Story or Chapter Title” or 
                    “during the wedding”). track.id was manually copied from spotify data for each 
                    song and added to the datafile.", style = "margin-right: 10px;
                                                               margin-left: 10px;
                                                               margin-top: 2px;
                                                               background-color: #333;"),
          tags$details(
            tags$summary("(Details about song 'mentions')", style="display:list-item"),
            br(),
            tags$p(tags$em("Wildmoore’s Music"), "is intended to include every song mentioned in 
                   a Wildmoore fanfic/story, so if a song was referenced or mentioned 
                   in any way - through the title of a story, described as playing over the radio in 
                   the chapter text, referenced jokingly in conversation between characters, 
                   commented on in the writer’s notes, etc. - it is included here… with a few 
                   exceptions and caveats."),
            tags$p("If a song is mentioned multiple times in a story (i.e. once in chapter 2, 
                   then again in chapter 10, and in the story title etc.), the song is 
                   only recorded as being mentioned once in the datafile and playlists. 
                   The first time the song is mentioned in chapter text is used as the 
                   main reference, even if it is referenced in the story title. "),
            tags$p("For example, although the lyrics of ‘The Way’ by Jill Scott 
                   are used as the title of alphagirl007 (kayandkimsmom1)’s story, 
                   ‘The Way You Love Me’, the song isn’t mentioned in the chapter text 
                   until chapter 5. So, the chapter 5 date and text are used for placement 
                   in the datafile and playlists and this is counted as ‘one mention’ even 
                   though technically it is mentioned twice (or 2+ times) in the story. "),
            tags$p("Nursery Rhymes like Mary Had a Little Lamb, Humpty Dumpty, and Ring 
                    Around the Rosie were not included. (You’d be surprised at how many of these 
                    were referenced lol). "), 
            tags$p("Kids Songs, however, like theme songs from Paw Patrol and Disney songs 
                   (e.g. from Princess and the Frog, Encanto or Frozen, etc.) were included."),
            tags$p("The general version of Happy Birthday was omitted, however, mentions of 
                   specific versions (like Stevie Wonder’s) were included."), 
            tags$p("Only explicit references to Megan Thee Stallion’s song Hot Girl Summer 
                   were counted. If someone mentioned ‘Hot girl summer’ in general, or 
                   some derivative of this like ‘Hot Girl Sophie” or “Hot Sophie Summer”, 
                   these were not counted as references to the song."),
            tags$p("I did not include songs that were mentioned in stories where 
                   Sophie Moore/Ryan Wilder are not the main ship. So stories about other 
                   arrowverse shows like the Flash or Legends of Tomorrow that just so 
                   happened to include Sophie and Ryan as a couple were not used. "),
            tags$p("Additionally (and this can be changed) I did not include songs from 
                   stories where Sophie/Ryan/Mary are the tag, because technically it is 
                   a different pairing (i.e. not just Sophie/Ryan). I have NO problem including 
                   these if anyone request it. "), 
            tags$p("I am 100% certain that I missed references to some songs, so if you
                   notice that one is not included (especially if you wrote the story the 
                   song is mentioned in) definitely let me know, and I can add it to the 
                   datafile and playlists."),
            tags$p("I am also 100% certain that I made some wrong assumptions about 
                   references to songs. As in the writer was not actually referring 
                   to any song but I thought they were. (Again, if this happened, especially 
                   if you are the writer, also let me know so I can remove the song) 
                   I tried though, in general, to err on the side of NOT including a song if 
                   I wasn’t sure about it."),  style = "padding-left:15px;"
            
          ),
          br(),
          tags$p(tags$strong("Spotify."), "As songs were added to the fanfiction/story datafile, the corresponding songs were 
                    added to a catch all playlist in Spotify called ‘wmcatch’. Song/track, artist, 
                    track art, artist art, track.id and audio features were mined for 
                    those songs using Spotify’s API and the spotifyr package in r.", 
                    style = "margin-right: 10px;
                             margin-left: 10px;
                             margin-top: 2px;
                             background-color: #333;"), 
                    
                    style = "background-color: #333;
                             color: #eee;
                             padding-top: 15px;"
          ),
          br(),
        ),
        
        # data carpentry and data sets
        
        tags$details(open = "open",
          tags$summary("Data Carpentry and Data Sets", style="display:list-item"),
          br(),
          tags$p("(Main and sub datasets)"),
            
          tags$div(tags$code("wm_fas"), "is the main dataset. It is the combined story features and song/audio features dataset", downloadButton("download_wm", " ")),
          tags$div(tags$code("artist_data"), "list all artists in order of number of times mentioned", downloadButton("download_ad", " ")),
          tags$div(tags$code("song_data"), "list all songs in order of number of times mentioned", downloadButton("download_sd", " ")),
          tags$div(tags$code("genre_data"), "list all genres (determined by spotify) in order of frequency in total song list", downloadButton("download_gd", " ")),
          tags$div(tags$code("playlist_data"), "stores all wildmoore's music playlist and associated coverart", downloadButton("download_pd", " ")),
          
          br(),
          tags$p("(Code for creating main and sub datasets)"),
  
          br(),
          tags$div( aceEditor("codeEditor1", value = c1Code, mode = "r", theme = "tomorrow_night",
                         fontSize = 14, height = "600px")
              )),
          
        # shiny app code display
    
          
        tags$details(open = "open",
          tags$summary("Shiny App Code", style="display:list-item"),
          br(),
          tags$details(open = "open",
            tags$summary("(functions.R file - includes functions - rbo, uv, inpop, and shinyLink - used in the app server code)", style="display:list-item"),
            br(),
            tags$div( aceEditor("codeEditor2", value = rCode, mode = "r", theme = "tomorrow_night",
                                fontSize = 14, height = "600px")
          )
        ),
          tags$details(open = "open",
            tags$summary("(Shiny App Server and UI code)", style="display:list-item"),
            br(),
            tags$div( aceEditor("codeEditor3", value = saCode, mode = "r", theme = "tomorrow_night",
                              fontSize = 14, height = "600px")
          )
        ),
        
          tags$details(open = "open",
            tags$summary("(css code for the desktop app, 'wildmoore_desktop.css' (style sheet))", style="display:list-item"),
            br(),
            tags$p(" this style sheet is a modified version of bootstrap's 'paper' theme. 
                   most additions/modifications are in lines 1 - 145 "),
            tags$div( aceEditor("codeEditor4", value = cssCode, mode = "css", theme = "tomorrow_night",
                              fontSize = 14, height = "600px")
          )
        ),
        
          tags$details(open = "open",
            tags$summary("(javascript code for shinyLink)", style="display:list-item"),
            br(),
            tags$div( aceEditor("codeEditor5", value = jsCode, mode = "javascript", theme = "tomorrow_night",
                              fontSize = 14, height = "600px")
          )
        ),
      ),
       
       
       )
    )
      
  ),
  tags$script(src = "shinyLink.js")
)


#### SERVER ####
# =================================================-

server <- function(input, output) {
  
  # Collapse and Retract NavBar upon tab selection
  
  observeEvent(input$navBar, {
    runjs('
      var elem = document.getElementsByClassName("navbar-collapse")[0]
      elem.setAttribute("aria-expanded", "false");
      elem.setAttribute("class", "navbar-collapse collapse");
    ')
  })
  
  # Home tab with Top Artists, Top Songs, Top Genres and Playlists  ----
  
  # TOP ARTISTS
  # RSlick carousel with pictures of artists  
  
  output$slick_output1 <- renderSlickR({
    ( slickR(obj = artist_data$artistart, slideId = "slda", height = 250, width = "95%") + settings(slidesToShow = 3) )%synch%
      ( slickR(paste0(row.names(artist_data),". ",artist_data$name), slideId = "sldb", height = 10, slideType = 'p') + settings(arrows = FALSE, slidesToShow = 3) ) %synch%
      ( slickR(paste0(artist_data$counts," Mentions"), slideId = "sldc", height = 10, slideType = 'p') + settings(arrows = FALSE, slidesToShow = 3) )
    
  })
  
  # active_slick will maintain the index of the clicked album art
  # initially it will be populated by 1  
  
  active_slick1 <- shiny::reactiveValues( clicked = c(1))
  shiny::observeEvent(input$slick_output1_current,{
    active_slick1$clicked <- input$slick_output1_current$.clicked
  })
  
  num1 <- rbo(reactive({ active_slick1$clicked }))
  
  output$selected <- renderText({ paste0(HTML('&emsp;'),"Selected: ", artist_data$name[num1()])})
  
  output$songlist <- renderUI({
    
    sl <- artist_data$song_names_list[num1()]
    sl <- unlist(strsplit(sl,","))
    
    lapply(seq_along(sl), function(i) {
      tags$a(
        sl[i],
        style = "color:gray; font-size: 15px; ",
        onclick = sprintf("Shiny.setInputValue(id = 'last_index_clicked', value = %s, {priority: 'event'});", i)
      )
      
    })
    
  })
  
  uv1 <- uv(reactive({ num1() }),(reactive({ input$last_index_clicked })))
  
  output$topartistsplayer <- renderUI({
    
    fdf <- filter(wm_fas, grepl(artist_data$name[num1()],Artist))
    
    if (fdf$MusicSource[uv1()] == 1) {
      https_input <- "https://www.youtube.com/embed/"
    } else {
      https_input <- "https://open.spotify.com/embed/track/"
    }
    
    tags$iframe(
      
      src = paste0(
        https_input,
        fdf$track.id[uv1()],
        "?utm_source=generator"
      ),
      seamless = "seamless",
      width = "85%",
      height = "100px",
      frameBorder = "none")
    
  })
  
  output$storywriter1 <- renderText({ 
    
    fdf <- filter(wm_fas, grepl(artist_data$name[num1()],Artist))
    
    paste0("Song mentioned in: ",fdf$Story[uv1()],
           " by ",fdf$Writer[uv1()])
  })
  
  # TOP SONGS
  # RSlick carousel with pictures of songs   
  
  output$slick_output2 <- renderSlickR({
    ( slickR(obj = song_data$trackart, slideId = "sldd", height = 250, width = "95%") + settings(slidesToShow = 3) )  %synch%
      ( slickR(paste0(row.names(song_data),". ",song_data$SongArtist), slideId = "slde", height = 10, slideType = 'p') + settings(arrows = FALSE, slidesToShow = 3) ) %synch%
      ( slickR(paste0(song_data$n," Mentions"), slideId = "sldf", height = 10, slideType = 'p') + settings(arrows = FALSE, slidesToShow = 3) )
    
  })
  
  active_slick2 <- shiny::reactiveValues( clicked = c(1))
  shiny::observeEvent(input$slick_output2_current,{
    active_slick2$clicked <- input$slick_output2_current$.clicked
  })
  
  num2 <- rbo(reactive({ active_slick2$clicked  }))
  
  output$topsongsplayer <- renderUI({
    
    if (song_data$MusicSource[num2()] == 1) {
      https_input <- "https://www.youtube.com/embed/"
    } else {
      https_input <- "https://open.spotify.com/embed/track/"
    }
    
    tags$iframe(
      
      src = paste0(
        https_input,
        song_data$track.id[num2()],
        "?utm_source=generator"
      ),
      seamless = "seamless",
      width = "85%",
      height = "100px",
      frameBorder = "none")
    
  })
  
  
  # TOP GENRES
  
  output$slick_output3 <- renderSlickR({
    slickR(obj = genre_data$genreart, slideId = "sldg", height = 150, width = "95%", elementId = 1) %synch%
      ( slickR(paste0(row.names(genre_data),"."), slideId = "sldh", height = 10, slideType = 'p') + settings(arrows = FALSE) ) %synch%
      ( slickR(paste0(genre_data$artist_names_list), slideId = "sldi", height = 375, slideType = 'p') + settings(arrows = FALSE) )
    
  })
  
  # PLAYLISTS
  
  output$slick_output4 <- renderSlickR({
    ( slickR(obj = playlist_data$playlistart, slideId = "sldj", height = 250, width = "95%", elementId = 1)  + settings(slidesToShow = 3) ) %synch%
      ( slickR(paste0(playlist_data$description), slideId = "sldk", height = 60, slideType = 'p') + settings(arrows = FALSE, slidesToShow = 3) )
  })
  
  active_slick4 <- shiny::reactiveValues( clicked = c(1))
  shiny::observeEvent(input$slick_output4_current,{
    active_slick4$clicked <- input$slick_output4_current$.clicked
  })
  
  num4 <- rbo(reactive({ active_slick4$clicked  }))
  
  output$playlistplayer <- renderUI({
    
    pid = playlist_data$pid[num4()]
    
    tags$iframe(
      
      src = paste0(
        "https://open.spotify.com/embed/playlist/",
        pid,
        "?utm_source=generator"
      ),
      seamless = "seamless",
      width = "85%",
      height = "600px",
      frameBorder = "none")
    
  })
  
  # Explore tab with Song Table, Song Player and Story Box  ----
  
  # EXPLORE TABLE (et) / SONG TABLE
  
  output$et = DT::renderDataTable(
    select(wm_fas, rn, TA, SASW), 
    server = FALSE, 
    selection = 'single',
    filter = 'top', 
    rownames = FALSE, 
    callback = JS("$(\"input[type='search']\").attr('placeholder','Search for: Song, Artist, Story or Writer');"),
    colnames = c("#","", "Song Title"),
    options = list(
      pageLength = -1, info = FALSE, dom = 't', 
      scrollY = "calc(100vh - 200px)",
      columnDefs = list(list(targets = c(0,1), searchable = FALSE), 
                        list(targets = c(0), width = '1vw'),
                        list(targets = c(1), width = '1vw'),
                        list(targets = c(2), width = '70vw')),
      autoWidth = TRUE
           ),
  
    escape=FALSE
  )
  
  # EXPLORE FRAME / SONG PLAYER
  
  output$exploreframe <- renderUI({
    
    if (wm_fas$MusicSource[inpop(input$et_rows_selected, 1)] == 1) {
      https_input <- "https://www.youtube.com/embed/"
      height_input <- "150px"
      margin_style <- "margin-bottom: 50px;" # Adjust margin for YouTube
    } else if (wm_fas$MusicSource[inpop(input$et_rows_selected, 1)] == 2) {
      https_input <- "https://www.facebook.com/plugins/video"
      height_input <- "200px"
      margin_style <- "margin-bottom: 0px;" # margin for Facebook
    } else if (wm_fas$MusicSource[inpop(input$et_rows_selected, 1)] == 3) {
      https_input <- "https://open.spotify.com/embed/playlist/"
      height_input <- "200px"
      margin_style <- "margin-bottom: 0px;" # Remove margin for Spotify
    } else {
      https_input <- "https://open.spotify.com/embed/track/"
      height_input <- "200px"
      margin_style <- "margin-bottom: 0px;" # Remove margin for Spotify
    }
    
    tags$div(
      tags$iframe(
        src = paste0(
          https_input,
          wm_fas$fframetrack.id[inpop(input$et_rows_selected,1)]
        ),
        seamless = "seamless",
        width = "100%",
        height = height_input,
        frameBorder = "none",
        style = margin_style # Apply margin style
      )
    )
    
  })
  
  # STORY BOX
  
  output$story <- renderText({ paste0("Story: ",wm_fas$Story[inpop(input$et_rows_selected,1)]," by ",wm_fas$Writer[inpop(input$et_rows_selected,1)])})
  output$song <- renderText({ paste0("Song: ",wm_fas$Song[inpop(input$et_rows_selected,1)]," by ",wm_fas$Artist[inpop(input$et_rows_selected,1)])})
  output$chapter <- renderText({ paste0("Mentioned in Chapter ",wm_fas$Chapter[inpop(input$et_rows_selected,1)])})
  output$datelink <- renderText({ 
    
    paste0(wm_fas$DateMentioned[inpop(input$et_rows_selected,1)]," | ",
           HTML(wm_fas$ao3link[inpop(input$et_rows_selected,1)])
    )
    
  })
  output$excerpt_text <- renderText({HTML(wm_fas$HTMLExcerpt[inpop(input$et_rows_selected,1)]) })
  output$readfull <- renderText({ paste0("Read the full chapter ",HTML(wm_fas$ao3link2[inpop(input$et_rows_selected,1)])) })
  
  
  # Data & Code tab with Data (Sources, Collection and Carpentry), Shiny App Code, and Data File ----
  
  
  # MAIN DATA SET (WM_FAS.CSV) Download button
  
  output$download_wm <- downloadHandler(filename = function() { "wildmoremusic.csv" },
    content = function(file) { write.csv(wm_fas, file, row.names = FALSE) } )

  
  # SUB DATA SET (ARTIST_DATA.CSV) Download button
  
  output$download_ad <- downloadHandler( filename = function() { "wm_artist_data.csv" },
    content = function(file) { write.csv(artist_data_full, file, row.names = FALSE) } )
  
  # SUB DATA SET (SONG_DATA.CSV) Download button
  
  output$download_sd <- downloadHandler( filename = function() { "wm_song_data.csv" },
    content = function(file) { write.csv(song_data_full, file, row.names = FALSE) } )
  
  # SUB DATA SET (GENRE_DATA.CSV) Download button
  
  output$download_gd <- downloadHandler( filename = function() { "wm_genre_data.csv" },
    content = function(file) { write.csv(genre_data_full, file, row.names = FALSE) } )
  
  
  # SUB DATA SET (PLAYLIST_DATA.CSV) Download button
  
  output$download_pd <- downloadHandler( filename = function() { "wm_playlist_data.csv" },
    content = function(file) { write.csv(playlist_data, file, row.names = FALSE) } )

  
}

shinyApp(ui, server)

