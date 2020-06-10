#### Title ####
## ================================================================================================================ ##
## STA141B Final Project -- Music Analysis using Genius and Spotify API
## 06/11/20
## Quilvio Hernandez
## ================================================================================================================ ##

#### Packages ####
library(shiny) # Package for the R Shiny interactions
library(shinythemes) # Themes for Shiny App
library(tidyverse) # Data Manipulation
library(geniusr) # Genius API Wrapper
library(spotifyr) # Spotify API Wrapper
library(jsonlite) # JSON parser
library(tidytext) # Text Sentiments
library(tm) # Text Mining Framework
library(textdata) # Sentiment Lexicons
library(wordcloud) # Word Clouds
library(reshape2) # Postive-Negative Word Cloud
library(proxy) # Dendrogram
library(igraph) # Network Graphs
library(ggraph) # Network Graphs
library(ggthemes) # Themes for plots
library(ggrepel) # Text Labels for ggplot
library(ggridges) # Density Ridges ggplot
library(wesanderson) # Wes Anderson Color Palatte
library(radarchart) # Radar Graph Package

nrc <- readRDS("data/NRCWordEmotion.rds")
afinn <- readRDS("data/afinn_111.rds")

#### UI ####
ui <- navbarPage("Music Analysis using Genius and Spotify API",
#### Album ####
#### Album Tab #####
             tabPanel("Album", 
                pageWithSidebar(
                 headerPanel('What Album?'),
                 # Sidebar Panel
                 sidebarPanel(width = 4,
                              textInput("album",
                                        label = "Album Name",
                                        value = "+"),
                              textInput("artist_album",
                                        label = "Artist Name",
                                        value = "Ed Sheeran"),
                              submitButton("Explore Album!", icon("refresh"))
                 ),
                 # Main Panel
                 mainPanel(
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("wordcloud"), plotOutput("sentiment_wordcloud"))
                   ),
                   p("Two word plots. One gives an overview of the most common words of the album, while the other assesses the positive and negative words."),
                   hr(),
                   plotOutput("word_per_song"),
                   p("Horizontal bar charts displaying the top 5 words from each song. It's common for the title of the song to appear as one of these top 5 words."),
                   hr(),
                   plotOutput("gross_sentiment"),
                   p("Comparison of positive and negative words for each song using bing lexicon."),
                   hr(),
                   plotOutput("net_sentiment"),
                   p("The net sentiment of each song using AFINN lexicon."),
                   hr(),
                   plotOutput("dendrogram"),
                   p("Dendrogram showing the hierarchical relationships between songs using cosine distance and ward.d2 clustering"),
                   hr(),
                   chartJSRadarOutput("songradar"),
                   p("Sentiment analysis of songs on the album using nrc lexicon. You can filter out songs by clicking on them."),
                   hr(),
                   plotOutput("pirate"),
                   p("Simple scatter plot displaying the 'Sonic Score' for each song. Sonic Score is a unique measure of sonic cohesiveness and computed by summing together valence, danceability, and energy. A description for all metric on the About page. Duplicates may arise due to Genius and Spotify discrepencies."),
                   hr(),
                   chartJSRadarOutput("radarchart"),
                   p("Radar chart displaying the Spotify metrics for each song. You can filter out songs by clicking on them. Discrepencies between the tracks on the two radar plots may arise due to discrepencies between Genius and Spotify tracklists."),
                   hr(),
                   plotOutput("spotify_density"),
                   p("Density plot of Spotify metrics for the album. The peaks indicate that in general that is the 'sound' of the album."),
                   hr(),
                   plotOutput("markow"),
                   p("Visualization for network structure of word bigrams. This visualization can also be thought of as a Markov chain, a common model in text processing. In a Markov chain each choice of word depends only on the previous word.")
                 )
               )
             ),

#### About Tab ####
             tabPanel("About",p("This project is made using the Genius and Spotify API wrappers", a("geniusr", href="https://cran.r-project.org/web/packages/geniusr/geniusr.pdf", target="_blank"),
                                "and ", a("spotifyr", href = "https://www.rcharlie.com/spotifyr/", target="_blank"), ", respectively. All the code for this project can be found", a("here.", href = "https://github.com/kiwilvio/Album-Analysis", target = "_blank"), style = "font-size:25px"),
                      hr(), 
                      p("This app will run various analysis on the given album. Namely, analyzing the sentiment of individual songs to give an overview on the album. Numerous Spotify metrics for each song on the album are also displayed.", style = "font-size:25px"),
                      hr(), 
                      p("Spotify Metircs:", style = "font-size:25px"),
                      tags$ul(
                        tags$li("acousticness - 	A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic."),
                        tags$li("danceability - Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                        tags$li("energy - Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                        tags$li("instrumentalness - Predicts whether a track contains no vocals. “Ooh” and “aah” sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly “vocal”. The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0."),
                        tags$li("liveness - Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."),
                        tags$li("speechiness - Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks."),
                        tags$li("valence - A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).")
                      ),
                      p("Lexicon Information", style = "font-size:25px"),
                      tags$ul(
                        tags$li("AFINN - AFINN is a lexicon of English words rated for valence with an integer between minus five (negative) and plus five (positive). The words have been manually labeled by Finn Årup Nielsen in 2009-2011. This dataset was published in Finn Ärup Nielsen (2011), “A new ANEW: Evaluation of a word list for sentiment analysis in microblogs”, Proceedings of the ESWC2011 Workshop on 'Making Sense of Microposts': Big things come in small packages (2011) 93-98."),
                        tags$li("bing - General purpose English sentiment lexicon that categorizes words in a binary fashion, either positive or negative. This dataset was first published in Minqing Hu and Bing Liu, “Mining and summarizing customer reviews.”, Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery & Data Mining (KDD-2004), 2004."),
                        tags$li("nrc - General purpose English sentiment/emotion lexicon. This lexicon labels words with six possible sentiments or emotions: 'negative', 'positive', 'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', or 'trust'. The annotations were manually done through Amazon's Mechanical Turk. This dataset was published in Saif Mohammad and Peter Turney. (2013), “Crowdsourcing a Word-Emotion Association Lexicon.” Computational Intelligence, 29(3): 436-465."),
                        ),
                      p("Acknowledgements:", style = "font-size:25px"),
                      tags$ul(
                        tags$li(a("Professor Lai for teaching me essential text mining and API knowledge.", href = "https://randycity.github.io/", target = "_blank")),
                        tags$li(a("tayloR", href = "https://medium.com/@simranvatsa5/taylor-f656e2a09cc3", target = "_blank")),
                        tags$li(a("spotifyr", href = "https://www.rcharlie.com/spotifyr/", target = "_blank")),
                        tags$li(a("geniusr", href = "https://cran.r-project.org/web/packages/geniusr/geniusr.pdf", target = "_blank")),
                        tags$li(a("Text Mining with R", href = "https://www.tidytextmining.com/", target = "_blank"))
                      )
                      ),
#### Contact Tab ####
             tabPanel("Developer",
                      p(a("Quilvio Hernandez", href="https://kiwilvio.github.io/", target="_blank"),style = "font-size:25px"),
                      p("e-mail: qahernandez@ucdavis.edu",style = "font-size:20px")
                      )
)

#### Actions ####
server <- function(input, output) {

#### Album Dataframes ####
  # Retrieve Album Track List
  track_list <- reactive({
    get_album_tracklist_search(input$artist_album, input$album) 
  })
  
  # Retrieve Lyrics for each song in the album
  album_lyrics <- reactive({
    album_lyrics <- tibble(line = character(), 
                           section_name = character(),
                           section_artist = character(),
                           song_name = character(),
                           artist_name = character())
    
    for (i in track_list()$song_title) {
      album_lyrics <- bind_rows(album_lyrics, get_lyrics_search(input$artist_album,
                                                                song_title = i))
    }
    album_lyrics
  })
  
  # Create word tokens from the lyrics
  album_tokens <- reactive({
    album_lyrics() %>%
      unnest_tokens(word, line) %>% 
      anti_join(stop_words) %>% 
      group_by(song_name) %>%
      count(word, sort = TRUE) %>% 
      arrange(song_name)
  })
  
  # Retrieve Spotify album audio features
  album_audio_features <- reactive({
    get_artist_audio_features(input$artist_album)%>% 
      inner_join(track_list(), by = c("track_name" = "song_title")) %>% 
      distinct(track_number, .keep_all = TRUE) %>% 
      select(track_name, danceability, energy, speechiness, 
             acousticness, instrumentalness, liveness, valence)
    }) 
  
  # Create Dataframe for Density plot
  density_df <- reactive({
    album_audio_features() %>% 
      pivot_longer(-track_name)
  })
  
  # Create Dataframe for Spotify metrics radar plot
  spotify_radar_df <- reactive({
    density_df() %>% 
      pivot_wider(names_from = track_name) %>% 
      select(name, (order(colnames(.))))
  })
  
  # Create Dataframe for Sonic Score plot
  pirate_df <- reactive({
    album_audio_features() %>% 
      mutate(album = input$album,
             sonic_score = valence + danceability + energy)
  })
  
  # Create Dataframe for nrc sentiments
  album_nrc <- reactive({
    album_tokens() %>%
      inner_join(nrc) %>%
      filter(!sentiment %in% c("positive", "negative"))
  })
  
  # Aggregate counts for each sentiment
  sentiment_nrc <- reactive({
    album_nrc() %>%
    group_by(song_name, sentiment) %>%
    count(song_name, sentiment) %>% 
    select(song_name, sentiment, sentiment_total = n)
  })
  
  # Collect sentiments for each song
  album_nrc2 <- reactive({
    album_nrc() %>%
    count(song_name) %>% 
    select(song_name, song_total = n)
  })
  
  # Create Dataframe for sentiment radar chart
  song_radar_df <- reactive({
    sentiment_nrc() %>% 
      inner_join(album_nrc2(), by = "song_name") %>% 
      mutate(percent = round((sentiment_total/song_total * 100), 3)) %>% 
      select(-sentiment_total, -song_total) %>%
      pivot_wider(names_from = "song_name", values_from = "percent", values_fill = 0) %>% 
      select(sentiment, order(colnames(.)))
  })
  
  # Create bigram dataframe for Markov graph
  bigram_df <- reactive({
    album_lyrics() %>%
      unnest_tokens(bigram, line, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>% 
      count(word1, word2, sort = TRUE)
  })

#### Album Graphs ####
  
  # Generic Wordcloud
  album_wordcloud <- reactive({
    album_tokens() %>% 
      with(wordcloud(
        word, n, min.freq = 2, max.words = 100, random.order = FALSE,
        colors = brewer.pal(8, "Dark2")))
  })
  
  # Sentiment Wordcloud
  sentiment_wordcloud <- reactive({
    album_tokens() %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("red", "green"),
                       max.words = 100)
  })
  
  # Top 5 Words per Song
  words_per_song <- reactive({
    album_tokens() %>%
      slice_max(n, n = 5, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(song_name, n) %>% 
      mutate(order = row_number()) %>% {
        ggplot(., aes(x = order, y = n, fill = song_name)) + 
          geom_bar(stat = "identity", na.rm = TRUE, show.legend = FALSE) +
          xlab("word") +
          facet_wrap(~song_name, scales = "free") +
          coord_flip() +
          scale_x_continuous(breaks = .$order, labels = .$word, expand = c(0, 0))
      }
  })
  
  # Dendrogram 
  album_dendrogram <- reactive({
    docsdissim <- dist(as.matrix(cast_dtm(album_tokens(), song_name, word, n)), method = "cosine")
    hclust(docsdissim, method = "ward.D2")
  })
  
  # Density Plot
  album_density <- reactive({
    density_df() %>% 
      ggplot(aes(x = value, y = name, fill = ..x..)) + 
      geom_density_ridges_gradient(scale = 0.9) +
      scale_fill_gradient(low = "white", high = "maroon3") + 
      theme_fivethirtyeight() + 
      theme(panel.background = element_rect(fill = "white")) +
      theme(plot.background = element_rect(fill = "white")) +
      xlim(0,1) +
      theme(legend.position = "none")
  })
  
  # Sonic Score Plot
  album_pirate <- reactive({
    pos <- position_jitter(width = 0.05)
    ggplot(pirate_df(), aes(album, sonic_score, label = track_name)) +
      geom_point(position = pos, color = wes_palette("IsleofDogs1")[2]) +
      geom_label_repel(position = pos) + 
      labs(title = paste0("Sonic Scores for ", input$album))
  })
  
  # Net Sentiments per Song
  album_net_sentiment <- reactive({
    album_tokens() %>% 
      inner_join(afinn) %>% 
      group_by(song_name) %>% 
      summarise(sum(value)) %>% 
      mutate(colour = case_when(
        `sum(value)` > 0 ~ "positive",
        `sum(value)` < 0 ~ "negative",
        TRUE ~ "neutral"
      )) %>% 
      filter(colour != "neutral") %>% 
      ggplot(aes(x = song_name, y = `sum(value)`, fill = colour)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_fivethirtyeight() +
      theme(panel.background = element_rect(fill = "white")) +
      theme(plot.background = element_rect(fill = "white")) +
      theme(legend.position="none") + 
      scale_fill_manual(values = c("palevioletred",  "olivedrab3")) +
      labs(title = "Net Sentiment (AFINN)")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Gross Sentiment for songs
  album_gross_sentiment <- reactive({
    album_tokens() %>% 
      inner_join(get_sentiments("bing")) %>%  
      unique() %>% 
      group_by(song_name, sentiment) %>%
      count(song_name, sentiment) %>% 
      mutate(n = case_when(
        sentiment == "negative" ~ -n,
        TRUE ~ n)) %>% 
      ggplot(aes(x = song_name, y = n, fill = sentiment)) + 
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_fivethirtyeight() +
    theme(panel.background = element_rect(fill = "white")) +
      theme(plot.background = element_rect(fill = "white")) +
      scale_fill_manual(values = c("palevioletred", "olivedrab3")) +
      theme(legend.position="none") +
      labs(title = "Gross Sentiment (bing)")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Markov Graph/Network
  markov_graph <- reactive({
    bigram_graph <- bigram_df() %>%
      filter(n > 3) %>%
      graph_from_data_frame()
    
    set.seed(2016)
    
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  })
  
#### Outputs ####
  output$wordcloud <- renderPlot({
    album_wordcloud()
  })
  
  output$sentiment_wordcloud <- renderPlot({
    sentiment_wordcloud()
  })
  
  output$word_per_song <- renderPlot({
    words_per_song()
  })
  
  output$net_sentiment <- renderPlot({
    album_net_sentiment()
  })
  
  output$gross_sentiment <- renderPlot({
    album_gross_sentiment()
  })
  
  output$dendrogram <- renderPlot({
    plot(album_dendrogram())
  })
  
  output$spotify_density <- renderPlot({
    album_density()
  })
  
  output$pirate <- renderPlot({
    album_pirate()
  })
  
  output$radarchart <- renderChartJSRadar({
    chartJSRadar(spotify_radar_df(), polyAlpha = 0.1, lineAlpha = 0.8, main = "Spotify Metrics")
  })
  
  output$songradar <- renderChartJSRadar({
    chartJSRadar(song_radar_df(), polyAlpha = 0.1, lineAlpha = 0.8,
                 byrow = F, nrow = 3, main = "Song Sentiments")
  })
  
  output$markow <- renderPlot({
    markov_graph()
  })
}

shinyApp(ui = ui, server = server)
