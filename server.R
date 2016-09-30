if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,leaflet,maps,ggplot2,data.table,
               scales,tidyr,plotly,ggvis,dplyr,RSQLite,
               ggmap,streamR)

# Set up handles to database tables on app start
setwd('/Datasets/openfootball_dataset')
sqlite    <- dbDriver("SQLite")
exampledb <- dbConnect(sqlite,"football.db")

db <- src_sqlite('football.db')

oauth_file<-file.path(getwd(), "sample_oauth.Rdata") 
load(oauth_file)

# get tweets method
get_tweets <- function(tweet_text) {
  
  # remember to run twitter_oauth_setup.R before hand
  
  # gather streaming data using filterstream
  tweets <- filterStream(file.name = "",
                         track = tweet_text,
                         language = "en",
                         timeout = 60,
                         oauth = my_oauth)
  
  #clean tweets
  tweets <- parseTweets(tweets)
  text <- gsub('[^[:graph:]]', ' ', as.character(tweets$text))
  text <- gsub('^ +', '', text)
  text <- gsub(' +$', '', text)
  text <- gsub(' +', ' ', text)
  tweets$text <- text
  
  return(tweets) 
}

# query method
search_teams_leagues <- function(team_league) {
  
  queryResult <- dbGetQuery(exampledb, 
                            sprintf("SELECT * FROM teams,games WHERE title like '%s' AND (teams.id = games.team1_id OR teams.id = games.team2_id)",team_league))
  
  if(nrow(queryResult) <= 0L) { 
    queryResult <- dbGetQuery(exampledb, 
                              sprintf("SELECT * FROM leagues WHERE title like '%s'",team_league))
  }
  
  return(queryResult)
}

plot_teams_time <- function(team_league) {
  
  teamsTimeQuery <- dbGetQuery(exampledb, 
                               sprintf("SELECT CASE 
                                       WHEN winner = '1'  THEN  score1
                                       WHEN winner = '2'  THEN score2
                                       ELSE score1 + score2
                                       END as plotted_score, play_at, pos
                                       FROM (SELECT * FROM teams,games WHERE title like '%s' AND (teams.id = games.team1_id  OR teams.id = games.team2_id)) ORDER BY play_at",team_league))
  
  kmeans_data <- data.frame(teamsTimeQuery$plotted_score,teamsTimeQuery$pos)
  km <- kmeans(kmeans_data, centers = 2, nstart = 100)
  teamsTimeQuery$cluster <- km$cluster
  
  return(teamsTimeQuery)
}

get_ground_info <- function(team_league){
  groundsResult <- dbGetQuery(exampledb, 
                              sprintf("SELECT * FROM grounds,games,teams WHERE teams.title like '%s' 
                                      AND teams.id in (SELECT team1_id from teams) AND
                                      grounds.id = games.ground_id ",team_league))
  
  geocoded_grounds <- na.omit(geocode(groundsResult$title))
  geocoded_grounds$displayText <- paste("Ground Name:",groundsResult$synonyms,"Capacity:",groundsResult$capacity,"Played at:", as.POSIXlt(groundsResult$play_at), "Score:",groundsResult$score1,"-",groundsResult$score2)
  
  return(geocoded_grounds)
}

computeRating <- function(team_league){
  ratingsResult <- dbGetQuery(exampledb, 
                              sprintf("SELECT CASE 
                                      WHEN team1_id = id  THEN  score1
                                      WHEN team2_id = id  THEN score2
                                      ELSE 0
                                      END as score_for, 
                                      CASE 
                                      WHEN team1_id = id  THEN  score2
                                      WHEN team2_id = id  THEN score1
                                      ELSE 0
                                      END as score_against
                                      FROM (SELECT * FROM teams,games WHERE title like '%s' AND (teams.id = games.team1_id  OR teams.id = games.team2_id))",team_league))
  
  if(length(ratingsResult) >= 1) {
    ratingsResultList<-c(sum(ratingsResult$score_for),sum(ratingsResult$score_against),sum(ratingsResult$score_for+ratingsResult$score_against))
    calculatedRating<-(ratingsResultList[1]/ratingsResultList[3])*100
    return(calculatedRating)
  }
  else {
    calculatedRating<-0.001
    return(calculatedRating)
  }
}

shinyServer(function(input,output,session){
  
  output$text <- renderText({
    paste(input$text)
  })
  output$query <- renderText({
    paste(search_teams_leagues(input$text))
  })
   output$tweet_text <- renderPrint({
     tweets_summary <- get_tweets(input$text)
     tweets_summary$text
   })
  output$rating<-renderInfoBox({
    infoBox("Rating", paste(round(computeRating(input$text),digits = 3)), icon = icon("thumbs-up", lib = "glyphicon"), color = "red")
  })
  output$plot1<-renderPlotly({
    plot_ly(data = plot_teams_time(input$text), x = as.POSIXlt(play_at), y = plotted_score, line = list(shape = "linear"))
  })
  output$soccerMap <- renderLeaflet({
    leaflet(get_ground_info(input$text))%>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, radius = 5, weight = 1, color = "#0000ff",
                       fillOpacity = 0.7, popup = ~displayText)
  })
  output$kmeans <- renderPlotly({
    plot_ly(data = plot_teams_time(input$text), x = pos, y = plotted_score, color = factor(cluster), mode = "markers")
  })
})
