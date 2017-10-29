library(shiny)
library(rvest)
library(xml2)
library(reshape2)
library(highcharter)
library(shinysky)

##########Scrape movie titles##########
wwAlltime <- read_html("http://www.boxofficemojo.com/alltime/world/")

#Extract worldwide, all time table
wwAlltimeTable <- wwAlltime %>%
  html_nodes("table")%>%
  .[[3]] %>%
  html_table()

#Remove first row which contains headings 
wwAlltimeTable <- wwAlltimeTable[-1,]

#Clean data
colnames(wwAlltimeTable) <- c("Rank",
                              "Title",
                              "Studio",
                              "Worldwide",
                              "DomesticVal",
                              "DomesticPer",
                              "OverseasVal",
                              "OverseasPer",
                              "Year"
)

urls <- wwAlltime %>%
  html_nodes("table")%>%
  .[[3]] %>%
  html_nodes("tr") %>%
  html_nodes("a") %>%
  html_attr("href")

#Delete header links
urls <- urls[-c(1:9)]

#Extract movie ids
movieIds <- substr(urls, start = 13, stop = nchar(urls)-4)

wwAlltimeTable$movieIds <- movieIds

####Current year top grosses
year <- format(Sys.time(), "%Y")
yearURL <- sprintf("http://www.boxofficemojo.com/yearly/chart/?yr=%s&p=.htm", year)
yearTableNode <-  read_html(yearURL) %>%
  html_nodes("table")%>%
  .[[7]] 

yearTitles <- html_nodes(yearTableNode, "tr") %>%
  html_nodes(xpath="//a[contains(@href, 'movies')]") %>%
  html_text()

yearLinks <- html_nodes(yearTableNode, "tr") %>%
  html_nodes(xpath="//a[contains(@href, 'movies')]") %>%
  html_attr("href")

yearMovieID <- substr(x = yearLinks, start = 13, stop = nchar(yearLinks)-4)

yearTable <- data.frame(yearTitles, yearMovieID)
yearTable <- yearTable[-1,]
colnames(yearTable) <- c("Title", "movieIds")

wwMap <- data.frame(wwAlltimeTable$Title, wwAlltimeTable$movieIds)
colnames(wwMap) <- c("Title", "movieIds")

fullMap <- rbind(wwMap, yearTable)
fullMap$Title <- as.character(fullMap$Title)
fullMap$movieIds <- as.character(fullMap$movieIds)

fullMap <- dplyr::arrange(fullMap, Title)

fullMap <- fullMap[!duplicated(fullMap),]

#mapStuff <- download_map_data("custom/world-palestine-lowres")

ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  busyIndicator(),
  titlePanel("Box Office Comparison Tool"),
  column(12,
  fluidRow(selectInput(inputId = "movieSelect", label = "Select a Movie", choices = c(fullMap$Title), multiple = TRUE)),
  fluidRow(radioButtons("graphType", label = "Type", choices = c("Cumulative", "Daily"),inline = TRUE)),
  fluidRow(actionButton("graphButton", "Graph")),
  
  br(),
  
  fluidRow(
    tabsetPanel(type = "pills",
    tabPanel("Movie Gross", highchartOutput("boxOfficeChart", width = "70%")),
    tabPanel("Gross Map", 
             fluidRow(uiOutput("mapList")),
             fluidRow(highchartOutput("map"))
)
)
)
)
)

server <- function(input, output, session){
  
  boxOfficeReciepts <- eventReactive(input$graphButton, {
    movie <- input$movieSelect
    
    
    recieptsTableFull <- data.frame()
    for (i in movie){
    mov <- dplyr::filter(fullMap, Title == i)
    #Build link
    mov <- mov$movieIds
    dailyURL <- sprintf("http://www.boxofficemojo.com/movies/?page=daily&view=chart&id=%s.htm", mov)
    
    
    tableSucceed <- FALSE
    #Scrape reciepts table
    while(tableSucceed == FALSE){
    recieptsTable <-  read_html(dailyURL) %>%
      html_nodes("table")%>%
      .[[8]] %>%
      html_table(header = TRUE, fill = TRUE)
    if(ncol(recieptsTable) == 10){
      tableSucceed <- TRUE
    }else if(ncol(recieptsTable) > 10){
      recieptsTable <-  read_html(dailyURL) %>%
        html_nodes("table")%>%
        .[[9]] %>%
        html_table(header = TRUE, fill = TRUE)
      tableSucceed <- TRUE
    }
    
    }
    
    
    #Clean data
    recieptsTable <- na.omit(recieptsTable)
    recieptsTable$`Gross-to-Date` <- sub(x = recieptsTable$`Gross-to-Date`, "\\$", replacement = "")
    recieptsTable$`Gross-to-Date` <- gsub(x = recieptsTable$`Gross-to-Date`, ",", replacement = "" )
    recieptsTable$`Gross-to-Date` <- as.numeric(recieptsTable$`Gross-to-Date`)
    recieptsTable$Gross <- sub(x = recieptsTable$Gross, "\\$", replacement = "")
    recieptsTable$Gross <- gsub(x = recieptsTable$Gross, ",", replacement = "" )
    recieptsTable$Gross <- as.numeric(recieptsTable$Gross)
    
    
    if(input$graphType == "Cumulative"){
    colnames(recieptsTable) <- c("Day",
                                 "Date",
                                 "Rank",
                                 "Gross",
                                 "dayChange",
                                 "weekChange",
                                 "theaterAvgNum",
                                 "theaterAvgAmnt",
                                 i,
                                 "dayNum"
    )
    recieptsTable[[i]] <- as.numeric(recieptsTable[[i]])
    recieptsmelt <- melt(data = recieptsTable[,c(10,9)], id.vars = c("dayNum"))
    recieptsTableFull <- rbind(recieptsTableFull, recieptsmelt)
    }else{
      colnames(recieptsTable) <- c("Day",
                                   "Date",
                                   "Rank",
                                   i,
                                   "dayChange",
                                   "weekChange",
                                   "theaterAvgNum",
                                   "theaterAvgAmnt",
                                   "cumGross",
                                   "dayNum"
      )
      recieptsTable[[i]] <- as.numeric(recieptsTable[[i]])
      recieptsmelt <- melt(data = recieptsTable[,c(10,4)], id.vars = c("dayNum"))
      recieptsTableFull <- rbind(recieptsTableFull, recieptsmelt)
    }
    
    }
    
    recieptsTableFull
  })
  


output$boxOfficeChart <- renderHighchart({
  
  recieptsTable <- boxOfficeReciepts()
  hchart(recieptsTable, "line", hcaes(x = dayNum, y = value, group = variable))%>%
    hc_xAxis(title = list(text = "Day #"))%>%
    hc_yAxis(title = list(text = "Cumulative Gross"))%>%
    hc_title(text = "Domestic Cumulative Box Office Gross")%>%
    hc_chart(zoomType = "x")
})


output$mapList <- renderUI({
  selectInput("mapMovie", label = "Select a Movie", choices = input$movieSelect)
})

mapData <- reactive({
  movie <- dplyr::filter(fullMap, Title == input$mapMovie)
  id <- movie$movieIds
  
  url <- sprintf("http://www.boxofficemojo.com/movies/?page=intl&id=%s.htm", id)
  
  mapHTML <- read_html(url)
  
  mapTable <- mapHTML %>%
    html_nodes("table")%>%
    .[[8]] %>%
    html_table()
  
  mapTable <- mapTable[-c(1:3),]
  colnames(mapTable) <- c("name", "dist", "releaseDate",
                          "openWeekend", "totalPerc", "totalGross", "latest")
  
  mapTable$totalGross <- sub(x = mapTable$totalGross, "\\$", replacement = "")
  mapTable$totalGross <- gsub(x = mapTable$totalGross, ",", replacement = "" )
  mapTable$totalGross <- as.numeric(mapTable$totalGross)
  mapTable
  
})

output$map <- renderHighchart({
  mapDat <- mapData()
  
  #mapdat <- get_data_from_map(download_map_data("custom/world-palestine-lowres"))
  
  hcmap("custom/world-palestine-lowres", data = mapDat, value = "totalGross",
        joinBy = "name",
        dataLabels = list(enabled = TRUE, format = '{point.name}'),
        borderColor = "black", borderWidth = 0.5,
        tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD"))%>%
    hc_mapNavigation(enabled = TRUE) 
  
})

}

shinyApp(ui = ui, server = server)