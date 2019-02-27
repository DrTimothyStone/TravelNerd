setwd('/Users/timothystone/Desktop')

library(shiny)
library(tibble)
allcodes <- read.csv('WikkleCodes - Sheet1.csv', stringsAsFactors = F, 
                     header=F, sep="\t", strip.white=TRUE)
allcodes <- cbind(allcodes, Score = seq(nrow(allcodes), 1, -1))

codevec <- allcodes[,3]
names(codevec) <- allcodes[,2]



colnames(allcodes) <- c("Name", "Code", "Score")
places <- as.tibble(read.csv("PlacesAndCodes - Sheet1.csv", stringsAsFactors = F, header=F, sep="\t", strip.white=T))
colnames(places) <- c("Place", "Codes", "Country", "City", "Visited")
places <- mutate(places, Country = as.factor(Country), City = as.factor(City))

nameslist <- allcodes[,2]
names(nameslist) <- allcodes[,1]   


countries <- places %>% select(Country) %>% sapply(levels)
cities <- places %>% select(City) %>% sapply(levels)
ui <- fluidPage(
  titlePanel("The World's Most Encyclopedic Places!"),
  sidebarLayout(
    sidebarPanel(
      #column(3,
      numericInput(inputId = "num", label="How many themes?", value = 500,
                   min = 1, max = length(codes))
      ,
      #column(4,             
      numericInput(inputId = "listlength", label = "Output list length", 
                   value = 50, min=1, max=500)
      ,
      #column(3,
      selectInput(inputId = "country", label = "Apply filter by country", choices = countries, 
                  multiple = T)
      ,
      #br(),
      
      actionButton(inputId = "go",
                   label="Update"
      )),
    mainPanel(
      #br(),
      #br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Places of Interest", tableOutput("totaltable")),
                  tabPanel("Cities to Visit", tableOutput("cities")),
                  tabPanel("Top Countries", tableOutput("countries")),
                  tabPanel("List of Themes", 
                           sidebarLayout(
                             sidebarPanel(actionButton("selectall", label="Select/Deselect all")
                             ),
                             mainPanel(
                               checkboxGroupInput("themes", "Themes:",
                                                  choices = nameslist,
                                                  selected = allcodes[,2]
                                                  )))),
                  tabPanel("List of Places", tableOutput("places"))
      )
    )
  )
)




server <- function(input, output, session) {
  
  
  data <- eventReactive(input$go, {
    tot <- rep(0, nrow(places))
    codes <- codevec[1:input$num]
    codes <- codes[input$themes]
    
    for (i in 1:nrow(places)) {
      thesecodes <- unlist(strsplit(as.character(places[i,2]), split= "\\s*/\\s*", perl = T))
      #thesecodes <- filter(thesecodes, !(value %in% excludelist))
      #colnames(thesecodes) <- "Code"
      
      tot[i] <- sum(codes[thesecodes], na.rm=T)
    }
    places <- mutate(places, total=tot) %>% arrange(desc(total))
    if (length(input$country) != 0) {
      places <- filter(places, Country %in% input$country)
    }
    places[1:input$listlength,]
  })
  
  data2 <- eventReactive(input$go, {
    tot <- rep(0, nrow(places))
    codes <- codevec[1:input$num]
    
    for (i in 1:nrow(places)) {
      thesecodes <- unlist(strsplit(as.character(places[i,2]), split= "\\s*/\\s*", perl = T))
      #thesecodes <- filter(thesecodes, !(value %in% excludelist))
      #colnames(thesecodes) <- "Code"
      
      tot[i] <- sum(codes[thesecodes], na.rm=T)
    }
    places <- mutate(places, total=tot) %>% arrange(desc(total))
    if (length(input$country) != 0) {
      places <- filter(places, Country %in% input$country)
    }
    places <- mutate(places, total=tot) %>% arrange(desc(total))
    cities <- places %>% 
      filter(City != "x" & City !="") %>% 
      mutate(countryCity = if_else(grepl(": ", City), as.character(Country), 
                                   paste(Country, City, sep=": "))) %>%
      group_by(countryCity) %>% 
      summarize(counts=sum(total)) %>% arrange(desc(counts))
    
    cities
  })
  
  data3 <- eventReactive(input$go, {
    tot <- rep(0, nrow(places))
    codes <- codevec[1:input$num]
    codes <- codes[input$themes]
    
    for (i in 1:nrow(places)) {
      thesecodes <- unlist(strsplit(as.character(places[i,2]), split= "\\s*/\\s*", perl = T))
      #thesecodes <- filter(thesecodes, !(value %in% excludelist))
      #colnames(thesecodes) <- "Code"
      
      tot[i] <- sum(codes[thesecodes], na.rm=T)
    }
    places <- mutate(places, total=tot) %>% arrange(desc(total))
    if (length(input$country) != 0) {
      places <- filter(places, Country %in% input$country)
    }
    places <- mutate(places, total=tot) %>% arrange(desc(total))
    countries <- places %>% 
      filter(Country != "x" & Country !="") %>% 
      group_by(Country) %>% 
      summarize(counts=sum(total)) %>% arrange(desc(counts))
    
    countries
  })
  
  output$totaltable <- renderTable({
    data()
  })
  observe({
    nameslist <- allcodes[,2]
    names(nameslist) <- allcodes[,1]                          
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0) {
        updateCheckboxGroupInput(session=session, inputId="themes", 
                                 choices = nameslist, 
                                 selected =allcodes[,2])
      }
      
    } else {
      updateCheckboxGroupInput(session=session, inputId="themes", 
                               choices = nameslist, 
                               selected = c())
    }
  })
  
  output$themes <- renderTable({
    allcodes[,1:2]
  })
  
  output$cities <- renderTable({
    data2()
  })
  
  output$countries <- renderTable({
    data3()
  })
  
  output$places <- renderTable({
    places
  })
  
  
  }
  
  shinyApp(ui, server)