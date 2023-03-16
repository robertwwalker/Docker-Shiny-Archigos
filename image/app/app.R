#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinydashboard)
library(tidyverse) 
library(magrittr)
library(utf8)
library(shiny)
library(DT)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(patchwork)
# load the data
load("data/archigos.rda")
# create the set of countries
Country.Select <- Archigos %$% 
  table(idacr) %>% 
  data.frame() %>% 
  mutate(Country = idacr) %>% 
  select(Country)
Archigos <- Archigos %>% mutate(tooltip = paste0("Country: ",idacr,"<br> Leader: ",leader,"<br> In: ",as.Date(eindate),"<br> Out: ",as.Date(eoutdate),"<br> Entry: ",entry,"<br> Exit: ",exit, sep=""))
# plot for durations
Plot.Durations <- function(data, state) {
p1a <-  data %>% ggplot(., aes(x=tenureY)) + geom_histogram() + theme_ipsum_rc() + labs(x="Durations", title=paste0("Durations: ",state))
p2a <-  data %>% ggplot(., aes(x=tenureY)) + geom_density() + theme_ipsum_rc() + labs(x="Durations", title=paste0("Durations: ",state))
p1a + p2a
}
# plot for chronology
Plot.Chronology <- function(data, state) {
pl1 <-  data %>% arrange(eindate) %>% 
    ggplot(., 
           aes(x=fct_reorder(leader, eindate), 
               color=leader, text=tooltip)) + 
  geom_errorbar(aes(ymin=eindate, ymax=eoutdate)) + 
  coord_flip() + 
  labs(x="", title=paste0("Leader Chronology: ",state)) + 
  theme_ipsum_rc(base_size = 9) + 
  scale_color_viridis_d(option = "E") + 
  guides(color="none") + labs(x="Leader", y="Date") + 
  scale_y_date()
ggplotly(pl1, tooltip = "text")
}

header <- dashboardHeader(title = "Archigos")
sidebar <-  dashboardSidebar(
  sidebarMenu(selectInput(inputId = "Country", label="Country:", choices = Country.Select$Country, selected="AFG"))
)
body <- dashboardBody(
  tabsetPanel(
    tabItem(tabName = "dashb1",
            title="Durations",
            # Boxes need to be put in a row (or column)
            fluidRow(box(plotOutput("plotDur"), width=12))
    ),
    tabItem(tabName = "dashb2",
            title="Chronology",
            fluidRow(box(plotlyOutput("plotChr"), width=12))
    )),
  fluidRow(DTOutput("plotDT"))
)
ui <- dashboardPage(skin = "purple", header, sidebar, body)

server <- function(input, output) {
  dataset <- reactive({
    Archigos %>% 
      filter(idacr==input$Country) %>% 
      arrange(desc(eoutdate))
  })
  output$plotDT <- renderDT({  dataset()}, options = list(scrollX = TRUE) 
  )
  output$plotChr <- renderPlotly({
    Plot.Chronology(dataset(), input$country)
  })
  output$plotDur <- renderPlot({
    Plot.Durations(dataset(), input$country)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
