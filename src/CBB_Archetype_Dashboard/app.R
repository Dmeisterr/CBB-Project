#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)

perfOverExpect <- readRDS("perfOverExpect.rds")
offensiveDataMin <- read.csv("offensiveDataMin.csv")
defensiveDataMin <- read.csv("defensiveDataMin.csv")
off_scaled_df <- read.csv("off_scaled_df.csv")
def_scaled_df <- read.csv("def_scaled_df.csv")


## Credit: https://towardsdatascience.com/how-to-make-a-spider-chart-in-r-using-ggplot2-85a4f1898cab/

ggspider <- function(p_data, legend_title = "Group", background_color = "gray97", 
                     area_fill = TRUE, central_distance = 0.2, axis_name_offset = 0.1, grid.min = 0, grid.max = 1){
  
  circle_coords <- function(r, n_axis = ncol(p_data) - 1){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r)
  }
  
  (step_1 <- map_df(seq(0, 1, 0.25) + central_distance, circle_coords) %>%
      ggplot(aes(x, y)) +
      geom_polygon(data = circle_coords(1 + central_distance), alpha = 1, fill = background_color) +
      geom_path(aes(group = r), lty = 2, alpha = 0.5) +
      theme_void())
  
  axis_coords <- function(n_axis){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)
    
    tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
  }
  
  text_data <- p_data %>%
    select(-group) %>%
    map_df(~ grid.min + (grid.max - grid.min) * seq(0, 1, 0.25)) %>%
    mutate(r = seq(0, 1, 0.25)) %>%
    pivot_longer(-r, names_to = "parameter", values_to = "value")
  
  text_coords <- function(r, n_axis = ncol(p_data) - 1){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r = r - central_distance)
  }
  
  labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
    bind_cols(text_data %>% select(-r))
  
  labels_data_top_axis <- labels_data %>%
    filter(parameter == names(p_data)[2])
  
  rescaled_coords <- function(r, n_axis){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    tibble(r, fi) %>% mutate(x = r*cos(fi), y = r*sin(fi)) %>% select(-fi)
  }
  
  rescaled_data <- p_data %>% 
    mutate(across(-group, ~ (.x - grid.min) / (grid.max - grid.min))) %>%
    mutate(copy = pull(., 2)) %>%
    pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
    group_by(group) %>%
    mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
    unnest()
  
  step_1 + 
    geom_line(data = axis_coords(ncol(p_data) - 1), 
              aes(x, y, group = id), alpha = 0.3) +
    geom_point(data = rescaled_data, 
               aes(x, y, group = group, col = group), size = 3) +
    geom_path(data = rescaled_data, 
              aes(x, y, group = group, col = group), size = 1) +
    {if(area_fill == TRUE) geom_polygon(data = rescaled_data, 
                                        aes(x, y, group = group, 
                                            col = group, fill = group), 
                                        size = 1, alpha = 0.05, show.legend = FALSE)} +
    geom_text(data = labels_data_top_axis, 
              aes(x, y, label = value), alpha = 1, size = 5) +
    geom_text(data = text_coords(1 + central_distance + axis_name_offset), 
              aes(x, y), label = labels_data$parameter[1:(ncol(p_data)-1)]) +
    labs(col = legend_title) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)) +
    coord_fixed()
}

makePolyPlot <- function(clusterN, scaled_df) {
  radarData <- makeCompTable(clusterN, scaled_df) %>% 
    filter(Type %in% c(paste("Cluster", clusterN), paste("Non Cluster", clusterN))) %>% 
    rename(group = Type)
  
  spiderPlot <- ggspider(radarData, grid.min = -2, grid.max = 2)
  
  return(spiderPlot)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    selectInput(
      inputId = "teamInput",
      label = "Team:",
      choices = c('Alabama', 'Arizona', 'Auburn')
    ),
    selectInput(
      inputId = "seasonInput",
      label = "Season",
      choices = c('2019-20', '2020-21', '2021-22', '2022-23', '2023-24')
    ),
    selectInput(
      inputId = "offArch",
      label = "Offensive Archetype:",
      choices = c(1, 2, 3, 4, 5, 6, 7, 8)
    ),
    selectInput(
      inputId = "defArch",
      label = "Offensive Archetype:",
      choices = c(1, 2, 3, 4, 5, 6, 7, 8)
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overviewTab", icon = NULL,
        menuSubItem("CBB Archetype Dashboard", tabName = "cbbArchetypeDash")         
      )
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .content-wrapper {
        background-color: white;
        overflow-y: auto;
      }
      
      .content-wrapper, .right-side {
        min-height: 100vh !important;
      }
      
      .box {
        padding: 0px;
        margin: 0px;
        border: none !important;
        box-shadow: none !important;
      }
                    
                    
    ")),
    tabItems(
      tabItem(
        tabName = "overviewTab",
        h2("Overview")
      ),
      tabItem(
        tabName = "cbbArchetypeDash",
        h2("CBB Archetype Dashboard"),
        box(
          width = 2,
          fluidRow(
            column(width = 12, h4("Similar Teams:"))
          ),
          fluidRow(
            column(width = 12,
                div(style = "width: 90%; margin: auto;", DTOutput("similarTeamsTable"))
            )
          )
        ),
        box(
          width = 10,
          fluidRow(
            column(width = 6, h2(textOutput("offensiveLbl"), align = "center")),
            column(width = 6, h2(textOutput("defensiveLbl"), align = "center"))
          ),
          fluidRow(
            column(width = 6,
                plotOutput("offensivePlot")
            ),
            column(width = 6,
                plotOutput("defensivePlot")
            )
          ),
          fluidRow(
            column(width = 6, 
                   div(style = "margin-top: 40px; margin-bottom: 40px;", h2("Offensive Statistics", align = "center"))
            ),
            column(width = 6, 
                   div(style = "margin-top: 40px; margin-bottom: 40px;", h2("Defensive Statistics", align = "center"))
            )
          ),
          fluidRow(
            column(width = 6,
              div(style = "width: 70%; margin: auto;", DTOutput("offenseTable"))
            ),
            column(width = 6,
              div(style = "width: 70%; margin: auto;", DTOutput("defenseTable"))
            )
          ),
          fluidRow(
            column(width = 12, 
                   div(style = "margin-top: 40px; margin-bottom: 40px;", h2("Matchup Analysis", align = "center"))
            )
          ),
          fluidRow(
            column(width = 6,
              div(style = "width: 70%; margin: auto;", DTOutput("versusDefTable"))
            ),
            column(width = 6,
              div(style = "width: 70%; margin: auto;", DTOutput("versusOffTable"))
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$similarTeamsTable <- renderDT({
    datatable(mtcars[,1:2], options = list(pageLength = 30, dom = 't'), class = "compact")
  })
  
  output$offensiveLbl <- renderText({
    paste("Offensive Cluster 1")
  })
  
  output$defensiveLbl <- renderText({
    paste("Defensive Cluster 1")
  })
  
  output$offensivePlot <- renderPlot({
    makePolyPlot(1, off_scaled_df)
  })
  
  output$defensivePlot <- renderPlot({
    makePolyPlot(1, def_scaled_df)
  })
  
  output$offenseTable <- renderDT({
    datatable(mtcars[,1:3], options = list(pageLength = 5, dom = 't'), class = "compact")
  })
  
  output$defenseTable <- renderDT({
    datatable(iris[,1:3], options = list(pageLength = 5, dom = 't'), class = "compact")
  })
  
  output$versusDefTable <- renderDT({
    datatable(mtcars[1:3], options = list(pageLength = 5, dom = 't'), class = "compact")
  })
  
  output$versusOffTable <- renderDT({
    datatable(iris[1:3], options = list(pageLength = 5, dom = 't'), class = "compact")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
