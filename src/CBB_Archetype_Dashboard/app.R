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

source("functions.R")

# Set working directory to inside CBB_Archetype_Dashboard

perfOverExpect <- readRDS("perfOverExpect.rds")
offensiveDataMin <- read.csv("offensiveDataMin.csv")
defensiveDataMin <- read.csv("defensiveDataMin.csv")
off_scaled_df <- read.csv("off_scaled_df.csv")
def_scaled_df <- read.csv("def_scaled_df.csv")

# Custom Cluster Names
offensiveClusterNames <- c(
  "Sloppy Foul-Prone Slashers",
  "Balanced Ball Controllers",
  "Average Perimeter Focused",
  "Elite Three Level Offense",
  "Inefficient Launchers",
  "Inefficient Paint Pounders",
  "Efficient Guard-Centric Perimeter",
  "Young Solid Rim Runners"
)

defensiveClusterNames <- c(
  "Paint Protectors",
  "Perimeter Pressers",
  "Well-Rounded Grinders",
  "Perimeter Leakers",
  "Frantic Interior Sieve",
  "Elite Turnover Forcers",
  "Plodding Veterans",
  "Soft Young Interior Pushovers"
)


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
    radioButtons(
      inputId = "teamOrClusterInput",
      label = "Select Filter:",
      choices = c("By Team", "By Cluster"),
      selected = c("By Team")
    ),
    conditionalPanel(
      condition = "input.teamOrClusterInput == 'By Team'",
      selectInput(
        inputId = "teamInput",
        label = "Team:",
        choices = unique(offensiveDataMin$TeamName)
      ),
      selectInput(
        inputId = "seasonInput",
        label = "Season",
        choices = unique(offensiveDataMin$Season)
      )
    ),
    conditionalPanel(
      condition = "input.teamOrClusterInput == 'By Cluster'",
      selectInput(
        inputId = "offArch",
        label = "Offensive Archetype:",
        choices = offensiveClusterNames
      ),
      selectInput(
        inputId = "defArch",
        label = "Defensive Archetype:",
        choices = defensiveClusterNames
      )
    ),
    actionButton("submit", "Submit"),
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
          width = 3,
          fluidRow(
            column(width = 12, h2("Comparable Teams:", align = "center"))
          ),
          fluidRow(
            column(width = 6, h3("Offense:", align = "center")),
            column(width = 6, h3("Defense:", align = "center"))
          ),
          fluidRow(
            column(width = 6,
                div(style = "width: 95%; margin: auto;", DTOutput("similarOffTeamsTable"))
            ),
            column(width = 6,
                   div(style = "width: 95%; margin: auto;", DTOutput("similarDefTeamsTable"))
            )
          )
        ),
        box(
          width = 9,
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
              div(style = "width: 60%; margin: auto;", DTOutput("offenseTable"))
            ),
            column(width = 6,
              div(style = "width: 60%; margin: auto;", DTOutput("defenseTable"))
            )
          ),
          fluidRow(
            column(width = 12, 
                   div(style = "margin-top: 40px; margin-bottom: 40px;", h2("Matchup Analysis", align = "center"))
            )
          ),
          fluidRow(
            column(width = 6,
              div(style = "width: 60%; margin: auto; padding-bottom: 40px;", DTOutput("versusDefTable"))
            ),
            column(width = 6,
              div(style = "width: 60%; margin: auto; padding-bottom: 40px;", DTOutput("versusOffTable"))
            )
          )
        )
      )
    )
  )
)

# Server Functions

setClusterLabels <- function(input, output, offensiveClusterNames, teamOffCluster, defensiveClusterNames, teamDefCluster) {
  output$offensiveLbl <- renderText({
    paste(offensiveClusterNames[teamOffCluster])
  })
  
  output$defensiveLbl <- renderText({
    paste(defensiveClusterNames[teamDefCluster])
  })
}

createSpiderPlots <- function(input, output, teamOffCluster, teamDefCluster, off_scaled_df, def_scaled_df) {
  
  # Produce Spider Plots for offensive and defensive clusters
  output$offensivePlot <- renderPlot({
    makePolyPlot(teamOffCluster, off_scaled_df)
  })
  
  output$defensivePlot <- renderPlot({
    makePolyPlot(teamDefCluster, def_scaled_df)
  })
}

createStatsTables <- function(input, output, teamOffensiveData, off_scaled_df, teamOffCluster,
                              teamDefensiveData, def_scaled_df, teamDefCluster) {
  offense_table_data <- teamOffensiveData %>% 
    pivot_longer(cols = everything(),
                 names_to = "Statistic",
                 values_to = "Value") %>%
    mutate(Value = round(Value, 2))
  
  off_scaled_table_data <- off_scaled_df %>% 
    filter(TeamName == input$teamInput, Season == input$seasonInput) %>% 
    select(-TeamName, -Season, -FullCluster, -X) %>% 
    pivot_longer(cols = everything(),
                 names_to = "Statistic",
                 values_to = "Z_Value") %>% 
    mutate(Z_Value = round(Z_Value, 2))
  
  off_var_df <- makeVarDf(teamOffCluster, off_scaled_df) %>% 
    rename(Statistic = feature, VarRed = variance) %>% 
    mutate(VarRed = round(VarRed, 2))
  
  offense_table_data <- offense_table_data %>%
    left_join(off_scaled_table_data, by = "Statistic") %>%
    left_join(off_var_df, by = "Statistic") %>% 
    arrange(-VarRed)
  
  defense_table_data <- teamDefensiveData %>%
    pivot_longer(cols = everything(), 
                 names_to = "Statistic", 
                 values_to = "Value") %>%
    mutate(Value = round(Value, 2)) 
  
  def_scaled_table_data <- def_scaled_df %>% 
    filter(TeamName == input$teamInput, Season == input$seasonInput) %>% 
    select(-TeamName, -Season, -FullCluster, -X) %>% 
    pivot_longer(cols = everything(),
                 names_to = "Statistic",
                 values_to = "Z_Value") %>% 
    mutate(Z_Value = round(Z_Value, 2))
  
  def_var_df <- makeVarDf(teamDefCluster, def_scaled_df) %>% 
    rename(Statistic = feature, VarRed = variance) %>% 
    mutate(VarRed = round(VarRed, 2))
  
  defense_table_data <- defense_table_data %>%
    left_join(def_scaled_table_data, by = "Statistic") %>%
    left_join(def_var_df, by = "Statistic") %>% 
    arrange(-VarRed)
  
  negated_off_stats <- c("off_BlockPct", "off_StlRate", "off_NSTRate", "off_TOPct")
  negated_def_stats <- c("def_eff_adj",
                         "def_FG2Pct",
                         "def_FG3Pct",
                         "def_FTPct",
                         "def_FG3Rate",
                         "def_ARate",
                         "def_eFGPct",
                         "def_ORPct",
                         "def_FTRate",
                         "Def_1",
                         "Def_2",
                         "Def_3")
  
  offense_table_data <- offense_table_data %>%
    mutate(Z_Value = ifelse(Statistic %in% negated_off_stats, -Z_Value, Z_Value))
  
  defense_table_data <- defense_table_data %>%
    mutate(Z_Value = ifelse(Statistic %in% negated_def_stats, -Z_Value, Z_Value))
  
  cuts <- seq(-2.5, 2.5, length.out = 11)
  colors <- colorRampPalette(c("red", "white", "blue"))(12)
  
  
  output$offenseTable <- renderDT({
    datatable(offense_table_data,
              options = list(pageLength = 30,
                             dom = 't',
                             columnDefs = list(list(visible = FALSE, targets = 2))),
              class = "compact",
              rownames = FALSE) %>%
      formatStyle(
        'Value',
        backgroundColor = styleInterval(
          cuts = cuts,
          values = colors
        ),
        valueColumns = 'Z_Value'  # use Z_Value to drive the coloring
      )
  })
  
  output$defenseTable <- renderDT({
    datatable(defense_table_data,
              options = list(pageLength = 30,
                             dom = 't',
                             columnDefs = list(list(visible = FALSE, targets = 2))),
              class = "compact",
              rownames = FALSE) %>%
      formatStyle(
        'Value',
        backgroundColor = styleInterval(
          cuts = cuts,
          values = colors
        ),
        valueColumns = 'Z_Value'
      )
  })
}

createClusterPoeTables <- function(input, output, perfOverExpect, teamOffCluster,
                                   offensiveClusterNames, teamDefCluster, defensiveClusterNames) {
  
  offVsDefensePoeData <- as.data.frame(t(perfOverExpect[teamOffCluster, ]))
  colnames(offVsDefensePoeData) <- defensiveClusterNames
  
  cuts2 <- seq(-15, 15, length.out = 29)
  colors2 <- colorRampPalette(c("red", "white", "blue"))(30)
  
  offVsDefensePoeTableData <- offVsDefensePoeData %>% 
    pivot_longer(cols = everything(),
                 names_to = "Defensive Cluster",
                 values_to = "POE") %>% 
    mutate(POE = round(POE, 2)) %>% 
    arrange(-POE)
  
  output$versusDefTable <- renderDT({
    datatable(offVsDefensePoeTableData,
              options = list(pageLength = 8, dom = 't'),
              class = "compact",
              rownames = FALSE) %>% 
      formatStyle(
        'POE',
        backgroundColor = styleInterval(
          cuts = cuts2,
          values = colors2
        ),
        valueColumns = 'POE'
      )
  })
  
  defVsOffensePoeData <- as.data.frame(t(perfOverExpect[, teamDefCluster]))
  colnames(defVsOffensePoeData) <- offensiveClusterNames
  
  defVsOffensePoeTableData <- defVsOffensePoeData %>% 
    pivot_longer(cols = everything(),
                 names_to = "Offensive Cluster",
                 values_to = "POE") %>% 
    mutate(POE = round(-POE, 2)) %>% 
    arrange(-POE)
  
  output$versusOffTable <- renderDT({
    datatable(defVsOffensePoeTableData,
              options = list(pageLength = 8, dom = 't'), 
              class = "compact",
              rownames = FALSE) %>% 
      formatStyle(
        'POE',
        backgroundColor = styleInterval(
          cuts = cuts2,
          values = colors2
        ),
        valueColumns = 'POE'
      )
  })
}

createSimilarTeamsTables <- function(input, output, offensiveDataMin, teamOffCluster, defensiveDataMin, teamDefCluster) {
  offClusterTeamsData <- offensiveDataMin %>% 
    filter(FullCluster == teamOffCluster) %>% 
    select(TeamName, Season) %>% 
    arrange(-Season)
  
  output$similarOffTeamsTable <- renderDT({
    datatable(offClusterTeamsData,
              options = list(pageLength = 150, dom = 't'),
              class = "compact",
              rownames = FALSE)
  })
  
  defClusterTeamsData <- defensiveDataMin %>% 
    filter(FullCluster == teamDefCluster) %>% 
    select(TeamName, Season) %>% 
    arrange(-Season)
  
  output$similarDefTeamsTable <- renderDT({
    datatable(defClusterTeamsData,
              options = list(pageLength = 150, dom = 't'),
              class = "compact",
              rownames = FALSE)
  })
}

clusterNameToClusterInd <- function(clusterName, namesVector) {
  return(which(namesVector == clusterName))
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  trigger_submit <- reactiveVal(TRUE)
  
  observeEvent(input$teamInput, {
    if(input$teamOrClusterInput == "By Team") {
      team_seasons <- offensiveDataMin %>% 
        filter(TeamName == input$teamInput) %>% 
        pull(Season) %>% 
        unique() %>% 
        sort()
      
      updateSelectInput(
        session,
        inputId = "seasonInput",
        choices = team_seasons,
        selected = team_seasons[1]
      )
    }
  })
  
  observeEvent({
    input$submit
    trigger_submit()
    }, {
    if(input$teamOrClusterInput == "By Team") {
      
      req(input$teamInput, input$seasonInput)
      
      teamOffensiveData <- offensiveDataMin %>% 
        filter(TeamName == input$teamInput, Season == input$seasonInput)
      
      teamDefensiveData <- defensiveDataMin %>% 
        filter(TeamName == input$teamInput, Season == input$seasonInput)
      
      teamOffCluster <- unique(teamOffensiveData$FullCluster)
      
      teamDefCluster <- unique(teamDefensiveData$FullCluster)
      
      teamOffensiveData <- teamOffensiveData  %>% 
        select(-TeamName, -Season, -FullCluster, -X)
      
      teamDefensiveData <- teamDefensiveData  %>% 
        select(-TeamName, -Season, -FullCluster, -X)
      
      # Set Dashboard Labels of clusters
      setClusterLabels(input, output, offensiveClusterNames, teamOffCluster, defensiveClusterNames, teamDefCluster)
      
      # Produce Spider Plots for offensive and defensive clusters
      createSpiderPlots(input, output, teamOffCluster, teamDefCluster, off_scaled_df, def_scaled_df)
      
      # Produce Tables Featuring Team Offensive / Defensive Stats
      createStatsTables(input, output, teamOffensiveData, off_scaled_df, teamOffCluster, teamDefensiveData, def_scaled_df, teamDefCluster)
      
      # Create Cluster POE Tables
      createClusterPoeTables(input, output, perfOverExpect, teamOffCluster, offensiveClusterNames, teamDefCluster, defensiveClusterNames)
      
      # Create Similar Teams Tables
      createSimilarTeamsTables(input, output, offensiveDataMin, teamOffCluster, defensiveDataMin, teamDefCluster)
      
    } else if (input$teamOrClusterInput == "By Cluster") {
      req(input$offArch, input$defArch)
      
      teamOffCluster <- clusterNameToClusterInd(input$offArch, offensiveClusterNames)
      
      teamDefCluster <- clusterNameToClusterInd(input$defArch, defensiveClusterNames)
      
      teamOffensiveData <- offensiveDataMin %>% 
        filter(FullCluster == teamOffCluster) %>% 
        group_by(FullCluster) %>% 
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>% 
        select(-FullCluster, -Season)
      
      teamDefensiveData <- defensiveDataMin %>% 
        filter(FullCluster == teamOffCluster) %>% 
        group_by(FullCluster) %>% 
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>% 
        select(-FullCluster, -Season)
      
      # Set Dashboard Labels of clusters
      setClusterLabels(input, output, offensiveClusterNames, teamOffCluster, defensiveClusterNames, teamDefCluster)
      
      # Produce Spider Plots for offensive and defensive clusters
      createSpiderPlots(input, output, teamOffCluster, teamDefCluster, off_scaled_df, def_scaled_df)
      
      # Produce Tables Featuring Team Offensive / Defensive Stats
      createStatsTables(input, output, teamOffensiveData, off_scaled_df, teamOffCluster, teamDefensiveData, def_scaled_df, teamDefCluster)
      
      # Create Cluster POE Tables
      createClusterPoeTables(input, output, perfOverExpect, teamOffCluster, offensiveClusterNames, teamDefCluster, defensiveClusterNames)
      
      # Create Similar Teams Tables
      createSimilarTeamsTables(input, output, offensiveDataMin, teamOffCluster, defensiveDataMin, teamDefCluster)
      
    }
    
    trigger_submit(FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
