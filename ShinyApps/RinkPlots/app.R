library(shiny)
library(ggplot2)
library(jpeg)
library(grid)
library(ggnewscale)
library(scales)
library(reshape2)
library(dplyr)

library(knitr)
library(tidyverse)
library(tidymodels)
library(magrittr)


# All user-defined functions are contained in the following helper script file. 
source("~/Hockey_Fall_2023/AnalysisCodeFunc.R")

# Size of rink image and of all plots
xsize <- 2000
ysize <- 850

# FPS of the video
fps <- 29.97

# Coordinates to the goal pipes
pipes_x <- 1890
lpipe_y <- 395
rpipe_y <- 455

#read in the data frames
shots_stats_goal.df <- readRDS("~/Hockey_Fall_2023/StudentData/shots_stats_goal.df.Rds")
shots_stats_cat <- readRDS("~/Hockey_Fall_2023/StudentData/UpdatedFactoredDiscretizedData.Rds")

# Read the rink images and format them to a raster used for graphing
filepath <- '~/Hockey_Fall_2023/FinalGoalShots/'
rink_raster <- makeRaster(filepath, 'Rink_Template.jpeg')
half_rink_raster <- makeRaster(filepath, 'Half_Rink_Template.jpeg')



ui <- fluidPage(
  titlePanel("Rink Displays"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting a feature for color
      selectInput("feature1", "Select Feature for Color:", choices = colnames(shots_stats_cat[c(17, 18, 19, 20, 21, 22, 23, 24, 25)])),
      
      # Dropdown for selecting a feature for shape
      selectInput("feature2", "Select Feature for Shape:", choices = colnames(shots_stats_cat[c(17, 18, 19, 20, 21, 22, 23, 24, 25)])),
      
      # Dropdown for selecting a feature for size
      selectInput("feature3", "Select Feature for Size:", choices = colnames(shots_stats_cat[c(17, 18, 19, 20, 21, 22, 23, 24, 25)])),
      
      
    ),
    
    mainPanel(
      plotOutput("dynamicPlot"),
      width = 4
    )
  )
)


server <- function(input, output, session) {
  
  
  # Generate dynamic plots based on user selection
  output$dynamicPlot <- renderPlot({
    
    # Base for rink visuals
    rink <- geom_point(aes(x = shotStatX(shots_stats_cat), y = shotStatY(shots_stats_cat), 
                           color = !!sym(input$feature1), shape = !!sym(input$feature2), size = !!sym(input$feature3)))
    
    
    # Graph players with features highlighted by color, shape, and size
    halfRinkGraph(shots_stats_cat) + rink + facet_wrap(~shotOutcome) +
      labs(color = input$feature1, shape = input$feature2, size = input$feature3)
    
    
  })
  
}

shinyApp(ui, server)