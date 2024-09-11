#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Libraries
library(shiny)
library(jpeg)
library(grid)
library(ggnewscale)
library(scales)
library(reshape2)
library(knitr)
library(tidyverse)
library(tidymodels)
library(magrittr)
library(kableExtra)
library(DT)
library(viridis)

source("~/Hockey_Fall_2023/AnalysisCodeFunc.R")

colorPalette <- c("Too Far" = "#A3D237","Mediocre" = "#39D0B0",
                  "Traffic Jam" = "#EA6522","Defender Block" = "#D373C6",
                  "Perfect Shot" = "#008BF8")

shots_stats.df <- readRDS("~/Hockey_Fall_2023/StudentData/shots_stats_goal_clusters2.df.Rds") #now just need to change the labels
xsize <- 2000
ysize <- 850
# Coordinates to the goal pipes
pipes_x <- 1890
lpipe_y <- 395
rpipe_y <- 455
# This file path should contain the hockey rink images and all the sequences
filepath <- '~/Hockey_Fall_2023/FinalGoalShots/'
# Read the rink images and format them to a raster used for graphing
rink_raster <- makeRaster(filepath, 'Rink_Template.jpeg')
half_rink_raster <- makeRaster(filepath, 'Half_Rink_Template.jpeg')

possibleFeats <- c('Puck Speed' = 'puckSpeed','Shooter Speed' = 'shooterSpeed',
  'Shooter Posession Time' = 'posTime','Number of Offensive Players' = 'NumOffense',
  'Number of Defensive Players' = 'NumDefense',
  'Defender Distance' = 'defDist','Defender Angle' = 'defAngle')
#above was in the notebook I took from Amy, not sure if it is actually needed



#Uses a lot of html stuff

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DAR Hockey Data Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(#img(src = "FOCI.png", height = 140, width = 400),
            #selectInput("clust",h3("Select a cluster to plot:"),
            #            choices = list("Too Far" = 1, "Mediocre" = 2,"Perfect Shot" = 3,
            #                           "Defender Block" = 4,"Traffic Jam" = 5) ),
            checkboxGroupInput("clust","Clusters to display:",
                               c("Too Far" = "Too Far", "Mediocre" = "Mediocre","Traffic Jam" = "Traffic Jam",
                                 "Defender Block" = "Defender Block","Perfect Shot" = "Perfect Shot"),selected = 1),
            selectInput("feature", "Select Feature for Size:", choices = possibleFeats),
            selectInput("filtFeature","Select Feature to Filter by", choices = possibleFeats),
            sliderInput("feat_range", "Show Shots with Features in Range:", 
                          min = 0, max = 1000, value = c(0, 1000))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h1("Play Plotter"), 
           div("Use the drop down menu to select clusters to plot, or click on a point to see an individual play in more detail"),
           plotOutput("rink", click = "plot_click"),
           actionButton("rem", "Return to Full Cluster View"),
           htmlOutput("tabName"),
           dataTableOutput("summaryTable"),
           width = 4
        )
    )
)

#The basic works, but now I need to deal with fixing the scaling of possesion time (especially with single shots)
#and adding in the extra detail

server <- function(input, output,session) {
  shots_stats.df <-cbind.data.frame(shots_stats.df,data.frame(x = shotStatX(shots_stats.df),y = shotStatX(shots_stats.df))) 
  #browser()
    #shots <- reactive({
    #  dat <- shots_stats.df[shots_stats.df$Cluster == input$clust,]
      #print(dat)
     # dat
    #  
   # })
  xsize <- 2000
  ysize <- 850
  # Coordinates to the goal pipes daniel is cute
  pipes_x <- 1890
  lpipe_y <- 395
  rpipe_y <- 455
  global_size_range <- range(shots_stats.df$posTime)/6
  # generate bins based on input$bins from ui.R
    values <- reactiveValues()
    shots_stats.df$Cluster <- as.factor(shots_stats.df$Cluster)
    levels(shots_stats.df$Cluster) <- c("Too Far","Mediocre","Traffic Jam","Defender Block","Perfect Shot")
    values$shots <- shots_stats.df
    #shots <- reactive({
    #  shots_stats.df[shots_stats.df$Cluster == input$clust,]
    #}) 
    
    #Code for filtering borrowed from Lieben
    # Observe changes in selected feature to update histogram range
    observeEvent(input$filtFeature, {
      min_val <-  floor(min(shots_stats.df[[input$filtFeature]], na.rm = TRUE))
      max_val <- ceiling(max(shots_stats.df[[input$filtFeature]], na.rm = TRUE))
      updateSliderInput(session, "feat_range", min = min_val, max = max_val, value = c(min_val, max_val))
    })
    
   observeEvent(input$feat_range,{
    #Warning: Error in : Can't subset `.data` outside of a data mask context.   
      values$shots <- shots_stats.df %>%
        filter(Cluster %in% input$clust) %>%
         filter(.data[[input$filtFeature]] >= input$feat_range[1], .data[[input$filtFeature]] <= input$feat_range[2])
    })
    
    observeEvent(input$rem, {
      #values$shots <- shots_stats.df[shots_stats.df$Cluster == input$clust,]
      values$shots <- filter(shots_stats.df, Cluster %in% input$clust)
    })
    
    observeEvent(input$plot_click,{
      values$shots <- values$shots[which.min(abs(input$plot_click$x - values$shots$x)),]
      #print(values$shots)
    })
    
    observeEvent(input$clust,{
      #values$shots <- shots_stats.df[shots_stats.df$Cluster == input$clust,]
      if(length(input$clust) >= 1){
        values$shots <- filter(shots_stats.df, Cluster %in% input$clust)
      }else{
        values$shots <- shots_stats.df[shots_stats.df$Cluster == -1,]
      }
    },ignoreNULL = FALSE)
    
    output$tabName <- renderPrint({
      if(nrow(values$shots) > 1){
        HTML(paste0("<h2>","Averages:","</h2>"))
      }else{
        HTML(paste0("<h2>","Shot Statistics:","</h2>"))
      }
    })
    
    output$summaryTable <- renderDataTable({
      #print(values$shots)
      if(nrow(values$shots) > 1){
        toSum <- cbind.data.frame(values$shots[,'Cluster'],values$shots[,1:10],values$shots[,12:13])
        names <-c('Cluster','Distance to Goal (ft)','Angle of Shot (°)','Puck Speed (mph)','Shooter Speed (mph)','Distance of Goalie from Goal (ft)',
                  'Angle of Goalie (°)','Shooter Posession Time','Number of Offensive Players',
                  'Number of Defensive Players','Is Right Handed?',
                  'Defender Distance (ft)','Defender Angle (°)')
        colnames(toSum) <- names
        toSum <-cbind.data.frame(toSum[,1] ,apply(toSum[,setdiff(names,c('Cluster'))],2,as.numeric))
        colnames(toSum) <- names
        #print(toSum)
        toSum$Cluster <- as.factor(toSum$Cluster)
        levels(toSum$Cluster) <- c("Too Far","Mediocre","Traffic Jam","Defender Block","Perfect Shot")
        tab <- as.data.frame(toSum) %>% group_by(Cluster) %>% summarise_all(.funs = mean)
        
        
        #
        #tab[,"Cluster"] <- c("Too Far","Mediocre","Perfect Shot","Defender Block","Traffic Jam")
        #tab <- as.data.frame(toSum) %>% group_by('values$shots[, "Cluster"]') %>% summarise_all(.funs = mean)
        tab[,"Distance to Goal (ft)"] <- tab[,"Distance to Goal (ft)"]/10
        tab[,"Distance of Goalie from Goal (ft)"] <- tab[,"Distance of Goalie from Goal (ft)"]/10
        tab[,"Is Right Handed?"] <- lapply(tab[,"Is Right Handed?"], as.logical)
        tab[,"Puck Speed (mph)"] <- tab[,"Puck Speed (mph)"]/1.467
        tab[,'Shooter Speed (mph)'] <- tab[,'Shooter Speed (mph)']/1.467
        tab[,'Angle of Goalie (°)'] <--1 *  (tab[,'Angle of Goalie (°)'] - 90) #currently positive is left, do I need to multiply by -1 to flip this?
        tab[,'Angle of Shot (°)'] <--1 *  (tab[,'Angle of Shot (°)'] - 90)
        tab[,'Defender Distance (ft)'] <- tab[,'Defender Distance (ft)']/10
        tab[,'Defender Angle (°)'] <- -1 *  (tab[,'Defender Angle (°)'] - 90)
        #colnames(tab) <- c('Cluster','Distance to Goal (ft)','Angle of Shot (°)','Puck Speed (ft/s)','Shooter Speed (ft/s)','Distance of Goalie from Goal (ft)',
        #                   'Angle of Goalie','Shooter Posession Time','Number of Offensive Players',
        #                   'Number of Defensive Players','Is Right Handed?',
        #                   'Defender Distance','Defender Angle')
        #Chat GPT wrote the datatable code because this is due tonight
        # Identify numeric columns
        numeric_columns <- sapply(tab, is.numeric)
        numericColnames <- names(tab)[numeric_columns]
        
        heatTable <- datatable(tab)
        
        for(col in numericColnames){
          brks <- quantile(tab[[col]], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
            {paste0("rgb(255,", ., ",", ., ")")}
          
          # Apply the style to the datatable
          heatTable <- heatTable %>% formatStyle(col, backgroundColor = styleInterval(brks, clrs)) %>% formatRound(columns = col, digits = 1)
        }
        heatTable
        
      }else{
        #str(values$shots)
        clusts <- as.factor(values$shots$Cluster)
        levels(clusts) <- c("Too Far","Mediocre","Traffic Jam", "Defender Block", "Perfect Shot" )
        tab <-cbind.data.frame(  values$shots[,1:14],clusts)
        tab[,1] <- tab[,1]/10
        tab[,5] <- tab[,5]/10
        tab[,10] <- as.logical(tab[,10])
        colnames(tab) <- c('Distance to Goal (ft)','Angle of Shot (°)','Puck Speed (ft/s)','Shooter Speed (ft/s)','Distance of Goalie from Goal (ft)',
                           'Angle of Goalie','Shooter Posession Time','Number of Offensive Players',
                           'Number of Defensive Players','Is Right Handed?','Closest Defender',
                           'Defender Distance','Defender Angle','Shot Outcome','Cluster')
        # Identify numeric columns
        numeric_columns <- sapply(tab, is.numeric)
        numericColnames <- names(tab)[numeric_columns]
        
        heatTable <- datatable(tab)
        
        for(col in numericColnames){
          brks <- quantile(tab[[col]], probs = seq(.05, .95, .05), na.rm = TRUE)
          clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
            {paste0("rgb(255,", ., ",", ., ")")}
          
          # Apply the style to the datatable
          heatTable <- heatTable  %>% formatRound(columns = col, digits = 1)
        }
        heatTable
        
      }
    })
    
    output$rink <- renderPlot({
      #if(nrow(values$shots) == 0){
      #  ggplot(values$shots) +
      #    ylim(2000, 1000) +
      #    xlim(0,ysize) +
       #   annotation_custom(half_rink_raster, xmin=0, xmax=ysize, ymin=-2000, ymax=-1000) +
      #    theme(axis.title.x = element_blank(),
      #          axis.text.x=element_blank(),
      #          axis.ticks.x=element_blank(),
      #          axis.title.y = element_blank(),
       #         axis.text.y = element_blank(),
      #          axis.ticks.y = element_blank())
      #}else{
      
      ShotType <- values$shots$shotOutcome
      levels(ShotType) <- c('D','G','S','M')
      
      clusts <- values$shots$Cluster
      
      #ChatGPT wrote this code
      labelMapping <- c("Too Far","Mediocre","Traffic Jam", "Defender Block", "Perfect Shot" )
      uniqueIntegers <- unique(clusts)
      factorVector <- factor(clusts,levels =  uniqueIntegers,labels = labelMapping[match(uniqueIntegers, labelMapping)] )
      
      #levels(clusts) <- c("Too Far","Mediocre","Traffic Jam", "Defender Block", "Perfect Shot" )
      
      colorPalette <- c("Too Far" = "#A3D237","Mediocre" = "#39D0B0",
                        "Traffic Jam" = "#EA6522","Defender Block" = "#D373C6",
                        "Perfect Shot" = "#008BF8")
      
      #Going to try and display 
      #angle_means <- shots %>%
        #group_by(puckAngle<=90) %>% 
      #  summarise(mean(goalieAngle), mean(goalieDist)) %>% 
      #  set_names(c('meanAngle', 'meanDist')) %>% 
      #  # Data for graphing
      #  mutate(xstart = ysize / 2) %>% 
      #  mutate(ystart = pipes_x) %>% 
      #  mutate(radius = meanDist)
      
      #Idea: do the arrows out of the goal for goalie angle and distance.
      #For how to divide it: maybe goalie angle average for points on the left side and goalie angle average for points on the right side?
      #Need to rescale the arrows
      if(nrow(values$shots) > 1){


        
        ggplot(values$shots) +
          ylim(2000, 1000) +
          xlim(0,ysize) +
          annotation_custom(half_rink_raster, xmin=0, xmax=ysize, ymin=-2000, ymax=-1000) +
          theme(axis.title.x = element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) + 
          # Graph players colored by handedness
          geom_text(aes(label = ShotType,color = clusts, x = shotStatX(values$shots), 
                        y = shotStatY(values$shots), size = !!sym(input$feature))) +
          scale_color_manual('Cluster', values = colorPalette) +
          scale_size(name = input$feature, range = global_size_range)#+ colors grabbed from https://www.nceas.ucsb.edu/sites/default/files/2022-06/Colorblind%20Safe%20Color%20Schemes.pdf okabe and ito
        #new_scale_color() +
        # Arrow pointing to average goalie angle
        # Length of arrow is average goalie distance
        #geom_spoke(data = angle_means, aes(x = xstart, y = ystart, angle =
        #                                     torad(meanAngle), radius = radius), 
        #           key_glyph = 'abline', linetype = 'solid', arrow = arrow(), linewidth = 1) +
        #scale_color_discrete('Average angle', type = c('red', 'blue'), 
        #labels = c('Left', 'Right')) +
        #labs(x = NULL, y = NULL)
      }else{#Just plot the single point
        defPos <- cbind.data.frame(shotStatX(values$shots) + cos(torad(values$shots$defAngle))*values$shots$defDist,shotStatY(values$shots) + sin(torad(values$shots$defAngle))*values$shots$defDist) 
        colnames(defPos) <- c('x','y')
        
        clusts <- as.factor(values$shots$Cluster)
        levels(clusts) <- c("Too Far","Mediocre","Traffic Jam", "Defender Block", "Perfect Shot" )
        
        angle <-  values$shots %>% mutate(xstart = ysize / 2) %>% 
          mutate(ystart = pipes_x) %>% 
          mutate(rad = goalieDist) %>%
          mutate(ang = goalieAngle)
        
        ggplot(values$shots) +
          ylim(2000, 1000) +
          xlim(0,ysize) +
          annotation_custom(half_rink_raster, xmin=0, xmax=ysize, ymin=-2000, ymax=-1000) +
          theme(axis.title.x = element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) + 
            geom_text(aes(label = ShotType, x = shotStatX(values$shots), color = clusts,
                          y = shotStatY(values$shots), size = !!sym(input$feature))) +
          scale_color_manual('Cluster', values = colorPalette)+ #why isn't this conserving color
          new_scale_color() +
            scale_size(name = 'Selected Feature', range = global_size_range)+
            geom_point(data = defPos,aes(x = x,y=y,color = "Turquoise",))+
            labs(x = NULL, y = NULL)+
            scale_color_discrete('',type = c('turquoise'))+
          geom_spoke(data = angle, aes(x = xstart, y = ystart, angle =
                                         torad(ang), radius = rad), 
                     key_glyph = 'abline', linetype = 'solid', arrow = arrow(), linewidth = 1)
      }
      #}
    })
    #plot click returns x and y, so I can grab the shot that is closest to the click

}

# Run the application 
shinyApp(ui = ui, server = server)
