library(ggplot2)
library(cluster)
library(GGally)
library(ggrepel)
library(knitr)
library(magick)
library(shinybusy)
library(caTools)
library(tidyverse)
library(factoextra)
library(plotly)
library(NbClust)
library(viridis)
library(gdata)
library(dplyr)
library(MASS)
library(gridExtra)
library(grid)
library(lattice)
library(SimilarityMeasures)
library(kmlShape)
library(pracma)
library(data.table)
library(formattable)
library(RCurl)
library(ggpubr)
library(shiny)
library(cowplot)
library(rsconnect)
library(png)
library(dendextend)

######INPUT DEFINITIONS######
#input_Player: player input, treated as target distribution when applcable. Example input: KevinDurant
#input_Player2: player input, treated as competing distribution when applcable. Example input: LebronJames
#graph_type: when a function outputs a graph, graph_type is an input. "loess" outputs a loess plot, anything else outputs a 2D kernel density estimate
#clusterCount: input the desired amount of clusters to be produced

graph_function_xz <- function(input_Player,graph_type){
  fileName <- (paste("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/",input_Player,"Combined.csv",sep=""))
  playerCombined_df <- read.csv(fileName)
  
  playerCombined_df$x_loc <- as.numeric(as.character(playerCombined_df$x_loc))
  playerCombined_df$radius <- as.numeric(as.character(playerCombined_df$radius))
  playerCombined_df$y_loc <- (as.numeric(as.character(playerCombined_df$y_loc)))
  playerCombined_df$id <- as.numeric(as.character(playerCombined_df$id))
  playerCombined_df <- playerCombined_df[playerCombined_df$loess_coords!="loess_coords",]
  playerCombined_df$abs_x <- (playerCombined_df$x_loc-19)
  playerCombined_df$abs_y <- (playerCombined_df$y_loc-25)
  playerCombined_df$loess_coords <- NULL
  playerCombined_df$loess_residuals <- NULL
  playerCombined_df$quad_residuals <- NULL
  playerCombined_df <- na.omit(playerCombined_df)
  if (nrow(playerCombined_df)>10000) {
    playerCombined_df <- playerCombined_df[c(rep(FALSE,24),TRUE), ]
  }
  
  
  attach(playerCombined_df)
  first_df <- aggregate(playerCombined_df, by=list(id), FUN=first, na.rm=TRUE)
  last_df <- aggregate(playerCombined_df, by=list(id), FUN=last, na.rm=TRUE)
  detach(playerCombined_df)
  first_filt_df <- data.frame(id=first_df$id, abs_y=first_df$abs_y, radius=first_df$radius)
  last_filt_df <- data.frame(id=last_df$id, abs_y=last_df$abs_y, radius=last_df$radius)
  filt_df <- merge(first_filt_df, last_filt_df, by="id")
  filt_df$slope <- (as.numeric(as.character(filt_df[,5]))-as.numeric(as.character(filt_df[,3])))/(as.numeric(as.character(filt_df[,4]))-as.numeric(as.character(filt_df[,2])))
  slope_df <- data.frame(id=filt_df$id, slope=filt_df$slope)
  playerCombined_df <- merge(playerCombined_df, slope_df, by="id")
  
  playerCombined_df$sign_indicator <- as.numeric(as.character(playerCombined_df$slope))
  playerCombined_df$sign_indicator <- (playerCombined_df$sign_indicator/abs(playerCombined_df$sign_indicator))*-1
  playerCombined_df$abs_y <- playerCombined_df$abs_y*playerCombined_df$sign_indicator
  
  
  if (graph_type=="loess") {
    
    loessMod <- try(loess(abs_x ~ radius, playerCombined_df, span=0.5), silent=T)
    playerCombined_df$loess_coords <- loessMod$fitted
    playerCombined_df$loess_residuals <- loessMod$residuals
    p1 <- ggplot(playerCombined_df, aes(x=radius, y=abs_x))+geom_smooth(method = "loess", formula = y ~x, span=0.5)+coord_flip()+xlab("z")+ylab("x")
    
  } else {
    
    kdplayer_xz <- playerCombined_df %>%
      with(
        MASS::kde2d(abs_x, radius, n = 101,
                    lims = c(
                      scales::expand_range(range(abs_x), .20),
                      scales::expand_range(range(radius), .20)
                    )
        )
      )
    
    
    kdeplayer_xz_df <- kdplayer_xz %>%
      .[c("x", "y")] %>%
      cross_df() %>%
      rename("abs_x" = "x", "radius" = "y") %>%
      mutate(density = as.vector(kdplayer_xz$z))
    
    kdeplayer_xz_df$density <- (kdeplayer_xz_df$density)*(1000/sum(kdeplayer_xz_df$density))
    p1 <- ggplot(kdeplayer_xz_df,aes(abs_x, radius, fill = density))+
      geom_raster()+scale_fill_viridis(option="inferno")+
      xlab("x")+ylab("z")+
      theme(panel.background = element_blank())
    
  }
  
  
  return(p1)
}   #function to produce xz shot form graphs
graph_function_xy <- function(input_Player,graph_type){
  fileName <- (paste("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/",input_Player,"Combined.csv",sep=""))
  playerCombined_df <- read.csv(fileName)
  
  playerCombined_df$x_loc <- as.numeric(as.character(playerCombined_df$x_loc))
  playerCombined_df$radius <- as.numeric(as.character(playerCombined_df$radius))
  playerCombined_df$y_loc <- (as.numeric(as.character(playerCombined_df$y_loc)))
  playerCombined_df$id <- as.numeric(as.character(playerCombined_df$id))
  playerCombined_df <- playerCombined_df[playerCombined_df$loess_coords!="loess_coords",]
  playerCombined_df$abs_x <- (playerCombined_df$x_loc-19)
  playerCombined_df$abs_y <- (playerCombined_df$y_loc-25)
  playerCombined_df$loess_coords <- NULL
  playerCombined_df$loess_residuals <- NULL
  playerCombined_df$quad_residuals <- NULL
  playerCombined_df <- na.omit(playerCombined_df)
  
  
  attach(playerCombined_df)
  first_df <- aggregate(playerCombined_df, by=list(id), FUN=first, na.rm=TRUE)
  last_df <- aggregate(playerCombined_df, by=list(id), FUN=last, na.rm=TRUE)
  detach(playerCombined_df)
  first_filt_df <- data.frame(id=first_df$id, abs_y=first_df$abs_y, radius=first_df$radius)
  last_filt_df <- data.frame(id=last_df$id, abs_y=last_df$abs_y, radius=last_df$radius)
  filt_df <- merge(first_filt_df, last_filt_df, by="id")
  filt_df$slope <- (as.numeric(as.character(filt_df[,5]))-as.numeric(as.character(filt_df[,3])))/(as.numeric(as.character(filt_df[,4]))-as.numeric(as.character(filt_df[,2])))
  slope_df <- data.frame(id=filt_df$id, slope=filt_df$slope)
  playerCombined_df <- merge(playerCombined_df, slope_df, by="id")
  
  playerCombined_df$sign_indicator <- as.numeric(as.character(playerCombined_df$slope))
  playerCombined_df$sign_indicator <- (playerCombined_df$sign_indicator/abs(playerCombined_df$sign_indicator))*-1
  playerCombined_df$abs_y <- playerCombined_df$abs_y*playerCombined_df$sign_indicator
  
  if (nrow(playerCombined_df)>10000) {
    playerCombined_df <- playerCombined_df[c(rep(FALSE,24),TRUE), ]
  }
  if (graph_type=="loess") {
    
    playerCombined_df$locator <- 1:nrow(playerCombined_df)
    # correct_x <- playerCombined_df[playerCombined_df$abs_x<=0,]
    # incorrect_x <- playerCombined_df[playerCombined_df$abs_x>0,]
    # incorrect_x$abs_x <- incorrect_x$abs_x*-1
    # playerCombined_df <- rbind(correct_x, incorrect_x)
    # playerCombined_df <- playerCombined_df[order(playerCombined_df$locator),]
    # correct_y <- playerCombined_df[playerCombined_df$abs_y<=0,]
    # incorrect_y <- playerCombined_df[playerCombined_df$abs_y>0,]
    # incorrect_y$abs_y <- incorrect_y$abs_y*-1
    # playerCombined_df <- rbind(correct_y, incorrect_y)
    # playerCombined_df <- playerCombined_df[order(playerCombined_df$locator),]
    
    
    playerCombined_df$abs_x <- (playerCombined_df$abs_x)
    # playerCombined_df$abs_y <- abs(playerCombined_df$abs_y)
    playerCombined_df$abs_y <- (playerCombined_df$abs_y)
    
    loessMod <- try(loess(abs_x ~ abs_y, playerCombined_df, span=0.5), silent=T)
    playerCombined_df$loess_coords <- loessMod$fitted
    # playerCombined_df$loess_residuals <- -abs(loessMod$residuals)
    playerCombined_df$loess_residuals <- (loessMod$residuals)
    
    p1 <- ggplot(playerCombined_df, aes(x=abs_y, y=(abs_x)))+geom_smooth(method = "loess", formula = y ~x, span=0.5)+
      xlab("y")+ylab("x")+geom_hline(yintercept=0, linetype="dashed", color="black", size=2)+annotate("text", x = (max(playerCombined_df$abs_y)+min(playerCombined_df$abs_y))/2, y = 0.1, label = "FT Line")+ 
      geom_segment(mapping=aes(x=((max(playerCombined_df$abs_y)+min(playerCombined_df$abs_y))/2), y=min(playerCombined_df$loess_coords), xend=((max(playerCombined_df$abs_y)+min(playerCombined_df$abs_y))/2), yend=min(playerCombined_df$loess_coords)-0.2), arrow=arrow(length = unit(0.04, "npc")), size=1, color="black")+
      scale_y_reverse()+annotate("text", x = (max(playerCombined_df$abs_y)+min(playerCombined_df$abs_y))/2, y = min(playerCombined_df$loess_coords)+0.05, label = "Towards Basket")
    
    
  } else {
    
    kdeplayer_xy <- playerCombined_df %>% 
      with( 
        MASS::kde2d(abs_y, abs_x, n = 101,
                    lims = c(
                      scales::expand_range(range(abs_y), .20),
                      scales::expand_range(range(abs_x), .20)
                    )
        )
      )
    
    kdeplayer_xy_df <- kdeplayer_xy %>% 
      .[c("x", "y")] %>% 
      cross_df() %>% 
      rename("abs_y" = "x", "abs_x" = "y") %>% 
      mutate(density = as.vector(kdeplayer_xy$z))
    
    kdeplayer_xy_df$density <- (kdeplayer_xy_df$density)*(1000/sum(kdeplayer_xy_df$density))
    p1 <- ggplot(kdeplayer_xy_df,aes(abs_y, abs_x, fill = density))+
      geom_raster()+scale_fill_viridis(option="inferno")+
      xlab("y")+ylab("x")+
      theme(panel.background = element_blank())
    
  }
  
  
  return(p1)
}   #function to produce xy shot form graphs

frechet_vec_fun <- function(input_Player,input_Player2){
  fileName1 <- (paste("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/",input_Player,"Combined.csv",sep=""))
  playerCombined_df <- read.csv(fileName1)
  playerCombined_df$x_loc <- as.numeric(as.character(playerCombined_df$x_loc))
  playerCombined_df$radius <- as.numeric(as.character(playerCombined_df$radius))
  playerCombined_df$y_loc <- (as.numeric(as.character(playerCombined_df$y_loc)))
  playerCombined_df$id <- as.numeric(as.character(playerCombined_df$id))
  playerCombined_df <- playerCombined_df[playerCombined_df$loess_coords!="loess_coords",]
  playerCombined_df$abs_x <- (playerCombined_df$x_loc-19)
  playerCombined_df$abs_y <- (playerCombined_df$y_loc-25)
  playerCombined_df$loess_coords <- NULL
  playerCombined_df$loess_residuals <- NULL
  playerCombined_df$quad_residuals <- NULL
  playerCombined_df <- na.omit(playerCombined_df)
  
  
  attach(playerCombined_df)
  first_df <- aggregate(playerCombined_df, by=list(id), FUN=first, na.rm=TRUE)
  last_df <- aggregate(playerCombined_df, by=list(id), FUN=last, na.rm=TRUE)
  detach(playerCombined_df)
  first_filt_df <- data.frame(id=first_df$id, abs_y=first_df$abs_y, radius=first_df$radius)
  last_filt_df <- data.frame(id=last_df$id, abs_y=last_df$abs_y, radius=last_df$radius)
  filt_df <- merge(first_filt_df, last_filt_df, by="id")
  filt_df$slope <- (as.numeric(as.character(filt_df[,5]))-as.numeric(as.character(filt_df[,3])))/(as.numeric(as.character(filt_df[,4]))-as.numeric(as.character(filt_df[,2])))
  slope_df <- data.frame(id=filt_df$id, slope=filt_df$slope)
  playerCombined_df <- merge(playerCombined_df, slope_df, by="id")
  
  playerCombined_df$sign_indicator <- as.numeric(as.character(playerCombined_df$slope))
  playerCombined_df$sign_indicator <- (playerCombined_df$sign_indicator/abs(playerCombined_df$sign_indicator))*-1
  playerCombined_df$abs_y <- playerCombined_df$abs_y*playerCombined_df$sign_indicator
  
  if (nrow(playerCombined_df)>10000) {
    playerCombined_df <- playerCombined_df[c(rep(FALSE,24),TRUE), ]
  }
  fileName1 <- (paste("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/",input_Player2,"Combined.csv",sep=""))
  player2Combined_df <- read.csv(fileName1)
  player2Combined_df$x_loc <- as.numeric(as.character(player2Combined_df$x_loc))
  player2Combined_df$radius <- as.numeric(as.character(player2Combined_df$radius))
  player2Combined_df$y_loc <- (as.numeric(as.character(player2Combined_df$y_loc)))
  player2Combined_df$id <- as.numeric(as.character(player2Combined_df$id))
  player2Combined_df <- player2Combined_df[player2Combined_df$loess_coords!="loess_coords",]
  player2Combined_df$abs_x <- (player2Combined_df$x_loc-19)
  player2Combined_df$abs_y <- (player2Combined_df$y_loc-25)
  player2Combined_df$loess_coords <- NULL
  player2Combined_df$loess_residuals <- NULL
  player2Combined_df$quad_residuals <- NULL
  player2Combined_df <- na.omit(player2Combined_df)
  
  
  attach(player2Combined_df)
  first_df <- aggregate(player2Combined_df, by=list(id), FUN=first, na.rm=TRUE)
  last_df <- aggregate(player2Combined_df, by=list(id), FUN=last, na.rm=TRUE)
  detach(player2Combined_df)
  first_filt_df <- data.frame(id=first_df$id, abs_y=first_df$abs_y, radius=first_df$radius)
  last_filt_df <- data.frame(id=last_df$id, abs_y=last_df$abs_y, radius=last_df$radius)
  filt_df <- merge(first_filt_df, last_filt_df, by="id")
  filt_df$slope <- (as.numeric(as.character(filt_df[,5]))-as.numeric(as.character(filt_df[,3])))/(as.numeric(as.character(filt_df[,4]))-as.numeric(as.character(filt_df[,2])))
  slope_df <- data.frame(id=filt_df$id, slope=filt_df$slope)
  player2Combined_df <- merge(player2Combined_df, slope_df, by="id")
  
  player2Combined_df$sign_indicator <- as.numeric(as.character(player2Combined_df$slope))
  player2Combined_df$sign_indicator <- (player2Combined_df$sign_indicator/abs(player2Combined_df$sign_indicator))*-1
  player2Combined_df$abs_y <- player2Combined_df$abs_y*player2Combined_df$sign_indicator
  
  if (nrow(player2Combined_df)>10000) {
    player2Combined_df <- player2Combined_df[c(rep(FALSE,24),TRUE), ]
  }
  
  save1_df <- playerCombined_df 
  save2_df <- player2Combined_df
  
  loessMod <- try(loess(abs_x ~ radius, playerCombined_df, span=0.5), silent=T)
  playerCombined_df$loess_coords <- loessMod$fitted
  playerCombined_df$loess_residuals <- loessMod$residuals
  
  loessMod <- try(loess(abs_x ~ radius, player2Combined_df, span=0.5), silent=T)
  player2Combined_df$loess_coords <- loessMod$fitted
  player2Combined_df$loess_residuals <- loessMod$residuals
  
  
  # frechet_x <- distFrechet(1:nrow(playerCombined_df),playerCombined_df$loess_coords,1:nrow(player2Combined_df), player2Combined_df$loess_coords, timeScale=0.04, FrechetSumOrMax = "sum")
  # # frechet_z <- distFrechet(1:nrow(playerCombined_df),playerCombined_df$radius,1:nrow(player2Combined_df), player2Combined_df$radius, timeScale=0.04, FrechetSumOrMax = "sum")
  frechet_xz <- distFrechet(playerCombined_df$radius,playerCombined_df$loess_coords,player2Combined_df$radius, player2Combined_df$loess_coords, timeScale=0.1, FrechetSumOrMax = "sum")
  frechet_xz <- frechet_xz/(nrow(player2Combined_df)+nrow(playerCombined_df))
  
  playerCombined_df<- save1_df
  player2Combined_df<- save2_df
  loessMod <- try(loess(abs_x ~ abs_y, playerCombined_df, span=0.5), silent=T)
  playerCombined_df$loess_coords <- loessMod$fitted
  playerCombined_df$loess_residuals <- -abs(loessMod$residuals)
  loessMod <- try(loess(abs_x ~ abs_y, player2Combined_df, span=0.5), silent=T)
  player2Combined_df$loess_coords <- loessMod$fitted
  player2Combined_df$loess_residuals <- -abs(loessMod$residuals)
  
  frechet_xy <- distFrechet(playerCombined_df$abs_y,playerCombined_df$loess_coords,player2Combined_df$abs_y, player2Combined_df$loess_coords, timeScale=0.1, FrechetSumOrMax = "sum")
  frechet_xy <- frechet_xy/(nrow(player2Combined_df)+nrow(playerCombined_df))
  
  playerCombined_df<- save1_df
  player2Combined_df<- save2_df
  loessMod <- try(loess(abs_y ~ radius, playerCombined_df, span=0.5), silent=T)
  playerCombined_df$loess_coords <- loessMod$fitted
  playerCombined_df$loess_residuals <- -abs(loessMod$residuals)
  loessMod <- try(loess(abs_y ~ radius, player2Combined_df, span=0.5), silent=T)
  player2Combined_df$loess_coords <- loessMod$fitted
  player2Combined_df$loess_residuals <- -abs(loessMod$residuals)
  
  frechet_yz <- distFrechet(playerCombined_df$radius,playerCombined_df$loess_coords,player2Combined_df$radius, player2Combined_df$loess_coords, timeScale=0.1, FrechetSumOrMax = "sum")
  frechet_yz <- frechet_yz/(nrow(player2Combined_df)+nrow(playerCombined_df))
  
  frechet_composite <- c(frechet_xz,frechet_xy,frechet_yz)
  frechet_3d_total <- sqrt(sum(frechet_composite^2))
  return(frechet_3d_total)
}  #function to find 3D frechet distance between input_Player and input_Player2
tabular_function3d <- function(input_Player,input_Player2){
  fileName1 <- (paste("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/",input_Player,"Combined.csv",sep=""))
  playerCombined_df <- read.csv(fileName1)
  playerCombined_df$x_loc <- as.numeric(as.character(playerCombined_df$x_loc))
  playerCombined_df$radius <- as.numeric(as.character(playerCombined_df$radius))
  playerCombined_df$y_loc <- (as.numeric(as.character(playerCombined_df$y_loc)))
  playerCombined_df$id <- as.numeric(as.character(playerCombined_df$id))
  playerCombined_df <- playerCombined_df[playerCombined_df$loess_coords!="loess_coords",]
  playerCombined_df$abs_x <- (playerCombined_df$x_loc-19)
  playerCombined_df$abs_y <- (playerCombined_df$y_loc-25)
  playerCombined_df$loess_coords <- NULL
  playerCombined_df$loess_residuals <- NULL
  playerCombined_df$quad_residuals <- NULL
  playerCombined_df <- na.omit(playerCombined_df)


  attach(playerCombined_df)
  first_df <- aggregate(playerCombined_df, by=list(id), FUN=first, na.rm=TRUE)
  last_df <- aggregate(playerCombined_df, by=list(id), FUN=last, na.rm=TRUE)
  detach(playerCombined_df)
  first_filt_df <- data.frame(id=first_df$id, abs_y=first_df$abs_y, radius=first_df$radius)
  last_filt_df <- data.frame(id=last_df$id, abs_y=last_df$abs_y, radius=last_df$radius)
  filt_df <- merge(first_filt_df, last_filt_df, by="id")
  filt_df$slope <- (as.numeric(as.character(filt_df[,5]))-as.numeric(as.character(filt_df[,3])))/(as.numeric(as.character(filt_df[,4]))-as.numeric(as.character(filt_df[,2])))
  slope_df <- data.frame(id=filt_df$id, slope=filt_df$slope)
  playerCombined_df <- merge(playerCombined_df, slope_df, by="id")

  playerCombined_df$sign_indicator <- as.numeric(as.character(playerCombined_df$slope))
  playerCombined_df$sign_indicator <- (playerCombined_df$sign_indicator/abs(playerCombined_df$sign_indicator))*-1
  playerCombined_df$abs_y <- playerCombined_df$abs_y*playerCombined_df$sign_indicator

  if (nrow(playerCombined_df)>10000) {
    playerCombined_df <- playerCombined_df[c(rep(FALSE,24),TRUE), ]
  }
  fileName1 <- (paste("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/",input_Player2,"Combined.csv",sep=""))
  player2Combined_df <- read.csv(fileName1)
  player2Combined_df$x_loc <- as.numeric(as.character(player2Combined_df$x_loc))
  player2Combined_df$radius <- as.numeric(as.character(player2Combined_df$radius))
  player2Combined_df$y_loc <- (as.numeric(as.character(player2Combined_df$y_loc)))
  player2Combined_df$id <- as.numeric(as.character(player2Combined_df$id))
  player2Combined_df <- player2Combined_df[player2Combined_df$loess_coords!="loess_coords",]
  player2Combined_df$abs_x <- (player2Combined_df$x_loc-19)
  player2Combined_df$abs_y <- (player2Combined_df$y_loc-25)
  player2Combined_df$loess_coords <- NULL
  player2Combined_df$loess_residuals <- NULL
  player2Combined_df$quad_residuals <- NULL
  player2Combined_df <- na.omit(player2Combined_df)


  attach(player2Combined_df)
  first_df <- aggregate(player2Combined_df, by=list(id), FUN=first, na.rm=TRUE)
  last_df <- aggregate(player2Combined_df, by=list(id), FUN=last, na.rm=TRUE)
  detach(player2Combined_df)
  first_filt_df <- data.frame(id=first_df$id, abs_y=first_df$abs_y, radius=first_df$radius)
  last_filt_df <- data.frame(id=last_df$id, abs_y=last_df$abs_y, radius=last_df$radius)
  filt_df <- merge(first_filt_df, last_filt_df, by="id")
  filt_df$slope <- (as.numeric(as.character(filt_df[,5]))-as.numeric(as.character(filt_df[,3])))/(as.numeric(as.character(filt_df[,4]))-as.numeric(as.character(filt_df[,2])))
  slope_df <- data.frame(id=filt_df$id, slope=filt_df$slope)
  player2Combined_df <- merge(player2Combined_df, slope_df, by="id")

  player2Combined_df$sign_indicator <- as.numeric(as.character(player2Combined_df$slope))
  player2Combined_df$sign_indicator <- (player2Combined_df$sign_indicator/abs(player2Combined_df$sign_indicator))*-1
  player2Combined_df$abs_y <- player2Combined_df$abs_y*player2Combined_df$sign_indicator

  if (nrow(player2Combined_df)>10000) {
    player2Combined_df <- player2Combined_df[c(rep(FALSE,24),TRUE), ]
  }

  save1_df <- playerCombined_df
  save2_df <- player2Combined_df

  loessMod <- try(loess(abs_x ~ radius, playerCombined_df, span=0.5), silent=T)
  playerCombined_df$loess_coords <- loessMod$fitted
  playerCombined_df$loess_residuals <- loessMod$residuals

  loessMod <- try(loess(abs_x ~ radius, player2Combined_df, span=0.5), silent=T)
  player2Combined_df$loess_coords <- loessMod$fitted
  player2Combined_df$loess_residuals <- loessMod$residuals


  playerCombined_df <- save1_df
  player2Combined_df <- save2_df





  kdplayer_3d <- kde3d(playerCombined_df$abs_x, playerCombined_df$abs_y, playerCombined_df$radius, n = 20)
  kdplayer2_3d <- kde3d(player2Combined_df$abs_x, player2Combined_df$abs_y, player2Combined_df$radius, n = 20)

  kdplayer_3d_ar <-array(kdplayer_3d$d, c(20, 20, 20))
  kdplayer2_3d_ar <-array(kdplayer2_3d$d, c(20, 20, 20))

  kdplayer_3d_ar <- kdplayer_3d_ar/sum(kdplayer_3d_ar)
  kdplayer2_3d_ar <- kdplayer2_3d_ar/sum(kdplayer2_3d_ar)
  
  binDiff <- sum(abs(kdplayer_3d_ar-kdplayer2_3d_ar))

  kld_unsummed <- (kdplayer2_3d_ar*log(kdplayer2_3d_ar/kdplayer_3d_ar))
  kld_unsummed[kld_unsummed == Inf] <- 0
  kld_unsummed[is.nan(kld_unsummed)] <- 0
  kld_unsummed[is.na(kld_unsummed)] <- 0
  kld <- sum(kld_unsummed)


  frechet_distance <- frechet_vec_fun(input_Player, input_Player2)

  similarity_table <- data.frame("Player 1"=input_Player,"Player 2"=input_Player2,"3D Frechet Distance"=frechet_distance,"3D Bin Difference"=binDiff,"3D Kullbach-Leibler Divergence"=kld)
  names(similarity_table)[names(similarity_table) == "X3D.Kullbach.Leibler.Divergence"] <- "3D Kullbach-Leibler Divergence"
  names(similarity_table)[names(similarity_table) == "X3D.Bin.Difference"] <- "3D Bin Difference"
  names(similarity_table)[names(similarity_table) == "X3D.Frechet.Distance"] <- "3D Frechet Distance"
  names(similarity_table)[names(similarity_table) == "Player.1"] <- "Player 1"
  names(similarity_table)[names(similarity_table) == "Player.2"] <- "Player 2"
  final_table <- formattable(final_table)
  return(final_table)
} #outputs a table of all 3 three-dimensional similarity measures between input_player and input_Player2


cluster_plot <- function(clusterCount){
  cluster_plot_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/ClusterCombined.csv")
  toDelete <- seq(0, length(cluster_plot_df), 2)
  cluster_plot_df <-  cluster_plot_df[-toDelete, ]
  cluster_player_vec <- cluster_plot_df$abbrName
  cluster_plot_df$abbrName <- NULL
  cluster_plot_df <- as.data.frame(sapply(cluster_plot_df, as.numeric ))
  cluster_plot_df$abbrName <- cluster_player_vec
  cluster_plot_df <- na.omit(cluster_plot_df)
  row.names(cluster_plot_df) <- cluster_player_vec
  cluster_plot_df<-cluster_plot_df[!(cluster_plot_df$abbrName=="AllPlayers"),]
  distance <- get_dist(cluster_plot_df)
  hc1 <- hclust(distance, method = "average" )
  plot(hc1, cex = 0.6, hang = -1)
  return(rect.hclust(hc1, k = clusterCount, border = 2:5))
} #outputs dendogram of [clusterCount] clusters
cluster_indPlot <- function(clusterCount,graph_type){
  cluster_plot_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NBA_FT_Form/master/ClusterCombined.csv")
  toDelete <- seq(0, length(cluster_plot_df), 2)
  cluster_plot_df <-  cluster_plot_df[-toDelete, ]
  cluster_player_vec <- cluster_plot_df$abbrName
  cluster_plot_df$abbrName <- NULL
  cluster_plot_df <- as.data.frame(sapply(cluster_plot_df, as.numeric ))
  cluster_plot_df$abbrName <- cluster_player_vec
  cluster_plot_df <- na.omit(cluster_plot_df)
  row.names(cluster_plot_df) <- cluster_player_vec
  cluster_plot_df<-cluster_plot_df[!(cluster_plot_df$abbrName=="AllPlayers"),]
  distance <- get_dist(cluster_plot_df)
  hc1 <- hclust(distance, method = "average")
  sub_grp <- cutree(hc1, k = clusterCount)
  sub_grp_df <- as.data.frame(sub_grp)
  sub_grp_df <- tibble::rownames_to_column(sub_grp_df, "abbrName")
  colnames(sub_grp_df)[2] <- "cluster"
  playerCluster_df <- merge(sub_grp_df, cluster_plot_df, by="abbrName")
  
  z_filter <- seq(3, length(playerCluster_df), 3)
  playerCluster_z_df<-  playerCluster_df[, z_filter]
  x_filter <- seq(4, length(playerCluster_df), 3)
  playerCluster_x_df<-  playerCluster_df[, x_filter]
  y_filter <- seq(5, length(playerCluster_df), 3)
  playerCluster_y_df<-  playerCluster_df[, y_filter]
  
  playerCluster_z_df$cluster <- playerCluster_df$cluster
  playerCluster_x_df$cluster <- playerCluster_df$cluster
  playerCluster_y_df$cluster <- playerCluster_df$cluster
  
  z_agg <- aggregate(playerCluster_z_df, by=list(playerCluster_z_df$cluster), FUN=mean, na.rm=TRUE)
  z_agg[,1] <- NULL
  z_agg[,91] <- NULL
  z_agg_t <- as.data.frame(t(z_agg))
  colnames(z_agg_t) <- 1:clusterCount
  
  y_agg <- aggregate(playerCluster_y_df, by=list(playerCluster_y_df$cluster), FUN=mean, na.rm=TRUE)
  y_agg[,1] <- NULL
  y_agg[,91] <- NULL
  y_agg_t <- as.data.frame(t(y_agg))
  colnames(y_agg_t) <- 1:clusterCount
  
  x_agg <- aggregate(playerCluster_x_df, by=list(playerCluster_x_df$cluster), FUN=mean, na.rm=TRUE)
  x_agg[,1] <- NULL
  x_agg[,91] <- NULL
  x_agg_t <- as.data.frame(t(x_agg))
  colnames(x_agg_t) <- 1:clusterCount
  

  ggarrange(, dp, dens, ncol = 2, nrow = 2)
  return(for (clusterId in 1:clusterCount) {
    # indCluster_plot_df <- data.frame(x=x_agg_t[,clusterId],y=y_agg_t[,clusterId],z=z_agg_t[,clusterId])
    # (ggplot(indCluster_plot_df, aes(x=x_agg_t[,clusterId],y=z_agg_t[,clusterId]))+geom_point())
    if (graph_type=="loess") {
      plot(x_agg_t[,clusterId],z_agg_t[,clusterId],main=paste("Cluster",clusterId, sep=" "),axes =FALSE,xlim=c(-2, 2), ylim=c(4, 11.5))
    } else{
      require(grDevices)
      cluster_coord_df <- data.frame(x=x_agg_t[,clusterId],z=z_agg_t[,clusterId])
      kdplayer_xz <- cluster_coord_df %>%
        with(
          MASS::kde2d(x, z, n = 100,
                      lims = c(
                        scales::expand_range(range(x), .20),
                        scales::expand_range(range(z), .20)
                      )
          )
        )
      
      kdeplayer_xz_df <- kdplayer_xz %>%
        .[c("x", "y")] %>%
        cross_df() %>%
        rename("abs_x" = "x", "radius" = "y") %>%
        mutate(density = as.vector(kdplayer_xz$z))
      
      kdeplayer_xz_df$density <- (kdeplayer_xz_df$density)*(1000/sum(kdeplayer_xz_df$density))
      p1 <- ggplot(kdeplayer_xz_df,aes(abs_x, radius, fill = density))+
        geom_raster()+scale_fill_viridis(option="inferno")+
        xlab("x")+ylab("z")+
        theme(panel.background = element_blank())
      print(p1)
    }
    
  })
} #outputs xz shot forms of each cluster
