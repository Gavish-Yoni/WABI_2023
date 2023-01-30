################################################################################
###                                                                          ###
###----- NAME: --------------------------------------------------------------###
###                                                                          ###
### Plot_WABI_Panel.R                                                        ###
###                                                                          ###
###                                                                          ###
###----- AIM: ---------------------------------------------------------------###
###                                                                          ###
### Plot for a selected WABI a traffic light panel with the colored bands.   ###
### The panels are used in the WABI panel of figure 1.                       ###
###                                                                          ###
###----- INPUT --------------------------------------------------------------###
###                                                                          ###
### For each WABI, ranges of bands are entered manually in the codes.        ###
###                                                                          ###
### DoHorizontal  -  T/F Control.                                            ### 
###   If T - Facet text is at the top and is suitable for a horizontal WABI  ###
###          panel that appears on the side of the map in figure 1.          ###
###   If F - Facet text is at the right side and is suitable for a vertical  ###
###          WABI panel that appears below the map in figure 1.              ###
###                                                                          ###
###----- PROCESS: -----------------------------------------------------------###
###                                                                          ###
### 1.  Packages.                                                            ###                                         
### 2.  Directories.                                                         ###                                         
### 3.  Controls.                                                            ###                                        
###   3.1.  General controls.                                                ###
###   3.2.  Select WABI to plot.                                             ###
###   3.3.  Controls to console.                                             ###
###   3.4.  Controls to file.                                                ###
### 4.  Wabi == "Generation".                                                ###                                        
### 5.  Wabi == "Collection".                                                ###                                         
### 6.  Wabi == "QualityCollection".                                         ###                                        
### 7.  Wabi == "Treatment".                                                 ###                                    
### 8.  Wabi == "QualityTreatment".                                          ###                                        
### 9.  Plot.                                                                ###                         
###   9.1.  Start plot.                                                      ###
###   9.2.  Background traffic light shades.                                 ###
###   9.3.  Scales, faceting and theme.                                      ###
### 10.  Write to console or file.                                           ###
###                                                                          ###
###----- OUTPUT: ------------------------------------------------------------###
###                                                                          ###
### 1.  A figure 1 panels, with the name of the WABI as facet, the range of  ###
###     the index in Y, and bands of traffic light colors. Location of text  ###
###     is on top if DoHorizontal == T, or on the side if DoHorizontal == F. ###
###                                                                          ###
###----- NOTES --------------------------------------------------------------###
###                                                                          ###
### 1.                                                                       ###
###                                                                          ###
###----- VERSIONS -----------------------------------------------------------###
###                                                                          ###
### 1.  14 Feb 2022 - original code                                          ###
###                                                                          ###
###----- DETAILS ------------------------------------------------------------###
###                                                                          ###
### 1. Author:      Gavish Yoni <gavishyoni@gmail.com>                       ###
###                                                                          ###
################################################################################


###--------------------------------------------------------------------------###
### 1.  Packages.                                         
###--------------------------------------------------------------------------###


library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(extrafont)           # for pdf output


###--------------------------------------------------------------------------###
### 2.  Directories.                                         
###--------------------------------------------------------------------------###


Dir1 <- "./Analysis/Figures/Figure1_SidePanel"


###--------------------------------------------------------------------------###
### 3.  Controls.                                         
###--------------------------------------------------------------------------###


###--------------------------------------------------------------------------###
### 3.1.  General controls.

ToFile <- T
#FormOut <- c(".png", ".pdf")     # formats of output files
FormOut  <- ".png"                # formats of output files

DoTitleY <- F
DoHorizontal <- F

###--------------------------------------------------------------------------###
### 3.2.  Select WABI to plot.
 
Wabi <- "Generation"
Wabi <- "Collection"
Wabi <- "QualityCollection"
Wabi <- "Treatment"
# Wabi <- "QualityTreatment"

###--------------------------------------------------------------------------###
### 3.3.  Controls to console.
 
if(!ToFile){
  SizeStrip     <- 10
  SizeTitleY    <- 10
  SizeTextY     <- 10
}

###--------------------------------------------------------------------------###
### 3.4.  Controls to file.
if(ToFile){
  if(DoHorizontal){
    Dim = c(3.7, 3.35)
    SizeStrip     <- 8
  }
  
  if(!DoHorizontal){
    Dim = c(10, 2.5)
    SizeStrip     <- 12
  }
 
  
  SizeTitleY    <- 5
  SizeTextY     <- 6
}


###--------------------------------------------------------------------------###
### 4.  Wabi == "Generation".                                         
###--------------------------------------------------------------------------###

if(Wabi == "Generation"){
  
  Name   <- "Wabi_Generation" # name of output file
  LabY  <- bquote('[Kg'%.%y^-1%.%p^-1*']')
  LimX1  <- c(0, 100)
  
  Shade1 <- data.frame(Label = "Wabi I-0:\nWaste generation\nrate",
                       Ymin  = c(0, 
                                 250, 
                                 500, 
                                 750), 
                       Ymax  = c(250, 
                                 500, 
                                 750,  
                                 1000), 
                       Color = c('#BBFFCF',
                                 '#FDFFBF',
                                 '#FFEDBF',
                                 '#FFCFC0'))
}


###--------------------------------------------------------------------------###
### 5.  Wabi == "Collection".                                         
###--------------------------------------------------------------------------###


if(Wabi == "Collection"){
  
  Name   <- "Wabi_Collection" # name of output file
  LabY  <- "Index value"
  LimX1  <- c(0, 100)
  
  Shade1 <- data.frame(Label = "Wabi I-1.1:\nWaste collection\ncoverage",
                       Ymin  = c(0, 
                                 49.5, 
                                 69.5, 
                                 89.5, 
                                 98.5), 
                       Ymax  = c(49.5, 
                                 69.5, 
                                 89.5,  
                                 98.5, 
                                 105), 
                       Color = c('#FFCFC0',
                                 '#FFEDBF', 
                                 '#FDFFBF',
                                 '#D4FFC9',
                                 '#BBFFCF'))
}


###--------------------------------------------------------------------------###
### 6.  Wabi == "QualityCollection".                                         
###--------------------------------------------------------------------------###


if(Wabi == "QualityCollection"){
  
  Name   <- "Wabi_QualityCollection" # name of output file
  LabY  <- "Index value"
  LimX1  <- c(0, 100)
  
  Shade1 <- data.frame(Label = "Wabi I-1C:\nQuality of waste\ncollection service",
                       Ymin  = c(0, 
                                 20, 
                                 40, 
                                 60, 
                                 80), 
                       Ymax  = c(20, 
                                 40,  
                                 60, 
                                 80, 
                                 100), 
                       Color = c('#FFCFC0',
                                 '#FFEDBF', 
                                 '#FDFFBF',
                                 '#D4FFC9',
                                 '#BBFFCF'))
}


###--------------------------------------------------------------------------###
### 7.  Wabi == "Treatment".                                         
###--------------------------------------------------------------------------###


if(Wabi == "Treatment"){
  
  Name   <- "Wabi_Treatment" # name of output file
  LabY  <- "Index value"
  LimX1  <- c(0, 100)
  
  Shade1 <- data.frame(Label = "Wabi I-2:\nControlled recovery\nand disposal",
                       Ymin  = c(0, 
                                 49.5, 
                                 74.5, 
                                 84.5, 
                                 94.5), 
                       Ymax  = c(49.5, 
                                 74.5, 
                                 84.5,  
                                 94.5, 
                                 100), 
                       Color = c('#FFCFC0',
                                 '#FFEDBF', 
                                 '#FDFFBF',
                                 '#D4FFC9',
                                 '#BBFFCF'))
}


###--------------------------------------------------------------------------###
### 8.  Wabi == "QualityTreatment".                                         
###--------------------------------------------------------------------------###


if(Wabi == "QualityTreatment"){
  
  Name   <- "Wabi_QualityTreatment" # name of output file
  LabY  <- "Index value"
  LimX1  <- c(0, 100)
  
  Shade1 <- data.frame(Label = "Wabi I-2E:\nEnvironmental\nprotection in I-2",
                       Name  = c("B1", "B2", "B3", "B4", "B5"),
                       Ymin  = c(0, 
                                 20, 
                                 40, 
                                 60, 
                                 80), 
                       Ymax  = c(20, 
                                 40,  
                                 60, 
                                 80, 
                                 100), 
                       Color = c('#FFCFC0',
                                 '#FFEDBF', 
                                 '#FDFFBF',
                                 '#D4FFC9',
                                 '#BBFFCF')) 
}


###--------------------------------------------------------------------------###
### 9.  Plot.                                         
###--------------------------------------------------------------------------###

if(DoHorizontal){
  Name <- paste0(Name, "_Horizontal")
}

if(!DoHorizontal){
  Name <- paste0(Name, "_Vertical")
}

###--------------------------------------------------------------------------###
### 9.1.  Start plot. 
P1 <- ggplot(data = Shade1)

###--------------------------------------------------------------------------###
### 9.2.  Background traffic light shades. 
P1 <- P1 + geom_rect(aes(xmin = LimX1[1],
                         xmax = LimX1[2],
                         ymin = Shade1[1, "Ymin"],
                         ymax = Shade1[1, "Ymax"]),
                     fill  = Shade1[1, "Color"])

P1 <- P1 + geom_rect(aes(xmin = LimX1[1],
                         xmax = LimX1[2],
                         ymin = Shade1[2, "Ymin"],
                         ymax = Shade1[2, "Ymax"]),
                     fill  = Shade1[2, "Color"])

P1 <- P1 + geom_rect(aes(xmin = LimX1[1],
                         xmax = LimX1[2],
                         ymin = Shade1[3, "Ymin"],
                         ymax = Shade1[3, "Ymax"]),
                     fill  = Shade1[3, "Color"])

P1 <- P1 + geom_rect(aes(xmin = LimX1[1],
                         xmax = LimX1[2],
                         ymin = Shade1[4, "Ymin"],
                         ymax = Shade1[4, "Ymax"]),
                     fill  = Shade1[4, "Color"])
if(nrow(Shade1) == 5){
  P1 <- P1 + geom_rect(aes(xmin = LimX1[1],
                           xmax = LimX1[2],
                           ymin = Shade1[5, "Ymin"],
                           ymax = Shade1[5, "Ymax"]),
                       fill  = Shade1[5, "Color"])
}

###--------------------------------------------------------------------------###
### 9.3.  Scales, faceting and theme.
P1 <- P1 + scale_x_continuous(expand= c(0,0))  
P1 <- P1 + scale_y_continuous(expand= c(0,0))

if(DoHorizontal){
  P1 <- P1 + facet_wrap(~ Label)
  P1 <- P1 + theme_bw()
  P1 <- P1 + theme(
    strip.text.y    = element_text(size = SizeStrip),
  )
}

if(!DoHorizontal){
  P1 <- P1 + facet_grid(rows = "Label")
  P1 <- P1 + theme_bw()
  P1 <- P1 + theme(
    #strip.text.x    = element_text(size = SizeStrip, angle = 0),
    strip.text.y    = element_text(size = SizeStrip, angle = 0),
  )
}


if(DoTitleY){
  P1 <- P1 + ylab(LabY)
  P1 <- P1 + theme(
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size = SizeTitleY),
    axis.text.x   = element_blank(),
    axis.text.y   = element_text(size = SizeTextY),
    axis.ticks.x  = element_blank()
  )
}


if(!DoTitleY){
 
  P1 <- P1 + theme(
    axis.title   = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(size = SizeTextY),
    axis.ticks.x = element_blank()
  )
}

 



###--------------------------------------------------------------------------###
### 10.  Write to console or file. 
###--------------------------------------------------------------------------###


if(!ToFile){
  # plot.new()
  # grid.draw(G1)
  P1
}

if(ToFile){
  setwd(Dir1)
  for(i in FormOut){
    if(i != ".pdf"){
      ggsave(file   = paste0(Name, i), 
             plot   = P1,
             width  = Dim[1], 
             height = Dim[2], 
             units  = c("cm"),
             dpi    = 300)
    }
    # use cairo for special characters. 
    if(i == ".pdf"){
      ggsave(file   = paste0(Name, i), 
             plot   = P1,
             width  = Dim[1], 
             height = Dim[2], 
             units  = c("cm"),
             dpi    = 300, 
             device = cairo_pdf)
    }
    
  }
  ###------------------------------------------------------------------------###
  ### clean
  rm(i)
  
}




################################################################################
###--------------------------------------------------------------------------###
###----------------------                              ----------------------### 
###-----------------                                        -----------------###
###-----------                                                    -----------###
###-----                            END CODE                            -----###
###-----------                                                    -----------###
###-----------------                                        -----------------###
###----------------------                              ----------------------### 
###--------------------------------------------------------------------------###
################################################################################

































