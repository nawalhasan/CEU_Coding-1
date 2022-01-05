#########################
## Assignment 4        ##
##      CLASS 4        ##
##  Deadline:          ##
##  2021/11/2 17:40   ##
#########################

##### my customized theme ####

theme_nawal <- function( base_size = 11, base_family = ""){
  theme(
    #1 title
    plot.title = element_text(color="black", size=14, face="bold.italic", hjust = 0.5),
    #2 background
    panel.background  = element_rect(fill = "whitesmoke"),
    #3 border
    plot.background = element_rect(fill="gray96", colour= "deeppink", linetype = 2),
    #4 panel for x
    panel.grid.major.x = element_line(colour = "lightpink", linetype = 3, size = 0.5),
    #5 panel for y
    panel.grid.major.y =  element_line(colour = "lightpink", linetype = 3, size = 0.5),
    #6 axis
    axis.title = element_text(face = "bold", size = 11),
    #7 axis ticks
    axis.ticks = element_line(color = "deeppink"),
    #8 axis lines
    axis.line = element_line(colour = "deeppink", size = 1, linetype = "solid")
  )
}



  
    
