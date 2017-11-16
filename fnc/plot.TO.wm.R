# Visualize Spatial Data function ####
plot.TO.wm <- function(y1,y2 = NULL,m1 = NULL,m2 = NULL, #next step is to generate option for breakdown by months in a year
                       face = TRUE, ncol = 5, band = 150, 
                       file.out = FALSE, w = NULL, h = NULL, title.size = 18) {
  
  my_theme <- theme(axis.text = element_blank(),axis.title = element_blank(),
                    axis.ticks = element_blank(), legend.position = "none",
                    panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                    strip.background = element_rect(fill = "grey85"),
                    strip.text = element_text(size = title.size*0.8), title = element_text(size = title.size),
                    plot.background = element_rect(fill = "grey95"))
  
  if (missing(y2)) {
    
    g.out <- ggplot(data = wm.df %>% filter(Year == y1), aes(x = X_coord, y = Y_coord)) +
      geom_point(size = 1, alpha = 0.5, colour = "grey5") +
      labs(title = paste0("Point Density Map of Watermain Breaks","\n",
                          "City of Toronto ","(",y1,")")) +
      stat_density_2d(aes(fill = ..density.., alpha = ..density..), 
                      geom = "tile", contour = FALSE, n = band) +
      scale_fill_viridis(guide = "none", option = "inferno") + my_theme +
      geom_text(data = wm.df %>% filter(Year == y1) %>% 
                  summarise(n = paste("n =",sum(Year=n()))),
                aes(x=328000,y=4830000,label = n), size = title.size*0.4)
    
    return(g.out)
    
  } else {
    
    g.out <- ggplot(data = wm.df %>% filter(Year %in% seq(y1,y2,1)), aes(x = X_coord, y = Y_coord)) +
      geom_point(size = 1, alpha = 0.5, colour = "grey5") +
      labs(title = paste0("Point Density Map of Watermain Breaks","\n",
                          "City of Toronto ","(Cumulative ",y1,"-",y2,")")) +
      stat_density_2d(aes(fill = ..density.., alpha = ..density..), 
                      geom = "tile", contour = FALSE, n = band) +
      scale_fill_viridis(guide = "none", option = "inferno") + my_theme
    
  }
  
  if (face == TRUE) {
    
    g.out <- g.out + facet_wrap(~Year, ncol = ncol, drop = TRUE) +
      geom_text(data = wm.df %>% filter(Year %in% seq(y1,y2,1)) %>%
                  group_by(Year) %>% summarise(n = paste("n =",length(Year))),
                aes(x=328000,y=4830000,label = n), size = title.size*0.4)  
    
  } else {
    
    wm.df %>% filter(Year %in% seq(y1,y2,1)) %>%
      summarise(n = paste("n =",sum(Year=n())))
    g.out <- g.out + 
      geom_text(data = wm.df %>% filter(Year %in% seq(y1,y2,1)) %>%
                  summarise(n = paste("n =",sum(Year=n()))),
                aes(x=328000,y=4830000,label = n), size = title.size*0.4)
  }
  
  if(file.out == TRUE) {
    
    ppi <- 300
    filename <- paste0("TO_watermains_",y1,"-",y2,".png")
    png(filename, width = w*ppi, height = h*ppi, res = ppi,units = "px", pointsize = .7)
    print(g.out)
    dev.off()
    
  } else {
    
    return(g.out)
    
  }
  
}
