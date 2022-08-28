## R6 class for the ggplot

CombinedSYPlot <- R6::R6Class(
  "CombinedSYPlot", 
  # Public members
  list(
    gPlot = NULL, 
    initialize = function(){
      self$gPlot <- ggplot2::ggplot()
    }, 
    
    # Methods ####
    AddSyPlotElement = function(listElement){
      SyData <- listElement[[1]]
      SY50 <- listElement[[2]]
      ConfidIntervals <- listElement[[3]]
      PredIntervals <- listElement[[4]]
      SY50Y <- listElement[[5]]
      
      self$gPlot + 
        ggplot2::geom_ribbon(data = PredIntervals, 
                             ggplot2::aes(x = x, 
                                          y = fit, 
                                          ymin = lwr, 
                                          ymax = upr), 
                             fill = I("red"), 
                             alpha = I(0.2)) + 
        ggplot2::geom_ribbon(data = ConfidIntervals, 
                             ggplot2::aes(x = x, 
                                          ymin = lwr, 
                                          ymax = upr), 
                             fill = I("blue"), 
                             alpha = I(0.2)) + 
        ggplot2::geom_line(data = ConfidIntervals, 
                           ggplot2::aes(x = x, y = fit), 
                           colour = I("blue"), 
                           lwd = 1) + 
        ggplot2::geom_point(data = SyData, 
                            ggplot2::aes(x = x, 
                                         y = fit, 
                                         ymin = NULL, 
                                         ymax = NULL), 
                            size = 3) + 
        ggplot2::geom_point(ggplot2::aes(SY50, SY50Y), 
                            col = "red", size = 3)
      
    }, 
    
    ApplyTheme = function(...){
      self$gPlot + 
        ggplot2::ylab("SY Percent") + 
        ggplot2::xlab("Energy") + 
        ggplot2::theme_classic(
          base_size = 12, 
          base_family = "Arial"
        ) + 
        ggplot2::theme(
          text = ggplot2::element_text(colour = "black")
        )
    }, 
    
    SavePlot = function(filePath){
      ggplot2::ggsave(
        plot = self$gPlot, 
        filename = file.path(filePath, "Combined_SYPlot.svg"), device = "svg")
    }
    
    
  )
  
  
  
)