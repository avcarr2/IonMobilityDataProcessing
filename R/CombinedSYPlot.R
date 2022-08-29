## R6 class for the ggplot
library(ggplot2)
CombinedSYPlot <- R6::R6Class(
  "CombinedSYPlot", 
  # Public members
  list(
    gPlot = NULL, 
    initialize = function(){
      self$gPlot = ggplot2::ggplot()
      invisible(self)
    },
    # Methods ####
    AddSyPlotElement = function(listElement){
      SyData <- listElement$EnergyVPercent
      SY50 <- listElement$Sy50
      ConfidIntervals <- listElement$ConfInt
      PredIntervals <- listElement$PredInt
      SY50Y <- listElement$PredSY50
      
      self$gPlot <- 
        ggplot2::`%+%`(self$gPlot, list(
          ggplot2::geom_ribbon(data = PredIntervals, 
                               ggplot2::aes(x = `x`, 
                                            y = `fit`, 
                                            ymin = `lwr`, 
                                            ymax = `upr`), 
                               fill = I("red"), 
                               alpha = I(0.2)), 
            ggplot2::geom_ribbon(data = ConfidIntervals, 
                                 ggplot2::aes(x = `x`, 
                                              ymin = `lwr`, 
                                              ymax = `upr`), 
                                 fill = I("blue"), 
                                 alpha = I(0.2)),  
            ggplot2::geom_line(data = ConfidIntervals, 
                               ggplot2::aes(x = `x`, 
                                            y = `fit`), 
                               colour = I("blue"), 
                               lwd = 1), 
            ggplot2::geom_point(data = SyData, 
                                ggplot2::aes(x = `EnergyVals`, 
                                             y = `PercentVals`), 
                                size = 3),   
            ggplot2::geom_point(ggplot2::aes(x = SY50, 
                                             y = SY50Y), 
                                col = "red", size = 3)
        ))
        invisible(self$gPlot)
      }, 
    
    AddPlot = function(singleElement){
      self$gPlot <- 
        self$gPlot %+%
        ggplot2::geom_point(data = singleElement, mapping = aes(x = x, y = y))
    }, 
    
    ApplyTheme = function(...){
      self$gPlot <- 
        ggplot2::`%+%`(self$gPlot,  
        list(
            ggplot2::ylab("SY Percent"),  
            ggplot2::xlab("Energy"),  
            ggplot2::theme_classic(), 
            ggplot2::theme(
              text = ggplot2::element_text(colour = "black")
            )
          )
        )
      invisible(self$gPlot)
    }, 
    
    SavePlot = function(filePath){
      ggplot2::ggsave(self$gPlot,
        filename = file.path(filePath, "Combined_SYPlot.pdf"), 
        device = "pdf")
    }
  )
)
