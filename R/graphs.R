stairstepn <- function( data, direction="hv", yvars="y" ) {
  direction <- match.arg( direction, c( "hv", "vh" ) )
  data <- as.data.frame( data )[ order( data$x ), ]
  n <- nrow( data )
  
  if ( direction == "vh" ) {
    xs <- rep( 1:n, each = 2 )[ -2 * n ]
    ys <- c( 1, rep( 2:n, each = 2 ) )
  } else {
    ys <- rep( 1:n, each = 2 )[ -2 * n ]
    xs <- c( 1, rep( 2:n, each = 2))
  }
  
  data.frame(
    x = data$x[ xs ]
    , data[ ys, yvars, drop=FALSE ]
    , data[ xs, setdiff( names( data ), c( "x", yvars ) ), drop=FALSE ]
  ) 
}

StatStepribbon <- 
  ggproto("stepribbon", Stat,
          compute_group = function(., data, scales, direction = "hv", yvars = c( "ymin", "ymax" ), ...) {
            stairstepn( data = data, direction = direction, yvars = yvars )
          },                        
          required_aes = c( "x", "ymin", "ymax" )
  )

stat_stepribbon <- 
  function(mapping = NULL, data = NULL, geom = "ribbon", position = "identity", inherit.aes = TRUE, ...) {
    ggplot2::layer(
      stat = StatStepribbon, mapping = mapping, data = data, geom = geom, 
      position = position, inherit.aes = inherit.aes, params=list(...)
    )
  }


ThemeShiny <- function(base_size = 12, base_family = "") {
theme(
  line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
  rect = element_rect(fill = "white",  colour = "black", size = 0.5, linetype = 1), text = element_text(family = base_family, 
    face = "plain", color = "black", size = base_size, hjust = 0.5, 
    vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), 
    debug = FALSE), axis.text = element_text(size = rel(0.8), 
    colour = "black"), strip.text = element_text(size = rel(0.8), 
    colour = "black"), axis.line.x = element_line(size = base_size/20), 
    axis.line.y = element_line(size = base_size/20), axis.text.x = element_text(vjust = 1, 
        margin = margin(5, 5, 10, 5, "pt")), axis.text.y = element_text(hjust = 1, 
        margin = margin(5, 5, 10, 5, "pt")), axis.ticks = element_line(), 
    axis.title = element_text(colour = "black"), axis.title.x = element_text(vjust = 1), 
    axis.title.y = element_text(angle = 90, vjust = 1), axis.ticks.length = unit(0.3, 
        "lines"), legend.background = element_rect(colour = NA), 
    legend.margin = unit(0.2, "cm"), legend.key = element_rect(fill = "white", 
        colour = "black"), legend.key.size = unit(0.1 * base_size, 
        "lines"), legend.key.height = NULL, legend.key.width = NULL, 
    legend.text = element_text(size = rel(0.8), colour = "black"), 
    legend.text.align = NULL, legend.title = element_text(size = rel(0.8), 
        face = "bold", hjust = 0, colour = "white"), legend.title.align = NULL, 
    legend.position = "bottom", legend.direction = "horizontal", 
    legend.justification = "center", legend.box = NULL, panel.background = element_rect(fill = NA, 
        colour = NA), panel.border = element_rect(fill = NA, 
        colour = NA), panel.grid.major = element_line(colour = "black", 
        size = rel(0.8), linetype = 3), panel.grid.minor = element_line(colour = "black", 
        size = rel(0.8), linetype = 3), panel.margin = unit(0.25, 
        "lines"), strip.background = element_rect(fill = "white", 
        colour = "white", size = 3), strip.text.x = element_text(), 
    strip.text.y = element_text(angle = -90), plot.background = element_rect(colour = NA, 
        fill = NA), plot.title = element_text(size = rel(1.2)), 
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"), complete = TRUE)
}

MakeLineThresholdPlot <- function(pd,x,dataVal,dataCIL=NULL,dataCIU=NULL,L1,L2,L3,L4,allPoints=TRUE,title=NULL,pointShift=0, xShift=0, weekNumbers=FALSE, step=FALSE, GetCols){
  pd <- as.data.frame(pd)
  pd$printYear <- format.Date(pd[[x]],"%G")
  pd$printWeek <- format.Date(pd[[x]],"%V")
  pd$printMonth <- format.Date(pd[[x]],"%m")
  pd$printDay <- format.Date(pd[[x]],"%d")
  if(step){
    pd$xShifted <- pd[[x]] + pointShift
    pd[[x]] <- pd[[x]] + xShift
  } else {
  pd$xShifted <- pd[[x]]
    pd[[x]] <- pd[[x]]
  }
  includeMedium <- nrow(pd[pd$status=="Medium",])>0
  includeHigh <- nrow(pd[pd$status=="High",])>0
  
  colours <- NULL
  if(includeHigh) colours <- c(colours,GetCols()[1])
  if(includeMedium) colours <- c(colours,GetCols()[2])
  
  limits <- range(pd[[x]])
  limitsSize <- max(1,(limits[2] - limits[1])*0.005)
  limits[1] <- limits[1] - limitsSize
  limits[2] <- limits[2] + limitsSize
  
  limitsY <- diff(range(c(pd[[L1]],pd[[L4]])))
  
  dateBreaks <- "6 months"
  if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.25){
    dateBreaks <- "2 weeks"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.5){
    dateBreaks <- "2 weeks"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*1){
    dateBreaks <- "1 month"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*2){
    dateBreaks <- "2 months"
  }
  
  q <- ggplot(pd,aes_string(x=x))
  if(step){
    q <- q + stat_stepribbon(aes_string(ymin = L3, ymax = L4, fill = shQuote("L1")), direction="vh", alpha = 0.4)
    q <- q + stat_stepribbon(aes_string(ymin = L2, ymax = L3, fill = shQuote("L2")), direction="vh", alpha = 0.4)
    q <- q + stat_stepribbon(aes_string(ymin = L1, ymax = L2, fill = shQuote("L3")), direction="vh", alpha = 0.4)
    if(!is.null(dataCIL) & !is.null(dataCIU)) q <- q + stat_stepribbon(aes_string(ymin = dataCIL, ymax = dataCIU), fill= "black", direction="vh", alpha = 0.4)
    q <- q + geom_step(aes_string(y = dataVal), direction="vh", lwd = 1)
  } else {
    q <- q + geom_ribbon(aes_string(ymin = L3, ymax = L4, fill = shQuote("L1")), alpha = 0.4)
    q <- q + geom_ribbon(aes_string(ymin = L2, ymax = L3, fill = shQuote("L2")), alpha = 0.4)
    q <- q + geom_ribbon(aes_string(ymin = L1, ymax = L2, fill = shQuote("L3")), alpha = 0.4)
    if(!is.null(dataCIL) & !is.null(dataCIU)) q <- q + geom_ribbon(aes_string(ymin = dataCIL, ymax = dataCIU), fill= "black", alpha = 0.4)
    q <- q + geom_point(aes_string(y = dataVal), lwd = 1)
  }
  
  if(allPoints){
    q <- q + geom_point(aes_string(x="xShifted", y = dataVal), size = 4, fill = "black")
  } else {
    if(includeMedium | includeHigh) q <- q + geom_point(aes_string(x="xShifted", y = dataVal), size = 4, fill = "black", data=pd[pd$status%in%c("Medium","High"),])
  }
  if(includeMedium) q <- q + geom_point(aes_string(x="xShifted", y = dataVal, colour=shQuote("L2")), size = 2, data=pd[pd$status=="Medium",])
  if(includeHigh) q <- q + geom_point(aes_string(x="xShifted", y = dataVal, colour=shQuote("L1")), size = 2, data=pd[pd$status=="High",])
  q <- q + ThemeShiny()
  
  breaksDF <- pd[pd$printWeek!="",]
  if(weekNumbers){
    denom <- 7
    breaksDF$printLabel <- paste0(breaksDF$printWeek,"/",breaksDF$printYear)
  } else {
    denom <- 1
    breaksDF$printLabel <- paste0(breaksDF$printDay,"/",breaksDF$printMonth)
  }
  if(as.numeric(difftime(limits[2],limits[1],"days"))/denom < 52*0.5){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),2),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/denom < 52*1){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),2),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/denom < 52*2){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),4),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/denom < 52*4){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),8),]
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/denom < 52*10){
    breaksDF <- breaksDF[seq(1,nrow(breaksDF),16),]
  }
  
  q <- q + scale_x_date("", breaks = breaksDF$xShifted,  labels = breaksDF$printLabel)
  q <- q + scale_y_continuous("")
  q <- q + scale_fill_manual(values=GetCols(),labels=c(
    "Betydelig høyere enn forventet",
    "Høyere enn forventet",
    "Forventet"))
  if(!is.null(colours)) q <- q + scale_colour_manual(values=colours)
  q <- q + guides(colour=FALSE)
  q <- q + coord_cartesian(xlim=limits,expand = FALSE)
  if(!is.null(title)) q <- q + labs(title=title)
  return(q)
}

MakeLineBrushPlot <- function(pd,x,dataVal,L2,L3, GetCols){
  pd <- as.data.frame(pd)
  includeHigh <- sum(pd$status=="High")>0
  includeMedium <- sum(pd$status=="Medium")>0
  includeNormal <- sum(pd$status=="Normal")>0
  
  colours <- NULL
  if(includeHigh) colours <- c(colours,GetCols()[1])
  if(includeMedium) colours <- c(colours,GetCols()[2])
  
  limitsX <- range(pd[[x]])
  limitsSize <- limitsX[2] - limitsX[1]
  limitsX[1] <- limitsX[1] - limitsSize*0.005
  limitsX[2] <- limitsX[2] + limitsSize*0.005
  
  limitsY <- range(pd[[dataVal]])
  limitsSize <- limitsY[2] - limitsY[1]
  limitsY[1] <- limitsY[1] - limitsSize*0.05
  limitsY[2] <- limitsY[2] + limitsSize*0.05
  
  limits <- range(pd[[x]])
  dateBreaks <- "6 months"
  if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.25){
    dateBreaks <- "2 weeks"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.5){
    dateBreaks <- "2 weeks"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*1){
    dateBreaks <- "1 month"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*2){
    dateBreaks <- "2 months"
  }
  
  q <- ggplot(pd,aes_string(x=x))
  q <- q + geom_line(aes_string(y = dataVal), lwd = 1)
  if(includeMedium | includeHigh) q <- q + geom_point(aes_string(y = dataVal), size = 4, fill = "black", data=pd[pd$status%in%c("Medium","High"),])
  if(includeMedium) q <- q + geom_point(aes_string(y = dataVal, colour=shQuote("L2")), size = 2, data=pd[pd$status=="Medium",])
  if(includeHigh) q <- q + geom_point(aes_string(y = dataVal, colour=shQuote("L1")), size = 2, data=pd[pd$status=="High",])
  q <- q + ThemeShiny()
  q <- q + scale_x_date("", date_breaks = dateBreaks)
  q <- q + scale_y_continuous("",breaks=NULL)
  if(!is.null(colours)) q <- q + scale_colour_manual(values=colours)
  q <- q + guides(colour=FALSE)
  q <- q + coord_cartesian(xlim=limitsX,ylim=limitsY,expand = FALSE)
  q <- q + labs(title="Velg tidsintervall")
  return(q)
}

MakeLineComparisonPlot <- function(pd,x,dataVal,comparisonNames, highlightCondition, namesFunction=NULL, title=NULL, GetCols){
  pd <- as.data.frame(pd)
  
  limits <- range(pd[[x]])
  limitsSize <- max(1,(limits[2] - limits[1])*0.005)
  limits[1] <- limits[1] - limitsSize
  limits[2] <- limits[2] + limitsSize
  
  dateBreaks <- "6 months"
  if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.25){
    dateBreaks <- "2 weeks"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.5){
    dateBreaks <- "2 weeks"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*1){
    dateBreaks <- "1 month"
  } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*2){
    dateBreaks <- "2 months"
  }
  
  q <- ggplot(pd,aes_string(x=x, group=comparisonNames))
  q <- q + geom_line(aes_string(y = dataVal), lwd = 0.25,alpha=0.3)
  if(is.null(namesFunction)){
    q <- q + geom_line(aes_string(y = dataVal, colour = comparisonNames), lwd = 2, alpha=0.8, data=pd[pd[[highlightCondition]],])
  } else {
    for(i in unique(pd[[comparisonNames]][pd[[highlightCondition]]])){
      newName <- namesFunction(i)
      q <- q + geom_line(aes_string(y = dataVal, colour = shQuote(newName)), lwd = 2, alpha=0.8,data=pd[pd[[highlightCondition]] & pd[[comparisonNames]]==i,])
    }
  }
  q <- q + ThemeShiny()
  q <- q + scale_x_date("", date_breaks = dateBreaks,  labels = scales::date_format("%V/%G"))
  q <- q + scale_y_continuous("")
  q <- q + scale_colour_brewer("",palette="Set1")
  q <- q + coord_cartesian(xlim=limits,expand = FALSE)
  if(!is.null(title)) q <- q + labs(title=title)
  return(q)
}

