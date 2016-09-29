DateBreaks <- function(breaksDF, limits, weekNumbers){
  if(weekNumbers){
    if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*0.5){
      desiredGap <- 2
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*1){
      desiredGap <- 2
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*2){
      desiredGap <- 4
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*3){
      desiredGap <- 13
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*6){
      desiredGap <- 26
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/7 < 52*20){
      desiredGap <- 50
    }

    desiredWeeks <- formatC(seq(1,52-desiredGap/2+1,desiredGap),flag="0",width=2)
    breaksDF <- breaksDF[breaksDF$printWeek %in% desiredWeeks,]
    breaksDF$printLabel <- paste0(breaksDF$printWeek,"/",breaksDF$printYear)
  } else {
    if(as.numeric(difftime(limits[2],limits[1],"days"))/1 < 52*0.5){
      desiredGap <- 2
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/1 < 52*1){
      desiredGap <- 2
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/1 < 52*2){
      desiredGap <- 7
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/1 < 52*3){
      desiredGap <- 14
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/1 < 52*6){
      desiredGap <- 14
    } else if(as.numeric(difftime(limits[2],limits[1],"days"))/1 < 52*20){
      desiredGap <- 30
    }

    desiredWeeks <- formatC(seq(1,31-desiredGap/2+1,desiredGap),flag="0",width=2)
    breaksDF <- breaksDF[breaksDF$printDay %in% desiredWeeks,]
    breaksDF$printLabel <- paste0(breaksDF$printDay,"/",breaksDF$printMonth)
  }
  return(breaksDF)
 }
