#This function assumes that the means, SDs, and sample size for each level have been calculated as above, as well the RpoS level for each.
plotBetaGals <- function(strain, xlab="RpoS Level", ylab="Beta-gal Activity (Miller Units)", main=strain, ...){
  plot(fusionAves[strain,]~ rpoSLevels, xlab=xlab, ylab=ylab, bty="n", main = main, pch = 16, cex=2, cex.axis=1.5, las=2, xlim=c(0,100), ... )
  
  arrows(x0 = rpoSLevels, y0 = fusionAves[strain,], x1 = rpoSLevels, y1 = fusionAves[strain,] + fusionSDs[strain,]/sqrt(fusionCounts[strain,]), length = .04, angle = 90)
  
  arrows(x0 = rpoSLevels, y0 = fusionAves[strain,], x1 = rpoSLevels, y1 = fusionAves[strain,] - fusionSDs[strain,]/sqrt(fusionCounts[strain,]), length = .04, angle = 90)
  
  segments(x0 = rpoSLevels[1], y0 =fusionAves[strain,1], x1 =  rpoSLevels[3], y1 = fusionAves[strain,3])
}



#This function is for plotting the raw data:
plotRawBetaGals <- function(strainToPlot, xlab="RpoS Level", ylab="Beta-gal Activity (Miller Units)", main=strainToPlot, xlim=c(0,100), ...){
  plot(millerUnits~rpoS, data = fusions, subset = strain==strainToPlot, main=main, ...)
  segments(x0 = rpoSLevels[1], y0 =fusionAves[strainToPlot,1], x1 =  rpoSLevels[3], y1 = fusionAves[strainToPlot,3])
}




#This function expects a data frame with columns millerUnits, strain, rpoS, date. Other columns are ignored.

#First, calculates the expected observation at 26% RpoS based on the mean of 0 and 89%
#Then, calculates each of the differences between these levels.
  calculateSingleSens <- function(subset) {
    rise <- subset$millerUnits[subset$rpoS == 89] - subset$millerUnits[subset$rpoS == 0]
    run <- 89
    slope <- rise/run
    predicted <- slope*26 + subset$millerUnits[subset$rpoS == 0] #This is the expected value on the line
    
    dif <- subset$millerUnits[subset$rpoS == 26] - predicted #This is the difference between the observed - predicted
    
    sens <- dif/rise #This is A/B, the metric for sensitivity.
    
    return(sens)
  }
  
