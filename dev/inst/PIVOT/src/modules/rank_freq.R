# Copyright (c) 2015,2016, Qin Zhu and Junhyong Kim, University of Pennsylvania.
# All Rights Reserved.
#
# You may not use this file except in compliance with the Kim Lab License
# located at
#
#     http://kim.bio.upenn.edu/software/LICENSE
#
# Unless required by applicable law or agreed to in writing, this
# software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License
# for the specific language governing permissions and limitations
# under the License.


# Rank Frequency Function

rank_freq <- function(df, logX = FALSE, logY = FALSE){
    sorted = apply(df, 2, function(x) {sort(x, decreasing=T)})
    sorted = sorted + 0.0000001
    ranks = 1:nrow(sorted)
    samples = colnames(sorted)
    nSamples = ncol(sorted)
    ymin = 1
    ymax = max(sorted)
    xmin = 1	
    xmax = dim(sorted)[1]
    colorMin = 1
    colorMax = nSamples
    # colorRamp produces custom palettes, using values between 0-1
    colorFunction = colorRamp(c("black", "blue", "red"))
    zMin = colorMin
    zMax = colorMax
    zDiff = zMax - zMin
    z = zMin:zMax
    zScaled = (z - zMin) / zDiff
    # Apply colorRamp
    zMatrix = colorFunction(zScaled)
    # switch to hexadecimal representation and sort to be consistent with colorFactor.
    zColors = sort(rgb(zMatrix[,1], zMatrix[,2], zMatrix[,3], maxColorValue=255))
    
    # create an empty character vector that will be used to store the line colors. This is necessary to color the legend.
    colorsUsed = character(nSamples)
    
    # configure which axis are log scale
    if (logX) {
        xAxis = "Rank (log)"
        logAxis = "x"
    } else {
        xAxis = "Rank"
        logAxis = ""
    }
    if (logY) {
        yAxis = "Frequency (log)"
        logAxis = paste(logAxis, "y", sep="")
    } else {
        yAxis = "Frequency"
    }
    title="Rank-Frequency"
    # set up an empty plot. We add the lines below.
    plot(ranks, sorted[ranks,1], xlim=c(xmin, xmax), ylim=c(ymin, ymax), log=logAxis, ylab=yAxis, xlab=xAxis, col='black', type='n', cex=0.4, main=title)
    
    for(i in 1:nSamples) {
        # using the sample name means the column order in colorFactorSorted file doesn't have to match the order in the counts file
        sample = samples[i]
        color = zColors[i]
        colorsUsed[i] = color
        lines(ranks, sorted[ranks,sample], type="l", lty=i, lwd=1.5, cex=1, col=color)
    }
    
    # add sample legend
    legend("topright", legend=samples, col=colorsUsed, lty=1:nSamples, lwd=1.8, cex=0.8)
}



############################## Rank Frequency Server End Module ##############################
# Original script

output$rankfreq_plt <- renderPlot({
    if(is.null(r_data$df)) return ()
    if(input$ercc_isolation) {
        rf_data <- r_data$df[!(rownames(r_data$df) %in% ercc$features),]
    } else {
        rf_data <- r_data$df
    }
    rank_freq(rf_data, input$rf_logX, input$rf_logY)
})

