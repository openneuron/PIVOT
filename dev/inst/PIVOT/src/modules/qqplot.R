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


# Script from Hannah, modified by Qin
##### Helpers:
openBlankPlot <- function ( xlims = c ( 0 , 1 ) , ylims = c ( 0 , 1 ) , log = "") {
    plot ( 1 , 1, type = 'n' , axes = FALSE ,  ylim = ylims , ylab='', xlab='' , xlim = xlims , log = log, main = ""  )
}
ppPlot <- function (countsTables, panel = "I." , trim = c ( 0 , 1 ) ) {

    #### Plot
    # File setup and plot layout settings:
    x1mar <- 0.2
    x2mar <- 0.01
    y1mar <- 0.2
    y2mar <- 0.025
    pointsize <- 6
    figWidth <- 1.28
    figHeight <- 1.25
    figResolution <- 600
    lwd <- 0.75
    
    # Axis settings
    # By hand
    ylims <- c ( 0.1 , 30000 )
    pos <- 10^seq ( -3 , 5 , 1 )
    lab <- seq ( -3 , 5 , 1 )
    ytitle <- expression ( "log" [10]~"counts" )
    
    xlims = c ( 1 , 7500 )
    xtitle <-  expression ( "log" [10]~"ref." )
    
    # 5) And plot:
    par ( mai = c ( y1mar, x1mar, y2mar, x2mar ) )
    
    openBlankPlot ( xlims = xlims , ylims = ylims , log = 'xy' )
    # Add 1:1
    lines ( ylims , ylims , col = "gray25" , lty = 1 , lwd = lwd )
    # Add lines for each subgroup
    groups <- unique(r_data$group)
    init <- TRUE
    print(groups)
    for ( group in groups ) {
        print ( group )
        # get values
        cts <- countsTables [, names(r_data$group[r_data$group == group])]
        samples <- colnames ( cts ) 
        qq <- qqplot ( rowMeans(cts) , do.call ( c , lapply ( samples , function ( x ) { cts [ , x ] } ) ) , plot.it = F )
        if ( init ) {
            plotThresh <- ( trim * length ( qq$x ) )
            pointInds <- seq ( 1 , length ( qq$x ) , length.out = 5 ) [ c ( 2 : 4 ) ]
            names ( pointInds ) <- c ( "25%ile" , "50%ile", "75%ile")
            plotInds <- seq ( ceiling ( plotThresh [ 1 ] )  , floor ( plotThresh [ 2 ] ) )
            abline ( v = qq$x [ ceiling( pointInds ) ] , col = "gray" , lwd = lwd )
            textInds <- pointInds [ qq$x [ ceiling( pointInds ) ] > xlims [ 1 ] ]
            text ( qq$x [ ceiling( textInds ) ] , ylims [ 2 ] , labels = names ( textInds ) , adj = c ( 1 , 1 ) , srt = 90 )
            init <- F
        }
        lines ( qq$x [ plotInds ], qq$y [ plotInds ], col = "blue" , lwd = lwd , pch = "." , cex = 0.25)
        print ( sum ( qq$y [ seq ( ceiling ( pointInds [ 2 ] ) , ceiling ( pointInds [ 3 ] ) ) ] > qq$x [ seq ( ceiling ( pointInds [ 2 ] ) , ceiling ( pointInds [ 3 ] ) ) ] ))
    }
    
    par ( mgp = c ( 0,0.3,0 ))
    axis(2, at=pos,labels=lab,tck = -0.01, cex=1 , las = 2)
    mtext( ytitle , side=2, cex=1, line=1 )
    
    par ( mgp = c ( 0,0.1,0 ))
    axis(1, at=pos,labels=lab,tck = -0.01, cex=1 )
    mtext ( xtitle, side=1, cex=1, line=1)
    
    # Add panel label:
    par ( fig = c ( 0 , 1 , 0 , 1 ) , new = T )
    par(oma=rep(0,4) )
    par ( mai = rep ( 0.01 , 4 ) , xaxs='i', yaxs='i')
    plot ( 1,1, xlim=c(0,1) , ylim = c ( 0 , 1 ) , axes=FALSE , type='n', xlab='', ylab='')
    text(0,1,panel ,adj=c(0,1))
}