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

###########################################################################
# Script by Sarah Middleton & Hannah Dueck
# ERCC detection assessment

#----------------------------------------------------------------------------------------------
# Note on calculating absolute molecules:
#
#       1 attomole = 602214.15 molecules
#       0.9 uL of a 1:4,000,000 ERCC dilution spiked into our samples
#       number of molecules spiked in = (Orig Conc in attomoles/uL) * ((0.9*602214.15)/4000000)
#                                     = (Orig Conc in attomoles/uL) * 0.13545
#
#       9 nL of a 1:40,000 ERCC dilution in each well
#       number of molecules spiked in = (Orig Conc in attomoles/uL) * ((0.009*602214.15)/40000)
#                                     = (Orig Conc in attomoles/uL) * 0.13545
#
#       > Also: we appear to be using Mix 1 only
#----------------------------------------------------------------------------------------------


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# YOU NEED TO GET THE USER TO GIVE YOU THE AMOUNT OF ERCC ADDED AND THE ERCC DILUTION.
# IN OUR LAB THIS IS TYPICALLY 0.9uL OF ERCC ADDED AT A CONCENTRAION OF 4000000 (i.e. 1:4,000,000).
# SEE CALCULATIONS ABOVE.

output$ercc_ui <- renderUI({
    list(
        fluidRow(
            column(8,
                   tabBox(
                       title = "Detection assessment",
                       width = NULL,
                       tabPanel("% detected vs expected", plotOutput("ercc_prob", height = "600px")),
                       tabPanel("Summary of model fit", verbatimTextOutput("ercc_glm_summary"))
                   ) 
            ),
            column(4,
                   box(title = "ERCC table",
                       width = NULL,
                       status = "primary",
                       solidHeader = T,
                       DT::dataTableOutput("ercc_tbl_preview"),
                       downloadButton('download_ercc_tbl', 'Download', class = "btn btn-success")
                   )
            )
        ),
        
        tabBox(
            title = "ERCC distribution",
            width = 12,
            tabPanel("Plot", plotOutput("ercc_distribution", height = "600px")),
            tabPanel("Coefficient of variation", DT::dataTableOutput("ercc_cv_tbl"), downloadButton('download_ercc_cv_tbl', 'Download',class = "btn btn-success"))
        )
    )
    
})


output$ercc_sb_text <- renderText({
    if(!is.null(input$proc_method) && input$proc_method == "none") {
        cur_scale = "input counts."
    } else {
        cur_scale = paste(input$proc_method, "normalized counts.")
    }
    paste("Data scale in this module is", cur_scale)
})


# load ERCC info (data files from ERCC product website)
erccStds <- read.table("src/built_in_files/ercc_standard_mix_conc.txt", header=T, row.names=2)
rownames(erccStds) <- make.names(rownames(erccStds))

observeEvent(input$recompute_ercc, {
    ercc$convert_ratio <- input$ercc_added * 602214.15/input$ercc_ratio
})

# extract ERCC rows (removed dataset specific restrictions)
ercc <- reactiveValues()
ercc$features <- NULL
ercc$sorted <- NULL
ercc$df_wt_mol <- NULL
ercc$convert_ratio <- 0.1354982

observe ({
    
    # The whole observe only respond to changes in r_data$df and ercc$convert_ratio
    if(is.null(r_data$df)) return()
    tmp_ercc <- as.data.frame(r_data$df[grep("ERCC", rownames(r_data$df)),])
    erccStds$molecules <- erccStds$concentration_in_Mix_1_.attomoles.ul. * ercc$convert_ratio
    
    isolate({
        ercc_df<- tmp_ercc
        ercc_df$PercentDetected <- apply(tmp_ercc, 1, function(x) sum(x > 0)/length(colnames(tmp_ercc)))
        ercc_df$Detected <- apply(tmp_ercc, 1, function(x) sum(x > 0))
        ercc_df$NotDetected <- apply(tmp_ercc, 1, function(x) sum(x == 0))
        ercc_df_merge <- merge(ercc_df, erccStds, by="row.names")
        if(nrow(ercc_df_merge) < 1) {
            ercc$features <- NULL
            ercc$sorted <- NULL
            ercc$df_wt_mol <- NULL
            return()
        }
        
        ercc_df_merge$Log10molecules <- log10(ercc_df_merge$molecules)
        ercc$sorted <- ercc_df_merge[with(ercc_df_merge, order(Log10molecules)), ]
        
        ercc$df_wt_mol <- ercc$sorted %>% dplyr::select(Row.names, molecules, one_of(r_data$sample_name))
        ercc$features<- rownames(ercc$df_wt_mol) <- ercc$df_wt_mol[,1]
        ercc$df_wt_mol <- ercc$df_wt_mol %>% dplyr::select(-Row.names)
    })
})

# ERCC data table

output$ercc_tbl_preview <- DT::renderDataTable({
    if(is.null(ercc$df_wt_mol)) return()
    DT::datatable(ercc$df_wt_mol, options = list(scrollX = TRUE, scrollY = "400px", lengthMenu = c(20, 50, 100)))
})

output$download_ercc_tbl <- downloadHandler(
    filename = function() {
        "ERCC_table.csv"
    },
    content = function(file) {
        write.csv(ercc$df_wt_mol, file)
    }
)

# ERCC glm model summary

ercc_glm <- reactive({
    glm(cbind(Detected, NotDetected) ~ Log10molecules, family=binomial(logit), data=ercc$sorted)
})

output$ercc_glm_summary <- renderPrint({
    if(is.null(ercc$sorted)) return()
    summary(ercc_glm())
})


# ERCC % detected vs expected plot

output$ercc_prob <- renderPlot({
    if(is.null(ercc$sorted)) return()
    glm.out = glm(PercentDetected ~ Log10molecules, family=binomial(logit), data=ercc$sorted)
    y_pred <-predict(glm.out, newdata=data.frame(Log10molecules = 0), type = "response")
    # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    # FOR THE MODEL FIT, IT WOULD BE HELPFUL TO COMPUTE THE Y INTERCEPT, GIVEN AN X INTERCEPT OF 1.0.
    # IN MY CASE, I JUST SET THE Y INTERCEPT MANUALLY (BY EYE) TO BE 0.635.
    ggplot(ercc$sorted, aes(molecules, PercentDetected)) + scale_x_log10() + xlab("Expected Number Molecules") + ylab("Fraction of samples detecting gene (>= 1 read)") + theme_classic() + geom_hline(yintercept=y_pred, color="gray") + geom_vline(xintercept=1, color="gray", linetype="longdash") + stat_smooth(method="glm", method.args = list(family = "binomial"), color="dodgerblue2", fill="lightgray") + geom_point(size=2) 
    # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
})


# ERCC Distribution
plotERCCDistribution <- function (counts) {
    molecules <- sapply ( counts[ , 'molecules' ] , function ( x ) { round ( x , digits = 2 ) } )
    names <- sapply ( rownames ( counts) , function ( x ) { strsplit ( x , 'ERCC.00' ) [[ 1 ]] [ 2 ] } )
    names <- paste ( names , molecules , sep = ', ' )
    
    tord <- order ( molecules )
    
    moleculeCts <- table ( molecules )
    transcriptBins <- list()
    transcriptBinXs <- list()
    spacing <- c(1)
    index <- 1

    for ( i in c ( 1 : length ( moleculeCts ) ) ) {
        start <- spacing [ length ( spacing ) ] + 2
        xs <- c ( start : ( start + moleculeCts [ i ] - 1 ) )
        spacing <- c ( spacing , xs )
        transcriptBinXs [[ i ]] <- c ( min ( xs ) , max ( xs ) ) - 1
        transcriptBins [[ i ]] <- c ( index : ( index + length ( xs ) - 1 ) )
        index <- index + length ( xs )
    }
    spacing <- spacing [ c ( 2 : length ( spacing ) ) ]
    spacing <- spacing - 1
    
    samples <- setdiff ( colnames ( counts ) , 'molecules' )
    
    # Plot:
    x1mar <- 0.5
    x2mar <- 0.05
    y1mar <- 1
    y2mar <- 0.3
    
    ymax_log <- ceiling(log10(max(counts[, samples])))

    if(ymax_log%%2) {
        ymax_log <- ymax_log+1
    } 
    ymax <- 10^ymax_log

    
    par(mai=c(y1mar, x1mar, y2mar , x2mar ) , xaxs= 'i', yaxs = 'i')
    plot ( 1 , 1 ,  xlim = c ( 0 , max ( spacing ) + 1 ), ylim = c ( 0.8, ymax) , axes = FALSE , type = 'n', yaxt='n', xaxt='n', ylab='', xlab='' , main = "Distribution of ERCCs" , log = 'y' )
    mtext (  'Normalized counts + 1'  , side = 2 , line = 2.5 )
    axis ( 2 , at = 10^seq(0,ymax_log,2 ) ,labels = 10^seq(0,ymax_log,2 ) ,cex.axis = 1 )
    axis ( 1 , at = spacing ,labels = names [ tord ] ,cex.axis = 1 , las = 2)
    
    
    ys <- do.call ( c , lapply ( samples , function ( x ) { counts [ , x ]  } ) )
    xs <- do.call ( c , lapply ( samples , function ( x ) { counts [ , 'molecules' ] } ) )
    keep <- ys > 0
    fit <- lm ( ys [ keep ] ~ 0 + xs [ keep ] )
    inflationFactor <- fit$coefficients [ 1 ]
    
    text ( 1 , ymax/2 , labels = paste ('normCounts = ', round ( inflationFactor ) , ' x molecules', sep = '' ) , adj = c ( 0 , 0.5 ))
    
    # Plot expectation
    for ( index in c ( 1 : length ( molecules ) ) ) {
        polygon ( c ( spacing [ index ] - 0.5 , spacing [ index ] + 0.5 , spacing [ index ] + 0.5 , spacing [ index ] - 0.5 ) ,
                  c ( rep ( ( qpois ( 0.025, molecules [ tord [ index ] ] , lower.tail = TRUE )*inflationFactor ) + 1, 2 ) ,  rep ( ( qpois ( 0.025, molecules [ tord [ index ] ] , lower.tail = FALSE )*inflationFactor ) + 1, 2 ) ) , col = 'gray90' , border = NA  )
        lines ( c ( spacing [ index ] - 0.5 , spacing [ index ] + 0.5 ) , rep ( ( molecules [ tord [ index ] ] )*inflationFactor + 1, 2 ) , lwd = 1, col = 'gray65' )
    }
    
    # Plot group averages
    for ( i in c ( 1 : length ( transcriptBins ) ) ) {
        transcriptBinYs <- do.call ( c , lapply ( samples , function ( x ) { as.numeric ( counts [ tord [ transcriptBins [[ i ]] ] , x ] ) } ) )
        transcriptBinAverage <- mean ( transcriptBinYs + 1 )
        lines ( c ( transcriptBinXs [[ i ]] [ 1 ] - 0.7 ,  transcriptBinXs [[ i ]] [ 2 ] + 0.7 ) ,  rep ( transcriptBinAverage , 2 ), col = 'red' , lwd = 1 , lty = 1 )
        sem <- sd ( transcriptBinYs ) / sqrt ( length ( transcriptBinYs ) )
        upperCI <- transcriptBinAverage + ( 2*sem )
        lines ( c ( transcriptBinXs [[ i ]] [ 1 ] - 0.7 ,  transcriptBinXs [[ i ]] [ 2 ] + 0.7 ) ,  rep ( upperCI , 2 ), col = 'red' , lwd = 0.25 , lty = 1 )
        lowerCI <-  transcriptBinAverage - ( 2*sem )
        if ( lowerCI < 1 ) {
            lowerCI <- 1
        }
        lines ( c ( transcriptBinXs [[ i ]] [ 1 ] - 0.7 ,  transcriptBinXs [[ i ]] [ 2 ] + 0.7 ) ,  rep ( lowerCI , 2 ), col = 'red' , lwd = 0.25 , lty = 1 )
        lines ( rep ( transcriptBinXs [[ i ]] [ 1 ] - 0.7  , 2 ) , c ( lowerCI , upperCI ), col = 'red' , lwd = 0.25 )
        lines ( rep ( transcriptBinXs [[ i ]] [ 2 ] + 0.7  , 2 ) , c ( lowerCI , upperCI ), col = 'red' , lwd = 0.25 )
        
    }
    
    # Plot individual samples
    for ( index in c ( 1 : length ( molecules ) ) ) {
        lines ( rep ( spacing [ index ] , 2 ) , range ( counts [ tord [ index ] , samples ] + 1 ) , lwd = 0.5  )
        points ( jitter ( rep ( spacing [ index ] , length ( samples ) ) , 0.1 ), counts[ tord [ index ] , samples ] + 1 , pch = 20 , bg = 'gray65' , fg = 'black', cex = 0.35 )
    }
    
    #legend ( "topleft" , legend = samples , pch = seq ( 21 , 25 , 1 )  )

}

erccCv <- function (counts) {
    
    molecules <- sapply ( counts[ , 'molecules' ] , function ( x ) { round ( x , digits = 2 ) } )
    samples <- setdiff ( colnames ( counts ) , 'molecules' )
    counts <- counts [ , samples ]
    CVs <- apply ( counts , 1, function ( y ) {  sd ( as.numeric ( y ) ) / mean ( as.numeric ( y ) ) } )
    CVs [ rowSums ( counts ) == 0 ] <- 0
    
    # And combine with number of molecules
    return(cbind ( "molecules"= molecules , "cv" = CVs )) 
}

output$ercc_distribution <- renderPlot({
    if(is.null(ercc$df_wt_mol)) return()
    plotERCCDistribution(ercc$df_wt_mol)
})


output$ercc_cv_tbl <- DT::renderDataTable({
    if(is.null(ercc$df_wt_mol)) return()
    DT::datatable(erccCv(ercc$df_wt_mol))
})

output$download_ercc_cv_tbl <- downloadHandler(
    filename = function() { 
        "ERCC_probeCVs.csv"
    },
    content = function(file) {
        write.csv(erccCv(ercc$df_wt_mol), file)
    }
)

