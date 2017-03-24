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




output$dispersion_box_ui <- renderUI({
    list(
        enhanced_box(
            title = "Mean Variability Plot",
            id = "mean_var_plot",
            status = "success",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            
            wellPanel(
                fluidRow(
                    column(6, 
                           shinyBS::tipify(
                               numericInput("meanvar_bins", "Number of bins", min = 1, max = 50, step = 1, value = 20),
                               title = "Total number of bins to use in the scaled analysis", placement = "bottom", options = list(container = "body")
                           )
                    )
                ),
                fluidRow(
                    column(3,
                           shinyBS::tipify(
                               numericInput("meanvar_y_cutoff1", "Dispersion bottom cutoff", min = 1, max = 10, step = 1, value = 2),
                               title = "E.g, Setting the cutoff to 2 identifies genes that are more than two standard deviations away from the average dispersion within a bin.", placement = "bottom", options = list(container = "body")
                           )
                    ),
                    column(3,
                           shinyBS::tipify(
                               numericInput("meanvar_y_cutoff2", "Dispersion top cutoff", min = 1, max = 20, step = 1, value = 12),
                               title = "Top cutoff on y-axis for identifying variable genes.", placement = "bottom", options = list(container = "body")
                           )
                    ),
                    column(3,
                           shinyBS::tipify(
                               numericInput("meanvar_x_cutoff1", "Expression bottom cutoff", min = 1, max = 10, step = 1, value = 1),
                               title = "Bottom cutoff on x-axis for identifying variable genes.", placement = "bottom", options = list(container = "body")
                           )
                    ),
                    column(3,
                           shinyBS::tipify(
                               numericInput("meanvar_x_cutoff2", "Expression top cutoff", min = 1, max = 20, step = 1, value = 8),
                               title = "Top cutoff on x-axis for identifying variable genes.", placement = "bottom", options = list(container = "body")
                           )
                    )
                )
            ),
            
            fluidRow(
                column(4,
                       DT::dataTableOutput("mean_var_genes"),
                       downloadButton("download_mean_var_genes", "Download", class = "btn btn-success")
                ),
                column(8, 
                       a(id = "meanvar_help_btn", icon("question-circle")),
                       shinyBS::bsModal(id = "meanvar_help", title = "About mean variability plot", trigger = "meanvar_help_btn", size = "small", list(
                           tags$li("This plot is generated using the Seurat package, for details please go to http://www.satijalab.org/clustertutorial1.html, or type '?mean.var.plot' in the R console."),
                           tags$li("Description: Identifies genes that are outliers on a 'mean variability plot'. First, uses a function to calculate average expression (fxn.x) and dispersion (fxn.y) for each gene. Next, divides genes into num.bin (deafult 20) bins based on their average expression, and calculates z-scores for dispersion within each bin. "),
                           tags$li("The purpose of this is to identify variable genes while controlling for the strong relationship between variability and average expression.")
                       )
                       ),
                       tags$p("The X-axis is the mean expression level, and Y-axis is the log(Variance/mean). The results are plotted in log-space."),
                       plotOutput("mean_var_plt", height = "600px")
                )
            ),
            tags$b("This plot is generated using Seurat package. Citation:"),
            tags$li("Rahul Satija (2015). Seurat: Seurat : R toolkit for single cell genomics. R package version 1.2.1. http://www.satijalab.org/seurat.", class = "citation")
        ),
        
        enhanced_box(
            title = "Per-feature Standard Deviation",
            id = "mean_sd_plot",
            status = "info",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            get_pdf = T,
            register_analysis= T,
            
            tags$p("Per-feature standard deviation (taken across samples), against the rank of the mean, for the log transformed data."),
            plotOutput("meansd_plt", height = "600px"),
            tags$b("This plot is generated using vsn package. Citation:"),
            tags$li("Wolfgang Huber, Anja von Heydebreck, Holger Sueltmann, Annemarie Poustka and Martin Vingron. Variance Stabilization Applied to Microarray Data
                    Calibration and to the Quantification of Differential Expression. Bioinformatics 18, S96-S104 (2002).", class = "citation")
            )
    )
})


# Function swiped from Seurat
meanNormFunction=function(data,myfuncX,myfuncY,nBin=20) {
    data_x=apply(data,1,myfuncX)
    data_y=apply(data,1,myfuncY)
    data_x_bin=cut(data_x,nBin)
    names(data_x_bin)=names(data_x)
    mean_y=tapply(data_y,data_x_bin,mean)
    sd_y=tapply(data_y,data_x_bin,sd)
    return((data_y-mean_y[as.numeric(data_x_bin)])/sd_y[as.numeric(data_x_bin)])
}

# Function swiped from Seurat
mean.var.plot <- function(df, fxn.x, fxn.y,do.plot=TRUE,set.var.genes=TRUE,do.text=TRUE,
                          x.low.cutoff=4,x.high.cutoff=8,y.cutoff=1,y.high.cutoff=12,cex.use=0.5,cex.text.use=0.5,do.spike=FALSE, 
                          pch.use=16, col.use="black", spike.col.use="red",plot.both=FALSE,do.contour=TRUE,
                          contour.lwd=3, contour.col="white", contour.lty=2,num.bin=20) {
    result <- list()
    
    data=df
    data.x=apply(data,1,fxn.x); data.y=apply(data,1,fxn.y); data.x[is.na(data.x)]=0
    data.norm.y=meanNormFunction(data,fxn.x,fxn.y,num.bin)
    data.norm.y[is.na(data.norm.y)]=0
    names(data.norm.y)=names(data.x)
    pass.cutoff=names(data.x)[which(((data.x>x.low.cutoff) & (data.x<x.high.cutoff)) & (data.norm.y>y.cutoff) & (data.norm.y < y.high.cutoff))]
    mv.df=data.frame(data.x,data.y,data.norm.y)
    rownames(mv.df)=rownames(data)
    result$mean.var=mv.df
    if (do.spike) spike.genes=rownames(subr(data,"^ERCC"))
    if (do.plot) {
        if (plot.both) {
            par(mfrow=c(1,2)) 
            smoothScatter(data.x,data.y,pch=pch.use,cex=cex.use,col=col.use,xlab="Average expression",ylab="Dispersion",nrpoints=Inf)
            
            if (do.contour) {
                data.kde=MASS::kde2d(data.x,data.y)
                contour(data.kde,add=TRUE,lwd=contour.lwd,col=contour.col,lty=contour.lty)
            }
            if (do.spike) points(data.x[spike.genes],data.y[spike.genes],pch=16,cex=cex.use,col=spike.col.use)
            if(do.text) text(data.x[pass.cutoff],data.y[pass.cutoff],pass.cutoff,cex=cex.text.use)
        }
        smoothScatter(data.x,data.norm.y,pch=pch.use,cex=cex.use,col=col.use,xlab="Average expression",ylab="Dispersion",nrpoints=Inf)
        if (do.contour) {
            data.kde=MASS::kde2d(data.x,data.norm.y)
            contour(data.kde,add=TRUE,lwd=contour.lwd,col=contour.col,lty=contour.lty)
        }
        if (do.spike) points(data.x[spike.genes],data.norm.y[spike.genes],pch=16,cex=cex.use,col=spike.col.use,nrpoints=Inf)
        if(do.text) text(data.x[pass.cutoff],data.norm.y[pass.cutoff],pass.cutoff,cex=cex.text.use)
    }
    if (set.var.genes) { 
        result$var.genes=pass.cutoff
        return(result)
        if (!set.var.genes) return(pass.cutoff)
    }
    
}

output$mean_var_plt <- renderPlot({
    if(input$meanvar_y_cutoff1 >= input$meanvar_y_cutoff2 || input$meanvar_x_cutoff1 >= input$meanvar_x_cutoff2) {
        session$sendCustomMessage(type = "showalert", "Bottom cutoff must be lower than top cutoff.")
        return()
    }
    
    r_data$seurat <- mean.var.plot(log10(r_data$df+1),
                                   y.cutoff = input$meanvar_y_cutoff1, y.high.cutoff = input$meanvar_y_cutoff2, 
                                   x.low.cutoff = input$meanvar_x_cutoff1, x.high.cutoff = input$meanvar_x_cutoff2, 
                                   fxn.x = function(x){return(log(mean(exp(x) - 1) + 1))}, 
                                   fxn.y = function(x){return(log(var(exp(x) - 1)/mean(exp(x) - 1)))}, 
                                   num.bin = input$meanvar_bins)
})

output$mean_var_genes <- DT::renderDataTable(({
    if(is.null(r_data$seurat)) return()
    DT::datatable(data.frame(var_genes = r_data$seurat$var.genes), extensions = c('Responsive'), options = list(
        scrollX = T, scrollY = "400px", lengthMenu = c(20, 50, 100)
    ))
}))

output$download_mean_var_genes <- downloadHandler(
    filename = "seurat_var_genes.csv",
    content = function(file) {
        write.csv(data.frame(var_genes = r_data$seurat$var.genes), file)
    }
)


# meanSDplot function swiped from VSN
rowV <- function (x, mean, ...) 
{
    sqr = function(x) x * x
    n = rowSums(!is.na(x))
    n[n < 1] = NA
    if (missing(mean)) 
        mean = rowMeans(x, ...)
    return(rowSums(sqr(x - mean), ...)/(n - 1))
}


meanSdPlot <- function(x, ranks=TRUE, xlab = ifelse(ranks, "rank(mean)", "mean"),
                       ylab = "sd", pch, plot = TRUE, bins = 50, ...) {
    
    stopifnot(is.logical(ranks), length(ranks)==1, !is.na(ranks))
    
    n = nrow(x)
    if (n == 0L) {
        warning("In 'meanSdPlot': matrix has 0 rows, there is nothing to be done.")
        return()
    }
    if (!missing(pch)) {
        warning("In 'meanSdPlot': 'pch' is ignored.")
    }
    
    px   = rowMeans(x, na.rm=TRUE)
    py   = sqrt(rowV(x, mean=px, na.rm=TRUE))
    rpx  = rank(px, na.last=FALSE, ties.method = "random")
    
    ## run median with centers at dm, 2*dm, 3*dm,... and width 2*dm
    dm        = 0.025
    midpoints = seq(dm, 1-dm, by = dm)
    within    = function(x, x1, x2) { x>=x1 & x<=x2 }
    mediwind  = function(mp) median(py[within(rpx/n, mp-2*dm, mp+2*dm)], na.rm=TRUE)
    rq.sds    = sapply(midpoints, mediwind)
    
    res = if(ranks) {
        list(rank=midpoints*n, sd=rq.sds, px=rpx, py=py)
    } else {
        list(quantile=quantile(px, probs=midpoints, na.rm=TRUE), sd=rq.sds, px=px, py=py)
    }
    
    fmt = function() function(x) format(round(x, 0), nsmall = 0L, scientific = FALSE)
    
    res$gg = ggplot(data.frame(px = res$px, py = res$py),
                    aes(x = px, y = py)) + xlab(xlab) + ylab(ylab) +
        stat_binhex(bins = bins, ...) +
        scale_fill_gradient(name = "count", trans = "log", labels = fmt()) + 
        geom_line(aes(x = x, y = y), data = data.frame(x = res[[1]], y = res$sd), color = "red")
    
    if (plot) print(res$gg)
    
    return(invisible(res))
}


output$meansd_plt <- renderPlot({
    if(is.null(r_data$df)) return()
    meanSdPlot(log10(as.matrix(r_data$df) + 1), bins = 100) 
})











