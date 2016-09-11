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


# corr heatmap
hmcol <- reactiveValues()
hmcol$val <- RColorBrewer::brewer.pal(9,"Blues") 
hmcol$idx <- 1

output$cor_hm_ui <- renderUI({
    enhanced_box(
        width = 12,
        title = "Sample Correlation", 
        id = "sample_corr_heatmap",
        status = "primary",
        solidHeader = T,
        collapsible = T,
        reportable = T,
        get_html = T,
        register_analysis= T,
        
        wellPanel(
            fluidRow(
                column(4, selectInput("cor_hm_dist", label = "Distance measure", choices = list("Correlation Distance (1-r)" = "corr"))),
                column(4, selectInput("cor_hm_method", label = "Correlation method", choices = list("pearson" = "pearson","spearman" = "spearman", "kendall" = "kendall"), selected = "pearson")),
                column(4, selectInput("cor_hm_agglo_method", "Agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
            )
        ),
        
        conditionalPanel("input.cor_hm_tool == 1", "heatmap.2", plotOutput("hmap_cor_2", height = "600px")),
        conditionalPanel("input.cor_hm_tool == 2", "d3heatmap", d3heatmap::d3heatmapOutput("hmap_cor_d3", height = "600px"))
    )
})

observeEvent(input$hm_col_switcher, {
    colvec <- c("Blues", "GnBu", "Greys", "OrRd", "YlGnBu", "YlOrRd", "RdYlBu")
    hmcol$idx <- (hmcol$idx + 1) %% (length(colvec))
    if(hmcol$idx == 0) {
        hmcol$val <- rev(RColorBrewer::brewer.pal(9,colvec[length(colvec)]))
    } else {
        hmcol$val <- RColorBrewer::brewer.pal(9,colvec[hmcol$idx]) 
    }
})

output$hmap_cor_2 <- renderPlot({
    if(is.null(data0())) return ()
    sample_cor <- cor(data0(), method = input$cor_hm_method)
    distfun1 = function(c) as.dist(1 - c)
    hclustfun1 = function(x, method=input$cor_hm_agglo_method, ...) hclust(x, method=method, ...)

    if(!is.null(plot_specs$info)) {
        if(input$cor_hm_dist == "corr") {
            gplots::heatmap.2(sample_cor, scale="none", Rowv=T, symm = T,dendrogram="both",
                      distfun=distfun1, 
                      hclustfun =hclustfun1, 
                      trace="none", col=hmcol$val, key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=1, density.info="density", revC=T, 
                      ColSideColors = plot_specs$color, margins = c(10,15))
        }

        legend("topright", legend = unique(plot_specs$info), bty="n", fill = plot_specs$legend_color, text.col = plot_specs$legend_color, border=FALSE, y.intersp = 1.2, cex = 0.9)
    }
    else
        gplots::heatmap.2(sample_cor, scale="none", Rowv = T, symm=TRUE, dendrogram="both", 
                  distfun=distfun1, 
                  hclustfun =hclustfun1, 
                  trace="none", col=hmcol$val, key.par=list(cex.axis=1), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=1, density.info="density", revC=T, margins=c(8,6))
})

output$hmap_cor_d3 <- d3heatmap::renderD3heatmap({
    if(is.null(data0())) return ()
    sample_cor <- cor(data0(), method = input$cor_hm_method)
    distfun1 = function(c) as.dist(1 - c)
    hclustfun1 = function(x, method=input$cor_hm_agglo_method, ...) hclust(x, method=method, ...)
    
    d3heatmap::d3heatmap(sample_cor, scale="none", Rowv = T,
              distfun=distfun1, 
              hclustfun =hclustfun1, 
              dendrogram="both", colors=hmcol$val, revC=T)
})