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


# hierachical clustering
output$hclust_ui <- renderUI({
    enhanced_box(
        title = "Hierarchical Clustering",
        id = "hierarchical_clust",
        status = "primary",
        width = 12,
        solidHeader = T,
        collapsible = T,
        reportable = T,
        get_html = T,
        register_analysis= T,
        wellPanel(
            fluidRow(
                column(4, selectInput("hc_dist_method", "Distance measure", choices = list("Euclidean" = "euclidean", "Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary", "Minkowski" = "minkowski", "Correlation Distance" = "corr", "SCDE Adjusted Distance" = 'scde'), selected = "euclidean")),
                column(4, uiOutput("hclust_corr_ui"),  uiOutput("hclust_scde_ui")),
                column(4, selectInput("hc_agglo_method", "Agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
            )
        ),
        uiOutput("dend_hclust"), 
        uiOutput("d3_hclust")
    )
})

output$dend_hclust<-renderUI({
    if(input$hclust_package != "dendextend") return()
    plotOutput("hclust")
})

output$hclust_corr_ui <- renderUI({
    if (input$hc_dist_method != 'corr') return()
    selectInput("hclust_cor_method", label = "Correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson")
})

output$hclust_scde_ui <- renderUI({
    if (input$hc_dist_method != 'scde') return()
    selectInput("hclust_scde_dist_method", "SCDE distance", choices = list("Direct drop-out" = "ddo", "Reciprocal weighting" = "rw", "Mode-relative weighting" = "mrw"), selected = "rw")
})

output$d3_hclust <- renderUI({
    if(input$hclust_package != "networkD3") return()
    list(
        wellPanel(
            fluidRow(
                column(6, selectInput("hc_dd_ori", "Orientation of the plot", choices = list("Vertical" = 'vertical', "Horizontal" = 'horizontal'))),
                column(6, selectInput("hc_dd_link_type", "Link type", choices = list("Diagonal" = "diagonal", "Elbow" = "elbow")))
            )
        ),
        fluidRow(
            column(1),
            column(11,
                   networkD3::dendroNetworkOutput("hclust.d3", height = "500px")
            )
        )
    )
})



# distance measure 
hclust0 <- reactive({
    if(!(input$hc_dist_method %in% c('scde', 'corr'))) {
        t(data0()) %>% dist(method = input$hc_dist_method) %>% hclust(method = input$hc_agglo_method)
    } else if(input$hc_dist_method == 'corr') {
        as.dist(1 - cor(data0(), method = input$hclust_cor_method)) %>% hclust(method = input$hc_agglo_method) 
    } else if(input$hc_dist_method == 'scde') {
        if(is.null(input$hclust_scde_dist_method)) return()
        if(is.null(r_data$scde_ddo) && is.null(r_data$scde_rw) && is.null(r_data$scde_mrw)) {
            session$sendCustomMessage(type = "showalert", "Please compute the SCDE-adjusted distance you want to use in SCDE module first.")
            updateTabItems(session, "tabs", "scde")
            return()
        }
        if(input$hclust_scde_dist_method == "ddo") {
            if(is.null(r_data$scde_ddo)) {
                session$sendCustomMessage(type = "showalert", "Please compute Direct drop-out adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                r_data$scde_ddo %>% hclust(method = input$hc_agglo_method)
            }
        } else if(input$hclust_scde_dist_method == "rw") {
            if(is.null(r_data$scde_rw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Reciprocal weighting adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                r_data$scde_rw %>% hclust(method = input$hc_agglo_method)
            } 
        } else if(input$hclust_scde_dist_method == "mrw") {
            if(is.null(r_data$scde_mrw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Mode-relative weighting adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                r_data$scde_mrw %>% hclust(method = input$hc_agglo_method)
            } 
        } 

    }
})


output$hclust <- renderPlot({
    par(mar=c(6.1, 5.1, 5.1, 8.1), xpd=TRUE)
    
    if(is.null(data0()) || is.null(hclust0())) return ()
    hc0 <- hclust0()
    # get max height of the tree, this will be used to adjust the group bar height
    max_height <- max(hc0$height)
    hc1 <- hc0 %>% as.dendrogram()
    if(!is.null(plot_specs$info)) {
            orderedColor <- plot_specs$color[order.dendrogram(hc1)]
            dendextend::labels_colors(hc1) <- orderedColor
            hc1 <- dendextend::assign_values_to_leaves_nodePar(hc1, 1, "lab.cex")
            par(oma = c(4, 1, 1, 1))
            plot(hc1)
            par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
            dendextend::colored_bars(plot_specs$color, hc1, y_scale = max_height/7, rowLabels = input$coloring_type)
            legend("topright", legend = unique(plot_specs$info),  bty="n", fill = unique(plot_specs$color), text.col = unique(plot_specs$color), border = FALSE, y.intersp = 2)
    }
    else
        plot(hc1)
})

output$hclust.d3 <- networkD3::renderDendroNetwork({
    if(is.null(data0()) || is.null(hclust0())) return ()
    Root <- hclust0()
    if(!is.null(plot_specs$info))
        networkD3::dendroNetwork(Root, fontSize = 15, textColour = plot_specs$color, treeOrientation = input$hc_dd_ori, linkColour = 'navyblue', nodeColour = 'grey', textOpacity = 1, opacity = 1,  zoom = T, linkType = input$hc_dd_link_type)
    else
        networkD3::dendroNetwork(Root, fontSize = 15, treeOrientation = input$hc_dd_ori, linkColour = 'navyblue', nodeColour = 'grey', textOpacity = 1, opacity = 1, zoom = T, linkType = input$hc_dd_link_type)
})

