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


output$mst_ui <- renderUI({
    list(
        enhanced_box(
            width = 12,
            title = "Minimum Spanning Tree",
            id = "community_mst",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            
            wellPanel(
                tags$b("Minimum Spanning Tree Parameters"),
                fluidRow(
                    column(4, selectInput("mst_dist_method", "Distance measure", choices = list("Euclidean" = "euclidean", "Correlation Distance" = "correlation", "SCDE Adjusted Distance" = 'scde'), selected = "correlation")),
                    uiOutput('mst_corr_ui'),
                    uiOutput('mst_scde_ui')
                )
            ),
            wellPanel(
                fluidRow(
                    column(4, selectInput("com_algorithm", "Community Detection Algorithm", choices = list("Walktrap" = "walktrap"))),
                    column(4, numericInput("com_wt_step", "Walktrap Steps", value = 4, min = 1, max = 100, step = 1))
                )
            ),
            wellPanel(
                tags$b("Visual Parameters"),
                fluidRow(
                    column(4, checkboxInput("mst_lbl", "Show vertex label", value = F)),
                    column(4, checkboxInput("color_com", "Color community", value = F))
                ),
                fluidRow(
                    column(4, sliderInput("vertex_size", "Vertex size", min = 1, max = 20, value = 5))
                )
            ),
            hr(),
            
            plotOutput("mst_plt", height = "600px"),
            uiOutput("down_com_ui")
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Csardi G, Nepusz T: The igraph software package for complex network research, InterJournal, Complex Systems 1695. 2006. http://igraph.org", class = "citation"),
                tags$li("Darmanis, S., Sloan, S. A., Zhang, Y., Enge, M., Caneda, C., Shuer, L. M., ... & Quake, S. R. (2015). A survey of human brain transcriptome diversity at the single cell level. Proceedings of the National Academy of Sciences, 201507125.", class = "citation")
            )
        ) 
    )
    
}) 

output$mst_corr_ui <- renderUI({
    if(input$mst_dist_method != "correlation") return()
    column(4, selectInput("mst_cor_method", label = "Correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"))
})

output$mst_scde_ui <- renderUI({
    if(input$mst_dist_method != 'scde') return()
    column(4, selectInput("mst_scde_dist_method", "SCDE Distance", choices = list("Direct drop-out" = "ddo", "Reciprocal weighting" = "rw", "Mode-relative weighting" = "mrw"), selected = "rw"))
})


output$down_com_ui <- renderUI({
    if(is.null(r_data$community)) return()
    downloadButton("download_community", "Download Community", class = "btn btn-success")
})

# Construct graph object from data
graph0 <- reactive({
    if(input$mst_dist_method == 'euclidean') {
        dist_mtx <- t(data0()) %>% dist() %>% as.matrix()
    } else if(input$mst_dist_method == 'correlation') {
        dist_mtx <- 1 - cor(data0(), method = input$mst_cor_method)
    } else if(input$mst_dist_method == 'scde') {
        if(is.null(input$mst_scde_dist_method)) return()
        if(is.null(r_data$scde_ddo) && is.null(r_data$scde_rw) && is.null(r_data$scde_mrw)) {
            session$sendCustomMessage(type = "showalert", "Please compute the SCDE-adjusted distance you want to use in SCDE module first.")
            updateTabItems(session, "tabs", "scde")
            return()
        }
        if(input$mst_scde_dist_method == "ddo") {
            if(is.null(r_data$scde_ddo)) {
                session$sendCustomMessage(type = "showalert", "Please compute Direct drop-out adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                dist_mtx <-r_data$scde_ddo %>% as.matrix()
            }
        } else if(input$mst_scde_dist_method == "rw") {
            if(is.null(r_data$scde_rw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Reciprocal weighting adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                dist_mtx <-r_data$scde_rw %>% as.matrix()
            } 
        } else if(input$mst_scde_dist_method == "mrw") {
            if(is.null(r_data$scde_mrw)) {
                session$sendCustomMessage(type = "showalert", "Please compute Mode-relative weighting adjusted distance in SCDE module first.")
                updateTabItems(session, "tabs", "scde")
                return()
            } else {
                dist_mtx <-r_data$scde_mrw %>% as.matrix()
            } 
        } 
    }
    
    igraph::graph.adjacency(dist_mtx, mode="undirected", weighted=TRUE, diag=FALSE, add.colnames='label')
})

# Minimum spanning tree plot
output$mst_plt <- renderPlot({
    if(is.null(graph0())) return()
    mst0 <-igraph::minimum.spanning.tree(graph0(), weights = igraph::E(graph0())$weight)
    
    if(!is.null(input$com_algorithm)) # Perform community detection
    {
        if(input$com_algorithm == "walktrap") {
            coms <- igraph::cluster_walktrap(mst0, steps = input$com_wt_step, merges = TRUE, modularity = TRUE, membership = TRUE)
            r_data$community <- paste0("community_",as.character(igraph::membership(coms)))
        }
    }
    
    label <- NA
    if(input$mst_lbl) {
        label <- V(mst0)$label
    }
    
    if(!is.null(plot_specs$info))
        V(mst0)$color <- plot_specs$color
    
    if(!is.null(input$color_com) && input$color_com) {
        igraph::plot.igraph(mst0, vertex.label=label, vertex.size=input$vertex_size, mark.groups = coms)
    } else {
        igraph::plot.igraph(mst0, vertex.label=label, vertex.size=input$vertex_size)
    }
    
})


output$download_community <- downloadHandler(
    filename = function() { 
        "walktrap_community.csv"
    },
    content = function(file) {
        coms <- r_data$community
        names(coms) <- r_data$sample_name
        write.csv(as.data.frame(coms), file)
    }
)


