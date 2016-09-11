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


################################## t-SNE ######################################

output$tsne_ui <- renderUI({
    list(
        fluidRow(
            column(6, 
                   box(
                       title = "T-SNE Parameters",
                       status = "primary",
                       width = NULL,
                       wellPanel(
                           numericInput("tsne_perplexity", label = "Perplexity", min = 1, max = 50, value = 1, step = 1),
                           radioButtons("tsne_pca", label = "Perform initial PCA step?", choices = list("Yes" = "TRUE", "No" = "FALSE"), selected = "TRUE", inline = TRUE)
                       )
                   )
            ),
            column(6,
                   box(
                       title = "About t-SNE implemented here",
                       status = "primary",
                       width = NULL,
                       tags$li("Rtsne package is used here, which implements Barnes-Hut t-SNE."),
                       tags$li("If not performing intial PCA, then this will produce results close to vi-SNE."),
                       tags$li("To perform exact vi-SNE, use http://www.c2b2.columbia.edu/danapeerlab/html/cyt.html.")
                   )
                   
            )
        ),
        fluidRow(
            column(6, 
                   enhanced_box(
                       width = NULL,
                       title = "1D", 
                       id = "tsne_1d",
                       status = "primary",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                       ggvis::ggvisOutput("tsne_1d"),
                       fluidRow(
                           column(6, selectInput("tsne_1d_plt_type", "Choose plot type", choices = list( "density" = "density", "histogram" = "hist"))),
                           column(6, uiOutput("tsne_1d_step_ui"))
                       )
                   )
            ),
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "2D", 
                       id = "tsne_2d",
                       status = "warning",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                       ggvis::ggvisOutput("tsne_2d")
                   )  
            )
        ),
        fluidRow(
            column(8, 
                   enhanced_box(
                       width = NULL,
                       title = "3D", 
                       id = "tsne_3d",
                       status = "danger",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,
                       
                       threejs::scatterplotThreeOutput("tsne_3d")
                   ) 
            )
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Jesse Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor Embedding using Barnes-Hut Implementation. R package version 0.10. http://CRAN.R-project.org/package=Rtsne.", class = "citation"),
                tags$li("Amir, E. A. D., Davis, K. L., Tadmor, M. D., Simonds, E. F., Levine, J. H., Bendall, S. C., ... & Pe'er, D. (2013). viSNE enables visualization of high dimensional single-cell data and reveals phenotypic heterogeneity of leukemia. Nature biotechnology, 31(6), 545-552.", class = "citation"),
                tags$li("Darmanis, S., Sloan, S. A., Zhang, Y., Enge, M., Caneda, C., Shuer, L. M., ... & Quake, S. R. (2015). A survey of human brain transcriptome diversity at the single cell level. Proceedings of the National Academy of Sciences, 201507125.", class = "citation")
            )
        )
    )
    
})

output$tsne_1d_step_ui <- renderUI({
    if(is.null(data0()) || is.null(tsne_1d()) || is.null(input$tsne_1d_plt_type)) return()
    if(input$tsne_1d_plt_type == "density") {
        sliderInput("tsne_1d_step",.1, 2, value = 1, step = .1, label = "Bandwidth")
    }
})


tsne_2d <- reactive({
    if(is.null(data0()) || is.null(input$tsne_perplexity)) return() # Parameters not initialized
    
    error_I <- 0
    tryCatch({
        set.seed(input$idv_seed)
        tsne_t <- Rtsne::Rtsne(t(data0()),perplexity = input$tsne_perplexity, theta = 0, pca = as.logical(input$tsne_pca))}, 
        error = function(e){
            session$sendCustomMessage(type = "showalert", "T-sne failed.")
            error_I <<- 1
        })
    if(error_I) return(NULL)
    return(tsne_t)
})

tsne_1d <- reactive({
    if(is.null(data0()) || is.null(input$tsne_perplexity)) return() # Parameters not initialized
    
    error_I <- 0
    tryCatch({
        set.seed(input$idv_seed)
        tsne_t <- Rtsne::Rtsne(t(data0()),perplexity = input$tsne_perplexity, theta = 0, dims = 1, pca = as.logical(input$tsne_pca))}, 
        error = function(e){
            error_I <<- 1
        })
    if(error_I) return(NULL)
    return(tsne_t)
})

tsne_3d <- reactive({
    if(is.null(data0()) || is.null(input$tsne_perplexity)) return() # Parameters not initialized
    
    error_I <- 0
    tryCatch({
        set.seed(input$idv_seed)
        tsne_t <- Rtsne::Rtsne(t(data0()),perplexity = input$tsne_perplexity, theta = 0, dims = 3, pca = as.logical(input$tsne_pca))}, 
        error = function(e){
            error_I <<- 1
        })
    if(error_I) return(NULL)
    return(tsne_t)
})

# 1D tsne
observe({
    tryCatch({
        if(is.null(data0()) || is.null(tsne_1d())) 
            return (data.frame(x = 0) %>% ggvis::ggvis(~x)) 
        projection <- data.frame(tsne_1d()$Y, r_data$sample_name)
        colnames(projection) <- c("D1", "rowname")
        
        if(!is.null(plot_specs$info)){
            projection <- cbind(projection, group = factor(plot_specs$info, levels = unique(plot_specs$info)))
        }
        tsne_1d_tmp <- projection %>% 
            ggvis::ggvis(~D1) %>%
            add_axis("x", title = "D1") %>% 
            set_options(width = 450, height = 300)
        
        if(input$tsne_1d_plt_type == "density") {
            layer_func <- function(...){layer_densities(adjust = input$tsne_1d_step, ...)}
        } else {
            layer_func <- layer_histograms
        }
        
        if(!is.null(plot_specs$info)){
            tsne_1d_tmp %>% 
                group_by(group) %>%
                layer_func(fill = ~group) %>%
                scale_nominal("fill", range = unique(plot_specs$color)) %>%
                add_legend("fill", title = input$coloring_type) %>%
                bind_shiny("tsne_1d")
        } else {
            tsne_1d_tmp %>% 
                layer_func() %>%
                bind_shiny("tsne_1d")
        }
    }, error = function(e){return(NULL)})
    
}) 

# 2D tsne
observe({
    if(is.null(data0()) || is.null(tsne_2d())) return (data.frame(x = 0, y = 0) %>% ggvis::ggvis(~x, ~y)) 
    tryCatch({
    projection <- data.frame(tsne_2d()$Y, r_data$sample_name)
    colnames(projection) <- c("D1", "D2", "rowname")
    
    if(!is.null(plot_specs$info)){
        projection <- cbind(projection, group = factor(plot_specs$info, levels = unique(plot_specs$info)))
    }
    tsne_2d_tmp <- projection %>% 
        ggvis::ggvis(~D1, ~D2, key := ~rowname) %>%
        add_tooltip(d2_tooltip, "hover") %>%
        add_axis("x", title = "D1") %>% 
        add_axis("y", title = "D2") %>%
        set_options(width = 450, height = 300)
    
    if(!is.null(plot_specs$info)){
        tsne_2d_tmp %>% 
            layer_points(fill = ~group, stroke:= "grey", size := 50, size.hover := 200, fillOpacity := 0.6, fillOpacity.hover := 0.9, stroke.hover := "orange") %>% 
            add_legend("fill", title = input$coloring_type) %>%
            scale_nominal("fill", range = unique(plot_specs$color)) %>% 
            bind_shiny("tsne_2d")
    } else {
        tsne_2d_tmp %>% 
            layer_points(fill := "steelBlue", stroke:= "grey", size := 50, size.hover := 200, fillOpacity := 0.6, fillOpacity.hover := 0.9, stroke.hover := "orange")%>% 
            bind_shiny("tsne_2d")
    }
    }, error = function(e){return(NULL)})
}) 

# 3d tsne
output$tsne_3d <- threejs::renderScatterplotThree({
    if(is.null(data0())|| is.null(tsne_3d())) return () 
    projection <- as.data.frame(tsne_3d()$Y)
    colnames(projection) <- c("D1", "D2", "D3")
    if(!is.null(plot_specs$info))
    {
        threejs::scatterplot3js(projection[,1:3], color = plot_specs$color, labels = r_data$sample_name, label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
    } else{
        threejs::scatterplot3js(projection[,1:3], color = "grey", labels = r_data$sample_name, label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
    }
})