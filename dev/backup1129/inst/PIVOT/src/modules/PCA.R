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

################################ PCA UI #################################

output$pca_ui <- renderUI({
    list(
        fluidRow(
            column(6, 
                   tabBox(
                       title = "Summary", 
                       width = NULL,
                       #status = "success",
                       #solidHeader = T,
                       tabPanel(
                           title = "Variance Explained",
                           DT::dataTableOutput("pca1_summary_tbl"),
                           downloadButton('download_pca1_summary', 'Download', class = "btn btn-success")
                       ),
                       tabPanel(
                           title = "Loading Table",
                           DT::dataTableOutput("pca1_loadings_tbl"),
                           downloadButton('download_pca1_loadings', 'Download', class = "btn btn-success")
                       ),
                       tabPanel(
                           title = "Projection Table",
                           DT::dataTableOutput("pca1_projection_tbl"),
                           downloadButton('download_pca1_projection', 'Download', class = "btn btn-success")
                       )
                   )
            ),
            column(6, 
                   enhanced_box(
                       width = NULL,
                       title = "Scree Plot",
                       id = "pca_scree",
                       status = "info",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                       ggvisOutput("pca1_scree")
                   )
            )
        ),
        fluidRow(
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "1D projection", 
                       id = "pca_1d",
                       status = "primary",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                       ggvisOutput("pca1_1d"),
                       fluidRow(
                           column(4, selectInput("pca_1d_plt_type", "Choose plot type", choices = list("density" = "density", "histogram" = "hist"), selected = "density")),
                           column(4, uiOutput("pca_1d_pc_ui")),
                           column(4, uiOutput("pca_1d_step_ui"))
                       )
                   ),
                   enhanced_box(
                       width = NULL,
                       title = "3D projection", 
                       id = "pca_3d",
                       status = "danger",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                       threejs::scatterplotThreeOutput("pca1_3d")
                   )  
            ),
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "2D projection", 
                       id = "pca_2d",
                       status = "warning",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                       ggvisOutput("pca1_2d"),
                       fluidRow(
                           column(6, uiOutput("pca_x_ui")),
                           column(6, uiOutput("pca_y_ui"))
                       ),
                       fluidRow(
                           column(12,
                                  actionButton("show_biplot", label = "Show Biplot", class = "btn btn-warning"),
                                  uiOutput("biplot_ui")
                           )
                           
                       )
                   )
            )
            
        )
    )
})

output$pca_1d_step_ui <- renderUI({
    if(is.null(r_data$pca) || is.null(input$pca_1d_plt_type)) return()
    if(input$pca_1d_plt_type == "density") {
        sliderInput("pca_1d_step",.1, 2, value = 1, step = .1, label = "Bandwidth")
    }
})



################################ PCA function module #################################

# PCA
observe({
    if(is.null(data0())) return () 
    error_I <- 0
    
    r_data$pca <- tryCatch({
        prcomp(t(data0()))
    },
    error = function(e) {
        session$sendCustomMessage(type = "showalert", "PCA failed.")
        r_data$pca <- NULL
        r_data$pca_var <- NULL
        r_data$pca_var_exp <- NULL
        return()
    })
    r_data$pca_var <- data.frame(variances=r_data$pca$sdev^2, pcomp=1:length(r_data$pca$sdev))
    r_data$pca_var_exp <- r_data$pca_var$variances / sum(r_data$pca_var$variances)
})


# pca summary
output$pca1_summary_tbl <- DT::renderDataTable({
    if(is.null(r_data$pca)) return () 
    DT::datatable(summary(r_data$pca)$importance, options = list(scrollX = TRUE, scrollY = "210px", searching=F))
})

output$download_pca1_summary <- downloadHandler(
    filename = function() { 
        "pca_vars_explained.csv"
    },
    content = function(file) {
        write.csv(summary(r_data$pca)$importance, file)
    }
)

# PCA projections

output$pca1_projection_tbl <- DT::renderDataTable({
    if (is.null(r_data$pca)) return()
    DT::datatable(t(r_data$pca$x), options = list(scrollX = TRUE, scrollY = "210px", searching=T))
})

output$download_pca1_projection <- downloadHandler(
    filename = function() { 
        "pca_projection.csv"
    },
    content = function(file) {
        write.csv(t(r_data$pca$x), file)
    }
)


# PCA loadings
output$pca1_loadings_tbl <- DT::renderDataTable({
    if(is.null(r_data$pca)) return () 
    DT::datatable(r_data$pca$rotation, options = list(scrollX = TRUE, scrollY = "210px"))
})

output$download_pca1_loadings <- downloadHandler(
    filename = function() { 
        "pca_loadings.csv"
    },
    content = function(file) {
        write.csv(r_data$pca$rotation, file)
    }
)

# scree plot
scree_tooltip <- function(df) {
    if (is.null(df)) return(NULL)
    if (is.null(df$pcomp)) return(NULL)
    
    paste0("<b>principal component ", df$pcomp, "</b><br>",
           "variance: ",round(df$variances, digits = 2), "<br>", 
           "variance_explained: ", round(r_data$pca_var_exp[df$pcomp] * 100, digits = 1), "%")
}

observe({
    if(is.null(r_data$pca_var)) return (data.frame(x = 0) %>% ggvis(~x)) 
    r_data$pca_var %>% 
        ggvis(~pcomp, ~variances) %>% 
        layer_points(size := 80, size.hover:= 200, fillOpacity := 0.5, fillOpacity.hover := 0.5)%>%
        layer_bars(fill := "gray", opacity := 0.8) %>%
        add_tooltip(scree_tooltip, "hover") %>%
        layer_lines(stroke := "grey") %>%
        add_axis("x", title = "Principle Component") %>% 
        set_options(width = 450, height = 350) %>%
        bind_shiny("pca1_scree")
})

# 1d_pca
# ui
output$pca_1d_pc_ui <- renderUI({
    if(is.null(r_data$pca)) return()
    projection <- as.data.frame(r_data$pca$x)
    PCs <- colnames(projection)
    PC_options <- as.list(PCs)
    names(PC_options) <- PCs
    selectInput("pca_1d_pc", label = tags$p("PC on X axis"), choices = PC_options, selected = PC_options[[1]])
})

# show 1d projection plot
observe({
    tryCatch({
        if(is.null(r_data$pca) || is.null(input$pca_1d_pc)) return (data.frame(x = 0) %>% ggvis(~x)) 
        projection <- as.data.frame(r_data$pca$x) 
        if(!is.null(plot_specs$info)){
            projection <- cbind(projection, group = factor(plot_specs$info, levels = unique(plot_specs$info)))
        }
        pca_1d_tmp <- projection %>% 
            ggvis(x = prop("x", as.symbol(input$pca_1d_pc))) %>%
            add_axis("x", title = paste0(input$pca_1d_pc, " (",round(r_data$pca_var_exp[as.numeric(gsub("PC","",input$pca_1d_pc))]*100, digits = 1), "%)")) %>% 
            set_options(width = 450, height = 300)
        
        if(input$pca_1d_plt_type == "density") {
            layer_func <- function(...){layer_densities(adjust = input$pca_1d_step, ...)}
        } else {
            layer_func <- layer_histograms
        }
        
        if(!is.null(plot_specs$info)){
            pca_1d_tmp %>%
                group_by(group) %>%
                layer_func(fill = ~group) %>%
                scale_nominal("fill", range = unique(plot_specs$color)) %>%
                add_legend("fill", title = input$coloring_type) %>%
                bind_shiny("pca1_1d")
        } else {
            pca_1d_tmp %>% 
                layer_func() %>%
                bind_shiny("pca1_1d")
        }
    }, error = function(e){
            return()
        })
    
}) 



# 2d_pca
d2_tooltip <- function(df) {
    if (is.null(df)) return(NULL)
    df$rowname
}

# 2d_pca select principal component UI
output$pca_x_ui <- renderUI({
    if(is.null(r_data$pca)) return()
    projection <- as.data.frame(r_data$pca$x)
    PCs <- colnames(projection)
    PC_options <- as.list(PCs)
    names(PC_options) <- PCs
    selectInput("pca_x", label = tags$p("PC on X axis"), choices = PC_options, selected = PC_options[[1]])
})

output$pca_y_ui <- renderUI({
    if(is.null(r_data$pca)) return()
    projection <- as.data.frame(r_data$pca$x)
    if (is.null(projection$PC2)) return()
    PCs <- colnames(projection)
    PC_options <- as.list(PCs)
    names(PC_options) <- PCs
    selectInput("pca_y", label = tags$p("PC on Y axis"), choices = PC_options, selected = PC_options[[2]])
})

# 2d_pca ggvis Plot
observe({
    if(is.null(r_data$pca) || is.null(input$pca_x) || is.null(input$pca_y)) return (data.frame(x = 0, y = 0) %>% ggvis(~x, ~y))
    tryCatch({
    projection <- as.data.frame(r_data$pca$x)
    
    if(!is.null(plot_specs$info)){
        projection <- cbind(projection, group = factor(plot_specs$info, levels = unique(plot_specs$info)))
    }
    if (is.null(projection$PC2)) return(data.frame(x = 0, y = 0) %>% ggvis(~x, ~y))
    pca_2d1_tmp <- projection %>% 
        tibble::rownames_to_column() %>%
        ggvis(x = prop("x", as.symbol(input$pca_x)), y = prop("y", as.symbol(input$pca_y)), key := ~rowname) %>%
        add_tooltip(d2_tooltip, "hover") %>%
        add_axis("x", title = paste0(input$pca_x, " (",round(r_data$pca_var_exp[as.numeric(gsub("PC","",input$pca_x))]*100, digits = 1), "%)")) %>% 
        add_axis("y", title = paste0(input$pca_y, " (",round(r_data$pca_var_exp[as.numeric(gsub("PC","",input$pca_y))]*100, digits = 1), "%)")) %>%
        set_options(width = 450, height = 300)
    
    if(!is.null(plot_specs$info)){
        pca_2d1_tmp %>% 
            layer_points(fill = ~group, stroke:= "grey", size := 50, size.hover := 200, fillOpacity := 0.6, fillOpacity.hover := 0.9, stroke.hover := "orange") %>% 
            add_legend("fill", title = input$coloring_type) %>%
            scale_nominal("fill", range = unique(plot_specs$color)) %>% 
            bind_shiny("pca1_2d")
    } else {
        pca_2d1_tmp %>% 
            layer_points(fill := "steelBlue", stroke:= "grey", size := 50, size.hover := 200, fillOpacity := 0.6, fillOpacity.hover := 0.9, stroke.hover := "orange")%>% 
            bind_shiny("pca1_2d")
    }
    }, error = function(e){
        return()
    })
}) 



output$biplot_ui <- renderUI({
    if(input$show_biplot)
        plotOutput("pca1_biplot", width = "600px", height = "600px")
})
#
# 2d ggbiplot
output$pca1_biplot <- renderPlot({
    if(is.null(r_data$pca)) return () 
    if(!is.null(plot_specs$info)){
        groups <- factor(plot_specs$info, levels = unique(plot_specs$info))

        ggbiplot::ggbiplot(r_data$pca, groups = groups, ellipse = TRUE, circle = TRUE, varname.size = 3, labels.size=3) +
            geom_point(aes(colour=groups), size = 3) +
            scale_colour_manual(values = unique(plot_specs$color)) + 
            theme(legend.direction = 'horizontal', legend.position = 'top')
    } else {
        ggbiplot::ggbiplot(r_data$pca, ellipse = TRUE, circle = TRUE, varname.size = 3, labels.size=3) +
            geom_point(size = 3) 
    }
})


# 3d_pca
output$pca1_3d <- threejs::renderScatterplotThree({
    if(is.null(r_data$pca)) return () 
    projection <- as.data.frame(r_data$pca$x)
    if(ncol(projection) < 3) return()
    if(!is.null(plot_specs$info)){
        threejs::scatterplot3js(projection[,1:3], color = plot_specs$color, labels = rownames(projection), label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
    } else{
        threejs::scatterplot3js(projection[,1:3], color = "grey", labels = rownames(projection), label.margin="150px 150px 150px 150px", size = 1.2, renderer = "canvas")
    }
})

