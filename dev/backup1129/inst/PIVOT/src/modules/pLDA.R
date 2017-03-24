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

################################ penalizedLDA UI #################################

output$plda_ui <- renderUI({
    if(is.null(data0())) return()
    if(is.null(r_data$group) || length(unique(r_data$group)) < 2) {
        return(tags$p("Group information is required for this module"))
    }
    
    list(
        enhanced_box(
            width = 12,
            title = "PenalizedLDA", 
            id = "plda_feature",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            wellPanel(
                fluidRow(
                    column(4, selectInput("plda_select_feature", label = "Perform PenalizedLDA on", choices = list("All features" = "all", "Custom feature list" = "custom"))),
                    column(4, uiOutput("plda_K_ui")),
                    column(4, numericInput("plda_L", "Lasso penality tuning parameter",
                                           min = 0.01,
                                           max = 1,
                                           step = 0.01,
                                           value = .14))
                ),
                fluidRow(
                    column(5, uiOutput("plda_feature_upload_ui")),
                    column(7, uiOutput("plda_feature_upload_text"))
                )
            ),
            actionButton("run_plda", "Run", class = "btn-info"),
            plotOutput("plda_feature_plot"),
            DT::dataTableOutput("plda_discrim_tbl"),
            downloadButton('download_plda_discrim', 'Download', class = "btn btn-success")
        ),
        fluidRow(
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "1D Projection", 
                       id = "plda_1d",
                       status = "warning",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                     ggvisOutput("plda_1d_proj_plt"),
                     fluidRow(
                         column(4, selectInput("plda_1d_plt_type", "Choose plot type", choices = list("density" = "density", "histogram" = "hist"), selected = "density")),
                         column(4, uiOutput("plda_1d_plt_dim_ui")),
                         column(4, uiOutput("plda_1d_plt_step_ui"))
                     )
                 )
  
                 
            ),
            column(6,
                   enhanced_box(
                       width = NULL,
                       title = "2D Projection", 
                       id = "plda_2d",
                       status = "danger",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                     ggvisOutput("plda_2d_proj_plt"), 
                     fluidRow(
                         column(6, uiOutput("plda_x_ui")),
                         column(6, uiOutput("plda_y_ui"))
                     )
                 )
            )
        ),
        fluidRow(
            column(8, 
                   enhanced_box(
                       width = NULL,
                       title = "3D Projection", 
                       id = "plda_3d",
                       status = "success",
                       solidHeader = T,
                       collapsible = T,
                       reportable = T,
                       get_html = T,
                       register_analysis= T,

                       threejs::scatterplotThreeOutput("plda_3d_proj_plt")  
                   )
            )
        )
    )
    
})

output$plda_feature_upload_ui <- renderUI({
    if(input$plda_select_feature != "custom") return()
    content <- list(
        fluidRow(
            column(6, 
                   wellPanel(
                       fileInput('plda_list_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       checkboxInput('plda_header', 'Header', value = F),
                       radioButtons('plda_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                       radioButtons('plda_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                       actionButton("plda_list_submit", "Submit List", class = "btn btn-info")
                   ),
                   uiOutput("plda_feature_upload_text_inmodal")
            ),
            column(6,
                   tags$p("[feature name in 1st column]"),
                   DT::dataTableOutput('plda_list_tbl_show')
            )
        )
    )
    
    list(
        actionButton("plda_custom_btn", label = "Upload a custom feature list for penalized LDA", class = "btn-warning"),
        shinyBS::bsModal(id = "plda_custom_modal", "Upload a custom gene list", "plda_custom_btn", size = "large", content) 
    )
})

output$plda_feature_upload_text <- renderUI({
    if(input$plda_select_feature != "custom") return()
    if(is.null(r_data$plda_flist)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$plda_flist), "genes have been successfully uploaded.")), style = "font-size:110%;")
    )
})

output$plda_K_ui <- renderUI({
    numericInput("plda_K", "Number of discriminant vectors",
                 min = 1, max = length(unique(r_data$group)) - 1, step = 1,
                 value = length(unique(r_data$group)) - 1)
})


output$plda_1d_plt_dim_ui <- renderUI({
    if(is.null(r_data$plda)) return()
    dims <- colnames(r_data$plda$discrim)[-1]
    names(dims) <- dims
    selectInput("plda_1d_plt_dim", "Discriminant vector", choices = dims)
})


output$plda_1d_plt_step_ui <- renderUI({
    if(is.null(r_data$plda) || is.null(input$plda_1d_plt_type)) return()
    if(input$plda_1d_plt_type == "density") {
        sliderInput("plda_1d_step",.1, 2, value = 1, step = .1, label = "Bandwidth")
    }
    
})

make_plda <- function(df, group, lambda, K) {
    result <- list()
    
    plda1 <- penalizedLDA::PenalizedLDA(t(df), as.numeric(as.factor(group)), lambda = lambda, K = K)
    proj1 <- plda1$xproj
    proj1 <- as.data.frame(proj1)
    rownames(proj1) <- colnames(df)
    proj1$group <- group
    
    discrim1<-as.data.frame(plda1$discrim)
    rownames(discrim1) <- rownames(df)
    discrim1 = subset(discrim1, rowSums(discrim1)!=0, drop = F)
    discrim1<-discrim1 %>% tibble::rownames_to_column("gene") %>% dplyr::arrange(-abs(V1)) 
    
    result$plda <- plda1
    result$proj <- proj1
    result$discrim <- discrim1
    return(result)
}

plda_gene <- reactiveValues()
plda_gene$tbl <- NULL

# process the upload feature list
observe({
    inFile <- input$plda_list_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            plda_gene$tbl <- read.csv(inFile$datapath, header=input$plda_header, sep=input$plda_sep, quote=input$plda_quote)
        }, 
        error = function(e){
            error_I <<- 1
        })
    }
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Unsupported file format.")
        return()
    }
})

output$plda_list_tbl_show <- DT::renderDataTable({
    if(is.null(plda_gene$tbl)) return()
    DT::datatable(plda_gene$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})

observeEvent(input$plda_list_submit, {
    
    # First process the marker feature file and get the list
    if (is.null(plda_gene$tbl) || nrow(plda_gene$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return(NULL)
    }
    marker_names <- make.names(as.character(unique(plda_gene$tbl[,1])))
    
    cur_flist <- rownames(r_data$raw)
    
    
    flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
    flist <- flist[!is.na(flist)]
    if(length(flist) != length(marker_names)) {
        message_gl <- paste0(length(marker_names) - length(flist)," features in your gene list (", length(marker_names),") are not found in the current dataset.")
        session$sendCustomMessage(type = "showalert", message_gl)
    }
    r_data$plda_flist <- flist
})


observeEvent(input$run_plda, {
    if(is.null(data0()) || is.null(r_data$group)) return()
    
    if(input$plda_K > length(unique(r_data$group)) - 1) {
        session$sendCustomMessage(type = "showalert", "The number of discriminant vectors must be no greater than (number of classes - 1).")
        return()
    }
    
    if(input$plda_select_feature == "custom") {
        if(is.null(r_data$plda_flist)) {
            session$sendCustomMessage(type = "showalert", "Please upload your feature list.")
            return()
        } 
        plda_data <- data0()[r_data$plda_flist, ]
    } else {
        plda_data <- data0()
    }
    
    tryCatch({
        r_data$plda<-make_plda(plda_data, r_data$group, lambda=input$plda_L,K=input$plda_K)
    }, 
        error = function(e) {
            session$sendCustomMessage(type = "showalert", "PenalizdeLDA failed. Please recheck your parameters.")
            return()
        }
    )

})

output$plda_feature_plot <- renderPlot({
    if(is.null(r_data$plda)) return()
    plot(r_data$plda$plda)
})


# show 1d projection plot
observe({
    tryCatch({
        if(is.null(r_data$plda)) return (data.frame(x = 0) %>% ggvis(~x)) 
        if(is.null(input$plda_1d_plt_type) || is.null(input$plda_1d_plt_dim) || is.null(input$plda_1d_step)) return()
        projection <- as.data.frame(r_data$plda$proj) 
        group_color <- get_color_vector(r_data$group, pal = group_pal$val, maxCol = length(unique(r_data$group)))
        cols<-unique(group_color)
        names(cols) <- unique(r_data$group)

        plda_1d_tmp <- projection %>% 
            ggvis(x = prop("x", as.symbol(input$plda_1d_plt_dim))) %>%
            add_axis("x", title = input$plda_1d_plt_dim) %>% 
            set_options(width = 450, height = 300) %>%
            group_by(group)
        
        if(input$plda_1d_plt_type == "density") {
            plda_1d_tmp %>% layer_densities(adjust = input$plda_1d_step, fill = ~group) %>%
                scale_nominal("fill", range = unique(group_color)) %>%
                add_legend("fill", title = "Group") %>%
                bind_shiny("plda_1d_proj_plt")
        } else {
            plda_1d_tmp %>% layer_histograms(fill = ~group) %>%
                scale_nominal("fill", range = unique(group_color)) %>%
                add_legend("fill", title = "Group") %>%
                bind_shiny("plda_1d_proj_plt")
        }

    }, error = function(e){
        #print(e) # Comment out, probably just a 1D density bug in ggvis
    })
    
}) 


output$plda_discrim_tbl <- DT::renderDataTable({
    if(is.null(r_data$plda)) return()
    DT::datatable(r_data$plda$discrim)
})

output$download_plda_discrim <-downloadHandler(
    filename = "discrim.csv",
    content = function(file) {
        if(is.null(r_data$plda)) return()
        write.csv(r_data$plda$discrim, file)
    })


# 2d_plda select discriminant vector UI
output$plda_x_ui <- renderUI({
    if(is.null(r_data$plda)) return()
    dims <- colnames(r_data$plda$discrim)[-1]
    names(dims) <- dims
    selectInput("plda_2d_x", "Discriminant vector on X axis", choices = dims)
})

output$plda_y_ui <- renderUI({
    if(is.null(r_data$plda)) return()
    dims <- colnames(r_data$plda$discrim)[-1]
    if(length(dims) < 2) return()
    names(dims) <- dims
    selectInput("plda_2d_y", "Discriminant vector on Y axis", choices = dims, selected = dims[2])
})

# plda 2d plot
observe({
    if(is.null(r_data$plda)) return (data.frame(x = 0, y = 0) %>% ggvis(~x, ~y))
    if(is.null(input$plda_2d_x) || is.null(input$plda_2d_y)) return()
    tryCatch({
        projection <- as.data.frame(r_data$plda$proj) 
        group_color <- get_color_vector(r_data$group, pal = group_pal$val, maxCol = length(unique(r_data$group)))
        cols<-unique(group_color)
        names(cols) <- unique(r_data$group)
        
        projection %>% 
            tibble::rownames_to_column() %>%
            ggvis(x = prop("x", as.symbol(input$plda_2d_x)), y = prop("y", as.symbol(input$plda_2d_y)), key := ~rowname) %>%
            add_tooltip(d2_tooltip, "hover") %>%
            add_axis("x", title = input$plda_2d_x) %>% 
            add_axis("y", title = input$plda_2d_y) %>%
            set_options(width = 450, height = 300) %>%
            layer_points(fill = ~group, stroke:= "grey", size := 50, size.hover := 200, fillOpacity := 0.6, fillOpacity.hover := 0.9, stroke.hover := "orange") %>% 
            add_legend("fill", title = input$coloring_type) %>%
            scale_nominal("fill", range = unique(group_color)) %>% 
            bind_shiny("plda_2d_proj_plt")

    }, error = function(e){
        return()
    })
}) 




output$plda_3d_proj_plt <- threejs::renderScatterplotThree({
    if(is.null(r_data$plda)) return()
    xproj1 <- as.data.frame(r_data$plda$plda$xproj)
    if(ncol(xproj1) < 3) return()
    group_color <- get_color_vector(r_data$group, pal = group_pal$val, maxCol = length(unique(r_data$group)))
    
    threejs::scatterplot3js(xproj1[,1:3], color=group_color,size=2,labels=rownames(xproj1), renderer="canvas")
})



