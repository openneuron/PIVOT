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

output$cor_ft_ui <- renderUI({
    if(is.null(r_data$df)) return()
    
    list(
        enhanced_box(
            width = 12,
            title = "Feature Correlation", 
            id = "feature_corr_heatmap",
            status = "danger",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            wellPanel(
                div(tags$p("Choose features to plot:"), class = "control-head"),
                selectInput("cor_ft_select_feature", label = "Select features based on", choices = list("Rank by Fano Factor" = "fano_factor", "Rank by Variance" = "variance","Rank by Average Expression" = "expression", "None (Row order)" = "row", "Custom gene list" = "custom"), selected = "fano_factor"),
                uiOutput("cor_ft_range_or_upload")
            ),
            wellPanel(
                div(tags$p("Heatmap parameters:"), class = "control-head"),
                fluidRow(
                    column(4, selectInput("cor_ft_dist", label = "Distance measure", choices = list("Correlation Distance (1-r)" = "corr1", "Absolute Correlation (1-|r|)" = "corr2"))),
                    column(4, selectInput("cor_ft_method", label = "Correlation method", choices = list("pearson" = "pearson","spearman" = "spearman", "kendall" = "kendall"), selected = "pearson")),
                    column(4, selectInput("cor_ft_agglo_method", "Agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
                )
            ),
            uiOutput("cor_ft_exceed_max_plot"),
            conditionalPanel("input.cor_ft_tool == 1", plotOutput("cor_ft_2", height = "800px")),
            conditionalPanel("input.cor_ft_tool == 2", d3heatmap::d3heatmapOutput("cor_ft_d3", height = "800px"))
        ),
        enhanced_box(
            width = 12,
            title = "Genes Correlated/Coexpressed with Target Gene", 
            id = "feature_corr_target",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            wellPanel(
                fluidRow(
                    column(3, textInput("ft_coe_target", "Target gene name:", placeholder = "E.g., Myc")),
                    column(3, selectInput("ft_coe_method", "Correlation method", choices = list("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"))),
                    column(3, selectInput("ft_coe_abs", "Correlation direction", choices = list("Positive (r)" = "positive", "Negative (-r)" = "negative", "Both (|r|)" = "both"))),
                    column(3, numericInput("ft_coe_threshold", "Threshold", min = 0.5, max = 1, step = 0.1, value = 0.5))
                )
            ),
            actionButton("ft_coe_search", "Search", class = "btn-info"),
            fluidRow(
                column(3, 
                       DT::dataTableOutput("coe_ft_tbl"),
                       downloadButton("coe_ft_tbl_download", class = "btn btn-success")
                ),
                column(9, uiOutput("coe_ft_hmap_ui"))
            )
        )
    )
    
})

############ Correlation Heatmap ############

output$cor_ft_range_or_upload <- renderUI({
    if(input$cor_ft_select_feature != "custom") {
        list(
            uiOutput("cor_ft_range_ui"),
            fluidRow(
                column(3,tags$b("Manually input a range:")),
                column(3, uiOutput("cor_ft_min_rank_ui")),
                column(3, uiOutput("cor_ft_max_rank_ui")),
                column(3, actionButton("cor_ft_update_range", "Update Range", class = "btn btn-info"))
            )
        )
    } else {
        fluidRow(
            column(6, uiOutput("cor_ft_custom_ui")),
            column(6, uiOutput("cor_ft_png_ui"))
        )
    }
})

# slider
output$cor_ft_range_ui <- renderUI({
    if(is.null(data0())) return()
    feature_num = nrow(data0())
    if (feature_num > 500) {
        max_bound = 500
    } else {
        max_bound = feature_num
    }
    
    if(input$cor_ft_select_feature != "custom") {
        sliderInput("cor_ft_range", label = "Rank Range", min = 1, max = max_bound, value = c(1, max_bound), step = 1)
    } else {
        return(NULL)
    }
})

# rank direct input
output$cor_ft_min_rank_ui <- renderUI({
    if(is.null(data0())) return()
    feature_num = nrow(data0())
    
    if (!is.null(input$cor_ft_range)) {
        cur_val = input$cor_ft_range[1]
    } else {
        cur_val = 1
    }
    
    numericInput_1("cor_ft_min_rank", label = "Min:", value = cur_val, min = 1, max = feature_num, step = 1)
})

output$cor_ft_max_rank_ui <- renderUI({
    if(is.null(data0())) return()
    feature_num = nrow(data0())
    
    if (!is.null(input$cor_ft_range)) {
        cur_val = input$cor_ft_range[2]
    } else {
        cur_val = feature_num
    }
    
    numericInput_1("cor_ft_max_rank", label = "Max:", value = cur_val, min = 2, max = feature_num, step = 1)
})


# Update range

observeEvent(input$cor_ft_update_range, {
    feature_num = nrow(data0())
    
    if(input$cor_ft_min_rank <= feature_num & input$cor_ft_max_rank <= feature_num) {
        if(input$cor_ft_max_rank - input$cor_ft_min_rank + 1 < 2) {
            session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
            return()
        }
        if(input$cor_ft_max_rank - input$cor_ft_min_rank + 1 > 1000) {
            session$sendCustomMessage(type = "showalert", "You specified more than 1000 features, please choose less.")
            return()
        }
        updateSliderInput(session, "cor_ft_range",
                          label = "Rank Range",
                          min = input$cor_ft_min_rank, max = input$cor_ft_max_rank, value = c(input$cor_ft_min_rank, input$cor_ft_max_rank), step = 1)
    } else {
        session$sendCustomMessage(type = "showalert", "You don't have that many features in the data set.")
        return()
    }
})

## Custom list ##
cor_ft_gene_list <- reactiveValues()
cor_ft_gene_list$tbl <- NULL

output$cor_ft_custom_ui <- renderUI({
    content <- list(
        fluidRow(
            column(6, 
                   wellPanel(
                       fileInput('cor_ft_list_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       checkboxInput('cor_ft_header', 'Header', value = F),
                       radioButtons('cor_ft_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                       radioButtons('cor_ft_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                       actionButton("cor_ft_list_submit", "Submit List", class = "btn btn-info")
                   ),
                   uiOutput("cor_ft_gene_clust_upload_text_inmodal")
            ),
            column(6,
                   tags$p("[feature name in 1st column]"),
                   DT::dataTableOutput('cor_ft_list_tbl_show')
            )
        )
    )
    list(
        actionButton("cor_ft_custom_btn", label = "Upload a custom gene list for feature correlation", class = "btn-warning"),
        shinyBS::bsModal(id = "cor_ft_custom_modal", "Upload a custom gene list", "cor_ft_custom_btn", size = "large", content) 
    )
})

observe({
    inFile <- input$cor_ft_list_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            cor_ft_gene_list$tbl <- read.csv(inFile$datapath, header=input$cor_ft_header, sep=input$cor_ft_sep, quote=input$cor_ft_quote)
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

output$cor_ft_list_tbl_show <- DT::renderDataTable({
    if(is.null(cor_ft_gene_list$tbl)) return()
    DT::datatable(cor_ft_gene_list$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})

output$cor_ft_png_ui <- renderUI({
    if(is.null(r_data$cor_ft_gene)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$cor_ft_gene), "genes have been successfully uploaded.")), style = "font-size:110%;")
    )
})

observeEvent(input$cor_ft_list_submit, {
    
    # First process the marker feature file and get the list
    if (is.null(cor_ft_gene_list$tbl) || nrow(cor_ft_gene_list$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return(NULL)
    }
    marker_names <- make.names(as.character(unique(cor_ft_gene_list$tbl[,1])))
    
    cur_flist <- rownames(r_data$raw)
    
    
    flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
    flist <- flist[!is.na(flist)]
    if(length(flist) != length(marker_names)) {
        message_gl <- paste0(length(marker_names) - length(flist)," features in your gene list (", length(marker_names),") are not found in the current dataset.")
        session$sendCustomMessage(type = "showalert", message_gl)
    }
    r_data$cor_ft_gene <- flist
})



cor_ft_data <- reactive({
    if(is.null(data0()) || is.null(input$cor_ft_range) || is.null(input$cor_ft_select_feature)) return()
    if(input$cor_ft_range[2] > nrow(data0())) {
        return(NULL)
    }
    if(input$cor_ft_select_feature == "fano_factor") {
        tmp<-data_by_fano() %>% dplyr::select(-fano, -avg_exp, -var, -feature)
        tmp[input$cor_ft_range[1]:input$cor_ft_range[2],]
    } else if(input$cor_ft_select_feature == "variance"){
        tmp<-data_by_var() %>% dplyr::select(-var, -feature)
        tmp[input$cor_ft_range[1]:input$cor_ft_range[2],]
    }
    else if(input$cor_ft_select_feature == "expression"){
        tmp <- data_by_exp() %>% dplyr::select(-avg_exp, -feature)
        tmp[input$cor_ft_range[1]:input$cor_ft_range[2],]
    } else if(input$cor_ft_select_feature == "row"){
        data0()[input$cor_ft_range[1]:input$cor_ft_range[2],]
    } else if(input$cor_ft_select_feature == "custom"){
        if(is.null(r_data$cor_ft_gene)) return()
        data0()[r_data$cor_ft_gene,]
    }
})


output$cor_ft_exceed_max_plot <- renderUI({
    if(is.null(cor_ft_data())) return()
    if(nrow(cor_ft_data()) > 1000) {
        tags$p("Exceeding maximum plotting capability (1000).")
    } else {
        return()
    }
})

output$cor_ft_2 <- renderPlot({
    if(is.null(cor_ft_data()) || nrow(cor_ft_data()) > 1000) return ()
    feature_cor <- cor(t(cor_ft_data()), method = input$cor_ft_method)

    hclustfun1 = function(x, method=input$cor_ft_agglo_method, ...) hclust(x, method=method, ...)
    
    if(input$cor_ft_dist == "corr1") {
        distfun1 = function(c) as.dist(1 - c)
    } else if(input$cor_ft_dist == "corr2") {
        distfun1 = function(c) as.dist(1 - abs(c))
    }

    gplots::heatmap.2(feature_cor, scale="none", Rowv = T, symm=TRUE, dendrogram="both", 
              distfun=distfun1, 
              hclustfun = hclustfun1, 
              trace="none", col=hmcol$val, key.par=list(cex.axis=1), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=1, density.info="density", revC=T, margins=c(8,6))
})

output$cor_ft_d3 <- d3heatmap::renderD3heatmap({
    if(is.null(cor_ft_data()) || nrow(cor_ft_data()) > 1000) return ()
    feature_cor <- cor(t(cor_ft_data()), method = input$cor_ft_method)
    
    hclustfun1 = function(x, method=input$cor_ft_agglo_method, ...) hclust(x, method=method, ...)
    
    if(input$cor_ft_dist == "corr1") {
        distfun1 = function(c) as.dist(1 - c)
    } else if(input$cor_ft_dist == "corr2") {
        distfun1 = function(c) as.dist(1 - abs(c))
    }
    
    d3heatmap::d3heatmap(feature_cor, scale="none", Rowv = T,
              distfun=distfun1, 
              hclustfun = hclustfun1, 
              dendrogram="both", colors=hmcol$val, revC=T)
})



######## Search for correlated genes ########

observeEvent(input$ft_coe_search, {
    target_gene <- input$ft_coe_target
    
    if(is.null(data0())) return()
    
    if(target_gene == "" || is.null(target_gene)) {
        session$sendCustomMessage(type = "showalert", "Please input your target gene.")
        return()
    }
    
    mat <- as.matrix(data0())
    idx <- which(rownames(mat) == target_gene)
    
    if(!length(idx)) {
        session$sendCustomMessage(type = "showalert", "Gene not found in the current dataset.")
        return()
    } else {
        gene <- mat[idx,]
    }
    
    withProgress(message = 'Processing...', value = 0.8, {
    
    resp <- c()
    est <- c()
    
    if(input$ft_coe_abs == "positive") {
        cust_f <- function(x){x}
    } else if(input$ft_coe_abs == "negative") {
        cust_f <- function(x){-x}
    } else {
        cust_f <- abs
    }
    
    mat <- as.matrix(data0())
    
    tbl <- do.call(rbind, apply(mat, 1, function(row) {
        x <- cor.test(gene, row, method = input$ft_coe_method)
        if (cust_f(x$estimate[[1]]) >= input$ft_coe_threshold){
            c(x$estimate[[1]], x$p.value)
        }
    }))
    
    if(input$ft_coe_method == "pearson") {
        colnames(tbl) <- c("cor", "p.value")
    } else if(input$ft_coe_method == "kendall") {
        colnames(tbl) <- c("tau", "p.value")
    } else if(input$ft_coe_method == "spearman"){
        colnames(tbl) <- c("rho", "p.value")
    }
    
    r_data$coe_ft_target <- target_gene
    r_data$coe_ft_tbl <- tbl
    })
})


output$coe_ft_tbl <- DT::renderDataTable({
    if(is.null(r_data$coe_ft_tbl)) return()
    DT::datatable(r_data$coe_ft_tbl, selection = 'single', caption = paste("Genes highly correlated with gene", r_data$coe_ft_target),
                  options = list(scrollX = TRUE, scrollY = "450px", lengthMenu = c(20, 50, 100)))
})

output$coe_ft_tbl_download <-downloadHandler(
    filename = paste0("genes_correlated_with_", r_data$coe_ft_target, ".csv"),
    content = function(file) {
        if(is.null(r_data$coe_ft_tbl)) return()
        write.csv(r_data$coe_ft_tbl, file)
    })


output$coe_ft_hmap_ui <- renderUI({
    if(is.null(r_data$coe_ft_tbl)) return()
    features <- rownames(r_data$coe_ft_tbl)
    if(length(features) < 2) {
        return(tags$p("Too few features for heatmap plotting."))
    } else if(length(features) > 1000) {
        return(tags$p("Exceeding maximum plotting capability (1000)."))
    } else {
        plotOutput("coe_ft_hmap", height = "800px")
    }
})

output$coe_ft_hmap <- renderPlot({
    if(is.null(r_data$coe_ft_tbl)) return()
    features <- rownames(r_data$coe_ft_tbl)
    data1 <- data0()[which(rownames(data0()) %in% features), ]
    
    gplots::heatmap.2(as.matrix(data1), scale="none", trace="none", col=hmcol$val, key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=.7, density.info="density", revC=T, cexCol = 1.2, margins=c(8,6))
})



