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


# Cell state ordering by monocle

output$monocle_state_ui <- renderUI({
    if(is.null(r_data$monocle_ok)) {
        return(tags$li("Please initiate monocle celldataset first."))
    }

    list(
        enhanced_box(
            width = 12,
            title = "Set Genes for Ordering/Clustering",
            id = "monocle_gene_var",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            fluidRow(
                column(4,  
                       selectInput("ordering_genes_type", "Genes to be used for cell ordering/clustering", 
                                   choices = list("Use all genes in the current dataset for ordering" = "all", 
                                                  "Use variably expressed genes for ordering" = "var",
                                                  "Use Monocle DE (qval < 0.1)" = "de"))
                ),
                column(8, 
                       uiOutput("mn_order_gene_msg"),
                       uiOutput("mn_order_gene_params")
                )
            ),
            fluidRow(
                column(4,
                       DT::dataTableOutput("mn_ordering_genes"),
                       downloadButton("mn_ordering_genes_download","Download", class = "btn btn-success")
                ),
                column(8, 
                       plotOutput("monocle_var_plot")     
                )
            )
        ),
        
        enhanced_box(
            width = 12,
            title = "Cell State Ordering/Unsupervised Clustering",
            id = "monocle_clust_result",
            status = "success",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            
            wellPanel(
                selectInput("mn_run_type", "Choose purpose of analysis", choices = list("Cell state ordering" = "state", "Unsupervised clustering" = "clust")),
                uiOutput("mn_run_params_ui")
            ),
            uiOutput("mn_run_btn_ui"),
            hr(),
            fluidRow(
                column(6,
                       tags$b("Ordering/Clustering Result", class = "table-cap"),
                       DT::dataTableOutput("monocle_state_tbl"),
                       downloadButton("download_monocle_state_tbl", "Download", class = "btn btn-success")
                ),
                column(6,
                       tags$b("State/Cluster - Group Comparison", class = "table-cap"),
                       uiOutput("monocle_state_group_compare_ui"),
                       DT::dataTableOutput("monocle_state_group_matrix"),
                       plotOutput("monocle_state_group_plt")
                )
            )
        ),
        
        enhanced_box(
            width = 12,
            title = "Cell Trajectory Plot",
            id = "monocle_trajectory",
            status = "warning",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            fluidRow(
                column(4,
                       DT::dataTableOutput("monocle_state_gene_tbl")
                ),
                column(8, 
                       wellPanel(
                           fluidRow(
                               column(6, 
                                      uiOutput("monocle_state_plt_color_ui")
                               ),
                               column(6,
                                      selectInput("monocle_state_show","Plot content", choices = list("Only show points" = "none", "Show tree" = "tree"), selected = "tree")
                               )
                           )
                       ),
                       plotOutput("monocle_state_plt", height = "450px")
                )
            )  
        ),
        
        enhanced_box(
            width = 12,
            title = "Gene Expression Plot",
            id = "monocle_gene_plot",
            status = "danger",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            fluidRow(
                column(3,
                       DT::dataTableOutput("monocle_genelist_tbl")
                ),
                column(9, 
                      
                       wellPanel(
                           fluidRow(
                               column(6, selectInput("monocle_gene_plt_type", "Plot type", choices = list("Gene expression in pseudotime" = "time", "Expression jitter plot" = "jitter"))),
                               column(6, uiOutput("monocle_time_plt_color_ui"))
                           )
                       ),
                       uiOutput("monocle_time_plt_ui")
                )
            )
            
        ),
        
        enhanced_box(
            width = 12,
            title = "Cluster Genes by Psudotemporal Expression Pattern",
            id = "monocle_gene_clust",
            status = "info",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            wellPanel(
                fluidRow(
                    column(6, selectInput("mn_gene_for_clust", "Choose genes for clustering", choices = list("Custom gene list" = "custom", "Monocle DE genes" = "de"))),
                    column(6, numericInput("mn_gene_clust_num", "Number of clusters", min = 1, max = 10, step =1, value = 4))
                ),
                uiOutput("mn_gene_clust_upload_ui"),
                tags$p(),
                uiOutput("mn_gene_clust_num_check")
            ),
            tags$p(),
            actionButton("mn_gene_clust", "Cluster genes", class = "btn-info"),
            hr(),
            fluidRow(
                column(4,
                       DT::dataTableOutput("monocle_geneclust_tbl"),
                       downloadButton("download_monocle_geneclust_tbl", "Download", class = "btn btn-success")
                ),
                column(8, 
                       plotOutput("monocle_clust_plt", height = "600px")
                )
            )
            
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Cole Trapnell and Davide Cacchiarelli et al (2014): The dynamics and regulators of cell fate decisions are revealed by pseudo-temporal
                        ordering of single cells. Nature Biotechnology", class = "citation"),
                tags$li("Monocle Website:", a("http://cole-trapnell-lab.github.io/monocle-release/", src = "http://cole-trapnell-lab.github.io/monocle-release/")),
                tags$li("Monocle was written by Cole Trapnell with input from Davide Cacchiarelli and is provided under the OSI-approved Artistic License (version 2.0).")
                )
        )
    )
})


output$mn_data_dist_text_ui <- renderUI({
    if(input$mn_family_fun == "tobit") {
        fluidRow(
            column(4,         
                   tags$b("Data type"),
                   tags$p("FPKM, TPM. ")
            ),
            column(8, 
                   tags$b("Notes"),
                   tags$p("Tobits are truncated normal distributions. Using tobit() will tell Monocle to log-transform your data where appropriate.")
            )
        )
    } else if(input$mn_family_fun == "negbinomial") {
        fluidRow(
            column(4,         
                   tags$b("Data type"),
                   tags$p("UMIs, Transcript counts from experiments with spike-ins or relative2abs, raw read counts. ")
            ),
            column(8, 
                   tags$b("Notes"),
                   tags$p("Using negbinomial() can be slow for very large datasets. In these cases, consider negbinomial.size().")
            )
        )
    } else if(input$mn_family_fun == "negbinomial.size") {
        fluidRow(
            column(4,         
                   tags$b("Data type"),
                   tags$p("UMIs, Transcript counts from experiments with spike-ins, raw read counts. ")
            ),
            column(8, 
                   tags$b("Notes"),
                   tags$p("Slightly less accurate for differential expression than negbinomial(), but much, much faster.")
            )
        )
    } else if(input$mn_family_fun == "gaussianff") {
        fluidRow(
            column(4,         
                   tags$b("Data type"),
                   tags$p("log-transformed FPKM/TPMs, Ct values from single-cell qPCR.")
            ),
            column(8, 
                   tags$b("Notes"),
                   tags$p("If you want to use Monocle on data you have already transformed to be normally distributed, you can use this function, though some Monocle features may not work well.")
            )
        )
    } 
})




output$mn_run_params_ui <- renderUI({
    if(input$mn_run_type == "state") {
        fluidRow(
            column(4, selectInput("mn_rd_method", "Dimensionality reduction method", choices = list("DDRTree" = "DDRTree", "ICA" = "ICA"))),
            column(4, uiOutput("mn_num_path_ui")),
            column(4, radioButtons("monocle_reverse","Reverse the biological process?", choices = list("No Reverse" = F, "Reverse" = T), inline= T, selected = F))
        )
    } else {
        fluidRow(
            column(4, numericInput("mn_cell_clust_num", "Number of clusters", min = 1, max = 20, step =1, value = 2))
        )
    }
    
})

output$mn_order_gene_msg <- renderUI({
    num_order <- sum(fData(r_data$cellset)$use_for_ordering)
    msg <- list()
    if(num_order > 1000) {
        return(tags$p("Warning: Ordering can be very slow with > 1000 genes."))
    }
})

output$mn_order_gene_params <- renderUI({
    if(input$ordering_genes_type == "all") {
        return()
    } else if(input$ordering_genes_type == "var") {
        return(fluidRow(
            column(6, numericInput("mn_mean_limit", "Set lower mean limit", value = 1, step = 1, min = 0)),
            column(6, numericInput("mn_var_limit", "Set lower dispersion limit", value = 1, step = 1, min = 0, max = 10))
        ))
    } else {
        if(is.null(r_data$monocle_results)) return()
        ordering_genes <- row.names(subset(r_data$monocle_results, qval <= 0.1))
        if(length(ordering_genes) > 1000) {
            return(sliderInput("state_order_top_gene", "Number of top genes (ranked by qval) to be used for ordering:", min = 2, max = 1000, value = 1000, step = 1, round = T))
        }
    }
})

output$mn_num_path_ui <- renderUI({
    if(input$mn_rd_method == "ICA") {
        numericInput_1("monocle_num_paths", "Number of end-point cell states allowed", value = 2)
    } else {
        return()
    }
})


output$mn_run_btn_ui <- renderUI({
    if(input$mn_run_type == "state") {
        actionButton("mn_generate_state", "Perform ordering", class = "btn-info")
    } else {
        actionButton("mn_generate_clust", "Perform clustering", class = "btn-info")
    }

})

observe({
    input$ordering_genes_type
    input$state_order_top_gene
    input$mn_mean_limit
    input$mn_var_limit
    
    isolate({
        if(is.null(r_data$cellset)) return()
        if(is.null(input$ordering_genes_type) || input$ordering_genes_type == "all") {
            ordering_genes <- row.names(r_data$df)
            r_data$monocle_ordering <- "all"
        } else if (input$ordering_genes_type == "var") {
            if(is.null(input$mn_var_limit)) return()
            if(is.null(r_data$cellset@dispFitInfo[["blind"]])) {
                session$sendCustomMessage(type = "showalert", "Dispersion estimation is not available for the expression family you selected for your data.")
                return()
            }
            disp_table <- dispersionTable(r_data$cellset)
            ordering_genes <- subset(disp_table, mean_expression >= input$mn_mean_limit & dispersion_empirical >= input$mn_var_limit * dispersion_fit)$gene_id
            r_data$monocle_ordering <- "var"
        } else if (input$ordering_genes_type == "de") { # Use monocle genes
            if(is.null(r_data$monocle_results)) {
                session$sendCustomMessage(type = "showalert", "Please perform Monocle DE analysis first.")
                updateSelectInput(session, "ordering_genes_type", "Choose which genes to be used for cell ordering", 
                                  choices = list("Use all genes in the current dataset for ordering" = "all", 
                                                 "Use variably expressed genes for ordering" = "var",
                                                 "Use Monocle DE genes (qval < 0.1)" = "de"))
                updateTabItems(session, "tabs", "monocle_de")
                return()
            } else {
                ordering_genes <- row.names(subset(r_data$monocle_results, qval <= 0.1))
                if(!is.null(input$state_order_top_gene)) {
                    ordering_genes <- ordering_genes[1:input$state_order_top_gene]
                }
                r_data$monocle_ordering <- "de"
            }
            if(length(ordering_genes) < 2) {
                fData(r_data$cellset)$use_for_ordering <- FALSE
                session$sendCustomMessage(type = "showalert", "Too few genes for ordering.")
                return()
            }
        } 
        r_data$cellset <- setOrderingFilter(r_data$cellset, ordering_genes)
    })
})

output$monocle_var_plot <- renderPlot({
    if(is.null(r_data$cellset)) return()
    if(is.null(r_data$cellset@dispFitInfo[["blind"]])) return()
    plot_ordering_genes(r_data$cellset)
})


output$mn_ordering_genes <- DT::renderDataTable({
    if(is.null(r_data$cellset)) return()
    tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
    DT::datatable(tbl, rownames = FALSE, options = list(scrollX = F, scrollY = "270px",lengthMenu = c(20, 50, 100)))
})

output$mn_ordering_genes_download <- downloadHandler(
    filename = "monocle_gene_for_ordering.csv",
    content = function(file) {
        tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
        write.csv(tbl, file)
    }
)


# Perform ordering
observeEvent(input$mn_generate_state, {
    if(is.null(r_data$cellset)) return()
    
    error_I <- 0
    withProgress(message = 'Processing...', value = 0.8, {
        tryCatch({
            r_data$cellset <- reduceDimension(r_data$cellset, reduction_method = input$mn_rd_method)
            if(input$mn_rd_method == "ICA") {
                r_data$cellset <- orderCells(r_data$cellset, num_paths = input$monocle_num_paths, reverse = input$monocle_reverse)
            } else if(input$mn_rd_method == "DDRTree") {
                r_data$cellset <- orderCells(r_data$cellset, reverse = input$monocle_reverse)
            }
        }, 
        error = function(e){
            error_I <<- 1
        })
    })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Ordering failed.")
        return()
    }
})

observeEvent(input$mn_generate_clust, {
    if(is.null(r_data$cellset)) return()
    
    error_I <- 0
    withProgress(message = 'Processing...', value = 0.8, {
        tryCatch({
            r_data$cellset <- clusterCells(r_data$cellset, num_clusters=input$mn_cell_clust_num)
        }, 
        error = function(e){
            error_I <<- 1
        })
    })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Clustering failed.")
        return()
    }
})



output$monocle_state_tbl <- DT::renderDataTable({
    if(is.null(r_data$cellset) || is.null(pData(r_data$cellset)$State)){
        return()
    }

    tbl <- pData(r_data$cellset)
    tbl <- tbl[, which(colnames(tbl)%in% c("Pseudotime", "State", "Cluster", "Group", "Batch"))]
    
    DT::datatable(tbl, 
                  extensions = c('Responsive'), options = list(
                      scrollY = "500px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_monocle_state_tbl <- downloadHandler(
    filename = "monocle_state_tbl.csv",
    content = function(file) {
        if(!is.null(r_data$group) && is.null(r_data$batch)) {
            tbl <- subset(pData(r_data$cellset), select = c(Pseudotime, State, Group))
        } else if(!is.null(r_data$group) && !is.null(r_data$batch)) {
            tbl <- subset(pData(r_data$cellset), select = c(Pseudotime, State, Group, Batch))
        } else if(is.null(r_data$group) && !is.null(r_data$batch)) {
            tbl <- subset(pData(r_data$cellset), select = c(Pseudotime, State, Batch))
        } else {
            
        }
        tbl <- pData(r_data$cellset)
        tbl <- tbl[, which(colnames(tbl)%in% c("Pseudotime", "State", "Group", "Batch"))]
        write.csv(tbl, file)
    }
)


output$monocle_state_group_compare_ui <- renderUI({
    if(is.null(r_data$cellset) || is.null(pData(r_data$cellset)$Group)) return()
    options <- list()
    if(!is.null(pData(r_data$cellset)$State)) {
        options$State <- "State"
    } 
    if(!is.null(pData(r_data$cellset)$Cluster)) {
        options$Cluster <- "Cluster"
    }
    selectInput("monocle_state_group_compare", "Compare group against", choices = options)
})

output$monocle_state_group_matrix <- DT::renderDataTable({
    if(is.null(r_data$cellset) || is.null(pData(r_data$cellset)$Group) || is.null(input$monocle_state_group_compare)) return()
    if(input$monocle_state_group_compare == "State") {
        if(is.null(pData(r_data$cellset)$State)) return()
        DT::datatable(as.data.frame.matrix(table(pData(r_data$cellset)$Group, pData(r_data$cellset)$State)))
    } else if (input$monocle_state_group_compare == "Cluster") {
        if(is.null(pData(r_data$cellset)$Cluster)) return()
        DT::datatable(as.data.frame.matrix(table(pData(r_data$cellset)$Group, pData(r_data$cellset)$Cluster)))
    }
})

output$monocle_state_group_plt <- renderPlot({
    if(is.null(r_data$cellset) || is.null(pData(r_data$cellset)$Group) || is.null(input$monocle_state_group_compare)) return()
    if(input$monocle_state_group_compare == "State") {
        if(is.null(pData(r_data$cellset)$State)) return()
        plot(as.factor(pData(r_data$cellset)$Group), pData(r_data$cellset)$State, xlab="Group", ylab = "State")
    } else if (input$monocle_state_group_compare == "Cluster") {
        if(is.null(pData(r_data$cellset)$Cluster)) return()
        plot(as.factor(pData(r_data$cellset)$Group), pData(r_data$cellset)$Cluster, xlab="Group", ylab = "Cluster")
    }
})

output$monocle_state_gene_tbl <- DT::renderDataTable({ # The same as monocle_genelist_tbl except only allow single gene choice, used for mst plot
    if(is.null(r_data$cellset) || is.null(r_data$monocle_ordering)) return()
    if(r_data$monocle_ordering == "de") {
        if(is.null(r_data$monocle_results)) return()
        tbl <- subset(r_data$monocle_results, qval <= 0.1) %>% tibble::rownames_to_column("gene")%>% dplyr::select(gene, pval, qval)
    } else {
        tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
    }
    
    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, selection = 'single', rownames = FALSE, options = list(
        scrollX = F, scrollY = "400px", lengthMenu = c(20, 50, 100)
    )
    )
})


output$monocle_state_plt_color_ui <- renderUI({
    if(is.null(r_data$cellset)) return()
    options <- list()
    if(!is.null(pData(r_data$cellset)$Group)) {
        options$Group <- "Group"
    }
    if(!is.null(pData(r_data$cellset)$State)) {
        options$State <- "State"
        options$Pseudotime <- "Pseudotime"
    } 
    if(!is.null(pData(r_data$cellset)$Cluster)) {
        options$Cluster <- "Cluster"
    }
    selectInput("monocle_state_plt_color", "Color cells by", choices = options)
})

output$monocle_state_plt <- renderPlot({
    if(is.null(r_data$cellset) || is.null(pData(r_data$cellset)$State) || is.null(input$monocle_state_plt_color)){
        return()
    }
    tree1 = ifelse(input$monocle_state_show %in% c("tree", "both"), T, F)
    mst1 = ifelse(input$monocle_state_show %in% c("mst", "both"), T, F)
    
    s = input$monocle_state_gene_tbl_row_last_clicked
    if(r_data$monocle_ordering == "de") {
        if(is.null(r_data$monocle_results)) return()
        tbl <- subset(r_data$monocle_results, qval <= 0.1) %>% tibble::rownames_to_column("gene_for_ordering")%>% dplyr::select(gene_for_ordering, pval, qval)
    } else {
        tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
    }
    s <- as.character(tbl$gene_for_ordering[s])
    print(s)
    plot_cell_trajectory(r_data$cellset,  color_by = input$monocle_state_plt_color, show_tree = tree1, show_backbone = mst1, markers = s)
})

output$monocle_genelist_tbl <- DT::renderDataTable({
    if(is.null(r_data$cellset) || is.null(r_data$monocle_ordering)) return()
    if(r_data$monocle_ordering == "de") {
        if(is.null(r_data$monocle_results)) return()
        tbl <- subset(r_data$monocle_results, qval <= 0.1) %>% tibble::rownames_to_column("gene")%>% dplyr::select(gene, pval, qval)
    } else {
        tbl<- data.frame(gene_for_ordering = fData(r_data$cellset)$gene_short_name[which(fData(r_data$cellset)$use_for_ordering == TRUE)])
    }
    
    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, rownames = FALSE, options = list(
        scrollX = F, scrollY = "400px", lengthMenu = c(20, 50, 100)
    )
    )
})


observe({
    s = input$monocle_genelist_tbl_rows_selected
    if(length(s)) {
        r_data$monocle_genelist <- s
    }
})

output$monocle_time_plt_color_ui <- renderUI({
    if(is.null(r_data$cellset)) return()
    options <- list()
    if(!is.null(pData(r_data$cellset)$Group)) {
        options$Group <- "Group"
    }
    if(!is.null(pData(r_data$cellset)$State)) {
        options$State <- "State"
        options$Pseudotime = "Pseudotime"
    } 
    if(!is.null(pData(r_data$cellset)$Cluster)) {
        options$Cluster <- "Cluster"
    }
    selectInput("monocle_time_plt_color", "Color cells by", choices = options)
})

output$monocle_time_plt <- renderPlot({
    if(is.null(r_data$cellset) || is.null(pData(r_data$cellset)$State) || is.null(r_data$monocle_genelist) || is.null(input$monocle_time_plt_color)){
        return()
    }
    cds_subset <- r_data$cellset[r_data$monocle_genelist, ]
    if(input$monocle_gene_plt_type == "time") {
        plot_genes_in_pseudotime(cds_subset, color_by = input$monocle_time_plt_color, cell_size = 3, ncol = 2)
    } else if(input$monocle_gene_plt_type == "jitter") {
        plot_genes_jitter(cds_subset, plot_trend = T, grouping = input$monocle_time_plt_color, color_by = input$monocle_time_plt_color, cell_size = 3, ncol = 2)
    }
})

output$monocle_time_plt_ui <- renderUI({
    if(is.null(r_data$cellset) || is.null(r_data$monocle_genelist)) return()
    n<-length(r_data$monocle_genelist)
    #h1 <- paste0(n * (250 - n*20), "px")
    plotOutput("monocle_time_plt")
})


# Cluster genes

output$mn_gene_clust_upload_ui <- renderUI({
    if(input$mn_gene_for_clust != "custom") return()
    content <- list(
        fluidRow(
            column(6, 
                   wellPanel(
                       fileInput('mn_list_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       checkboxInput('mn_header', 'Header', value = F),
                       radioButtons('mn_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                       radioButtons('mn_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                       actionButton("mn_list_submit", "Submit List", class = "btn btn-info")
                   ),
                   uiOutput("mn_gene_clust_upload_text_inmodal")
            ),
            column(6,
                   tags$p("[feature name in 1st column]"),
                   DT::dataTableOutput('mn_list_tbl_show')
            )
        )
    )
    
    fluidRow(
        column(4, 
               actionButton("mn_custom_btn", label = "Upload a custom gene list for clustering", class = "btn-warning"),
               shinyBS::bsModal(id = "mn_custom_modal", "Upload a custom gene list", "mn_custom_btn", size = "large", content)      
        ),
        column(8, 
               uiOutput("mn_gene_clust_upload_text")
        )
    )
})

output$mn_gene_clust_upload_text <- renderUI({
    if(is.null(r_data$monocle_gene_submitted)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$monocle_gene_submitted), "genes have been successfully uploaded.")), style = "font-size:110%;")
    )
})

output$mn_gene_clust_num_check <- renderUI({
    if(is.null(r_data$monocle_gene_for_clust)) return()
    if(length(r_data$monocle_gene_for_clust) > 1000) {
        return(tags$p("Warning: Clustering can be very slow with > 1000 genes."))
    }
})

output$mn_gene_clust_upload_text_inmodal <- renderUI({
    if(is.null(r_data$monocle_gene_submitted)) return()
    list(
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b(paste(length(r_data$monocle_gene_submitted), "genes have been successfully uploaded.")), style = "font-size:110%;")
    )
})


mn_clust_gene <- reactiveValues()
mn_clust_gene$tbl <- NULL

# process the upload feature list
observe({
    inFile <- input$mn_list_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            mn_clust_gene$tbl <- read.csv(inFile$datapath, header=input$mn_header, sep=input$mn_sep, quote=input$mn_quote)
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

output$mn_list_tbl_show <- DT::renderDataTable({
    if(is.null(mn_clust_gene$tbl)) return()
    DT::datatable(mn_clust_gene$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})

observeEvent(input$mn_list_submit, {
    
    # First process the marker feature file and get the list
    if (is.null(mn_clust_gene$tbl) || nrow(mn_clust_gene$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return(NULL)
    }
    marker_names <- make.names(as.character(unique(mn_clust_gene$tbl[,1])))
    
    cur_flist <- rownames(r_data$raw)
    
    
    flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
    flist <- flist[!is.na(flist)]
    if(length(flist) != length(marker_names)) {
        message_gl <- paste0(length(marker_names) - length(flist)," features in your gene list (", length(marker_names),") are not found in the current dataset.")
        session$sendCustomMessage(type = "showalert", message_gl)
    }
    r_data$monocle_gene_submitted <- flist
})



output$monocle_geneclust_tbl <- DT::renderDataTable({
    if(is.null(r_data$monocle_clusters)) return()
    tbl <- r_data$monocle_clusters$tbl
    DT::datatable(tbl, 
                  options = list(
                      scrollX = F, scrollY = "400px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_monocle_geneclust_tbl <- downloadHandler(
    filename = "monocle_geneclust_tbl.csv",
    content = function(file) {
        tbl <-r_data$monocle_clusters$tbl
        write.csv(tbl, file)
    }
)

observe({
    if(is.null(input$mn_gene_for_clust)) return()
    if(input$mn_gene_for_clust == "custom") {
        if(is.null(r_data$monocle_gene_submitted)) {
            return()
        } else {
            r_data$monocle_gene_for_clust <- r_data$monocle_gene_submitted
        }
    } else if(input$mn_gene_for_clust == "de") {
        if(is.null(r_data$monocle_results)) {
            session$sendCustomMessage(type = "showalert", "Please perform Monocle DE analysis first.")
            updateSelectInput(session, "mn_gene_for_clust", "Choose genes for clustering", choices = list("Custom gene list" = "custom", "Monocle DE genes" = "de"))
            updateTabItems(session, "tabs", "monocle_de")
            return()
        } else {
            r_data$monocle_gene_for_clust <- row.names(subset(r_data$monocle_results, qval <= 0.1))
        }
    }
})


observeEvent(input$mn_gene_clust,{
    if(is.null(r_data$cellset) || is.null(r_data$monocle_gene_for_clust)) return()
    if(is.null(pData(r_data$cellset)$State)) {
        session$sendCustomMessage(type = "showalert", "Please perform Monocle state ordering first.")
        return()
    }
    
    flist <- r_data$monocle_gene_for_clust
    error_I <- 0
    withProgress(message = 'Processing...', value = 0.8, {
        tryCatch({
            cds_subset <- r_data$cellset[flist,]
            r_data$monocle_clusters <- plot_pseudotime_heatmap2(cds_subset, num_clusters = input$mn_gene_clust_num, cores = 1, show_rownames = T, return_list = T)
        },
        error = function(e){
            error_I <<- 1
        })
    })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Gene clustering failed.")
        return()
    }
})


output$monocle_clust_plt <- renderPlot({
    if(is.null(r_data$monocle_clusters)) return()
    ph_res <- r_data$monocle_clusters$hmap
    grid::grid.rect(gp = grid::gpar("fill", col = NA))
    grid::grid.draw(ph_res$gtable)
})

# function override
table.ramp <- function (n, mid = 0.5, sill = 0.5, base = 1, height = 1) 
{
    x <- seq(0, 1, length.out = n)
    y <- rep(0, length(x))
    sill.min <- max(c(1, round((n - 1) * (mid - sill/2)) + 1))
    sill.max <- min(c(n, round((n - 1) * (mid + sill/2)) + 1))
    y[sill.min:sill.max] <- 1
    base.min <- round((n - 1) * (mid - base/2)) + 1
    base.max <- round((n - 1) * (mid + base/2)) + 1
    xi <- base.min:sill.min
    yi <- seq(0, 1, length.out = length(xi))
    i <- which(xi > 0 & xi <= n)
    y[xi[i]] <- yi[i]
    xi <- sill.max:base.max
    yi <- seq(1, 0, length.out = length(xi))
    i <- which(xi > 0 & xi <= n)
    y[xi[i]] <- yi[i]
    height * y
}

rgb.tables <- function (n, red = c(0.75, 0.25, 1), green = c(0.5, 0.25, 1), 
          blue = c(0.25, 0.25, 1)) 
{
    rr <- do.call("table.ramp", as.list(c(n, red)))
    gr <- do.call("table.ramp", as.list(c(n, green)))
    br <- do.call("table.ramp", as.list(c(n, blue)))
    rgb(rr, gr, br)
}

plot_pseudotime_heatmap2 <- function (cds_subset, cluster_rows = TRUE, hclust_method = "ward.D2", 
          num_clusters = 6, hmcols = NULL, add_annotation_row = NULL, 
          add_annotation_col = NULL, show_rownames = FALSE, use_gene_short_name = TRUE, 
          norm_method = c("vstExprs", "log"), scale_max = 3, scale_min = -3, 
          trend_formula = "~sm.ns(Pseudotime, df=3)", return_list = FALSE, 
          cores = 1) 
{
    newdata <- data.frame(Pseudotime = seq(0, max(pData(cds_subset)$Pseudotime), 
                                           length.out = 100))
    m <- monocle::genSmoothCurves(cds_subset, cores = cores, trend_formula = trend_formula, 
                         relative_expr = T, new_data = newdata)
    m = m[!apply(m, 1, sum) == 0, ]
    norm_method <- match.arg(norm_method)
    if (norm_method == "vstExprs" && is.null(cds_subset@dispFitInfo[["blind"]]$disp_func) == 
        FALSE) {
        m = monocle::vstExprs(cds_subset, expr_matrix = m)
    }
    else if (norm_method == "log") {
        m = log10(m + pseudocount)
    }
    m = m[!apply(m, 1, sd) == 0, ]
    m = Matrix::t(scale(Matrix::t(m), center = TRUE))
    m = m[is.na(row.names(m)) == FALSE, ]
    m[is.nan(m)] = 0
    m[m > scale_max] = scale_max
    m[m < scale_min] = scale_min
    heatmap_matrix <- m
    row_dist <- as.dist((1 - cor(Matrix::t(heatmap_matrix)))/2)
    row_dist[is.na(row_dist)] <- 1
    bks <- seq(-3.1, 3.1, by = 0.1)
    if (is.null(hmcols)) {
        hmcols <- rgb.tables(length(bks) - 1, red = c(0.8, 0.2, 1), green = c(0.5, 0.4, 0.8), 
                   blue = c(0.2, 0.2, 1))
    }
    ph <- pheatmap::pheatmap(heatmap_matrix, useRaster = T, cluster_cols = FALSE, 
                   cluster_rows = cluster_rows, show_rownames = F, show_colnames = F, 
                   clustering_distance_rows = row_dist, clustering_method = hclust_method, 
                   cutree_rows = num_clusters, silent = TRUE, filename = NA, 
                   breaks = bks, color = hmcols)
    annotation_row <- data.frame(Cluster = factor(cutree(ph$tree_row, 
                                                         num_clusters)))
    if (use_gene_short_name == TRUE) {
        if (is.null(fData(cds_subset)$gene_short_name) == FALSE) {
            feature_label <- as.character(fData(cds_subset)[row.names(heatmap_matrix), 
                                                            "gene_short_name"])
            feature_label[is.na(feature_label)] <- row.names(heatmap_matrix)
            row_ann_labels <- as.character(fData(cds_subset)[row.names(annotation_row), 
                                                             "gene_short_name"])
            row_ann_labels[is.na(row_ann_labels)] <- row.names(annotation_row)
        }
        else {
            feature_label <- row.names(heatmap_matrix)
            row_ann_labels <- row.names(annotation_row)
        }
    }
    else {
        feature_label <- row.names(heatmap_matrix)
        row_ann_labels <- row.names(annotation_row)
    }
    row.names(heatmap_matrix) <- feature_label
    row.names(annotation_row) <- row_ann_labels
    colnames(heatmap_matrix) <- c(1:ncol(heatmap_matrix))
    ph_res <- pheatmap::pheatmap(heatmap_matrix[, ], useRaster = T, cluster_cols = FALSE, 
                       cluster_rows = cluster_rows, show_rownames = show_rownames, 
                       show_colnames = F, clustering_distance_rows = row_dist, 
                       clustering_method = hclust_method, cutree_rows = num_clusters, 
                       annotation_row = annotation_row, treeheight_row = 20, 
                       breaks = bks, fontsize = 6, color = hmcols, silent = TRUE, 
                       filename = NA)
    grid::grid.rect(gp = grid::gpar("fill", col = NA))
    grid::grid.draw(ph_res$gtable)
    if (return_list) {
        return(list(hmap = ph_res, tbl = annotation_row))
    }
}

