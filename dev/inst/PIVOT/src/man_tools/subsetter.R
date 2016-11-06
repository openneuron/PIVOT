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


output$subset_ui <- renderUI({
    
    if(is.null(r_data$glb.raw)) return(tags$p("Please upload data first."))

    list(
        tags$div(tags$b("Sample Subsetter Type:"), class = "param_setting_title"),
        #a(id = "subset_type_help_btn", icon("question-circle")),
        #shinyBS::bsModal(id = "subset_type_help", title = "Subsetter help", trigger = "subset_type_help_btn", size = "large", list(
        #    tags$li("You can select or delete samples from the input set to create subset for analysis."),
        #    tags$li("An implicit filtering will happen to get features with >0 total expression."),
        #    tags$li("Previous filtering will be undone.")
        #)),
        
        fluidRow(
            column(6, 
                   selectInput("subsetter_type", label = "Subsetter type", choices = list("Manually pick samples" = "manual", "Upload a sample list" = "list", "Subset based on sample stats" = "stats"), selected = "manual")
            ),
            column(6, 
                   selectInput("is_neg_subsetter", label = "Select/Delete sample", choices = list("Positive subsetter (select)" = FALSE, "Negative subsetter (delete)" = TRUE), selected = FALSE)
            )
        ),
        
        uiOutput("subsetter_renormalize_ui"),
        
        hr(),
        
        conditionalPanel(
            condition = "input.subsetter_type == 'manual'", 
            fluidRow(
                column(6,  
                       tags$div(tags$b("Subset by Sample:"), class = "param_setting_title"),
                       textInput("sample_search", label = "Name Filter", value = ""), 
                       uiOutput("manual_select_ui"),
                       uiOutput("manual_subset_btn_ui")
                ),
                column(6, 
                       uiOutput("category_subset_ui")
                )
            )
        ),
        
        conditionalPanel(
            condition = "input.subsetter_type == 'list'", 
            tags$div(tags$b("Upload Sample List:"), class = "param_setting_title"),
            fluidRow(
                column(6, 
                       wellPanel(
                           fluidRow(
                               column(8, fileInput('sb_sample_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                               column(4, 
                                      a(id = "sample_upload_help_btn", icon("question-circle")),
                                      shinyBS::bsModal(id = "sample_upload_help", title = "File format", trigger = "sample_upload_help_btn", size = "small", list(
                                          tags$p("The samples in the first column of the uploaded file will be selected. Example:"),
                                          img(src = "sample_tbl_exp.png", width = 150)
                                      ))
                               )
                           ),
                           checkboxInput('sb_header', 'Header', value = TRUE),
                           radioButtons('sb_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                           radioButtons('sb_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"')
                       ),
                       uiOutput("sb_submit_ui")
                ),
                column(6,
                       tags$p("[sample name in 1st column]"),
                       DT::dataTableOutput('sb_sample_tbl')
                )
            )
        ),
        conditionalPanel(
            condition = "input.subsetter_type == 'stats'", 
            tags$div(tags$b("Choose Range on Graph:"), class = "param_setting_title"),
            fluidRow(
                column(6,
                       uiOutput("sample_stats_plt_type_ui")
                ),
                column(6, 
                       uiOutput("sample_stats_group_ui")
                )
            ),
            uiOutput("sample_stats_instr_ui"),
            uiOutput("sample_stats_ui")
        ),
        fluidRow(
            column(12,        shinyBS::tipify(actionButton('undo_subset_sample', label = "Undo Subset", icon = icon("times"), class = "btn-danger btn_rightAlign"), 
                                              title = "Data will return to the original input dataset. All filtration/subsetting will be undone.", 
                                              placement = "bottom", options = list(container = "body")))
        )
    )
})


output$is_neg_subsetter_ui <- renderUI({
    if(is.null(input$subsetter_type)) return()
    if(input$subsetter_type == "stats") {
        return(tags$p("You can only select samples by specifying ranges in the graph below."))
    } else {
        
    }
})

output$subsetter_renormalize_ui <- renderUI({
    tip <- paste("The data will be renormalized with the specified normalization method:", input$proc_method)
    shinyBS::tipify(checkboxInput("subsetter_renormalize", tags$b("Subset with renormalization"), value = FALSE), title = tip, placement = "bottom", options = list(container = "body"))
})


### Visualize stats ###
# A copy of sample_stats_tbl to put at sample subsetter
output$input_sample_stats_tbl <- DT::renderDataTable({
    if(is.null(r_data$sample_meta)) return()
    DT::datatable(r_data$sample_meta, selection = 'single', options = list(
        scrollX = T, scrollY = "500px", lengthMenu = c(20, 50, 100)
    )
    )
})

output$download_sample_stats_tbl <- downloadHandler(
    filename = "sample_stats.csv",
    content = function(file) {
        write.csv(r_data$sample_meta, file)
    }
)

output$sample_stats_plt_type_ui <- renderUI({
    options <- list("Number of genes expressed" = "num_genes_expressed", "Total raw counts" = "total_raw_reads")
    if(r_data$norm_param$method %in% c("DESeq", "Modified_DESeq")) {
        options$"Total normalized counts" <- "total_normalized_counts"
        options$"DESeq size factor" <- "deseq_size_factor"
        options$"Cook's Distance" <- "cooks"
    } else if (r_data$norm_param$method != "None"){
        options$"Total normalized counts" <- "total_normalized_counts"
    }
    selectInput("sample_stats_plt_type", "Subset based on", 
                choices = options
    )
})

output$sample_stats_group_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    options$None = "None"
    selectInput("sample_stats_group", label = "Color by", choices = options)
})

output$sample_stats_instr_ui <- renderUI({
    if(is.null(input$sample_stats_plt_type)) return()
    if(input$sample_stats_plt_type == "cooks") {
        tags$p("See DESeq2 vignettes for details. This module only provide visualization. You need to manually remove the sample with abnormal cook's distance.")
    } else {
        tags$p("Click and drag in the y axis direction to specify range (inclusive, only y range will be used for filtering samples).")
    }
})

output$sample_stats_ui <- renderUI({
    if(is.null(input$sample_stats_plt_type)) return()
    
    if(input$sample_stats_plt_type == "cooks") {
        list(
            actionButton("cooks_btn", "Compute cook's distance", class = "btn-info"),
            plotOutput("cooks_distance_plt")
        )
    } else {
        list(
            plotly::plotlyOutput("sample_stats_plot"),
            tags$br(),
            uiOutput("current_sample_ui"),
            uiOutput("plt_subset_btn_ui")
        )
    }
})

output$plt_subset_btn_ui <- renderUI({
    if(is.null(input$subsetter_renormalize) || is.null(input$is_neg_subsetter)) return()
    negf <- as.logical(input$is_neg_subsetter)
    rnorm <- as.logical(input$subsetter_renormalize)
    if(negf) {
        session$sendCustomMessage(type = "showalert", "Negative subsetter (Delete) mode is not allowed with plot range selection.")
        return()
    }
    if(!rnorm) {
        actionButton('plt_subset_btn', label = "Select", class = "btn-info btn_rightAlign")
    } else if(rnorm) {
        actionButton('plt_subset_btn', label = "Select & Renorm", class = "btn-info btn_rightAlign")
    } 
})

output$sample_stats_plot <- renderPlotly({
    if(is.null(r_data$sample_meta) || is.null(input$sample_stats_plt_type) || input$sample_stats_plt_type == "cooks") return()
    r_data$glb.meta
    input$sample_stats_group
    isolate({
        withProgress(message = 'Processing...', value = 0.5, {
            tbl <- r_data$sample_meta %>% tibble::rownames_to_column("sample") 
            colnames(tbl)[which(colnames(tbl) == input$sample_stats_plt_type)] <- "y"
            if(!is.null(input$sample_stats_group) && input$sample_stats_group != "None") {
                tbl$Group <- r_data$glb.meta[,input$sample_stats_group][match(tbl$sample,r_data$glb.meta[,1])]
                plt1 <- tbl %>% plot_ly(x = sample, y = y, type = "bar", color = as.character(tbl$Group), source = "sample_range_select")
            } else {
                plt1 <- tbl %>% plot_ly(x = sample, y = y, type = "bar", source = "sample_range_select")
            }
            
            plt1 %>% plotly::layout(
                xaxis = list(title = "sample"),
                yaxis = list(title = input$sample_stats_plt_type))
        })
        
    })
})

output$cooks_distance_plt <- renderPlot({
    if(is.null(r_data$cooks)) return()
    boxplot(r_data$cooks, range=0, las=2)
    title("Boxplot of Cook's Distance")
})

observeEvent(input$cooks_btn,  {
    withProgress(message = 'Processing...', value = 0.8, {
        error_I <- 0
        tryCatch({
            samplesAll <- data.frame(row.names=colnames(r_data$raw), celltype=rep("nt",length(colnames(r_data$raw))))
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = r_data$raw, colData=samplesAll, design = ~ 1)
            dds <- DESeq2::estimateSizeFactors(dds)
            dds <- DESeq2::DESeq(dds)
            r_data$cooks<-log10(assays(dds)[["cooks"]])
        }, 
        error = function(e){
            error_I <<- 1
        }
        )
        
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Failed to compute Cook's distance.")
            return()
        }
    })
})



cur_sp_range <- reactiveValues()
cur_sp_range$lower <- NULL
cur_sp_range$upper <- NULL
cur_sp_range$inlist <- NULL
cur_sp_range$outlist <- NULL


observe({
    evt1 <- plotly::event_data("plotly_relayout", source = "sample_range_select")
    if(is.null(evt1$`yaxis.range[0]`)) return()
    cur_sp_range$lower <- round(evt1$`yaxis.range[0]`, digits = 1)
    cur_sp_range$upper <- round(evt1$`yaxis.range[1]`, digits = 1)
})

output$current_sample_ui <- renderUI({
    if(is.null(r_data$sample_meta) || is.null(input$sample_stats_plt_type)) return()
    
    if(is.null(cur_sp_range$lower) || is.null(cur_sp_range$upper)) return()
    if(input$sample_stats_plt_type == "cooks") return()
    
    tbl <- r_data$sample_meta %>% tibble::rownames_to_column("sample") 
    cnts <- tbl[,which(colnames(tbl) == input$sample_stats_plt_type)]
    
    cur_sp_range$inlist <- tbl$sample[which(cnts >= cur_sp_range$lower & cnts <= cur_sp_range$upper)]
    cur_sp_range$outlist <- tbl$sample[which(!tbl$sample %in% cur_sp_range$inlist)]
    
    list(
        tags$li(paste0("Selected range: ", cur_sp_range$lower, " ≤ ", input$sample_stats_plt_type, " ≤ ", cur_sp_range$upper)),
        tags$p(),
        tags$li(paste0("Samples within range: ", paste(cur_sp_range$inlist, collapse = ", "))),
        tags$p(),
        tags$li(paste0("Samples to be removed: ", paste(cur_sp_range$outlist, collapse = ", "))),
        tags$br()
    )
})

observeEvent(input$plt_subset_btn, {
    if(is.null(cur_sp_range$inlist)){
        session$sendCustomMessage(type = "showalert", "Please specify a range in the graph.")
        return()
    } else if (length(cur_sp_range$inlist) < 2){
        session$sendCustomMessage(type = "showalert", "Too few samples left!")
        return()
    }
    
    withProgress(message = 'Subsetting', value = 0, {
        incProgress(0.3, detail = "Getting new sample list...")
        clear_results()
        rnorm <- as.logical(input$subsetter_renormalize)
        
        slist <- cur_sp_range$inlist # Update sample_name
        
        new_raw <- r_data$glb.raw[, slist] # first subset raw counts
        new_raw <- new_raw[rowSums(new_raw) > 0, ] # Reselecting features with larger than 0 total counts in the new set.
        flist <- rownames(new_raw) # the feature list thus may change (implicit filtration)
        
        if(!rnorm) {
            # Reset data for analysis, using r_data$feature_list as key, current dataset will be updated
            r_data$feature_list <- flist
            r_data$sample_name <- slist
            r_data$raw <- new_raw
            r_data$df <- r_data$glb.df[flist,slist]
        } else {
            incProgress(0.3, detail = "Perform data normalization...")
            result<-normalize_data(method = input$proc_method, 
                                   params = list(gene_len = r_data$gene_len, deseq_threshold = input$deseq_threshold), 
                                   raw = new_raw)
            if(is.null(result)) {
                session$sendCustomMessage(type = "showalert", "Normalization failed!")
                return()
            } else {
                r_data$feature_list <- flist
                r_data$sample_name <- slist
                r_data$raw <- new_raw
                r_data$df <- result$df
                r_data$norm_param <- result$norm_param
            }
        }

        if(!is.null(r_data$glb.group)) {
            r_data$group <- factor(r_data$glb.group[r_data$sample_name])
            if(length(unique(r_data$group)) == 1) r_data$group <- NULL
        }
        if(!is.null(r_data$glb.batch)) {
            r_data$batch <- r_data$glb.batch[r_data$sample_name]
            if(length(unique(r_data$batch)) == 1) r_data$batch <- NULL
        }
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        
        actionText <- paste("Subset samples within range: ", cur_sp_range$lower, " ≤ ", input$sample_stats_plt_type, " ≤ ", cur_sp_range$upper)
        
        if(!rnorm) {
            norm_method <- paste(r_data$his_tbl$norm_method[[1]], "(inherit)")
        } else {
            norm_method <- paste(r_data$norm_param$method, "(re-norm)")
        }
        
        r_data <- update_history(r_data, parent = r_data$history[[1]]$name, "Subset", actionText, list(feature = r_data$feature_list, sample = r_data$sample_name, df = r_data$df), norm_method, r_data$norm_param)
        
        setProgress(1)
    })
})

############################### Data subsetter module ###############################
# This module facilitates the sample subsetting from the global data set. It will update two things in "data" object:
# r_data$raw and r_data$sample_name. r_data$sample_name will be used as the key to extract normalized data from global set when "Analyze!" button is pressed.
# r_data$raw is updated here because it gives updates to the data preview module to allow the user have a direct visualization of the subsetting result.
# r_data$raw won't be used for analysis.

# manual subsetter UI module 


output$manual_select_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return()
    sample_lis <- grep(input$sample_search, colnames(r_data$glb.raw), value = "TRUE")

    if(!is.null(r_data$glb.meta) && !is.null(input$category_subset) && !is.null(input$group_subset_input)) {
        samples <- r_data$glb.meta[,1]
        sample_lis <- samples[which(r_data$glb.meta[, input$category_subset] %in% input$group_subset_input)]
    }
    
    if(length(sample_lis) <= 20) {
        inlen <- length(sample_lis)
    } else {
        inlen <- 20
    }
    
    selectInput('sample_subset_input', 
                label = NULL, 
                choices = sample_lis, 
                selected = sample_lis,
                selectize = F,
                size = inlen,
                multiple = TRUE)
})

output$category_subset_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    list(
        tags$div(tags$b("Subset by Category:"), class = "param_setting_title"),
        selectInput("category_subset", label = "Choose category", choices = options),
        uiOutput("group_subset_ui")
    )
})

output$group_subset_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$category_subset)) return() # Redundant conditions to prevent error when change meta file
    group_lis <- as.character(unique(r_data$glb.meta[, input$category_subset]))
    
    if(length(group_lis) <= 20) {
        inlen <- length(group_lis)
    } else {
        inlen <- 20
    }
    list(
        selectInput('group_subset_input', 
                    label = "Select Group", 
                    choices = group_lis, 
                    selected = group_lis,
                    selectize = F,
                    size = inlen,
                    multiple = TRUE),
        tags$p("The samples in the chosen groups will be selected in the left panel.")
    )
})

output$manual_subset_btn_ui <- renderUI ({
    if(is.null(input$subsetter_renormalize) || is.null(input$is_neg_subsetter)) return()
    negf <- as.logical(input$is_neg_subsetter)
    rnorm <- as.logical(input$subsetter_renormalize)
    if(!negf && !rnorm) {
        actionButton('manual_subset_btn', label = "Select", class = "btn-info btn_rightAlign")
    } else if(negf && !rnorm) {
        actionButton('manual_subset_btn', label = "Delete", class = "btn-info btn_rightAlign")
    } else if(!negf && rnorm) {
        actionButton('manual_subset_btn', label = "Select & Renorm", class = "btn-info btn_rightAlign")
    } else if(negf && rnorm) {
        actionButton('manual_subset_btn', label = "Delete & Renorm", class = "btn-info btn_rightAlign")
    }
})

observeEvent(input$manual_subset_btn, {
    negf <- as.logical(input$is_neg_subsetter)
    rnorm <- as.logical(input$subsetter_renormalize)
    
    if(is.null(input$sample_subset_input)){
        session$sendCustomMessage(type = "showalert", "No sample selected!")
        return()
    } else if ((length(input$sample_subset_input) < 2 && !negf) || ((length(colnames(r_data$glb.raw)) - length(input$sample_subset_input)) < 2 && negf)){
        session$sendCustomMessage(type = "showalert", "Too few samples left!")
        return()
    }
    withProgress(message = 'Subsetting', value = 0, {
        incProgress(0.3, detail = "Getting new sample list...")
        clear_results()
        slist <- input$sample_subset_input # Update sample_name
        
        neglist <- colnames(r_data$glb.raw)[!colnames(r_data$glb.raw)%in%slist]
        
        if(negf) {
            slist <- neglist
        } 
        
        new_raw <- r_data$glb.raw[, slist] # first subset raw counts
        new_raw <- new_raw[rowSums(new_raw) > 0, ] # Reselecting features with larger than 0 total counts in the new set.
        flist <- rownames(new_raw) # the feature list thus may change (implicit filtration)
        
        if(!rnorm) {
            # Reset data for analysis, using r_data$feature_list as key, current dataset will be updated
            r_data$feature_list <- flist
            r_data$sample_name <- slist
            r_data$raw <- new_raw
            r_data$df <- r_data$glb.df[flist,slist]
        } else {
            incProgress(0.3, detail = "Perform data normalization...")
            result<-normalize_data(method = input$proc_method, 
                                   params = list(gene_len = r_data$gene_len, deseq_threshold = input$deseq_threshold), 
                                   raw = new_raw)
            if(is.null(result)) {
                session$sendCustomMessage(type = "showalert", "Normalization failed!")
                return()
            } else {
                r_data$feature_list <- flist
                r_data$sample_name <- slist
                r_data$raw <- new_raw
                r_data$df <- result$df
                r_data$norm_param <- result$norm_param
            }
        }
        
        if(!is.null(r_data$glb.group)) {
            r_data$group <- factor(r_data$glb.group[r_data$sample_name])
            if(length(unique(r_data$group)) == 1) r_data$group <- NULL
        }
        if(!is.null(r_data$glb.batch)) {
            r_data$batch <- r_data$glb.batch[r_data$sample_name]
            if(length(unique(r_data$batch)) == 1) r_data$batch <- NULL
        }
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        
        actionText <- paste("Subset manually picked samples")
        
        if(!rnorm) {
            norm_method <- paste(r_data$his_tbl$norm_method[[1]], "(inherit)")
        } else {
            norm_method <- paste(r_data$norm_param$method, "(re-norm)")
        }
        
        r_data <- update_history(r_data, r_data$history[[1]]$name, "Subset", actionText, list(feature = r_data$feature_list, sample = r_data$sample_name, df = r_data$df), norm_method, r_data$norm_param)
        
        setProgress(1)
    })
})


observeEvent(input$undo_subset_sample, {
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Retrieving input dataset...")
        r_data$raw <- r_data$glb.raw
        r_data$sample_name <- colnames(r_data$raw) # Get original sample list
        r_data$feature_list <- rownames(r_data$raw) # get original feature list
        r_data$norm_param <- r_data$history[[1]]$norm_param
        
        clear_results()
        if(!is.null(r_data$glb.group)) {
            r_data$group <- factor(r_data$glb.group)
            group_lis <- as.character(unique(r_data$glb.group))
            updateSelectInput(session, 'group_subset_input', 
                        label = "Select Group", 
                        choices = group_lis, 
                        selected = group_lis)
        }
        if(!is.null(r_data$glb.batch)) {
            r_data$batch <- r_data$glb.batch[r_data$sample_name]
            if(length(unique(r_data$batch)) == 1) r_data$batch <- NULL
        }
        
        # Current set now same as global set
        r_data$df <- r_data$glb.df
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        
        actionText <- "Return to input dataset"

        r_data$his_tbl$is_activated <- c("Y", rep("N", nrow(r_data$his_tbl) - 1))
        
        setProgress(1)
    })
})


output$sb_submit_ui <- renderUI({
    if(!is.null(input$sb_sample_file) && !is.null(input$is_neg_subsetter)) {
        negf <- as.logical(input$is_neg_subsetter)
        rnorm <- as.logical(input$subsetter_renormalize)
        if(!negf && !rnorm) {
            actionButton('file_subset_btn', label = "Select", class = "btn btn-info")
        } else if(negf && !rnorm) {
            actionButton('file_subset_btn', label = "Delete", class = "btn btn-info")
        } else if(!negf && rnorm) {
            actionButton('file_subset_btn', label = "Select & Renorm", class = "btn btn-info")
        } else if(negf && rnorm) {
            actionButton('file_subset_btn', label = "Delete & Renorm", class = "btn btn-info")
        }
    }
})

# process the upload sample list
output$sb_sample_tbl <- DT::renderDataTable({
    inFile <- input$sb_sample_file
    if (is.null(inFile))
        return(NULL)
    error_I <- 0
    tryCatch({tmp_tbl <- read.csv(inFile$datapath, header=input$sb_header, sep=input$sb_sep, quote=input$sb_quote)},
             error = function(e){
                 error_I <<- 1
             })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Unsupported file format.")
        return()
    }
    DT::datatable(tmp_tbl, options = list(scrollX = TRUE, scrollY = "300px", searching = FALSE))
})

# get marker_feature list when the user click submit button
observeEvent(input$file_subset_btn, {
    # First process the marker feature file and get the list
    negf <- as.logical(input$is_neg_subsetter)
    rnorm <- as.logical(input$subsetter_renormalize)
    
    inFile <- input$sb_sample_file
    error_I <- 0
    if (is.null(inFile))
    {
        session$sendCustomMessage(type = "showalert", "Please upload a sample sheet.")
        return(NULL)
    }
    tryCatch({sample_tbl <- read.csv(inFile$datapath, header=input$sb_header, sep=input$sb_sep, quote=input$sb_quote)},
             error = function(e){
                 error_I <<- 1
             })
    if(error_I) {
        session$sendCustomMessage(type = "showalert", "Unsupported file format.")
        return()
    }
    
    sample_names <- as.character(sample_tbl[,1]) 
    slist <- sample_names[which(sample_names %in% colnames(r_data$glb.raw))]
    
    if(length(slist) != length(sample_names)) {
        message_ss <- paste0(length(sample_names) - length(slist)," samples in your sample list (", length(sample_names),") are not found in the global dataset.")
        session$sendCustomMessage(type = "showalert", message_ss)
    }
    
    neglist <- colnames(r_data$glb.raw)[!colnames(r_data$glb.raw) %in% slist]

    if(negf) {
        if(length(neglist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few samples left!")
            return(NULL)
        }
        slist <- neglist
    } else {
        if(length(slist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few samples left!")
            return(NULL)
        }
    }
    
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Getting new sample list...")
        clear_results()
        
        new_raw <- r_data$glb.raw[, slist] # first subset raw counts
        new_raw <- new_raw[rowSums(new_raw) > 0, ] # Reselecting features with larger than 0 total counts in the new set.
        flist <- rownames(new_raw) # the feature list thus may change (implicit filtration)
        
        if(!rnorm) {
            # Reset data for analysis, using r_data$feature_list as key, current dataset will be updated
            r_data$feature_list <- flist
            r_data$sample_name <- slist
            r_data$raw <- new_raw
            r_data$df <- r_data$glb.df[flist,slist]
        } else {
            incProgress(0.3, detail = "Perform data normalization...")
            result<-normalize_data(method = input$proc_method, 
                                   params = list(gene_len = r_data$gene_len, deseq_threshold = input$deseq_threshold), 
                                   raw = new_raw)
            if(is.null(result)) {
                session$sendCustomMessage(type = "showalert", "Normalization failed!")
                return()
            } else {
                r_data$feature_list <- flist
                r_data$sample_name <- slist
                r_data$raw <- new_raw
                r_data$df <- result$df
                r_data$norm_param <- result$norm_param
            }
        }
        
        if(!is.null(r_data$glb.group)) {
            r_data$group <- factor(r_data$glb.group[r_data$sample_name])
            if(length(unique(r_data$group)) == 1) r_data$group <- NULL
        }
        if(!is.null(r_data$glb.batch)) {
            r_data$batch <- r_data$glb.batch[r_data$sample_name]
            if(length(unique(r_data$batch)) == 1) r_data$batch <- NULL
        }
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        
        actionText <- paste("Subset based on uploaded sample sheet")
        
        if(!rnorm) {
            norm_method <- paste(r_data$his_tbl$norm_method[[1]], "(inherit)")
        } else {
            norm_method <- paste(r_data$norm_param$method, "(re-norm)")
        }
        
        r_data <- update_history(r_data, r_data$history[[1]]$name, "Subset", actionText, list(feature = r_data$feature_list, sample = r_data$sample_name, df = r_data$df), norm_method, r_data$norm_param)
        
        setProgress(1)
    })
})

