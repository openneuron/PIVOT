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



output$deseq_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2){
        return(
            list(
                tags$li("This module requires design information input.")
            )
        )
    }
    
    list(
        enhanced_box(
            width = 12,
            title = "DESeq Differential Expression Analysis",
            id = "deseq_results",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            tags$div(tags$b("General Settings:"), class = "param_setting_title"),
            fluidRow(
                column(4, 
                       selectInput("deseq_design_set", "Experiment Design", 
                                   choices = list(
                                       "~ Condition" = "condition", 
                                       "~ Condition + Batch" = "condition_batch",
                                       "Time Course Experiment" = "timecourse"
                                   )
                       ),
                       uiOutput("deseq_design_formula")
                ),
                uiOutput("deseq_category_ui")
            ),

            uiOutput("perform_deseq_ui")
        ),
        uiOutput("deseq_results_box"),
        uiOutput("deseq_ma_box"),
        uiOutput("deseq_gene_box"),
        
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Michael I Love, Wolfgang Huber and Simon Anders (2014): Moderated estimation of fold change and dispersion for RNA-Seq data with DESeq2. Genome Biology.", class = "citation")
            )
        )
    )

})


output$deseq_design_formula <- renderUI({
    if(is.null(input$deseq_design_set)) return()
    if(input$deseq_design_set == "condition") {
        return(tags$p("Formula: design = ~ condition"))
    } else if(input$deseq_design_set == "condition_batch") {
        return(tags$p("Formula: design = ~ batch + condition"))
    } else if(input$deseq_design_set == "timecourse") {
        return(tags$p("Formula: design = ~ group + time + group:time (find genes that react in a group-specific manner over time)"))
    } else {
        return()
    }
})

output$deseq_category_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_design_set)) return()
    if(input$deseq_design_set == "condition") {
        list(
            column(4, 
                   uiOutput("deseq_condition_ui"),
                   uiOutput("deseq_condition_text")
            )
        )
    } else if(input$deseq_design_set == "condition_batch") {
        list(
            column(4, 
                   uiOutput("deseq_condition_ui"),
                   uiOutput("deseq_condition_text")
            ),
            column(4, 
                   uiOutput("deseq_batch_ui"),
                   uiOutput("deseq_batch_text")
            )
        )
    } else if(input$deseq_design_set == "timecourse") {
        list(
            column(4, 
                   uiOutput("deseq_condition_ui"),
                   uiOutput("deseq_condition_text")
            ),
            column(4, 
                   uiOutput("deseq_time_ui"),
                   uiOutput("deseq_time_text")
            )
        )
    } else {
        return()
    }
})

deseq_sanity <- reactiveValues()
deseq_sanity$condition <- 0
deseq_sanity$batch <- 0
deseq_sanity$time <- 0

output$deseq_condition_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_design_set)) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    list(
        selectInput("deseq_condition", label = "Condition:", choices = options)
    )
})

output$deseq_condition_text <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_condition)) return()
    groups = r_data$glb.meta[,input$deseq_condition]
    if(any(is.na(groups)) || any(groups == "")) {
        deseq_sanity$condition <- 0
        return(tags$p("'NA' or '' detected. Please specify a valid group name or remove these samples."))
    }
    if(length(unique(groups)) < 2) {
        deseq_sanity$condition <- 0
        return(tags$p("Only one group in this category."))
    }
    deseq_sanity$condition <- 1
    group_uniq <- as.character(unique(groups))
    return(tags$p(paste0(length(group_uniq), " groups (",  paste(group_uniq, collapse = " "), ") found in the current category.")))
})

# Function from DESeq2 (checkFullRank)
checkRank <- function (modelMatrix) 
{
    if (qr(modelMatrix)$rank < ncol(modelMatrix)) {
        if (any(apply(modelMatrix, 2, function(col) all(col == 
                                                        0)))) {
            msg <- ("the model matrix is not full rank, so the model cannot be fit as specified.\n  Levels or combinations of levels without any samples have resulted in\n  column(s) of zeros in the model matrix.\n\n  Please read the vignette section 'Model matrix not full rank':\n\n  vignette('DESeq2')")
        }
        else {
            msg <- ("the model matrix is not full rank, so the model cannot be fit as specified.\n  One or more variables or interaction terms in the design formula are linear\n  combinations of the others and must be removed.\n\n  Please read the vignette section 'Model matrix not full rank':\n\n  vignette('DESeq2')")
        }
        return(FALSE)
    } else {
        return(TRUE)
    }
}

output$deseq_batch_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_design_set)) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    list(
        selectInput("deseq_batch", label = "Batch:", choices = options)
    )
})

output$deseq_batch_text <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_condition) || is.null(input$deseq_batch)) return()
    batches = r_data$glb.meta[,input$deseq_batch]
    groups = r_data$glb.meta[, input$deseq_condition]
    if(deseq_sanity$condition == 0) {
        deseq_sanity$batch <- 0
        return(tags$p("Please correct condition input first."))
    }
    if(input$deseq_batch == input$deseq_condition) {
        deseq_sanity$batch <- 0
        return(tags$p("Batch cannot be the same as condition."))
    }
    if(any(is.na(batches)) || any(batches == "")) {
        deseq_sanity$batch <- 0
        return(tags$p("'NA' or '' detected. Please specify a valid group name or remove these samples."))
    }
    if(length(unique(batches)) < 2) {
        deseq_sanity$batch <- 0
        return(tags$p("Only one group in this category."))
    }
    # Detect perfect confounding
    df <- data.frame(group=groups, batch=batches)
    mm <- model.matrix(~batch+group, df)
    if(!checkRank(mm)){
        deseq_sanity$batch <- 0
        return(tags$p("The model matrix is not full rank. Please check design again."))
    }
    deseq_sanity$batch <- 1
    batch_uniq <- as.character(unique(batches))
    return(tags$p(paste0(length(batch_uniq), " batches (",  paste(batch_uniq, collapse = " "), ") found in the current category.")))
})

output$deseq_time_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_design_set)) return()
    categories = colnames(r_data$glb.meta)[-1]
    names(categories) <- categories
    options <- as.list(categories)
    list(
        selectInput("deseq_time", label = "Time:", choices = options)
    )
})

output$deseq_time_text <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_condition) || is.null(input$deseq_time)) return()
    timecourse = r_data$glb.meta[,input$deseq_time]
    groups = r_data$glb.meta[, input$deseq_condition]
    if(deseq_sanity$condition == 0) {
        deseq_sanity$time <- 0
        return(tags$p("Please correct condition input first."))
    }
    if(input$deseq_time == input$deseq_condition) {
        deseq_sanity$time <- 0
        return(tags$p("Time cannot be the same as condition."))
    }
    if(any(is.na(timecourse)) || any(timecourse == "")) {
        deseq_sanity$time <- 0
        return(tags$p("'NA' or '' detected. Please specify a valid group name or remove these samples."))
    }
    if(length(unique(timecourse)) < 2) {
        deseq_sanity$time <- 0
        return(tags$p("Only one group in this category."))
    }
    # Detect perfect confounding
    df <- data.frame(group=groups, time=timecourse)
    mm <- model.matrix(~ group + time + group:time, df)
    if(!checkRank(mm)){
        deseq_sanity$time <- 0
        return(tags$p("The model matrix is not full rank. Please check design again."))
    }
    deseq_sanity$time <- 1
    time_uniq <- as.character(unique(timecourse))
    return(tags$p(paste0(length(time_uniq), " time points (",  paste(time_uniq, collapse = " "), ") found in the current category.")))
})

output$perform_deseq_ui <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_condition)) return()
    # examine if required deseq_sanity check are passed
    if(deseq_sanity$condition == 0) return() # All design require condition checked
    method_ui <- selectInput("deseq_test_method", "Test Method", choices = list("Wald" = "Wald", "LRT" = "LRT"), selected = "Wald")
    if(input$deseq_design_set == "condition") {
    } else if(input$deseq_design_set == "condition_batch") {
        if(deseq_sanity$batch == 0) return()
    } else if(input$deseq_design_set == "timecourse") {
        if(deseq_sanity$time == 0) return()
        method_ui <- selectInput("deseq_test_method", "Test Method", choices = list("LRT" = "LRT"))
    } else{
        return()
    }
    list(
        fluidRow(
            column(4, method_ui),
            column(8, 
                   uiOutput("deseq_test_explain")
            )
        ),
        actionButton("perform_deseq", "Perform DE Analysis", class = "btn-info btn_leftAlign")
    )
})

output$deseq_test_explain <- renderUI({
    if(is.null(input$deseq_test_method)) return()
    if(input$deseq_test_method == "Wald") {
        list(
            tags$b("Wald test for the GLM coefficients: "),
            tags$li("This function tests for significance of coefficients in a Negative Binomial GLM."),
            tags$li("Note: This is the default test for DESeq.")
        )
    } else if(input$deseq_test_method == "LRT") {
        list(
            tags$b("Likelihood ratio test (chi-squared test) for GLMs: "),
            tags$li("This function tests for significance of change in deviance between a full and reduced model."),
            tags$li("Note: Useful for testing multiple terms at once, conceptually similar to ANOVA.")
            )
    } else {
        return()
    }
})

observeEvent(input$perform_deseq, {
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(input$deseq_test_method)) return()
    if(is.null(input$deseq_design_set)) return()
    
    withProgress(message = 'Processing...', value = 0.5, {
        error_I <- 0
        # Perform size factor re-estimation if necessary
        tryCatch({
            samplesAll <- data.frame(row.names=colnames(r_data$raw), celltype=rep("nt",length(colnames(r_data$raw))))
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = r_data$raw, colData=samplesAll, design = ~ 1) # Design here does not matter, overwrite later
            
            # If the data was normalized by DESeq modified, use the new size factor estimation
            if(r_data$norm_param$method == "Modified_DESeq") {
                DESeq2::sizeFactors(dds) <- r_data$norm_param$sizeFactor$size_factor
            } else {
                # If the data was not normalized by DESeq modified, re-estimate size factors using deseq2
                dds <- DESeq2::estimateSizeFactors(dds)
            }
        }, 
        error = function(e){
            error_I <<- 1
        }
        )
        
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "DESeq failed.")
            return()
        }

        groups = r_data$glb.meta[,input$deseq_condition]
        SummarizedExperiment::colData(dds)$group <- factor(groups, levels = unique(groups)) # update dds with group info
        group_cate = input$deseq_condition
        batch_cate = NULL
        time_cate = NULL
        
        if(input$deseq_design_set == "condition") {
            BiocGenerics::design(dds) <- formula(~ group)
        } else if(input$deseq_design_set == "condition_batch") {
            batches = r_data$glb.meta[,input$deseq_batch]
            batch_cate = input$deseq_batch
            SummarizedExperiment::colData(dds)$batch <- factor(batches, levels = unique(batches))
            BiocGenerics::design(dds) <- formula(~ batch + group)
        } else if(input$deseq_design_set == "timecourse") {
            timecourse = r_data$glb.meta[,input$deseq_time]
            time_cate = input$deseq_time
            assign("dds",dds,env=.GlobalEnv)
            assign("timecourse",timecourse,env=.GlobalEnv)
            SummarizedExperiment::colData(dds)$timecourse <- factor(timecourse, levels = unique(timecourse))
            BiocGenerics::design(dds) <- formula(~ group + timecourse + group:timecourse)
        } else {
            return()
        }
        
        if(input$deseq_test_method == "Wald") {
            r_data$dds <- DESeq2::DESeq(dds)
        } else if(input$deseq_test_method == "LRT") {
            if(input$deseq_design_set == "condition_batch") {
                r_data$dds <- DESeq2::DESeq(dds, test = "LRT", reduced = ~ batch)
            } else if(input$deseq_design_set == "timecourse") {
                r_data$dds <- DESeq2::DESeq(dds, test = "LRT", reduced = ~ group + timecourse)
            } else {
                r_data$dds <- DESeq2::DESeq(dds, test = "LRT", reduced = ~1)
            }
        }

        r_data$deseq_params <- list(design = input$deseq_design_set, test = input$deseq_test_method, condition = input$deseq_condition, batch = batch_cate, timecourse = time_cate)
    })
})

output$deseq_results_box <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(r_data$dds)) return()
    groups = unique(as.character(r_data$glb.meta[,r_data$deseq_params$condition]))
    names(groups) = groups
    
    deseq_group_ui <- list(
        if(r_data$deseq_params$test != "LRT") {
            fluidRow(
                column(4, tags$br(),tags$b("Pairwise comparison:")),
                column(3, selectInput("deseq_group1", "Group 1", choices = as.list(groups), selected = groups[[1]])),
                column(1, tags$b("vs")),
                column(3, selectInput("deseq_group2", "Group 2", choices = as.list(groups), selected = groups[[2]]))
            )
        } else {
            options<-resultsNames(r_data$dds)[-1]
            names(options) <- options
            list(
                fluidRow(
                    column(4, selectInput("deseq_pval_type", "P value type", choices = list("Wald" = "Wald", "LRT" = "LRT"))),
                    column(8, 
                           selectInput("deseq_result_name", "Choose comparison/individual points", 
                                       choices = as.list(options))
                    )
                )
            )
            
        }
        
    )
    
    if(!is.null(r_data$batch)) {
        deseq_batch_ui <- checkboxInput("deseq_batch_yes", "Control Batch Effects", value = F)
    } else {
        deseq_batch_ui <- NULL
    }
    
    enhanced_box(
        width = 12,
        title = NULL,
        status = "primary",
        solidHeader = T,
        tags$div(tags$b("Results Table:"), class = "param_setting_title"),
        deseq_group_ui,
        fluidRow(
            column(4, 
                   uiOutput("deseq_test_method_text")
            ),
            column(4, numericInput("deseq_alpha", "FDR cutoff", value = 0.1, min = 0, max = 0.5, step = 0.001)),
            column(4, checkboxInput("deseq_cuttbl", "Only show significant genes", value = F))
        ),
        DT::dataTableOutput("deseq_result_tbl"),
        uiOutput("download_deseq_result_ui"),
        hr(),
        uiOutput("deseq_sig_genes")
    )
})

output$deseq_sig_genes <- renderUI({
    if(is.null(r_data$deseq_results)) return()
    sm <- capture.output(DESeq2::summary.DESeqResults(r_data$deseq_results))
    list(
        tags$h4("Summary"),
        tags$li(paste0("Total number of significant genes: ", sum(r_data$deseq_results$padj < input$deseq_alpha, na.rm = T), ".")),
        tags$li(sm[4]),
        tags$li(sm[5]),
        tags$li(sm[6]),
        tags$li(paste(sm[7], sm[8]))
    )

})

output$deseq_test_method_text <- renderUI({
    if(is.null(r_data$deseq_results)) return()
    if(r_data$deseq_params$test == "Wald") {
        test_text1 <- r_data$deseq_results@elementMetadata$description[4]
        test_text2 <- "Note: The Wald p-value will be different for different pairwise comparisons / individual points."
    } else {
        test_text1 <- r_data$deseq_results@elementMetadata$description[4]
        test_text2 <- "Note: The LRT p-value does not depend on the group choice."
    }
    return(
        list(
            tags$li(test_text1),
            tags$li(test_text2)
        )
    )
})

output$download_deseq_result_ui <- renderUI({
    if(is.null(r_data$deseq_results)) return()
    tbl<-as.data.frame(r_data$deseq_results)
    if(nrow(tbl) == 0) return()
    download_deseq_result_ui <- downloadButton("download_deseq_result","Download", class = "btn btn-success")
})

observe({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) < 2 || is.null(r_data$dds)) return()
    if(r_data$deseq_params$test == "LRT") {
        if(is.null(input$deseq_pval_type) || is.null(input$deseq_result_name)) return()
    } else {
        if(is.null(input$deseq_group1)) return()
        if(input$deseq_group1 == input$deseq_group2) return()
    }
    withProgress(message = 'Processing...', value = 0.5, {
        if(r_data$deseq_params$test == "LRT") {
            assign("dds",r_data$dds, env = .GlobalEnv)
            assign("deseq_param", r_data$deseq_params, env = .GlobalEnv)
            res1 <- DESeq2::results(r_data$dds, test = input$deseq_pval_type,
                                    name = input$deseq_result_name, 
                                    alpha = input$deseq_alpha)
        } else {
            res1 <- DESeq2::results(r_data$dds, contrast = c("group", input$deseq_group1, input$deseq_group2), alpha = input$deseq_alpha)
        }
        
        r_data$deseq_group <- c(input$deseq_group1, input$deseq_group2)

        resOrdered <- res1[order(res1$padj),]
        if(input$deseq_cuttbl) {
            r_data$deseq_results <- subset(resOrdered, padj<input$deseq_alpha)
        } else {
            r_data$deseq_results <- resOrdered
        }
    })
})

output$deseq_result_tbl <- DT::renderDataTable({
    if(is.null(r_data$deseq_results)) return()
    tbl<-as.data.frame(r_data$deseq_results)
    if(nrow(tbl) == 0) return()
    if(r_data$deseq_params$test == "Wald") {
        DT::datatable(tbl, selection = 'single', options = list(scrollX = TRUE, scrollY = "250px", searching=T, order = list(list(6, 'asc')) , orderClasses = T))
    } else {
        DT::datatable(tbl, selection = 'single', options = list(scrollX = TRUE, scrollY = "250px", searching=T, order = list(list(6, 'asc')) , orderClasses = T))
    }
    
})

output$download_deseq_result <- downloadHandler(
    filename = function() { 
        "deseq_results.csv"
    },
    content = function(file) {
        write.csv(as.data.frame(r_data$deseq_results), file)
    }
)


output$deseq_ma_box <- renderUI({
    if(is.null(r_data$deseq_results)) return()
    enhanced_box(
        width = 12,
        title = "MA plot",
        id = "deseq_ma_plot",
        status = "warning",
        solidHeader = T,
        collapsible = T,
        reportable = T,
        get_html = T,
        register_analysis= T,
        plotOutput("deseq_ma_plt", height = "600px")
    )
})

output$deseq_ma_plt <- renderPlot({
    if(is.null(r_data$deseq_results)) return()
    BiocGenerics::plotMA(r_data$deseq_results, main="DESeq2", ylim=c(-2,2))
})

output$deseq_gene_box <- renderUI({
    if(is.null(r_data$deseq_results)) return()
    enhanced_box(
        width = 12,
        title = "Expression Plot of Selected Gene",
        id = "deseq_gene_plot",
        status = "danger",
        solidHeader = T,
        collapsible = T,
        reportable = T,
        get_html = T,
        register_analysis= T,
        wellPanel(
            fluidRow(
                column(4, checkboxInput("deseq_gene_plt_all", "Show counts in all groups", value = F)),
                column(4, selectInput("deseq_gene_plt_style", "Plot type", choices = list("Plot points" = "points", "Box plot" = "box", "Violin plot" = "violin"), selected = "points"))
            )
        ),
        plotOutput("deseq_gene_plt", height = "600px")
    )
})


output$deseq_gene_plt <- renderPlot({

    s = input$deseq_result_tbl_row_last_clicked
    tbl<-as.data.frame(r_data$deseq_results)
    
    if (length(s)) {    
        r_data$dds_gene <- rownames(tbl[s, , drop = FALSE]) 
    } else {
        return()
    }
    if(is.na(r_data$dds_gene) || r_data$dds_gene == "NA") {
        return()
    }

    if(is.null(input$deseq_gene_plt_type) || input$deseq_gene_plt_type == "group") {
        d <- DESeq2::plotCounts(r_data$dds, gene=r_data$dds_gene, intgroup="group", returnData=TRUE)
        if(!is.null(input$deseq_gene_plt_all) && input$deseq_gene_plt_all) {
        } else {
            d <- subset(d, group %in% c(input$deseq_group1, input$deseq_group2))
        }
        colnames(d) <- c("expression_level", "group")
        feature_super_plot(d, r_data$dds_gene, plot_group = "group", style = input$deseq_gene_plt_style, legend_pos = "top")

    } else if(input$deseq_gene_plt_type == "batch") {
        d <- DESeq2::plotCounts(r_data$dds, gene=r_data$dds_gene, intgroup="batch", returnData=TRUE)
        colnames(d) <- c("expression_level", "batch")
        feature_super_plot(d, r_data$dds_gene, plot_group = "batch", style = input$deseq_gene_plt_style, legend_pos = "top")
    }
    
})

