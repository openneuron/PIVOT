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
    if(is.null(r_data$group) || length(unique(r_data$group)) < 2) {
        return(
            list(
                tags$li("This module requires group (condition) information.")
            )
        )
    }
    if(!is.null(r_data$batch)) {
        batch_ui <- selectInput("deseq_gene_plt_type", "Plot by", choices = list("Group" = "group", "Batch" = "batch"))
    } else {
        batch_ui <- NULL
    }
    
    list(
        uiOutput("deseq_results_box"),
        uiOutput("deseq_ma_box"),
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
                    column(4, selectInput("deseq_gene_plt_style", "Plot type", choices = list("Plot points" = "points", "Box plot" = "box", "Violin plot" = "violin"), selected = "points")),
                    column(4, batch_ui)
                )
            ),
            plotOutput("deseq_gene_plt", height = "600px")
        ),
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

output$deseq_results_box <- renderUI({
    groups <- as.list(unique(r_data$group))
    names(groups) <- unique(r_data$group)

    deseq_group_ui <- wellPanel(
        fluidRow(
            column(3, selectInput("deseq_group1", "Group 1", choices = groups, selected = as.character(groups[[1]]))),
            column(1, tags$b("vs")),
            column(3, selectInput("deseq_group2", "Group 2", choices = groups, selected = as.character(groups[[2]]))),
            column(4, numericInput("deseq_alpha", "FDR cutoff", value = 0.1, min = 0, max = 0.5, step = 0.001))
        ),
        fluidRow(
            column(8, checkboxInput("deseq_cuttbl", "Only show significant genes", value = F))
        )
    )
    
    if(!is.null(r_data$batch)) {
        deseq_batch_ui <- checkboxInput("deseq_batch_yes", "Control Batch Effects", value = F)
    } else {
        deseq_batch_ui <- NULL
    }
    
    enhanced_box(
        width = 12,
        title = "Differential Expression Analysis",
        id = "deseq_results",
        status = "primary",
        solidHeader = T,
        collapsible = T,
        reportable = T,
        get_html = T,
        register_analysis= T,
        deseq_batch_ui,
        actionButton("perform_deseq", "Perform DE Analysis", class = "btn-info"),
        hr(),
        deseq_group_ui,
        DT::dataTableOutput("deseq_result_tbl", width = "850px"),
        uiOutput("download_deseq_result_ui"),
        hr(),
        uiOutput("deseq_sig_genes")
    )
})




observeEvent(input$perform_deseq, {
    if(is.null(r_data$group)) return() 
    withProgress(message = 'Processing...', value = 0.8, {
        error_I <- 0
        tryCatch({
            samplesAll <- data.frame(row.names=colnames(r_data$raw), celltype=rep("nt",length(colnames(r_data$raw))))
            dds <- DESeq2::DESeqDataSetFromMatrix(countData = r_data$raw, colData=samplesAll, design = ~ 1)
            
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

        SummarizedExperiment::colData(dds)$group <- factor(r_data$group, levels = unique(r_data$group)) # update dds with group info
        if(!is.null(input$deseq_batch_yes) && input$deseq_batch_yes) {
            SummarizedExperiment::colData(dds)$batch <- factor(r_data$batch, levels = unique(r_data$batch))
            BiocGenerics::design(dds) <- formula(~ group + batch)
        } else {
            BiocGenerics::design(dds) <- formula(~ group)
        }
        
        r_data$dds <- DESeq2::DESeq(dds)
    })
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

output$download_deseq_result_ui <- renderUI({
    if(is.null(r_data$deseq_results)) return()
    tbl<-as.data.frame(r_data$deseq_results)
    if(nrow(tbl) == 0) return()
    download_deseq_result_ui <- downloadButton("download_deseq_result","Download", class = "btn btn-success")
})

observe({
    if(is.null(r_data$dds) || is.null(r_data$group)) return()
    if(is.null(input$deseq_group1)) return()
    if(input$deseq_group1 == input$deseq_group2) return()
    res1 <- DESeq2::results(r_data$dds, contrast = c("group", input$deseq_group1, input$deseq_group2), addMLE = T, alpha = input$deseq_alpha)
    r_data$deseq_group <- c(input$deseq_group1, input$deseq_group2)
    resOrdered <- res1[order(res1$padj),]
    if(input$deseq_cuttbl) {
        r_data$deseq_results <- subset(resOrdered, padj<input$deseq_alpha)
    } else {
        r_data$deseq_results <- resOrdered
    }
})

output$deseq_result_tbl <- DT::renderDataTable({
    if(is.null(r_data$deseq_results)) return()
    tbl<-as.data.frame(r_data$deseq_results)
    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, selection = 'single', options = list(scrollX = TRUE, scrollY = "250px", searching=T, order = list(list(7, 'asc')) , orderClasses = T))
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
        selectInput("deseq_ma_shrunk", NULL, choices = list("Plot DESeq2 shrunken log2 fold change" = "shrunken", "Plot unshrunken estimates of log2 fold change" = "unshrunken")),
        plotOutput("deseq_ma_plt", height = "600px")
    )
})

output$deseq_ma_plt <- renderPlot({
    if(is.null(r_data$deseq_results)) return()
    if(input$deseq_ma_shrunk == "shrunken") {
        BiocGenerics::plotMA(r_data$deseq_results, main="DESeq2", ylim=c(-2,2))
    } else {
        BiocGenerics::plotMA(r_data$deseq_results, MLE=TRUE, main="unshrunken LFC", ylim=c(-2,2))
    }
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

