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


# DE analysis

output$monocle_de_ui <- renderUI({
    if(is.null(r_data$df)) return()
    infos <- list()
    
    if(is.null(r_data$group) || length(unique(r_data$group)) < 2) {
        infos[[length(infos) + 1]] <- tags$li("This module requires group (condition) information.")
    }

    if(is.null(r_data$monocle_ok)) {
        infos[[length(infos) + 1]] <- tags$li("Please initiate monocle celldataset first.")
    }

    if(length(infos)) {
        return(infos)
    }
    
    if(!is.null(r_data$batch)) {
        batch_ui <- selectInput("monocle_gene_plt_type", "Plot by", choices = list("Group" = "group", "Batch" = "batch"))
    } else {
        batch_ui <- NULL
    }
    
    list(
        enhanced_box(
            width = 12,
            title = "Monocle Differential Expression Analysis",
            id = "monocle_de_result",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            tags$li("Please check the background R session while running this DE analysis. If you do not see progress, please restart a fresh R session and try again."),
            tags$p(),
            actionButton("perform_monocle_de", "Perform Test", class = "btn-info"),
            hr(),
            fluidRow(
                column(12, 
                       wellPanel(
                           fluidRow(
                               column(6, numericInput_1("monocle_alpha", "Adjusted-P cutoff", value = 0.1, min = 0, max = 0.5, step = 0.001)),
                               column(6, checkboxInput("monocle_cuttbl", "Only show significant genes", value = F))
                           )
                       ),
                       
                       tags$b("Monocle DE analysis result",  class = "table-cap"),
                       DT::dataTableOutput("monocle_de_result_tbl"),
                       downloadButton("download_monocle_de_result", "Download", class = "btn btn-success"),
                       hr(),
                       uiOutput("monocle_de_summary")
                )
            )
        ),
        enhanced_box(
            width = 12,
            title = "Expression Plot of Selected Gene",
            id = "monocle_de_gene_plot",
            status = "danger",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            wellPanel(
                fluidRow(
                    column(6, selectInput("monocle_gene_plt_style", "Plot type", choices = list("Plot points" = "points", "Box plot" = "box", "Violin plot" = "violin"), selected = "points")),
                    column(6, batch_ui)
                )
            ),
            plotOutput("monocle_gene_plt", height = "600px")
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

observeEvent(input$perform_monocle_de, {
    if(is.null(r_data$cellset)) {
        session$sendCustomMessage(type = "showalert", "Please initiate monocle celldataset first.")
        updateTabItems(session, "tabs", "monocle")
        return()
    }
    if(is.null(pData(r_data$cellset)$Group)) {
        session$sendCustomMessage(type = "showalert", "Please re-initiate monocle celldataset to include the group information.")
        updateTabItems(session, "tabs", "monocle")
        return()
    }
    withProgress(message = 'Processing...', value = 0.8, {
        tmp_tbl <- differentialGeneTest(r_data$cellset, fullModelFormulaStr = "~Group", verbose = T)
        r_data$monocle_results <- tmp_tbl[order(tmp_tbl$qval),] %>% dplyr::select(status, family, pval, qval, percent_cells_expressed)
    })
})

# The monocle DE analysis often freezes, examing the source code

differentialGeneTest2 <- function (cds, fullModelFormulaStr = "~sm.ns(Pseudotime, df=3)", 
                                  reducedModelFormulaStr = "~1", relative_expr = TRUE, cores = 1, 
                                  verbose = FALSE) 
{

    diff_test_res <- smartEsApply(cds, 1, diff_test_helper, 
                                  convert_to_dense = TRUE, fullModelFormulaStr = fullModelFormulaStr, 
                                  reducedModelFormulaStr = reducedModelFormulaStr, 
                                  expressionFamily = cds@expressionFamily, relative_expr = relative_expr, 
                                  disp_func = cds@dispFitInfo[["blind"]]$disp_func, 
                                  verbose = verbose)
    diff_test_res <- do.call(rbind.data.frame, diff_test_res)
    diff_test_res$qval <- 1
    diff_test_res$qval[which(diff_test_res$status == "OK")] <- p.adjust(subset(diff_test_res, 
                                                                               status == "OK")[, "pval"], method = "BH")
    diff_test_res <- merge(diff_test_res, fData(cds), by = "row.names")
    row.names(diff_test_res) <- diff_test_res[, 1]
    diff_test_res[, 1] <- NULL
    diff_test_res
}

diff_test_helper <- function (x, fullModelFormulaStr, reducedModelFormulaStr, expressionFamily, 
                              relative_expr, weights, disp_func = NULL, verbose = FALSE) 
{
    reducedModelFormulaStr <- paste("f_expression", reducedModelFormulaStr, 
                                    sep = "")
    fullModelFormulaStr <- paste("f_expression", fullModelFormulaStr, 
                                 sep = "")
    x_orig <- x
    disp_guess <- 0
    if (expressionFamily@vfamily %in% c("negbinomial", "negbinomial.size")) {
        if (relative_expr == TRUE) {
            x <- x/Size_Factor
        }
        f_expression <- round(x)
        if (is.null(disp_func) == FALSE) {
            disp_guess <- calulate_NB_dispersion_hint(disp_func, 
                                                      round(x_orig))
            if (is.null(disp_guess) == FALSE && disp_guess > 
                0 && is.na(disp_guess) == FALSE) {
                if (expressionFamily@vfamily == "negbinomial") 
                    expressionFamily <- negbinomial(isize = 1/disp_guess)
                else expressionFamily <- negbinomial.size(size = 1/disp_guess)
            }
        }
    }
    else if (expressionFamily@vfamily %in% c("gaussianff", "uninormal")) {
        f_expression <- x
    }
    else if (expressionFamily@vfamily %in% c("binomialff")) {
        f_expression <- x/Size_Factor
    }
    else {
        f_expression <- log10(x)
    }
    test_res <- tryCatch({
        if (expressionFamily@vfamily %in% c("binomialff")) {
            if (verbose) {
                full_model_fit <- VGAM::vglm(as.formula(fullModelFormulaStr), 
                                             epsilon = 0.1, family = expressionFamily)
                reduced_model_fit <- VGAM::vglm(as.formula(reducedModelFormulaStr), 
                                                epsilon = 0.1, family = expressionFamily)
            }
            else {
                full_model_fit <- suppressWarnings(VGAM::vglm(as.formula(fullModelFormulaStr), 
                                                              epsilon = 0.1, family = expressionFamily))
                reduced_model_fit <- suppressWarnings(VGAM::vglm(as.formula(reducedModelFormulaStr), 
                                                                 epsilon = 0.1, family = expressionFamily))
            }
        }
        else {
            if (verbose) {
                full_model_fit <- VGAM::vglm(as.formula(fullModelFormulaStr), 
                                             epsilon = 0.1, family = expressionFamily)
                reduced_model_fit <- VGAM::vglm(as.formula(reducedModelFormulaStr), 
                                                epsilon = 0.1, family = expressionFamily)
            }
            else {
                full_model_fit <- suppressWarnings(VGAM::vglm(as.formula(fullModelFormulaStr), 
                                                              epsilon = 0.1, family = expressionFamily))
                reduced_model_fit <- suppressWarnings(VGAM::vglm(as.formula(reducedModelFormulaStr), 
                                                                 epsilon = 0.1, family = expressionFamily))
            }
        }
        compareModels(list(full_model_fit), list(reduced_model_fit))
    }, error = function(e) {
        if (verbose) 
            print(e)
        data.frame(status = "FAIL", family = expressionFamily@vfamily, 
                   pval = 1, qval = 1)
    })
    test_res
}

smartEsApply <- function (X, MARGIN, FUN, convert_to_dense, ...) 
{
    parent <- environment(FUN)
    if (is.null(parent)) 
        parent <- emptyenv()
    e1 <- new.env(parent = parent)
    multiassign(names(pData(X)), pData(X), envir = e1)
    environment(FUN) <- e1
    if (isSparseMatrix(exprs(X))) {
        res <- sparseApply(exprs(X), MARGIN, FUN, convert_to_dense, 
                           ...)
    }
    else {
        res <- apply(exprs(X), MARGIN, FUN, ...)
    }
    if (MARGIN == 1) {
        names(res) <- row.names(X)
    }
    else {
        names(res) <- colnames(X)
    }
    res
}


output$monocle_de_result_tbl <- DT::renderDataTable({
    if(is.null(r_data$monocle_results)) return()
    if(input$monocle_cuttbl) {
        tbl <- subset(r_data$monocle_results, qval <= input$monocle_alpha) 
    } else {
        tbl <- r_data$monocle_results
    }
    if(nrow(tbl) == 0) return()
    DT::datatable(tbl, selection = 'single', options = list(scrollY = "400px", lengthMenu = c(20, 50, 100), order = list(list(2, 'asc')), orderClasses = T
    ))
})

output$download_monocle_de_result <- downloadHandler(
    filename = "monocle_de_result.csv",
    content = function(file) {
        if(input$monocle_cuttbl) {
            tbl <- subset(r_data$monocle_results, qval <= input$monocle_alpha) 
        } else {
            tbl <- r_data$monocle_results
        }
        if(nrow(tbl) == 0) return()
        write.csv(tbl, file)
    }
)

output$monocle_de_summary <- renderUI({
    tags$li(paste0("Total number of significant genes: ", sum(r_data$monocle_results$qval < input$monocle_alpha, na.rm = T), "."))
})



output$monocle_gene_plt <- renderPlot({
    
    s = input$monocle_de_result_tbl_row_last_clicked
    if(input$monocle_cuttbl) {
        tbl <- subset(r_data$monocle_results, qval <= input$monocle_alpha) 
    } else {
        tbl <- r_data$monocle_results
    }
    if (length(s)) {    
        r_data$monocle_gene <- rownames(tbl[s, , drop = FALSE]) 
    } else {
        return()
    }
    
    d<- as.data.frame(t(r_data$df[r_data$monocle_gene,])) %>% tibble::rownames_to_column()
    colnames(d) <- c("sample", "expression_level")
    
    if(!is.null(r_data$group)) {
        d$group = r_data$group
    }  
    if(!is.null(r_data$batch)) {
        d$batch = r_data$batch
    }  
    if(!is.null(r_data$community)) {
        d$community = r_data$community
    } 

    if(is.null(input$monocle_gene_plt_type) || input$monocle_gene_plt_type == "group") {
        feature_super_plot(d, r_data$monocle_gene, plot_group = "group", style = input$monocle_gene_plt_style, legend_pos = "top")
    } else if(input$monocle_gene_plt_type == "batch") {
        feature_super_plot(d, r_data$monocle_gene, plot_group = "batch", style = input$monocle_gene_plt_style, legend_pos = "top")
    }
    
})






