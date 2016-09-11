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

# Add report blocks to the ace editor

update_rmd <- function(session, rmd, id) {
    updateTabItems(session, "tabs", "report")
    
    if(is.null(rmd)) {
        rmd <- ""
    }
    
    file_path <- paste0("src/reports/", id, ".Rmd")
    
    new_block <- paste0(readLines(file_path), collapse = "\n")
    
    rmd <- paste0(rmd, new_block, sep = "\n")
    
    shinyAce::updateAceEditor(session, "report_rmd", value = rmd)
    
    return(rmd)
}

update_reg_history <- function(r_data, parent, action_type = "analysis", action, lists, norm_method, norm_param){
    if(is.null(r_data$reg_history)) {
        r_data$reg_history <- list()
    } 
    
    name = action
    parent = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")]
    
    r_data$reg_history[[length(r_data$reg_history) + 1]] <- list(
        name = as.character(name),
        time = lubridate::now(), 
        parent_data = as.character(parent),
        action_type = action_type,
        action = action, 
        lists = lists,
        norm_method = NA,
        norm_param = NA,
        feature_num = NA,
        sample_num = NA,
        user_notes = ""
    )
    reg_tbl <- plyr::ldply(r_data$reg_history, function(x){data.frame(x[c(1,2,3,4,5,7,9,10,11)])})
    reg_tbl$is_activated <- NA
    reg_tbl$name <- as.character(reg_tbl$name) # change factor to string so good to replace
    reg_tbl$parent_data <- as.character(reg_tbl$parent_data) # same as above
    reg_tbl$user_notes <- as.character(reg_tbl$user_notes)
    r_data$reg_tbl <- reg_tbl
}


observe({
    if(is.null(input)) return()
    if(is.null(input$report_rmd)) return()
    r_data$rmd <- input$report_rmd
})



generate_block_report <- function(id, file = NULL, mode = "html_document") {
    file_path <- paste0("src/reports/", id, ".Rmd")
    print(file_path)
    new_block <- paste0(readLines(file_path), collapse = "\n")
    error_I <- 0
    tryCatch({
        fileConn<-file("www/tmp.rmd")
        writeLines(new_block, fileConn)
        close(fileConn)
        path <- rmarkdown::render(input = "www/tmp.rmd", output_format = mode, output_file = file)},
        error = function(e) {
            session$sendCustomMessage(type = "showalert", "PIVOT failed to generate your report.")
            error_I <<- 1
        }
    )
    if(error_I) {
        return()
    } else {
        return(path)
    }
}



####################################### OPTIMIZE THIS ############################################
# The code below handles several classes of events. 
# It is copied multiple times for each modules. Theoretically it can/should be reduced to one general code block.

####### Data table show ######
observeEvent(input$data_table_report, {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "data_table")
})

observeEvent(input$data_table_reg, {
    id = "data_table"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Data table") 
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$data_table_html <- downloadHandler(
    filename = function() {
        id = "data_table"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "data_table"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Data table selected gene plot ######

observeEvent(input$"data_table_gene_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "data_table_gene_plot")
})

observeEvent(input$"data_table_gene_plot_reg", {
    id = "data_table_gene_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Gene expression plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$data_table_gene_plot_pdf <- downloadHandler(
    filename = function() {
        id = "data_table_gene_plot"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "data_table_gene_plot"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$data_table_gene_plot_html <- downloadHandler(
    filename = function() {
        id = "data_table_gene_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "data_table_gene_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Data distribution plot ######

observeEvent(input$"data_distribution_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "data_distribution")
})

observeEvent(input$"data_distribution_reg", {
    id = "data_distribution"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Data distribution plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$data_distribution_pdf <- downloadHandler(
    filename = function() {
        id = "data_distribution"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "data_distribution"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$data_distribution_html <- downloadHandler(
    filename = function() {
        id = "data_distribution"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "data_distribution"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Rank frequency plot ######

observeEvent(input$"rank_frequency_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "rank_frequency")
})

observeEvent(input$"rank_frequency_reg", {
    id = "rank_frequency"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Rank frequency plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$rank_frequency_pdf <- downloadHandler(
    filename = function() {
        id = "rank_frequency"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "rank_frequency"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$rank_frequency_html <- downloadHandler(
    filename = function() {
        id = "rank_frequency"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "rank_frequency"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Mean variability plot ######

observeEvent(input$"mean_var_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mean_var_plot")
})

observeEvent(input$"mean_var_plot_reg", {
    id = "mean_var_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Mean variability plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mean_var_plot_pdf <- downloadHandler(
    filename = function() {
        id = "mean_var_plot"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "mean_var_plot"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$mean_var_plot_html <- downloadHandler(
    filename = function() {
        id = "mean_var_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mean_var_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Mean standard-deviation plot ######

####### Mean variability plot ######

observeEvent(input$"mean_sd_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mean_sd_plot")
})

observeEvent(input$"mean_sd_plot_reg", {
    id = "mean_sd_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Per-feature standard deviation plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mean_sd_plot_pdf <- downloadHandler(
    filename = function() {
        id = "mean_sd_plot"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "mean_sd_plot"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$mean_sd_plot_html <- downloadHandler(
    filename = function() {
        id = "mean_sd_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mean_sd_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### hierarchical cluster plot ######

observeEvent(input$"hierarchical_clust_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "hierarchical_clust")
})

observeEvent(input$"hierarchical_clust_reg", {
    id = "hierarchical_clust"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Hierarchical clustering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$hierarchical_clust_pdf <- downloadHandler(
    filename = function() {
        id = "hierarchical_clust"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "hierarchical_clust"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$hierarchical_clust_html <- downloadHandler(
    filename = function() {
        id = "hierarchical_clust"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "hierarchical_clust"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### SCDE #######
# id = scde_error_model

observeEvent(input$"scde_error_model_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "scde_error_model")
})

observeEvent(input$"scde_error_model_reg", {
    id = "scde_error_model"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "SCDE error modeling")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$scde_error_model_html <- downloadHandler(
    filename = function() {
        id = "scde_error_model"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "scde_error_model"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = scde_de_result

observeEvent(input$"scde_de_result_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "scde_de_result")
})

observeEvent(input$"scde_de_result_reg", {
    id = "scde_de_result"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "SCDE DE result")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$scde_de_result_html <- downloadHandler(
    filename = function() {
        id = "scde_de_result"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "scde_de_result"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = scde_gene_plot

observeEvent(input$"scde_gene_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "scde_gene_plot")
})

observeEvent(input$"scde_gene_plot_reg", {
    id = "scde_gene_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = paste("SCDE gene plot:", r_data$scde_gene))
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$scde_gene_plot_html <- downloadHandler(
    filename = function() {
        id = "scde_gene_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "scde_gene_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### DESeq ######
# id = deseq_results

observeEvent(input$deseq_results_report, {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "deseq_results")
})

observeEvent(input$deseq_results_reg, {
    # First save a html output to tmp folder (how to get it go with state?)
    id = "deseq_results"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "DESeq results")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$deseq_results_html <- downloadHandler(
    filename = function() {
        id = "deseq_results"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "deseq_results"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = deseq_ma_plot

observeEvent(input$"deseq_ma_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "deseq_ma_plot")
})

observeEvent(input$"deseq_ma_plot_reg", {
    id = "deseq_ma_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "DESeq MA Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$deseq_ma_plot_html <- downloadHandler(
    filename = function() {
        id = "deseq_ma_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "deseq_ma_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = deseq_gene_plot

observeEvent(input$"deseq_gene_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "deseq_gene_plot")
})

observeEvent(input$"deseq_gene_plot_reg", {
    id = "deseq_gene_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = paste("DESeq Gene Plot:", r_data$dds_gene))
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$deseq_gene_plot_html <- downloadHandler(
    filename = function() {
        id = "deseq_gene_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "deseq_gene_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### MWW DE Test #######

# id = mww_test_result

observeEvent(input$"mww_test_result_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mww_test_result")
})

observeEvent(input$"mww_test_result_reg", {
    id = "mww_test_result"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Mann–Whitney U Test Result")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mww_test_result_html <- downloadHandler(
    filename = function() {
        id = "mww_test_result"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mww_test_result"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = mww_gene_plot
observeEvent(input$"mww_gene_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "mww_gene_plot")
})

observeEvent(input$"mww_gene_plot_reg", {
    id = "mww_gene_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = paste("MWW Gene Plot:", r_data$mww_gene))
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$mww_gene_plot_html <- downloadHandler(
    filename = function() {
        id = "mww_gene_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "mww_gene_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Monocle #######

#id = monocle_gene_var

observeEvent(input$"monocle_gene_var_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_gene_var")
})

observeEvent(input$"monocle_gene_var_reg", {
    id = "monocle_gene_var"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Cell Clustering/Ordering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_gene_var_html <- downloadHandler(
    filename = function() {
        id = "monocle_gene_var"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_gene_var"
        generate_block_report(id, file, mode = "html_document")
    }
)

#id = monocle_clust_result

observeEvent(input$"monocle_clust_result_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_clust_result")
})

observeEvent(input$"monocle_clust_result_reg", {
    id = "monocle_clust_result"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Gene Dispersion Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_clust_result_html <- downloadHandler(
    filename = function() {
        id = "monocle_clust_result"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_clust_result"
        generate_block_report(id, file, mode = "html_document")
    }
)

#id = monocle_trajectory

observeEvent(input$"monocle_trajectory_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_trajectory")
})

observeEvent(input$"monocle_trajectory_reg", {
    id = "monocle_trajectory"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Gene Expression Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_trajectory_html <- downloadHandler(
    filename = function() {
        id = "monocle_trajectory"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_trajectory"
        generate_block_report(id, file, mode = "html_document")
    }
)

#id = monocle_gene_plot

observeEvent(input$"monocle_gene_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_gene_plot")
})

observeEvent(input$"monocle_gene_plot_reg", {
    id = "monocle_gene_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Cell Trajectory Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_gene_plot_html <- downloadHandler(
    filename = function() {
        id = "monocle_gene_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_gene_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

#id = monocle_de_gene_plot

observeEvent(input$"monocle_de_gene_plot_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_de_gene_plot")
})

observeEvent(input$"monocle_de_gene_plot_reg", {
    id = "monocle_de_gene_plot"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Cell Trajectory Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_de_gene_plot_html <- downloadHandler(
    filename = function() {
        id = "monocle_de_gene_plot"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_de_gene_plot"
        generate_block_report(id, file, mode = "html_document")
    }
)

#id = monocle_gene_clust

observeEvent(input$"monocle_gene_clust_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_gene_clust")
})

observeEvent(input$"monocle_gene_clust_reg", {
    id = "monocle_gene_clust"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle Gene Clustering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_gene_clust_html <- downloadHandler(
    filename = function() {
        id = "monocle_gene_clust"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_gene_clust"
        generate_block_report(id, file, mode = "html_document")
    }
)


#id = monocle_de_result

observeEvent(input$"monocle_de_result_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "monocle_de_result")
})

observeEvent(input$"monocle_de_result_reg", {
    id = "monocle_de_result"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Monocle DE Analysis Results")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$monocle_de_result_html <- downloadHandler(
    filename = function() {
        id = "monocle_de_result"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "monocle_de_result"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### KMEANS clustering #######

# id = kmeans_clust
observeEvent(input$"kmeans_clust_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "kmeans_clust")
})

observeEvent(input$"kmeans_clust_reg", {
    id = "kmeans_clust"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "K-means Clustering")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$kmeans_clust_html <- downloadHandler(
    filename = function() {
        id = "kmeans_clust"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "kmeans_clust"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Community Detection #######

# id = community_mst
observeEvent(input$"community_mst_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "community_mst")
})

observeEvent(input$"community_mst_reg", {
    id = "community_mst"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Community Detection with MST")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$community_mst_html <- downloadHandler(
    filename = function() {
        id = "community_mst"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "community_mst"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Pairwise Correlation #######
# id = pair_corr
observeEvent(input$"pair_corr_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "pair_corr")
})

observeEvent(input$"pair_corr_reg", {
    id = "pair_corr"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Pairwise Correlations Scatterplot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$pair_corr_html <- downloadHandler(
    filename = function() {
        id = "pair_corr"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "pair_corr"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Sample Corrrelation Heatmap #######
# id = sample_corr_heatmap
observeEvent(input$"sample_corr_heatmap_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "sample_corr_heatmap")
})

observeEvent(input$"sample_corr_heatmap_reg", {
    id = "sample_corr_heatmap"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Sample Correlation Heatmap")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$sample_corr_heatmap_html <- downloadHandler(
    filename = function() {
        id = "sample_corr_heatmap"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "sample_corr_heatmap"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Feature Corrrelation Heatmap #######
# id = feature_corr_heatmap
observeEvent(input$"feature_corr_heatmap_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "feature_corr_heatmap")
})

observeEvent(input$"feature_corr_heatmap_reg", {
    id = "feature_corr_heatmap"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Feature Correlation Heatmap")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$feature_corr_heatmap_html <- downloadHandler(
    filename = function() {
        id = "feature_corr_heatmap"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "feature_corr_heatmap"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = feature_corr_target
observeEvent(input$"feature_corr_target_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "feature_corr_target")
})

observeEvent(input$"feature_corr_target_reg", {
    id = "feature_corr_target"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = paste("Features Correlated with", r_data$coe_ft_target))
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$feature_corr_target_html <- downloadHandler(
    filename = function() {
        id = "feature_corr_target"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "feature_corr_target"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Feature Heatmap ######

observeEvent(input$"feature_heatmap_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "feature_heatmap")
})

observeEvent(input$"feature_heatmap_reg", {
    # First save a html output to tmp folder (how to get it go with state?)
    id = "feature_heatmap"
    tmp_path <- generate_block_report(id, mode = "html_document")
    rawHTML <- paste(readLines(tmp_path), collapse="\n")
    
    # Register the event in the reg_tbl
    r_data <- update_reg_history(r_data, lists = rawHTML, action = "Feature heatmap")
    session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
})

output$feature_heatmap_pdf <- downloadHandler(
    filename = function() {
        id = "feature_heatmap"
        paste0(id, ".pdf")
    },
    content = function(file) {
        id = "feature_heatmap"
        generate_block_report(id, file, mode = "pdf_document")
    }
)

output$feature_heatmap_html <- downloadHandler(
    filename = function() {
        id = "feature_heatmap"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "feature_heatmap"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### PCA #######

# id = pca_scree
observeEvent(input$"pca_scree_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "pca_scree")
})

observeEvent(input$"pca_scree_reg", {
    id = "pca_scree"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "PCA Scree Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$pca_scree_html <- downloadHandler(
    filename = function() {
        id = "pca_scree"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "pca_scree"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = pca_1d
observeEvent(input$"pca_1d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "pca_1d")
})

observeEvent(input$"pca_1d_reg", {
    id = "pca_1d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "PCA 1D Projection")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$pca_1d_html <- downloadHandler(
    filename = function() {
        id = "pca_1d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "pca_1d"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = pca_2d
observeEvent(input$"pca_2d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "pca_2d")
})

observeEvent(input$"pca_2d_reg", {
    id = "pca_2d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "PCA 2D Projection")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$pca_2d_html <- downloadHandler(
    filename = function() {
        id = "pca_2d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "pca_2d"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = pca_3d
observeEvent(input$"pca_3d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "pca_3d")
})

observeEvent(input$"pca_3d_reg", {
    id = "pca_3d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "PCA 3D Projection")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$pca_3d_html <- downloadHandler(
    filename = function() {
        id = "pca_3d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "pca_3d"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### T-sne #######

# id = tsne_1d
observeEvent(input$"tsne_1d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "tsne_1d")
})

observeEvent(input$"tsne_1d_reg", {
    id = "tsne_1d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "T-SNE 1D Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$tsne_1d_html <- downloadHandler(
    filename = function() {
        id = "tsne_1d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "tsne_1d"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = tsne_2d
observeEvent(input$"tsne_2d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "tsne_2d")
})

observeEvent(input$"tsne_2d_reg", {
    id = "tsne_2d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "T-SNE 2D Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$tsne_2d_html <- downloadHandler(
    filename = function() {
        id = "tsne_2d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "tsne_2d"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = tsne_3d
observeEvent(input$"tsne_3d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "tsne_3d")
})

observeEvent(input$"tsne_3d_reg", {
    id = "tsne_3d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "T-SNE 3D Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$tsne_3d_html <- downloadHandler(
    filename = function() {
        id = "tsne_3d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "tsne_3d"
        generate_block_report(id, file, mode = "html_document")
    }
)


####### Penalized LDA ######
# id = plda_feature
observeEvent(input$"plda_feature_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "plda_feature")
})

observeEvent(input$"plda_feature_reg", {
    id = "plda_feature"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "PenalizedLDA Feature Selection")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$plda_feature_html <- downloadHandler(
    filename = function() {
        id = "plda_feature"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "plda_feature"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = plda_1d
observeEvent(input$"plda_1d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "plda_1d")
})

observeEvent(input$"plda_1d_reg", {
    id = "plda_1d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Penalized LDA 1D Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$plda_1d_html <- downloadHandler(
    filename = function() {
        id = "plda_1d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "plda_1d"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = plda_2d
observeEvent(input$"plda_2d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "plda_2d")
})

observeEvent(input$"plda_2d_reg", {
    id = "plda_2d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Penalized LDA 2D Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$plda_2d_html <- downloadHandler(
    filename = function() {
        id = "plda_2d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "plda_2d"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = plda_3d
observeEvent(input$"plda_3d_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "plda_3d")
})

observeEvent(input$"plda_3d_reg", {
    id = "plda_3d"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Penalized LDA 3D Plot")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$plda_3d_html <- downloadHandler(
    filename = function() {
        id = "plda_3d"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "plda_3d"
        generate_block_report(id, file, mode = "html_document")
    }
)

####### Network Analysis #######

# id = trans_de_tbl
observeEvent(input$"trans_de_tbl_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "trans_de_tbl")
})

observeEvent(input$"trans_de_tbl_reg", {
    id = "trans_de_tbl"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "DE Table for Network Analysis")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$trans_de_tbl_html <- downloadHandler(
    filename = function() {
        id = "trans_de_tbl"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "trans_de_tbl"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = trans_net
observeEvent(input$"trans_net_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "trans_net")
})

observeEvent(input$"trans_net_reg", {
    id = "trans_net"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Network Visualization of DE Genes")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$trans_net_html <- downloadHandler(
    filename = function() {
        id = "trans_net"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "trans_net"
        generate_block_report(id, file, mode = "html_document")
    }
)

# id = trans_result
observeEvent(input$"trans_result_report", {
    r_data$rmd <- update_rmd(session, r_data$rmd, id = "trans_result")
})

observeEvent(input$"trans_result_reg", {
    id = "trans_result"
    withProgress(message = 'Processing...', value = 0.6, {
        tmp_path <- generate_block_report(id, mode = "html_document")
        if(!is.null(tmp_path)){
            rawHTML <- paste(readLines(tmp_path), collapse="\n")
            r_data <- update_reg_history(r_data, lists = rawHTML, action = "Network Based Transcription Factor Scoring and Ranking")
            session$sendCustomMessage(type = "showalert", "Analysis report generated and linked to the current dataset.")
        }
    })
})

output$trans_result_html <- downloadHandler(
    filename = function() {
        id = "trans_result"
        paste0(id, ".html")
    },
    content = function(file) {
        id = "trans_result"
        generate_block_report(id, file, mode = "html_document")
    }
)

