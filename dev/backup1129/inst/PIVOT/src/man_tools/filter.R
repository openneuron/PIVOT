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


output$filter_renormalize_ui <- renderUI({
    tip <- paste("The data will be renormalized with the specified normalization method:", input$proc_method)
    shinyBS::tipify(checkboxInput("filter_renormalize", tags$b("Filter with renormalzation"), value = FALSE), title = tip, placement = "bottom", options = list(container = "body"))
})


output$filter_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return(
        tags$p("Looks there's nothing to filter.")
    )
    list(
        wellPanel(
            tags$b("Filter:"),
            a(id = "filter_type_help_btn", icon("question-circle")),
            shinyBS::bsModal(id = "filter_type_help", title = "Filter help", trigger = "filter_type_help_btn", size = "large", list(
                tags$li("You can select or delete features from the input set to create subset for analysis."),
                tags$li("You can use two types of filters, range filter and marker filter."),
                tags$li("Range filter allow you to choose features within a certain range of average/total expression."),
                tags$li("You can also provide a marker feature list to only analyze the ones you are interested in.")
            )),
            fluidRow(
                column(6, 
                       selectInput("feature_filter_type", label = "Filter based on", choices = list("Feature expression" = "range", "Marker feature" = "marker"))
                ),
                column(6, 
                       selectInput("is_neg_filter", label = "Select/Delete Feature", choices = list("Positive filter (select)" = FALSE, "Negative filter (delete)" = TRUE), selected = FALSE)
                )
            ),
            fluidRow(
                column(6, 
                       shinyBS::tipify(
                            checkboxInput("keep_filter", tags$b("Keep filtering the current dataset"), value = FALSE),
                            title = "If unchecked filtration will only be performed on the input dataset, otherwise filtration will be performed on the current dataset (retaining any previous filtering or subsetting effects).", 
                            placement = "bottom", options = list(container = "body")
                       )
                ),
                column(6, uiOutput("filter_renormalize_ui"))
            )

        ),
        hr(),
        uiOutput("filters"),

        hr(),
        fluidRow(
            column(9),
            column(3, actionButton('undo_filter_feature', label = "Undo Filter", icon = icon("times"), class = "btn btn-danger"))
        )
    )
})

output$filters <- renderUI({
    if(is.null(r_data$glb.raw)) return()
    if(input$feature_filter_type == "range") {
        list(
            wellPanel(
                fluidRow(
                    column(4,
                           selectInput("filter_type1", "Filter based on counts:", choices = c("Average" = "average", "Sum" = "sum"))
                    ),
                    column(6,
                           uiOutput("filter_type2_ui")
                    )
                )
            ),
            wellPanel(
                fluidRow(
                    column(8,
                           uiOutput("feature_cnt_filter")  
                    )
                ),
                fluidRow(
                    column(8,
                           fluidRow(
                               column(5,uiOutput("min_cnt_ui")),
                               column(2, tags$p("-")),
                               column(5, uiOutput("max_cnt_ui"))
                           )
                    ),
                    column(4,
                           uiOutput("range_filter_btn_ui")
                    )
                )
            ),
            hr(),
            wellPanel(
                fluidRow(
                    column(6, uiOutput("min_express_cells_ui")),
                    column(6, uiOutput("min_express_cells_percent"))
                ),
                uiOutput("express_filter_btn_ui")
            )
        )
    } else if(input$feature_filter_type == "marker") {
        list(
            fluidRow(
                column(6, 
                       wellPanel(
                           tags$b("Marker feature filter"),
                           wellPanel(
                               fluidRow(
                                   column(8, fileInput('mk_feature_file', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                   column(4, 
                                          a(id = "mk_feature_help_btn", icon("question-circle")),
                                          shinyBS::bsModal(id = "mk_feature_upload_help", title = "File format", trigger = "mk_feature_help_btn", size = "small", list(
                                              tags$p("The first column of the uploaded file will be used as markers (case insensitive). Example:"),
                                              img(src = "marker_tbl_exp.png", width = 200)
                                          ))
                                   )
                               ),
                               checkboxInput('mk_header', 'Header', value = F),
                               radioButtons('mk_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = '\t'),
                               radioButtons('mk_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"'),
                               uiOutput("mk_submit_ui")
                           )
                       )
                ),
                column(6,
                       tags$p("[feature name in 1st column, case insensitive]"),
                       DT::dataTableOutput('mk_feature_tbl')
                )
            )
        )
    } else {
        return()
    }
})

output$min_express_cells_ui <- renderUI({
    if(is.null(filter_data())) return()
    express_min <- min(rowSums(filter_data()$raw > 0))
    numericInput("min_express_cells", "Minimum number of cells expressed", min = 1, max = length(r_data$sample_name), value = express_min)
})

output$min_express_cells_percent <- renderUI({
    if(is.null(filter_data()) || is.null(input$min_express_cells)) return()
    list(
        tags$b("Percentage"),
        tags$p(),
        tags$p(paste0(round(input$min_express_cells/length(r_data$sample_name) * 100, digits = 1),"%"))
    )
})

############################### feature Filter module ###############################

# Filter data (global or local)
filter_data <- reactive({
    if(input$keep_filter) {
        return(list(raw = r_data$raw, df = r_data$df))
    } else {
        return(list(raw = r_data$glb.raw[, r_data$sample_name], df = r_data$glb.df[, r_data$sample_name]))
    }
})


get_max_min <- function() {
    if(input$filter_type2 == "normalized"){
        if(input$filter_type1 == "average") 
            list(max = ceiling(max(rowMeans(filter_data()$df))), min = floor(min(rowMeans(filter_data()$df))))
        else if(input$filter_type1 == "sum")
            list(max = ceiling(max(rowSums(filter_data()$df))), min = floor(min(rowSums(filter_data()$df))))
    } else if(input$filter_type2 == "raw"){
        if(input$filter_type1 == "average")
            list(max = ceiling(max(rowMeans(filter_data()$raw))), min = floor(min(rowMeans(filter_data()$raw))))
        else if(input$filter_type1 == "sum")
            list(max = ceiling(max(rowSums(filter_data()$raw))), min = floor(min(rowSums(filter_data()$raw))))
    }
}

# select range filter type
output$filter_type2_ui <- renderUI({
    if(is.null(filter_data())) return()
    if(input$proc_method != "none") {
        selectInput("filter_type2", label = br(), choices = list("raw counts" = "raw", "normalized counts" = "normalized"), selected = "raw")
    } else {
        selectInput("filter_type2", label = br(), choices = list("raw counts" = "raw"), selected = "raw")
    }
})

# define range and output ui
output$feature_cnt_filter <- renderUI({
    if(is.null(filter_data()) || is.null(input$filter_type2)) return()
    cnt <- get_max_min()
    if(is.null(r_data$f_range)) {
        cur_val <- c(cnt$min, cnt$max)
    } else {
        cur_val <- r_data$f_range
    }
    sliderInput("range_feature_cnt", label = paste("Select range of", input$filter_type1, input$filter_type2, "counts"), min = cnt$min, max = cnt$max, value = cur_val, step = 1)
})

output$min_cnt_ui <- renderUI({
    if(is.null(filter_data()) || is.null(input$filter_type2)) return()
    cnt <- get_max_min()
    
    if(is.null(input$range_feature_cnt)) {
        cur_value = cnt$min
    } else {
        cur_value = input$range_feature_cnt[1]
    }
    numericInput("min_cnt", label = NULL, value = cur_value, min = cnt$min, max = cnt$max, step = 1)
})

output$max_cnt_ui <- renderUI({
    if(is.null(filter_data()) || is.null(input$filter_type2)) return()
    cnt <- get_max_min()
    
    if(is.null(input$range_feature_cnt)) {
        cur_value = cnt$max
    } else {
        cur_value = input$range_feature_cnt[2]
    }
    numericInput("max_cnt", label = NULL, value = cur_value, min = cnt$min, max = cnt$max, step = 1)
})

# range filter button
output$range_filter_btn_ui <- renderUI({
    if(is.null(input$filter_renormalize)) return()
    negf <- as.logical(input$is_neg_filter)
    rnorm <- as.logical(input$filter_renormalize)
    if(!negf && !rnorm) {
        actionButton('range_filter_btn', label = "Select", class = "btn btn-info")
    } else if(negf && !rnorm) {
        actionButton('range_filter_btn', label = "Delete", class = "btn btn-info")
    } else if(!negf && rnorm) {
        actionButton('range_filter_btn', label = "Select & Renorm", class = "btn btn-info")
    } else if(negf && rnorm) {
        actionButton('range_filter_btn', label = "Delete & Renorm", class = "btn btn-info")
    }
})

output$express_filter_btn_ui <- renderUI({
    if(is.null(input$filter_renormalize)) return()
    negf <- as.logical(input$is_neg_filter)
    rnorm <- as.logical(input$filter_renormalize)
    if(!negf && !rnorm) {
        actionButton('express_filter_btn', label = "Select", class = "btn btn-info")
    } else if(negf && !rnorm) {
        actionButton('express_filter_btn', label = "Delete", class = "btn btn-info")
    } else if(!negf && rnorm) {
        actionButton('express_filter_btn', label = "Select & Renorm", class = "btn btn-info")
    } else if(negf && rnorm) {
        actionButton('express_filter_btn', label = "Delete & Renorm", class = "btn btn-info")
    }
})


# range filter: respond to button click
observeEvent(input$range_filter_btn, {
    negf <- as.logical(input$is_neg_filter)
    rnorm <- as.logical(input$filter_renormalize)
    
    # Set the new count range according to numeric input
    cnt <- get_max_min()
    
    if(input$min_cnt <= cnt$max & input$max_cnt<= cnt$max) {
        if(input$max_cnt - input$min_cnt + 1 < 2) {
            session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
            return()
        }
    } else {
        session$sendCustomMessage(type = "showalert", "The range must be within the slider range!")
        return()
    }
    
    withProgress(message = 'Filtering', value = 0, {
        clear_results()
        
        incProgress(0.3, detail = "Getting new feature list...")
        r_data$f_range <- c(input$min_cnt, input$max_cnt)
        # Now do the filtering
        new_df <- filter_data()$df
        new_raw <- filter_data()$raw
        
        if(input$filter_type2 == "normalized") {
            # normalized_cnts decides feature list
            if(input$filter_type1 == "average")
                tmp_df <- new_df[rowMeans(new_df) > input$min_cnt & rowMeans(new_df) <= input$max_cnt, ] # filter the current dataset 
            else if(input$filter_type1 == "sum")
                tmp_df <- new_df[rowSums(new_df) > input$min_cnt & rowSums(new_df) <= input$max_cnt, ]
            if(nrow(tmp_df) == 0) {
                session$sendCustomMessage(type = "showalert", "No features are found in this range!")
                return()
            }
            flist <- rownames(tmp_df) # Get the filtered list (key)
        } else if(input$filter_type2 == "raw") {
            # Raw decides feature list
            if(input$filter_type1 == "average")
                tmp_raw <- new_raw[rowMeans(new_raw) > input$min_cnt & rowMeans(new_raw) <= input$max_cnt, ] # filter the current dataset 
            else if(input$filter_type1 == "sum")
                tmp_raw <- new_raw[rowSums(new_raw) > input$min_cnt & rowSums(new_raw) <= input$max_cnt, ]
            if(nrow(tmp_raw) == 0) {
                session$sendCustomMessage(type = "showalert", "No features are found in this range!")
                return()
            }
            flist <- rownames(tmp_raw) # Get the filtered list (key)
        }
        
        neglist <- rownames(new_raw)[!rownames(new_raw)%in%flist]
        
        if(negf) {
            flist <- neglist
        }
        
        if(length(flist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left!")
            return()
        } 
        
        # use flist to update, do not interfere with r_data until success
        
        if(!rnorm) {
            # Reset data for analysis, using r_data$feature_list as key, current dataset will be updated
            r_data$feature_list <- flist
            r_data$raw <- new_raw[flist,]
            r_data$df <- new_df[flist,]
        } else {
            incProgress(0.3, detail = "Perform data normalization...")
            result <- normalize_data(input, new_raw[flist,])
            if(is.null(result)) {
                session$sendCustomMessage(type = "showalert", "Normalization failed!")
                return()
            } else {
                r_data$feature_list <- flist
                r_data$raw <- new_raw[flist,]
                r_data$df <- result$df
                r_data$norm_param <- result$norm_param # update the current normalization method for this subset
            }
        }
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        
        if(negf) {
            tmpText <- "Remove"
        } else {
            tmpText <- "Keep"
        }
        
        actionText <- paste(tmpText, "features in range:", input$min_cnt, "≤", input$filter_type1, input$filter_type2, "counts", "≤", input$max_cnt)
        
        if(!rnorm) {
            norm_method <- paste(r_data$norm_param$method, "(inherit)")
        } else {
            norm_method <- paste(r_data$norm_param$method, "(re-norm)")
        }
        
        if(input$keep_filter) {
            parent = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")]
        } else {
            parent <- search_subset_node(r_data$his_tbl)
        }
        
        r_data <- update_history(r_data, parent, "Filter", actionText, list(feature = r_data$feature_list, sample = r_data$sample_name), norm_method, r_data$norm_param)
        
        setProgress(1)
    })
})

# range filter: respond to button click
observeEvent(input$express_filter_btn, {
    negf <- as.logical(input$is_neg_filter)
    rnorm <- as.logical(input$filter_renormalize)
    
    # Set the new count range according to numeric input
    maxnumcells <- length(r_data$sample_name)
    if(input$min_express_cells > maxnumcells & input$min_express_cells > maxnumcells) {
        if(input$max_cnt - input$min_cnt + 1 < 2) {
            session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
            return()
        }
    } 
    
    withProgress(message = 'Filtering', value = 0, {
        clear_results()
        new_df <- filter_data()$df
        new_raw <- filter_data()$raw
    
        express <- rowSums(filter_data()$raw > 0)
        incProgress(0.3, detail = "Getting new feature list...")
        # Now do the filtering
        flist <- rownames(new_raw[express >= input$min_express_cells,])
        
        neglist <- rownames(new_raw)[!rownames(new_raw)%in%flist]
        
        if(negf) {
            flist <- neglist
        }
        
        if(length(flist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left!")
            return()
        } 
        
        # use flist to update, do not interfere with r_data until success
        
        if(!rnorm) {
            # Reset data for analysis, using r_data$feature_list as key, current dataset will be updated
            r_data$feature_list <- flist
            r_data$raw <- new_raw[flist,]
            r_data$df <- new_df[flist,]
        } else {
            incProgress(0.3, detail = "Perform data normalization...")
            result <- normalize_data(input, new_raw[flist,])
            if(is.null(result)) {
                session$sendCustomMessage(type = "showalert", "Normalization failed!")
                return()
            } else {
                r_data$feature_list <- flist
                r_data$raw <- new_raw[flist,]
                r_data$df <- result$df
                r_data$norm_param <- result$norm_param # update the current normalization method for this subset
            }
        }
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        
        if(negf) {
            tmpText <- "Remove"
        } else {
            tmpText <- "Keep"
        }
        
        actionText <- paste(tmpText, "features expressed in more than", input$min_express_cells, "cells")
        
        if(!rnorm) {
            norm_method <- paste(r_data$norm_param$method, "(inherit)")
        } else {
            norm_method <- paste(r_data$norm_param$method, "(re-norm)")
        }
        
        if(input$keep_filter) {
            parent = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")]
        } else {
            parent <- search_subset_node(r_data$his_tbl)
        }
        
        r_data <- update_history(r_data, parent, "Filter", actionText, list(feature = r_data$feature_list, sample = r_data$sample_name), norm_method, r_data$norm_param)
        
        
        setProgress(1)
    })
})

# Clear marker table
clear_marker <- function() {
    session$sendCustomMessage(type = "resetFileInputHandler", "mk_feature_file")
    marker_feature$tbl <- NULL
}

# Press the undo filtering has the same effect of filtering larger than 0
observeEvent(input$undo_filter_feature, {
    withProgress(message = 'Processing...', value = 0, {
        clear_results()
        
        incProgress(0.3, detail = "Retrieving unfiltered dataset...")
        r_data$raw <- r_data$glb.raw[, r_data$sample_name] # get raw set with sample list
        r_data$raw <- r_data$raw[rowSums(r_data$raw) > 0,] # Get non-zero feature set
        r_data$feature_list <- rownames(r_data$raw) # Get feature list 

        # Reset data for analysis, using r_data$feature_list as key, current dataset will be updated
        r_data$df <- r_data$glb.df[r_data$feature_list,r_data$sample_name]
        r_data$norm_param <- r_data$history[[1]]$norm_params # return the normalization information to input
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        incProgress(0.3, detail = "Updating UI...")
        r_data$f_range <- NULL
        # Reset ui
        cnt <- get_max_min()
        updateSliderInput(session, "range_feature_cnt",
                          label = NULL,
                          min = cnt$min, max = cnt$max, value = c(cnt$min, cnt$max), step = 1)
        updateNumericInput(session, "min_cnt", label = NULL, value = cnt$min, min = cnt$min, max = cnt$max, step = 1)
        updateNumericInput(session, "max_cnt", label = NULL, value = cnt$max, min = cnt$min, max = cnt$max, step = 1)
        clear_marker()
        
        # Return to the closest subset node
        parent <- search_subset_node(r_data$his_tbl)
        r_data$his_tbl$is_activated <- rep("N", nrow(r_data$his_tbl))
        r_data$his_tbl$is_activated[which(r_data$his_tbl$name == parent)] <- "Y"
        
        setProgress(1)
    })
})


output$mk_submit_ui <- renderUI({
    if(!is.null(input$mk_feature_file)) {
        negf <- as.logical(input$is_neg_filter)
        rnorm <- as.logical(input$filter_renormalize)
        if(!negf && !rnorm) {
            actionButton('submit_marker', label = "Select", class = "btn btn-info")
        } else if(negf && !rnorm) {
            actionButton('submit_marker', label = "Delete", class = "btn btn-info")
        } else if(!negf && rnorm) {
            actionButton('submit_marker', label = "Select & Renorm", class = "btn btn-info")
        } else if(negf && rnorm) {
            actionButton('submit_marker', label = "Delete & Renorm", class = "btn btn-info")
        }
    }
})


marker_feature <- reactiveValues()
marker_feature$tbl <- NULL

# process the upload feature list
observe({
    inFile <- input$mk_feature_file
    error_I <- 0
    if (!is.null(inFile)) {
        tryCatch({
            marker_feature$tbl <- read.csv(inFile$datapath, header=input$mk_header, sep=input$mk_sep, quote=input$mk_quote)
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

output$mk_feature_tbl <- DT::renderDataTable({
    if(is.null(marker_feature$tbl)) return()
    DT::datatable(marker_feature$tbl, options = list(scrollX = TRUE, scrollY = "350px", searching = FALSE))
})



# get marker_feature list when the user click submit button
observeEvent(input$submit_marker, {
    negf <- as.logical(input$is_neg_filter)
    rnorm <- as.logical(input$filter_renormalize)
    
    # First process the marker feature file and get the list
    if (is.null(marker_feature$tbl) || nrow(marker_feature$tbl) == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return(NULL)
    }
    marker_names <- make.names(as.character(unique(marker_feature$tbl[,1])))
    
    cur_flist <- rownames(filter_data()$raw)
    

    flist <- cur_flist[match(toupper(marker_names), toupper(cur_flist))]
    flist <- flist[!is.na(flist)]
    found_num <- length(marker_names) - length(flist)
    dup_num <- sum(duplicated(flist))
    flist <- flist[!duplicated(flist)] # remove duplicated

    # Some feature in the list may have been filtered out due to < threshold counts, 
    withProgress(message = 'Filtering...', value = 0, {
        clear_results()
        
        if(length(flist) != length(marker_names)) {
            message_gl <- paste0(length(marker_names) - length(flist)," features in your marker list (", length(marker_names),") are not found in the current dataset.")
            if(dup_num) {
                message_gl <- paste(message_gl, "Note some of your matched features have duplicated entries in the uploaded list.")
            }
            session$sendCustomMessage(type = "showalert", message_gl)
        }
        
        neglist <- cur_flist[!cur_flist%in%flist]
        
        if(negf) {
            flist <- neglist
        }
        
        if(length(flist) < 2) {
            session$sendCustomMessage(type = "showalert", "Too few features left!")
            return()
        }
        
        new_raw <- filter_data()$raw 
        new_df <- filter_data()$df
        
        if(!rnorm) {
            # Reset data for analysis, using r_data$feature_list as key, current dataset will be updated
            r_data$feature_list <- flist
            r_data$raw <- new_raw[flist,]
            r_data$df <- new_df[flist,]
        } else {
            incProgress(0.3, detail = "Perform data normalization...")
            result <- normalize_data(input, new_raw[flist,])
            if(is.null(result)) {
                session$sendCustomMessage(type = "showalert", "Normalization failed!")
                return()
            } else {
                r_data$feature_list <- flist
                r_data$raw <- new_raw[flist,]
                r_data$df <- result$df
                r_data$norm_param <- result$norm_param # update the current normalization method for this subset
            }
        }
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data)
        
        if(negf) {
            tmpText <- "Remove"
        } else {
            tmpText <- "Keep"
        }
        
        actionText <- paste(tmpText, length(flist), "marker features")
        
        if(!rnorm) {
            norm_method <- paste(r_data$norm_param$method, "(inherit)")
        } else {
            norm_method <- paste(r_data$norm_param$method, "(re-norm)")
        }
        
        if(input$keep_filter) {
            parent = r_data$his_tbl$name[which(r_data$his_tbl$is_activated == "Y")]
        } else {
            parent <- search_subset_node(r_data$his_tbl)
        }
        
        r_data <- update_history(r_data, parent, "Filter", actionText, list(feature = r_data$feature_list, sample = r_data$sample_name), norm_method, r_data$norm_param)
        
        setProgress(1)
    })
})