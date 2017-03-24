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


options(shiny.maxRequestSize=1000*1024^2)


init_state <- function(r_data) {
    
    ################################ Count files processor ##################################
    # r_data
    r_data$file_path <- NULL
    r_data$input_type <- NULL
    
    r_data$history <- NULL # Data manipulation history (with all parameters)
    r_data$his_num <- NULL
    r_data$his_tbl <- NULL # Data manipulation history (as table)
    
    r_data$his_nodes <- NULL
    r_data$his_edges <- NULL
    
    r_data$reg_history <- NULL # Analysis register history (with parameters and links to reports)
    r_data$reg_tbl <- NULL # Analysis register table
    
    r_data$glb.raw <- NULL # This raw counts are combined or loaded raw counts (not filtered)
    r_data$glb.df <- NULL # This is DESeq normalzied data (not filtered)
    r_data$glb.group <- NULL
    r_data$glb.batch <- NULL

    r_data$norm_param <- NULL
    
    r_data$dds <- NULL
    r_data$deseq_results <- NULL
    r_data$deseq_group <- NULL
    
    r_data$raw <- NULL # This raw is different from data_ed$raw, this one is filtered raw cnts.
    r_data$df <- NULL # This is the filtered DESeq normalized data
    
    # These two (+ r_data$raw) serve as keys (and bridge) for subsetting/filtering purposes.
    r_data$feature_list <- NULL # This contains the filtered feature list used for analysis, every time the user choose a different feature set, the analyzer will grab that set of data using this key
    r_data$sample_name <- NULL # This has same value as sample_key once analyze btn is pressed.
    
    r_data$feature_meta <- NULL
    r_data$sample_meta <- NULL
    
    r_data$group <- NULL # Group info
    r_data$batch <- NULL # Batch info
    
    r_data$f_range <- NULL
    
    
    r_data$pca <- NULL
    r_data$pca_var <- NULL
    r_data$pca_var_exp <- NULL
    
    r_data$cooks <- NULL
    
    r_data$cfm <- NULL
    
    r_data$cor_ft_gene <- NULL
    r_data$coe_ft_target <- NULL
    r_data$coe_ft_tbl <- NULL
    
    r_data$plda <- NULL
    r_data$plda_flist <- NULL
    
    r_data$scde_ifm <- NULL
    r_data$scde_invalid <- NULL
    r_data$scde_ediff <- NULL
    r_data$scde_prior <- NULL
    r_data$scde_group <- NULL
    r_data$scde_sample <- NULL
    r_data$scde_batch <- NULL
    r_data$scde_gene <- NULL
    r_data$scde_ddo <- NULL
    r_data$scde_rw <- NULL
    r_data$scde_mrw <- NULL
    r_data$community <- NULL
    r_data$coms <- NULL
    r_data$mst <- NULL
    r_data$scde_results <- NULL
    
    r_data$dds_gene <- NULL
    
    r_data$mww_results <- NULL
    r_data$mww_group<- NULL
    r_data$mww_gene <- NULL
    
    r_data$cellset <- NULL # Monocle data object
    r_data$monocle_ok <- NULL 
    r_data$monocle_results <- NULL # monocle de result
    r_data$monocle_gene <- NULL
    r_data$monocle_genelist <- NULL
    r_data$monocle_gene_submitted <- NULL
    r_data$monocle_gene_for_clust <- NULL
    r_data$monocle_ordering <- NULL
    r_data$monocle_clusters <- NULL
    
    #r_data$stringdb <- NULL
    #r_data$stringdb_species <- NULL
    #r_data$stringdb_g <- NULL
    #r_data$string_score <- NULL
    r_data$stringdb_link <- NULL
    r_data$stringdb_genetbl <- NULL
    r_data$string_meta <- NULL
    
    r_data$input_tfs <- NULL
    r_data$tf_list <- NULL
    r_data$tf_de_tbl <- NULL
    r_data$tf_de_tbl_vis <- NULL
    r_data$tf_tbl_net <- NULL
    r_data$tf_tbl1 <- NULL
    r_data$tf_tbl2 <- NULL
    r_data$tfs <- NULL
    r_data$tf_nets <- NULL
    r_data$tf_g1 <- NULL
    r_data$tf_g2 <- NULL
    r_data$tf_neighbor_order <- NULL
    r_data$tf_group <- NULL
    
    r_data$reg_g <- NULL
    
    r_data$venn_list <- NULL
    
    r_data$rmd <- NULL
    r_data$rmd_list <- NULL
    
    r_data
}



# check if a button was NOT pressed
not_pressed <- function(x) if(is.null(x) || x == 0) TRUE else FALSE


shinyServer(function(input, output, session) {
    ip_inputs <- paste0("PIVOT_Inputs")
    ip_data <- paste0("PIVOT_Values")
    ip_dump <- paste0("PIVOT_DumpTime")

    # load previous state if available
    if (exists("r_state") && exists("r_data")) {
        r_data <- do.call(reactiveValues, r_data)
        r_state <- r_state
        rm(r_data, r_state, envir = .GlobalEnv)
    } else if (exists(ip_inputs) && exists(ip_data)) {
        r_data <- do.call(reactiveValues, get(ip_data))
        r_state <- get(ip_inputs)
        rm(list = c(ip_inputs, ip_data, ip_dump), envir = .GlobalEnv)
    } else {
        r_state <- list()
        r_data <- init_state(reactiveValues())
    }

    r_env <<- pryr::where("r_data")
    
    ################################################################################
    # function to save app state on refresh or crash (radiant)
    ################################################################################
    saveStateOnRefresh <- function(session = session) {
        session$onSessionEnded(function() {
            isolate({
                if(not_pressed(input$session_clear_sc) &&
                   not_pressed(input$exit_and_save) &&
                   is.null(input$uploadState)) {
                    
                    assign(ip_inputs, reactiveValuesToList(input), envir = .GlobalEnv)
                    assign(ip_data, reactiveValuesToList(r_data), envir = .GlobalEnv)
                    assign(ip_dump, lubridate::now(), envir = .GlobalEnv)
                    try(rm(r_env, envir = .GlobalEnv), silent = TRUE)
                }
            })
        })
    }
    
    
    #########################################################
    # Control the namespace DLLs
    #########################################################
    
    
    # Construct metadata
    init_meta <- function(r_data, type = "both") {
        if(!type %in% c("both", "sample", "feature")) {
            stop("unrecognized input type variable")
        }
        
        if(is.null(r_data$df)) return()
        
        if(type %in% c("both", "sample")) {
            r_data$sample_meta <- data.frame(total_normalized_counts = colSums(r_data$df))
            
            if(r_data$norm_param$method != "None") {
                r_data$sample_meta$total_raw_reads <- colSums(r_data$raw)
                if(r_data$norm_param$method %in% c("DESeq", "Modified_DESeq") ) {
                    r_data$sample_meta$deseq_size_factor <- r_data$norm_param$sizeFactor[r_data$sample_name,,drop = F]$size_factor
                } 
            }
            
            r_data$sample_meta$num_genes_expressed <- colSums(r_data$raw > 0)
            
            if(!is.null(r_data$group)){
                r_data$sample_meta$Group <- r_data$group
            }
            if(!is.null(r_data$batch)){
                r_data$sample_meta$Batch <- r_data$batch
            }
            
        }
        
        if(type %in% c("both", "feature")) {
            r_data$feature_meta <- data.frame(gene = r_data$feature_list, cap_name = toupper(r_data$feature_list))
            rownames(r_data$feature_meta) <- r_data$feature_list
            
            if(r_data$norm_param$method != "None") {
                r_data$feature_meta$total_raw_reads <- rowSums(r_data$raw)
            }
            
            r_data$feature_meta$total_normalized_counts = rowSums(r_data$df)
            r_data$feature_meta$num_cells_expressed <- rowSums(r_data$raw > 0)
            r_data$feature_meta$percent_cells_expressed <-  r_data$feature_meta$num_cells_expressed / length(r_data$sample_name)
        }

        return(r_data)
    }
    
    
    for(file in list.files("src/input",
                           pattern="\\.(r|R)$",
                           full.names = TRUE)) {
        
        source(file, local = TRUE)
    }
    
    for(file in list.files("src/helper_function",
                           pattern="\\.(r|R)$",
                           full.names = TRUE)) {
        
        source(file, local = TRUE)
    }
  
    for(file in list.files("src/man_tools",
                           pattern="\\.(r|R)$",
                           full.names = TRUE)) {
        
        source(file, local = TRUE)
    }
    
 
    ############# Final Data for analysis ##########
    # This will return different normalized sets according to the user command
    data0 <- reactive({
        if(is.null(r_data$df)) return()
        
        df <- r_data$df
        log <- log10(df + 1) # global DESeq_log10
        nm <- as.data.frame(t(scale(t(df)))) # global DESeq_normal
        log_nm <- as.data.frame(t(scale(t(log10(df + 1))))) # global DESeq_log10_normal
        
        
        if(input$ercc_isolation) {
            if(input$scale == "normalized_cnts")
                return(df[!(rownames(df) %in% ercc$features),])
            else if(input$scale == "log10")
                return(log[!(rownames(log) %in% ercc$features),])
            else if(input$scale == "standardized")
                return(nm[!(rownames(nm) %in% ercc$features),])
            else
                return(log_nm[!(rownames(log_nm) %in% ercc$features),])
        } else {
            if(input$scale == "normalized_cnts")
                return(df)
            else if(input$scale == "log10")
                return(log)
            else if(input$scale == "standardized")
                return(nm)
            else
                return(log_nm)
        }
    })
    
    
    for(file in list.files("src/modules",
                           pattern="\\.(r|R)$",
                           full.names = TRUE)) {
        
        source(file, local = TRUE)
    }
    
    #######################################
    # Save state
    #######################################
    saveState <- function(filename) {
        isolate({
            LiveInputs <- reactiveValuesToList(input)
            r_state[names(LiveInputs)] <- LiveInputs
            r_data <- reactiveValuesToList(r_data)
            save(r_state, r_data , file = filename)
        })
    }
    
    output$state_save_sc <- downloadHandler(
        filename = function() { paste0("PIVOTState-",Sys.Date(),".rda") },
        content = function(file) {
            saveState(file)
        }
    )
    
    #######################################
    # Load previous state
    #######################################
    observe({
        inFile <- input$uploadState
        if(!is.null(inFile)) {
            isolate({
                tmpEnv <- new.env()
                load(inFile$datapath, envir=tmpEnv)
                if (exists("r_data", envir=tmpEnv, inherits=FALSE))
                    assign(ip_data, tmpEnv$r_data, envir=.GlobalEnv)
                if (exists("r_state", envir=tmpEnv, inherits=FALSE))
                    assign(ip_inputs, tmpEnv$r_state, envir=.GlobalEnv)
                assign(ip_dump, lubridate::now(), envir = .GlobalEnv)
                rm(tmpEnv)
            })
        }
    })
    
    output$refreshOnUpload <- renderUI({
        inFile <- input$uploadState
        if(!is.null(inFile)) {
            # Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
            tags$script("window.location.reload();")
        }
    })
    
    observe({
        if(not_pressed(input$exit_and_save)) return()
        
        # quit R, unless you are running an interactive session
        if(interactive()) {
            # flush input and r_data into Rgui or Rstudio
            isolate({
                assign("r_state", reactiveValuesToList(input), envir = .GlobalEnv)
                assign("r_data", reactiveValuesToList(r_data), envir = .GlobalEnv)
                stop_message <- "\nStopping PIVOT. State available as 'r_state' and 'r_data'.\n"
                # if(!is.null(input$rmd_report) && input$rmd_report != "") {
                try(rm(r_env, envir = .GlobalEnv), silent = TRUE) # removing the reference to the shiny environment
                stopApp("PIVOT closed, state successfully saved to current R environment.")
            })
        } else {
            stopApp("PIVOT closed")
            q("no")
        }
    })
    
    saveStateOnRefresh(session)
    
})


