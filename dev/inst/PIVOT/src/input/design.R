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


output$design_ui <- renderUI({
    if(is.null(r_data$glb.raw)) return()
    if(input$add_group_way == 1) { # manual add design 
        list(
            fluidRow(
                column(5,
                       tags$div(tags$b("Add Category:"), class = "param_setting_title"),
                       textInput("man_cate_name", "Category(column) name:", placeholder = "e.g., Group/Batch/Condition"),
                       actionButton('man_add_cate', label = "Add Category", class = "btn btn-info"),
                       tags$p(),
                       uiOutput("man_choose_cate_ui"),
                       tags$p(),
                       uiOutput("man_cate_add_group_ui")
                ),
                column(7, 
                       tags$div(tags$b("Design Table Preview"), class = "param_setting_title"),
                       DT::dataTableOutput("group_tbl_man_show"),
                       tags$p(),
                       uiOutput("grp_submitted_img1"),
                       uiOutput("man_design_submit_ui")
                )        
            )
        )
    } else { # upload design table
        list(
            fluidRow(
                column(5,
                       tags$div(tags$b("Design Table Upload:"), class = "param_setting_title"),
                       fluidRow(
                           column(8, fileInput('file_group', label = NULL, accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                           column(4, 
                                  a(id = "design_upload_help_btn", icon("question-circle")),
                                  shinyBS::bsModal(id = "design_upload_help", title = "File format", trigger = "design_upload_help_btn", size = "large", list(
                                      tags$p("The file must contain one 'Sample' column, one 'Group' and/or 'Batch' column. Example:"),
                                      img(src = "design_tbl_exp.png", width = 300)
                                  ))
                           )
                           
                       ),
                       wellPanel(
                           checkboxInput('group_header', 'Header', TRUE),
                           radioButtons('group_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = ','),
                           radioButtons('group_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"')
                       )
                ),
                column(7,
                       tags$div(tags$b("Design Table Preview"), class = "param_setting_title"),
                       DT::dataTableOutput("group_tbl_up_show"),
                       br(),
                       checkboxInput("sample_reorder", "Reorder samples according to the design table.", value = F),
                       uiOutput("grp_submitted_img2"),
                       actionButton('submit_design_upload', label = "submit design", class = "btn-primary btn_rightAlign")
                )
            )
        )
    }
})

design_value <- reactiveValues()

custom_merge <- function(df, df1, by, col) {
    sp <- df[,by]
    gp <- df[,col]
    names(gp) <- as.character(sp)
    gp [as.character(df1[,by])] <- as.character(df1[,col])
    df[,col] <- gp
    return(df)
}

output$man_choose_cate_ui <- renderUI({
    if(length(design_value$cates) < 1) return()
    categories = design_value$cates
    names(categories) = categories
    list(
        tags$div(tags$b("Edit Category:"), class = "param_setting_title"),
        selectInput("man_choose_cate", "Choose category", choices = as.list(categories)),
        actionButton('man_del_cate', label = "Delete Category", class = "btn-danger")
    )
})

output$manual_add_group_ui <- renderUI({
    if(input$add_group_way != 1) return()
    sample_name <- colnames(r_data$glb.raw)
    isolate({
        group_options <- as.list(sample_name)
        names(group_options) <- sample_name
        list(
            textInput("man_group_name", label = "Group name:", value = "group_1"),
            selectizeInput('man_sample_in_group', label = "Add samples to this group", choices = group_options, multiple = TRUE),
            verbatimTextOutput("man_group_show"),
            actionButton('man_add_group', label = "Add Group", class = "btn-info")
        )
    })
})

output$man_cate_add_group_ui <- renderUI({
    if(is.null(input$man_choose_cate)) return()
    if(!input$man_choose_cate %in% colnames(design_value$df)) return()
    list(
        tags$div(tags$b(paste("Add Groups to Category"),input$man_choose_cate), class = "param_setting_title"),
        uiOutput("manual_add_group_ui")
    )
})

output$man_design_submit_ui <- renderUI({
    not_filled <- sum(is.na(design_value$df))
    if(!not_filled) {
        actionButton("man_submit_design", "Submit Design", class = "btn-primary btn_rightAlign")
    } else {
        list(
            modalTriggerButton("man_submit_trigger", "#submitConfirmDlg", paste("Submit Design"), class = "btn action-button btn-primary btn_rightAlign"),
            modalDialog(id="submitConfirmDlg", 
                        header = "Alert: Empty cells detected",
                        body = "System detects some cells are still empty, these cells will be replaced with NA values (as 'NA' group).",
                        footer=list(
                            modalTriggerButton("submitConfirmDlgBtn", "#submitConfirmDlg", "Proceed", class = "btn action-button btn-primary"),
                            tags$button(type = "button", class = "btn btn-danger", 'data-dismiss' = "modal", "Cancel")
                        )
            )
        )
    }
})

################################## Group input handler #########################################
clear_design <- function() {
    if(!is.null(design_upload$df)) {
        session$sendCustomMessage(type = "resetFileInputHandler", "file_group")
        design_upload$df <- data.frame()
    }
    if(!is.null(r_data$glb.raw)) {
        design_value$df <- data.frame(Sample = colnames(r_data$glb.raw))
        design_value$cates <- NULL
    } else {
        design_value <- NULL
    }
    
    plot_specs$info <- NULL
    
    r_data$glb.meta <- data.frame(sample = colnames(r_data$glb.raw))
    r_data$group <- NULL
    r_data$glb.group <- NULL
    r_data$batch <- NULL
    r_data$glb.batch <- NULL
    
    clear_results()
}

clear_results <-function() {
    r_data$community <- NULL
    r_data$coms <- NULL
    r_data$mst <- NULL
    
    r_data$dds_gene <- NULL
    
    r_data$deseq_results <- NULL
    r_data$deseq_group <- NULL
    
    r_data$scde_ifm<-NULL
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
    r_data$scde_results <- NULL
    
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
    
    r_data$mww_results <- NULL
    r_data$mww_group<- NULL
    r_data$mww_gene <- NULL
    
    r_data$cellset <- NULL # Monocle data object
    r_data$monocle_ok <- NULL
    
    r_data$monocle_results <- NULL
    r_data$monocle_gene <- NULL
    r_data$monocle_ordering <- NULL
    r_data$monocle_genelist <- NULL
    r_data$monocle_gene_submitted <- NULL
    r_data$monocle_gene_for_clust <- NULL
    r_data$monocle_alpha<- NULL
    r_data$monocle_clusters <- NULL
    
    #r_data$stringdb <- NULL # Do we need to remove this?
    #r_data$stringdb_species <- NULL
    #r_data$string_score <- NULL
    #r_data$stringdb_g <- NULL # same above?
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
}

###### Handle manual group input #####
observe({
    # Init df
    if(is.null(r_data$glb.raw)) return()
    if(is.null(design_value$df)) {
        design_value$df <- data.frame(Sample = colnames(r_data$glb.raw))
        design_value$df$Group <- rep(NA, nrow(design_value$df))
        design_value$cates <- colnames(design_value$df)[-1]
    }
})

observe({
    if(is.null(r_data$glb.raw) || is.null(input$man_choose_cate) || is.null(design_value$df)) return ()
    if(!input$man_choose_cate %in% colnames(design_value$df)) return()
    isolate({
        #assign("design_value", design_value$df, env = .GlobalEnv)
        left_sample <- design_value$df$Sample[which(is.na(design_value$df[,input$man_choose_cate]))]
        group_options <- as.list(left_sample)
        names(group_options) <- left_sample
        updateSelectInput(session, "man_sample_in_group",
                          label = "Add samples to this group:",
                          choices = group_options, selected = NULL)
    })
})

current_group <- reactive({
    if(is.null(input$man_sample_in_group)) return()
    cur_cate <- input$man_choose_cate
    df<-data.frame(Sample = input$man_sample_in_group)
    df[,cur_cate] <- rep(input$man_group_name, nrow(df))
    return(df)
})

output$man_group_show <- renderPrint({
    current_group()
})

observeEvent(input$man_add_cate, {
    if(is.null(input$man_cate_name) || input$man_cate_name == ""){
        session$sendCustomMessage(type = "showalert", "Please specify a category name.")
        return()
    }
    if(input$man_cate_name %in% colnames(design_value$df)) {
        session$sendCustomMessage(type = "showalert", "Category already present in the table.")
        return()
    }
    if(input$man_cate_name %in% c("None", "none")) {
        session$sendCustomMessage(type = "showalert", "'None' or 'none' cannot be used as category name.")
        return()
    }
    design_value$df[,input$man_cate_name] <- rep(NA, nrow(design_value$df))
    design_value$cates <- colnames(design_value$df)[-1]
})

observeEvent(input$man_del_cate, {
    if(is.null(input$man_choose_cate)) return()
    design_value$df[,input$man_choose_cate] <- NULL
    design_value$cates <- colnames(design_value$df)[-1]
})

observeEvent(input$man_add_group, {
    if(is.null(input$man_sample_in_group)){
        session$sendCustomMessage(type = "showalert", "Please add samples to this group.")
        return()
    }
    design_value$df <- custom_merge(design_value$df, current_group(), by = "Sample", col = input$man_choose_cate)
})

output$group_tbl_man_show <- DT::renderDataTable({
    DT::datatable(design_value$df, options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE, searching = FALSE)) 
})

observeEvent(input$clear_group_1, {
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Removing design info...")
        clear_design()
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data, type = "sample")
        setProgress(1)
    })
})

# submit group (manual module)
observeEvent(input$man_submit_design, {
    if(ncol(design_value$df) < 2) {
        session$sendCustomMessage(type = "showalert", "Please add at least one category.")
        return()
    }

    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Adding design info...")
        df_tmp <- design_value$df
        # Other sanity checks? Like cols contain NA, empty cols(only spaces), cols contain only one category?
        r_data$glb.meta <- df_tmp
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data, type = "sample")
        setProgress(1)
    })
})

observeEvent(input$submitConfirmDlgBtn, {
    if(ncol(design_value$df) < 2) {
        session$sendCustomMessage(type = "showalert", "Please add at least one category.")
        return()
    }
    
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Adding design info...")
        df_tmp <- design_value$df
        # Other sanity checks? Like cols contain NA, empty cols(only spaces), cols contain only one category?
        r_data$glb.meta <- df_tmp
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data, type = "sample")
        setProgress(1)
    })
})


##### Handle group info input file #####
design_upload <- reactiveValues()
design_upload$df <- data.frame()

observe({
    inFile <- input$file_group
    if (is.null(inFile))
        return(NULL)
    tryCatch({
        design_upload$df <- read.csv(inFile$datapath, header=input$group_header, sep=input$group_sep, quote=input$group_quote)
    }, 
    error = function(e){
        session$sendCustomMessage(type = "showalert", "Unsupported file format.")
        return()
    })
})

output$group_tbl_up_show <- DT::renderDataTable({
    DT::datatable(design_upload$df, options = list(scrollX = TRUE, scrollY = "350px", paging = FALSE, searching = FALSE))
})

observeEvent(input$clear_group_2, {
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Removing design info...")
        clear_design()
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data, type = "sample")
        setProgress(1)
    })
})

# Upload group info handler
observeEvent(input$submit_design_upload, {
    if(dim(design_upload$df)[1] == 0)
    {
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return()
    }
    
    df_tmp <- design_upload$df
    # assign('df_tmp', df_tmp, env = .GlobalEnv)
    # Take first column as sample column
    sample_col <- df_tmp[,1] 
    matched_sp <- match(colnames(r_data$glb.raw), sample_col) # If contain NA, some sample are not found in sample_col
    if(any(is.na(matched_sp))) 
    {
        session$sendCustomMessage(type = "showalert", "Please provide a meta table for ALL your input samples. Some sample names are not found.")
        return()
    }
    
    if(any(colnames(df_tmp) %in% c("None", "none"))) {
        session$sendCustomMessage(type = "showalert", "'None' or 'none' cannot be used as category name.")
        return()
    }
    
    withProgress(message = 'Processing', value = 0, {
        incProgress(0.3, detail = "Adding design info...")
        if(input$sample_reorder) {
            sp_ordered<-colnames(r_data$glb.raw)[match(sample_col, colnames(r_data$glb.raw))]
            sp_ordered <- sp_ordered[!is.na(sp_ordered)]
            
            r_data$glb.raw <- r_data$glb.raw[sp_ordered]
            r_data$glb.df <- r_data$glb.df[sp_ordered]
            
            r_data$sample_name <- sp_ordered[which(sp_ordered %in% r_data$sample_name)]
            r_data$raw <- r_data$raw[r_data$sample_name]
            r_data$df <- r_data$df[r_data$sample_name]
        }
        # Other sanity checks? Like cols contain NA, empty cols(only spaces), cols contain only one category?
        r_data$glb.meta <- df_tmp
        
        incProgress(0.3, detail = "Updating metadata...")
        r_data <- init_meta(r_data, type = "sample")
        setProgress(1)
    })
})

# Group submitted indicator
# The image indicating file is submitted
output$grp_submitted_img1 <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) == 1) return()
    img(src = "button_ok.png", width = 35, height = 35, align = "right")
})

output$grp_submitted_img2 <- renderUI({
    if(is.null(r_data$glb.meta) || ncol(r_data$glb.meta) == 1) return()
    img(src = "button_ok.png", width = 35, height = 35, align = "right")
})

output$input_design_tbl <- DT::renderDataTable({
    if(is.null(r_data$glb.meta)) return()
    DT::datatable(r_data$glb.meta, selection = 'single', 
                  options = list(
                      scrollX = T, scrollY = "500px", lengthMenu = c(20, 50, 100)
                  )
    )
})

output$download_design_tbl <- downloadHandler(
    filename = "design_tbl.csv",
    content = function(file) {
        write.csv(r_data$glb.meta, file)
    }
)




##### Group color #####
get_brewer_set <- function(palette = c("sequential", "diverging", "qualitative")) {
    match.arg(palette,
              several.ok = TRUE)
    
    sequential_palette <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 
                            'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                            'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
    names(sequential_palette) <- sequential_palette
    diverging_palette <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
    names(diverging_palette) <- diverging_palette
    qualitative_palette <- c('Accent','Dark2','Paired', 'Pastel1', 'Pastel2','Set1', 'Set2', 'Set3')
    names(qualitative_palette) <- qualitative_palette
    return_palette = list()
    if("sequential" %in% palette) {
        return_palette <- c(return_palette, as.list(sequential_palette))
    }
    if("diverging" %in% palette) {
        return_palette <- c(return_palette, as.list(diverging_palette))
    }
    if("qualitative" %in% palette) {
        return_palette <- c(return_palette, as.list(qualitative_palette))
    }
    return(return_palette)
}

get_color_vector <- function(labels, pal="Set1", maxCol=9)
{
    unq <- unique(labels)
    hmcol <- RColorBrewer::brewer.pal(maxCol, pal)
    
    colv <- rep(NA, length(labels))

    if (length(unq) > 9)
    {
        cp <- colorRampPalette(hmcol)
        hmcol <- cp(length(unq))
    }
    
    for (i in 1:length(unq))
    {
        colv[labels == unq[i]] <- hmcol[i]
    }
    
    return(colv)
}


output$sample_coloring_ui <- renderUI({
    if(is.null(r_data$group) && is.null(r_data$batch) && is.null(r_data$community)) return()
    
    coloring_choices <- list("none" = "none")
    if(!is.null(r_data$group))
        coloring_choices$group <- "group"
    if(!is.null(r_data$batch))
        coloring_choices$batch <- "batch"
    if(!is.null(r_data$community))
        coloring_choices$community <- "community"
    if(length(coloring_choices)) {
        list(
            sidebarwell2(
                selectInput("coloring_type", "Color samples by", choices = coloring_choices),
                actionButton("group_color_switcher", label = "Change Color", class = "sidebar-button")
            )
        )
    }
})

group_pal <- reactiveValues()
group_pal$val <- "Set1"
group_pal$idx <- 1

observeEvent(input$group_color_switcher, {
    colvec <- c("Set1", "Dark2", "RdYlGn", "Paired")
    group_pal$idx <- (group_pal$idx + 1) %% (length(colvec))
    if(group_pal$idx == 0) {
        group_pal$val  <- colvec[length(colvec)]
    } else {
        group_pal$val  <- colvec[group_pal$idx]
    }
})

group_color <- reactive({
    if(is.null(r_data$group)) return(NULL)
    get_color_vector(r_data$group, pal = group_pal$val, maxCol = length(unique(r_data$group)))
})

group_color_legend <- reactive({
    if(is.null(r_data$group)) return(NULL)
    tmp_gc <- unique(group_color())
    tmp_name <- unique(r_data$group)
    names(tmp_gc) <- tmp_name
    tmp_gc
})

batch_color <- reactive({
    if(is.null(r_data$batch)) return(NULL)
    get_color_vector(r_data$batch, pal = group_pal$val, maxCol = length(unique(r_data$batch)))
})

batch_color_legend <- reactive({
    if(is.null(r_data$batch)) return(NULL)
    tmp_gc <- unique(batch_color())
    tmp_name <- unique(r_data$batch)
    names(tmp_gc) <- tmp_name
    tmp_gc
})

community_color <- reactive({
    if(is.null(r_data$community)) return(NULL)
    get_color_vector(r_data$community, pal = group_pal$val, maxCol = length(unique(r_data$community)))
})

community_color_legend <- reactive({
    if(is.null(r_data$community)) return(NULL)
    tmp_gc <- unique(community_color())
    tmp_name <- unique(r_data$community)
    names(tmp_gc) <- tmp_name
    tmp_gc
})


plot_specs <- reactiveValues()
plot_specs$info <- NULL
plot_specs$color <- NULL
plot_specs$color_legend <- NULL

observe({
    if(!is.null(input$coloring_type)) {
        if(input$coloring_type == "group"){
            plot_specs$info <- r_data$group
            plot_specs$color <- group_color()
            plot_specs$legend_color <- group_color_legend()
        } else if (input$coloring_type == "batch") {
            plot_specs$info <- r_data$batch
            plot_specs$color <- batch_color()
            plot_specs$legend_color <- batch_color_legend()
        } else if (input$coloring_type == "community") {
            plot_specs$info <- r_data$community
            plot_specs$color <- community_color()
            plot_specs$legend_color <- community_color_legend()
        } else if(input$coloring_type == "none") {
            plot_specs$info <- NULL
        }
    }
})
