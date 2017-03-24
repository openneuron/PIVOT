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


################################ Data table module #################################

output$table_box_ui <- renderUI({
    list(
        enhanced_box(
            title = "Data Table",
            id = "data_table",
            status = "primary",
            width = NULL,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            
            wellPanel(
                fluidRow(
                    column(6, 
                           selectInput("tbl_scale", label = tags$p("Data scale in the table"), choices = list("Counts (raw)" = "raw_cnts", "Counts (normalized)" = "normalized_cnts", "Relative Frequency" = "relative_frequency",  "Log10" = "log10_cnts", "Standardized" = "standardized_cnts", "Log10 & Standardized" = "log10_standardized_cnts"), selected = "normalized_cnts")  
                    ),
                    column(6, 
                           selectInput("tbl_order", label = tags$p("Order features by"), choices = list("alphabet" = "alphabet", "variance" = "variance", "fano factor" = "fano_factor", "row average" = "row_average", "row median" = "row_median"), selected = "none")  
                    )
                )
            ),
            fluidRow(
                column(12,
                       DT::dataTableOutput("data_tbl")
                )
            ),
            downloadButton('downloadData', 'Download', class = "btn btn-success")
        ),
        
        enhanced_box(
            title = "Expression Plot of Selected Gene",
            id = "data_table_gene_plot",
            status = "warning",
            width = NULL,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            get_pdf = T,
            register_analysis= T,
            fluidRow(
                column(6, uiOutput("feature_stats_options")),
                column(6, uiOutput("feature_box_add_ui"))
            ),
            plotOutput("feature_stats_plt")
        )
    )
    
})


generateRelativeCounts <- function(raw){
    as.data.frame(apply(raw,2,function(col) col/sum(col)))
}
# First set the scale
 
pre_data <- reactive({
    if(is.null(input$tbl_scale)) return()
    raw <- r_data$raw
    df <- r_data$df
    log <- log10(df + 1) # global DESeq_log10
    nm <- as.data.frame(t(scale(t(df)))) # global DESeq_normal
    log_nm <- as.data.frame(t(scale(t(log10(df + 1))))) # global DESeq_log10_normal
    
    if(input$ercc_isolation) {
        if(input$tbl_scale == "normalized_cnts")
            return(df[!(rownames(df) %in% ercc$features),])
        else if(input$tbl_scale == "log10_cnts")
            return(log[!(rownames(log) %in% ercc$features),])
        else if(input$tbl_scale == "standardized_cnts")
            return(nm[!(rownames(nm) %in% ercc$features),])
        else if (input$tbl_scale == "log10_standardized_cnts")
            return(log_nm[!(rownames(log_nm) %in% ercc$features),])
        else if (input$tbl_scale == "raw_cnts")
            return(raw[!(rownames(raw) %in% ercc$features),])
        else if (input$tbl_scale == "relative_frequency")
            return(generateRelativeCounts(raw[!(rownames(raw) %in% ercc$features),]))
    } else {
        if(input$tbl_scale == "normalized_cnts")
            return(df)
        else if(input$tbl_scale == "log10_cnts")
            return(log)
        else if(input$tbl_scale == "standardized_cnts")
            return(nm)
        else if(input$tbl_scale == "log10_standardized_cnts")
            return(log_nm)
        else if(input$tbl_scale == "raw_cnts")
            return(raw)
        else if(input$tbl_scale == "relative_frequency")
            return(generateRelativeCounts(raw))
    }
})

# Second order the table
data_for_tbl <- reactive({
    if(is.null(pre_data())) return()
    
    if(input$tbl_order == "alphabet") {
        tmp_tbl <- pre_data() %>% tibble::rownames_to_column("feature")
    } else if(input$tbl_order == "variance") {
        tmp_tbl <- pre_data() %>%
            tibble::rownames_to_column("feature") %>%
            dplyr::mutate(variance = apply(pre_data(),1,var)) %>% # Compute variance of feature across sample
            dplyr::arrange(desc(variance)) %>% 
            dplyr::select(-variance)
    } else if(input$tbl_order == "row_average") {
        tmp_tbl <- pre_data() %>%
            tibble::rownames_to_column("feature") %>%
            dplyr::mutate(average = apply(pre_data(),1,mean)) %>%
            dplyr::arrange(desc(average)) %>%
            dplyr::select(-average)
    } else if(input$tbl_order == "fano_factor"){
        tmp_tbl <- pre_data() %>%
            tibble::rownames_to_column("feature") %>%
            dplyr::mutate(average = apply(pre_data(),1,mean)) %>%
            dplyr::mutate(variance = apply(pre_data(),1,var)) %>% # Compute variance of feature across sample
            dplyr::mutate(fano = variance/average) %>%
            dplyr::arrange(desc(fano)) %>%
            dplyr::select(-average, -variance, -fano)
    } else if(input$tbl_order == "row_median") {
        tmp_tbl <- pre_data() %>%
            tibble::rownames_to_column("feature") %>%
            dplyr::mutate(median = apply(pre_data(),1,median)) %>%
            dplyr::arrange(desc(median)) %>%
            dplyr::select(-median)
    }

    rownames(tmp_tbl) <- tmp_tbl$feature
    tmp_tbl %>% dplyr::select(-feature)
})


# Normalized Data
output$data_tbl <- DT::renderDataTable({
    if(is.null(r_data$df)) return ()
    DT::datatable(data_for_tbl(), selection = 'single', options = list(scrollX = TRUE, scrollY = "600px", lengthMenu = c(20, 50, 100)))
})


output$downloadData <- downloadHandler(
    filename = function() { 
        paste0(input$tbl_scale, '.csv') 
    },
    content = function(file) {
        write.csv(data_for_tbl(), file)
    }
)

output$feature_stats_options <- renderUI({
    coloring_choices <- list("Sample" = "sample")
    if(!is.null(r_data$group))
        coloring_choices$"Group" <- "group"
    if(!is.null(r_data$batch))
        coloring_choices$"Batch" <- "batch"
    if(!is.null(r_data$community))
        coloring_choices$"Community" <- "community"
    
    if(length(coloring_choices)) {
        selectInput("stats_gene_plt_type", "Plot by", choices = coloring_choices) 
    }
})

output$feature_box_add_ui <- renderUI({
    if(!is.null(input$stats_gene_plt_type) && input$stats_gene_plt_type != "sample") {
        selectInput("stats_gene_plt_style", "Plot type", choices = list("Plot points" = "points", "Box plot" = "box", "Violin plot" = "violin"))
    } else {
        return()
    }
})

output$feature_stats_plt <- renderPlot({
    if(is.null(data_for_tbl())) return()
    s = input$data_tbl_row_last_clicked
    tbl<-as.data.frame(data_for_tbl())
    
    if (length(s)) {    
        selected_gene <- rownames(tbl[s, , drop = FALSE]) 
    } else {
        return()
    }
    d <- as.data.frame(t(tbl[selected_gene,])) %>% tibble::rownames_to_column()
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
    
    if(input$stats_gene_plt_type == "sample") {
        ggplot(d, aes(x=sample, y=expression_level)) + 
            geom_point(size = 3) + 
            scale_y_log10(breaks=c(25,100,400)) +
            theme(text = element_text(size=15)) + 
            ggtitle(paste("Expression level of gene", selected_gene, "across samples"))
    } else {
        feature_super_plot(d, selected_gene, plot_group = input$stats_gene_plt_type, style = input$stats_gene_plt_style, legend_pos = "right")
    }
    
})






