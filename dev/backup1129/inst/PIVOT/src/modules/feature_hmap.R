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


######## Some control ui #########

output$hm_ui <- renderUI({
    list(
        enhanced_box(
            width = 12,
            title = "Feature Heatmap",
            id = "feature_heatmap",
            status = "warning",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            get_pdf = T,
            register_analysis = T,
            
            wellPanel(
                div(tags$p("Choose features to plot:"), class = "control-head"),
                fluidRow(
                    column(4, selectInput("hm_rank_method", label = "Rank features by", choices = list("Fano Factor" = "fano_factor", "Variance" = "variance","Average Expression" = "expression", "Row order" = "row"), selected = "fano_factor")),
                    column(8, uiOutput("feature_range_ui"))
                ),
                fluidRow(
                    column(3,tags$b("Manually input a range:")),
                    column(3, uiOutput("min_rank_ui")),
                    column(3, uiOutput("max_rank_ui")),
                    column(3, actionButton("update_hm_range", "Update Range", class = "btn btn-info"))
                )
            ),
            
            wellPanel(
                fluidRow(
                    column(4, div(tags$p("Heatmap Ordering:"), class = "control-head")),
                    column(4, selectInput("hm_sp_order_type", NULL, choices = list("Order sample by hierarchical cluster" = "hc", "Do not cluster sample" = "io"), selected = "hc")),
                    column(4, selectInput("hm_ft_order_type", NULL, choices = list("Order feature by hierarchical cluster" = "hc", "Do not cluster feature" = "io"), selected = "hc"))
                ),
                uiOutput("hm_sp_hc_control"),
                uiOutput("hm_ft_hc_control")
            ),
            
            conditionalPanel(
                "input.hm_feature_tool == 2",
                d3heatmap::d3heatmapOutput("hmap_d3", height = "800px")
            ),
            conditionalPanel(
                "input.hm_feature_tool == 1",
                plotOutput("hmap_2", height = "800px")
            ),
            
            wellPanel(
                uiOutput("hmap_value_p"),
                uiOutput("hmap_order_p")
            ),
            downloadButton('download_featurevar', 'Download Ranked Table',class = "btn btn-success")
        )
    )
})

output$hm_sp_hc_control <- renderUI({
    if(input$hm_sp_order_type == "io") return()
    fluidRow(
        column(4, selectInput("hm_sp_dist_method", "Sample distance", choices = list("Euclidean" = "euclidean", "Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary", "Minkowski" = "minkowski", "Correlation Distance" = "corr"), selected = "euclidean")),
        uiOutput("hm_sp_corr_ui"),
        column(4, selectInput("hm_sp_agglo_method", "Sample agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
    )
})

output$hm_ft_hc_control <- renderUI({
    if(input$hm_ft_order_type == "io") return()
    fluidRow(
        column(4, selectInput("hm_ft_dist_method", "Feature distance", choices = list("Euclidean" = "euclidean", "Manhattan" = "manhattan", "Correlation Distance (1-r)" = "corr1", "Absolute Correlation (1-|r|)" = "corr2"), selected = "euclidean")), 
        uiOutput("hm_ft_corr_ui"),
        column(4, selectInput("hm_ft_agglo_method", "Feature agglomeration method", choices = list("Ward.D" = "ward.D", "Ward.D2" = "ward.D2","Single"= "single", "Complete"="complete", "Average"= "average", "Mcquitty"="mcquitty", "Median"= "median", "Centroid" = "centroid"), selected = "complete"))
    )
})


output$hm_sp_corr_ui <- renderUI({
    if (input$hm_sp_dist_method != 'corr') return()
    column(4,selectInput("hm_sp_cor_method", label = "Sample correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"))
})

output$hm_ft_corr_ui <- renderUI({
    if (!(input$hm_ft_dist_method %in% c('corr1','corr2'))) return()
    column(4,selectInput("hm_ft_cor_method", label = "Feature correlation method", choices = list("Pearson" = "pearson","Spearman" = "spearman", "Kendall" = "kendall"), selected = "pearson"))
})


# Heatmap scale

output$hmap_value_p <- renderUI({
    if(is.null(input$scale)) return()
    tags$li(paste("The values shown in this heatmap are normalized counts of", input$scale, "scale."))
})

output$hmap_order_p <- renderUI({
    if(is.null(input$hm_rank_method) || is.null(input$feature_range)) return()

    tags$li(paste0("The features shown in this heatmap have ranks range from ", input$feature_range[1], " to ", input$feature_range[2], " based on ranking by ", input$hm_rank_method, "."))

})

# range choice

# slider
output$feature_range_ui <- renderUI({
    if(is.null(data0())) return()
    feature_num = nrow(data0())
    if (feature_num > 500) {
        max_bound = 500
    } else {
        max_bound = feature_num
    }

    sliderInput("feature_range", label = "Rank Range", min = 1, max = max_bound, value = c(1, max_bound), step = 1)
})

observeEvent(input$update_hm_range, {
    feature_num = nrow(data0())

    if(input$min_rank <= feature_num & input$max_rank <= feature_num) {
        if(input$max_rank - input$min_rank + 1 < 2) {
            session$sendCustomMessage(type = "showalert", "Max must be larger than min by at least 1!")
            return()
        }
        if(input$max_rank - input$min_rank + 1 > 1000) {
            session$sendCustomMessage(type = "showalert", "You specified more than 1000 features, please choose less.")
            return()
        }
        updateSliderInput(session, "feature_range",
                          label = "Rank Range",
                          min = input$min_rank, max = input$max_rank, value = c(input$min_rank, input$max_rank), step = 1)
    } else {
        session$sendCustomMessage(type = "showalert", "You don't have that many features in the data set.")
        return()
    }

})

# direct input
output$min_rank_ui <- renderUI({
    if(is.null(data0())) return()
    feature_num = nrow(data0())
    
    if (!is.null(input$feature_range)) {
        cur_val = input$feature_range[1]
    } else {
        cur_val = 1
    }
    
    numericInput_1("min_rank", label = "Min:", value = cur_val, min = 1, max = feature_num, step = 1)
})

output$max_rank_ui <- renderUI({
    if(is.null(data0())) return()
    feature_num = nrow(data0())
    
    if (!is.null(input$feature_range)) {
        cur_val = input$feature_range[2]
    } else {
        cur_val = feature_num
    }
    
    numericInput_1("max_rank", label = "Max:", value = cur_val, min = 2, max = feature_num, step = 1)
})

###### Actual heatmap plotting module ######

# rank by variance
data_by_var <- reactive({
    data.tmp <- data0() %>%
        dplyr::mutate(var = apply(data0(),1,var)) %>% # Compute variance of feature across sample
        dplyr::mutate(feature = rownames(data0())) # save feature name, this will go along with table modification
    # Sort data by variance    
    data.tmp <-data.tmp[order(data.tmp$var, decreasing = T),] 
    rownames(data.tmp) <- data.tmp$feature
    data.tmp # clean up
})

# rank by average expression
data_by_exp <- reactive({
    data.tmp <- data0() %>% 
        dplyr::mutate(avg_exp = rowSums(data0())/ncol(data0())) %>%
        dplyr::mutate(feature = rownames(data0())) # save feature name, this will go along with table modification
    # Sort data by expression level
    data.tmp <-data.tmp[order(data.tmp$avg_exp, decreasing = T),] 
    rownames(data.tmp) <- data.tmp$feature
    data.tmp # clean up
})

# rank by fano factor
data_by_fano <- reactive({
    data.tmp <- data0() %>% 
        dplyr::mutate(avg_exp = rowSums(data0())/ncol(data0())) %>%
        dplyr::mutate(var = apply(data0(),1,var)) %>% # Compute variance of feature across sample
        dplyr::mutate(fano = var/avg_exp) %>%
        dplyr::mutate(feature = rownames(data0())) # save feature name, this will go along with table modification
    # Sort data by fano factor
    data.tmp <-data.tmp[order(data.tmp$fano, decreasing = T),] 
    rownames(data.tmp) <- data.tmp$feature
    data.tmp 
})

hm_data <- reactive({
    if(is.null(data0()) || is.null(input$feature_range) || is.null(input$hm_rank_method)) return()
    if(input$feature_range[2] > nrow(data0())) {
        return(NULL)
    }
    if(input$hm_rank_method == "fano_factor") {
        tmp<-data_by_fano() %>% dplyr::select(-fano, -avg_exp, -var, -feature)
        tmp[input$feature_range[1]:input$feature_range[2],]
    } else if(input$hm_rank_method == "variance"){
        tmp<-data_by_var() %>% dplyr::select(-var, -feature)
        tmp[input$feature_range[1]:input$feature_range[2],]
    }
    else if(input$hm_rank_method == "expression"){
        tmp <- data_by_exp() %>% dplyr::select(-avg_exp, -feature)
        tmp[input$feature_range[1]:input$feature_range[2],]
    } else {
        data0()[input$feature_range[1]:input$feature_range[2],]
    }
})

# sample clustering result
hm_sp_dend <- reactive({
    if(input$hm_sp_order_type == 'hc') {
        if(input$hm_sp_dist_method == 'corr') {
            as.dist(1 - cor(hm_data(), method = input$hm_sp_cor_method)) %>% hclust(method = input$hm_sp_agglo_method) %>% as.dendrogram()
        } else {
            t(hm_data()) %>% dist(method = input$hm_sp_dist_method) %>% hclust(method = input$hm_sp_agglo_method) %>% as.dendrogram()
        }
    } else {
        return(FALSE)
    }
    
})

hm_ft_dend <- reactive({
    if(input$hm_ft_order_type == 'hc') {
        if(input$hm_ft_dist_method == 'corr1') {
            as.dist(1 - cor(t(hm_data()), method = input$hm_ft_cor_method)) %>% hclust(method = input$hm_ft_agglo_method) %>% as.dendrogram()
        } else if(input$hm_ft_dist_method == 'corr2') {
            as.dist(1 - abs(cor(t(hm_data()), method = input$hm_ft_cor_method))) %>% hclust(method = input$hm_ft_agglo_method) %>% as.dendrogram()
        } else {
            hm_data() %>% dist(method = input$hm_ft_dist_method) %>% hclust(method = input$hm_ft_agglo_method) %>% as.dendrogram()
        }
    } else {
        return(FALSE)
    }
    
})


output$hmap_d3 <- d3heatmap::renderD3heatmap({
    if(is.null(hm_data()) || is.null(hm_sp_dend()) || is.null(hm_ft_dend())) return ()
    d3heatmap::d3heatmap(as.matrix(hm_data()), scale="none", Rowv = hm_ft_dend(), Colv = hm_sp_dend(), dendrogram="both", colors=hmcol$val, revC=F, margins=c(8,6))
})

output$hmap_2 <- renderPlot({
    if(is.null(hm_data()) || is.null(hm_sp_dend()) || is.null(hm_ft_dend())) return ()
    if(input$hm_sp_order_type == 'hc' && input$hm_ft_order_type == 'hc') {
        dend1 <- 'both'
    } else if(input$hm_sp_order_type == 'hc' && input$hm_ft_order_type != 'hc') {
        dend1 <- 'column'
    } else if(input$hm_sp_order_type != 'hc' && input$hm_ft_order_type == 'hc') {
        dend1 <- 'row'
    } else {
        dend1 <- 'none'
    }
    if(!is.null(plot_specs$info)) {
            gplots::heatmap.2(as.matrix(hm_data()), scale="none", Rowv = hm_ft_dend(), Colv = hm_sp_dend(), dendrogram=dend1, trace="none", col=hmcol$val, key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=.7, density.info="density", revC=T, cexCol = 1.2, margins=c(10,15),
                      ColSideColors = plot_specs$color)
            legend("topright", legend = unique(plot_specs$info), bty="n", fill = plot_specs$legend_color, text.col = plot_specs$legend_color, border=FALSE, y.intersp = 1.2, cex = 0.9)
    }
    else
        gplots::heatmap.2(as.matrix(hm_data()), scale="none", Rowv = hm_ft_dend(), Colv = hm_sp_dend(), dendrogram=dend1, trace="none", col=hmcol$val, key.par=list(cex.axis=0.7), key.title=NA, key.xlab="  ", key.ylab=NA, keysize=.7, density.info="density", revC=T, cexCol = 1.2, margins=c(8,6))
})

output$download_featurevar <- downloadHandler(
    filename = function(){
        if(input$hm_rank_method == "fano_factor")
            paste0("features_ranked_by_fano_factor(",input$scale,").csv")
        else if(input$hm_rank_method == "variance")
            paste0("features_ranked_by_variance(",input$scale,").csv")
        else
            paste0("features_ranked_by_average_expression(",input$scale,").csv")
    },
    content = function(file) {
        if(input$hm_rank_method == "fano_factor")
            write.csv(data_by_fano() %>% dplyr::select(feature, fano), file, row.names = FALSE)
        else if(input$hm_rank_method == "variance")
            write.csv(data_by_var() %>% dplyr::select(feature, var), file, row.names = FALSE)
        else
            write.csv(data_by_exp() %>% dplyr::select(feature, avg_exp), file, row.names = FALSE)
    }
)

