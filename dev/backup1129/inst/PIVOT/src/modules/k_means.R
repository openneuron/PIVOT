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


output$km_boxes <- renderUI({
    list(
        enhanced_box(
            width = 12,
            title = "K-means Clustering",
            id = "kmeans_clust",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,

            wellPanel(
                fluidRow(
                    column(6, uiOutput("km_centers_ui")),
                    shinyBS::tipify(column(6, sliderInput("km_nstart", label = tags$p("nstart"), min = 1, max = 500, value = 10, step = 1)), title = "The number of initial random sets to be chosen.", placement = "bottom", options = list(container = "body"))
                )
            ),
            
            DT::dataTableOutput("km_tbl"),
            hr(),
            div(tags$b("K-means Clustering Results"), class = "table-cap"),
            DT::dataTableOutput("km_assignment"),
            downloadButton("km_as_download", "Download", class = "btn btn-success")
        )
    )
})


# k-means
km1 <- reactive({
    if(is.null(input$km_centers) || is.null(input$km_nstart)) return()
    set.seed(input$idv_seed)
    kmeans(t(data0()), centers = input$km_centers, nstart = input$km_nstart)
})

output$km_centers_ui <- renderUI({
    if(is.null(r_data$group) || length(unique(r_data$group)) < 2)
        shinyBS::tipify(sliderInput("km_centers", label = tags$p("number of clusters (k)"), min = 1, max = length(r_data$sample_name) - 1, value = 1, step = 1), title = "A random set of (distinct) samples is chosen as the initial k centres", placement = "bottom", options = list(container = "body"))
    else    
        shinyBS::tipify(sliderInput("km_centers", label = tags$p("number of clusters"), min = 1, max = length(r_data$sample_name) - 1, value = length(unique(r_data$group)), step = 1), title = "A random set of (distinct) rows is chosen as the initial k centres", placement = "bottom", options = list(container = "body"))
})

output$km_tbl <- DT::renderDataTable({
    if(is.null(km1())) return()
    if(is.null(r_data$group) || length(unique(r_data$group)) < 2)
    {
        tbl <- as.data.frame(table(km1()$cluster))
        colnames(tbl) <- c("Group", "Number of samples assigned")
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    } else {
        sample_gp <- r_data$group
        names(sample_gp) <- r_data$sample_name
        tbl <- as.data.frame.matrix(table(km1()$cluster, sample_gp))
        colnam <- names(tbl)
        names(tbl) <- sapply(colnam, function(x) paste("Is", x))
        rownam <- rownames(tbl)
        rownames(tbl) <- sapply(rownam, function(x) paste("Allocated to cluster", x))
        DT::datatable(tbl, options = list(paging = FALSE, searching = FALSE))
    }
})

km_assign_tbl <- reactive({
    if(is.null(km1())) return()
    if(is.null(r_data$group) || length(r_data$group) == 0)
    {
        tbl <- data.frame(km1()$cluster)
        colnames(tbl) <- c("assigned_cluster")
    } else {
        actual_group <- r_data$group
        names(actual_group) <- r_data$sample_name
        tbl <- data.frame(actual_group)
        tbl$assigned_cluster <- km1()$cluster
    }
    tbl
})

output$km_assignment <- DT::renderDataTable({
    DT::datatable(km_assign_tbl(), options = list(scrollX = TRUE, scrollY = "400px", lengthMenu = c(20, 50, 100)))
})

output$km_as_download <- downloadHandler(
    filename = function() { 
        "Kmeans_assigment.csv"
    },
    content = function(file) {
        write.csv(km_assign_tbl(), file)
    }
)

