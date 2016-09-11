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


# this module handles the initiation of the Monocle dataset

output$monocle_init_ui <- renderUI({
    if(is.null(r_data$raw)) return()
    list(
        box(
            width = 12,
            title = "Initiate Monocle CellDataSet",
            status = "primary",
            solidHeader = T,
            fluidRow(
                column(4,  
                       selectInput("mn_family_fun", "Choose a distribution for your expression data",
                                   choices = list("Tobits" = "tobit", "Negbinomial" = "negbinomial", "Negbinomial.size" = "negbinomial.size", "Gaussianff" = "gaussianff"),
                                   selected = "negbinomial")
                ),
                column(8,
                       uiOutput("mn_data_dist_text_ui")
                )
            ),
            tags$br(),
            tags$b("Using the wrong expressionFamily for your data will lead to bad results"),
            tags$p("Note: PIVOT will supply monocle with your raw read count matrix. 
                   The normalization procedure monocle uses is different from DESeq. 
                   If your input dataset has already been normalized, or are relative expression values,
                   please first convert your data to transcript counts."),
            fluidRow(
                column(4, numericInput("mn_min_expr", "Minimum expression level (detection limit)",  min = 0.01, value = 0.1, step = 1)),
                column(4, numericInput("mn_min_cell", "Minimum cell number (>=2)",  min = 2, value = 2, step = 1)),
                column(4, tags$p("Please filter your data to remove genes with very low expression level.  
                                 Due to a bug in monocle 2.1, R could stuck in infinite loops if you have genes with expression in only 1 cell.")
                )
            ),
            
                actionButton("init_monocle", "Create Monocle CellDataSet", class = "btn btn-info"),
                tags$p(),
                uiOutput("monocle_ok")
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

observeEvent(input$init_monocle, {
    pd <- new("AnnotatedDataFrame", data = r_data$sample_meta)
    feature_meta <- r_data$feature_meta
    colnames(feature_meta)[1] <- "gene_short_name"
    fd <- new("AnnotatedDataFrame", data = feature_meta)
    cellset <- monocle::newCellDataSet(as.matrix(r_data$raw),
                                       phenoData = pd,
                                       featureData = fd,
                                       lowerDetectionLimit = input$mn_min_expr,
                                       expressionFamily=do.call(input$mn_family_fun, list()))
    expressed_genes <- row.names(subset(fData(cellset), num_cells_expressed >= input$mn_min_cell))
    cellset <- cellset[expressed_genes,]
    if(input$mn_family_fun %in% c("negbinomial", "negbinomial.size")) {
        cellset <- estimateSizeFactors(cellset)
        error_I <- 0
        tryCatch({
            cellset <- estimateDispersions(cellset)
        },
        error = function(e) {
            error_I <<- 1
        })
        if(error_I) {
            session$sendCustomMessage(type = "showalert", "Parametric dispersion fit failed, please set a different minimum expression level.")
            return()
        }
    } else {
        cellset <- estimateSizeFactors(cellset)
    }
    r_data$cellset <- cellset
    r_data$monocle_ok <- 1
    session$sendCustomMessage(type = "showalert", "Monocle CellDataSet created.")
})

output$monocle_ok <- renderUI({
    if(!is.null(r_data$cellset)){
        tags$li(img(src = "button_ok.png", width = 35, height = 35), tags$b("Monocle CellDataSet successfully initiated. You can now use other monocle modules."), style = "font-size:110%;")
    } else {
        return()
    }
})


