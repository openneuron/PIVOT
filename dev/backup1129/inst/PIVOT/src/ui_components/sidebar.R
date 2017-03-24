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


################################### Sidebar module ######################################

sidebar <- dashboardSidebar(
    # Some personalized css
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(
        tags$style(HTML(".sidebar {
                        height: 1200px; overflow-y: auto;
                        }
                        
                        .content {
                        padding-bottom: 50px; overflow-y: auto;
                        }
                        
                        "
        ) # close HTML
        )            # close tags$style
        ),    
    hr(),
    ##################### Menu Module ###################
    sidebarMenu(id="tabs",
                menuItem("Data", icon=icon("file"), 
                         menuSubItem("Data Input", tabName="data", icon = icon("angle-right"), selected=TRUE),
                         menuSubItem("Data Management", tabName="data_man", icon = icon("angle-right"))
                ),
                
                menuItem("Basic Statistics",  icon = icon("cube"),
                         menuSubItem("Data Table", tabName="table", icon = icon("angle-right")),
                         menuSubItem("Data Distribution", tabName = "data_distribution", icon = icon("angle-right")),
                         menuSubItem("Data Dispersion", tabName = "data_dispersion", icon = icon("angle-right")),
                         menuSubItem("ERCC", tabName = "ercc", icon =  icon("angle-right"))
                ),
                menuItem("Differential Expression", icon = icon("eyedropper"),
                         menuSubItem("SCDE", tabName = "scde", icon = icon("angle-right")),
                         menuSubItem("DESeq", tabName = "deseq", icon = icon("angle-right")),
                         menuSubItem("Mann–Whitney U test", tabName = "mww", icon = icon("angle-right"))
                ),
                menuItem("Monocle", icon = icon("umbrella"),
                         menuSubItem("Monocle Cell Dataset", tabName = "monocle_init", icon = icon("angle-right")),
                         menuSubItem("Monocle DE Analysis", tabName = "monocle_de", icon = icon("angle-right")),
                         menuSubItem("Cell State Ordering ", tabName = "monocle_state", icon = icon("angle-right"))
                ),
                menuItem("Clustering",  icon = icon("share-alt"),
                         menuSubItem("Hierachical", tabName = "hclust", icon = icon("angle-right")),
                         menuSubItem("K-means", tabName = "kmeans", icon = icon("angle-right")),
                         menuSubItem("Community Detection", tabName = "community", icon = icon("angle-right"))
                ),
                menuItem("Correlation",  icon=icon("line-chart"),
                         menuSubItem("Pairwise Scatterplot", tabName = "pairwise",icon = icon("angle-right")),
                         menuSubItem("Sample Correlation Heatmap", tabName = "correlation_hm",icon = icon("angle-right")),
                         menuSubItem("Feature Correlation Heatmap", tabName = "cor_ft",icon = icon("angle-right"))
                ),
                menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
                menuItem("Dimension Reduction", icon = icon("yelp"),
                         menuSubItem("PCA", tabName = "pca", icon = icon("angle-right")),
                         menuSubItem("t-SNE", tabName = "tsne", icon = icon("angle-right")),
                         menuSubItem("penalizedLDA", tabName = "plda", icon = icon("angle-right"))
                ),
                menuItem("Network Analysis", icon = icon("connectdevelop"), tabName = "transdiff"),
                
                menuItem("Classification", tabName = "caret", icon = icon("delicious")),
                
                menuItem("Toolkit", icon = icon("wrench"),
                         menuSubItem("Venn Diagram", tabName = "venn", icon = icon("angle-right"))
                ),
                
                menuItem("Report", tabName = "report", icon = icon("file-pdf-o")),
                menuItem("User Manual", tabName = "manual_file", icon=icon("mortar-board")
                ),
                menuSubItem("About", tabName = "about", icon = icon("paw"))
    ),
    hr(),
    
    ##################### Option Module ##################

    
    conditionalPanel("input.tabs != 'data' &&  input.tabs != 'ercc' && 
                     input.tabs != 'manual_file' && input.tabs != 'manual_filter_subset' && input.tabs != 'manual_analyzer' 
                     && input.tabs != 'about' && input.tabs != 'table' && input.tabs != 'deseq' && input.tabs != 'scde' 
                     && input.tabs != 'mww' && input.tabs!= 'stats' && input.tabs != 'transdiff' && input.tabs != 'network'
                     && input.tabs != 'venn'", # Tried %in% but it didn't seem to work
                     column(1),
                     column(10,
                            sidebarwell2(
                                selectInput("scale", label = tags$p("Data scale"), choices = list("Counts (normalized)" = "normalized_cnts", "Log10" = "log10", "Standardized" = "standardized", "Log10 & Standardized" = "log10_standardized"), selected = "log10")
                            )
                     )
    ),
    
    
    conditionalPanel("input.tabs == 'tsne' || input.tabs == 'caret' || input.tabs == 'kmeans' || input.tabs == 'scde'",
                    column(1),
                    column(10,
                           sidebarwell2(
                                numericInput("idv_seed", label = "Set seed", value = 1, min = 1, max = 5000, step = 1) 
                           )
                    )
    ),
    
    conditionalPanel("input.tabs == 'heatmap' || input.tabs == 'correlation_hm' || input.tabs == 'pca' || input.tabs == 'tsne' || input.tabs == 'plda' || input.tabs == 'hclust' || input.tabs == 'community'",
                     column(1),
                     column(10,
                            uiOutput("sample_coloring_ui")
                     )
    ),
    
    conditionalPanel("input.tabs == 'hclust'",
                     column(1),
                     column(10,
                            sidebarwell2(
                                selectInput("hclust_package", "Plotting package", choices = list("Dendexdend" = "dendextend", "networkD3" = "networkD3"), selected = "dendextend")
                            )
                     )
    ),
    
    conditionalPanel("input.tabs == 'cor_ft'",
                     column(1),
                     column(10,
                            sidebarwell2(
                                selectInput("cor_ft_tool", label = "Plotting Package", choices = list("heatmap.2" = 1, "d3heatmap" = 2), selected = 1)
                            )
                     )
    ),
    
    conditionalPanel("input.tabs == 'heatmap'",
                     column(1),
                     column(10,
                            sidebarwell2(
                                selectInput("hm_feature_tool", label = "Plotting Package", choices = list("heatmap.2" = 1, "d3heatmap" = 2), selected = 1)
                            )
                     )
    ),

    conditionalPanel("input.tabs == 'ercc'",
                     column(1),
                     column(10,
                            sidebarwell2(
                                sidebarwell2(textOutput("ercc_sb_text")),
                                sidebarwell2(
                                    numericInput("ercc_added", label = "Amount of ERCC added (µL)", value = 0.9, min = 0, max = 100, step = .1),
                                    numericInput("ercc_ratio", label = "ERCC dilution, 1 : ", value = 4000000, min = 0, max = 10e9, step = 100)
                                ),
                                actionButton("recompute_ercc", label = "Recompute", class = "sidebar-button")
                            )
                    )
    ),
    

    conditionalPanel("input.tabs == 'correlation_hm'",
                     column(1),
                     column(10,
                            sidebarwell2(
                                selectInput("cor_hm_tool", label = "Plotting Package", choices = list("heatmap.2" = 1, "d3heatmap" = 2), selected = 1)
                            )
                     )
    ),

    
    conditionalPanel("input.tabs == 'caret'",
                     column(1),
                     column(10,
                            sidebarwell2(
                                numericInput("caret_cv_fold", label = "Cross-validation fold", value = 6, min = 2, max = 20, step = 1),
                                numericInput("caret_cv_repeat", label = "Cross-validation repeat", value = 6, min = 2, max = 20, step = 1)
                            )
                     )
    ),
    
    conditionalPanel("input.tabs == 'heatmap' || input.tabs == 'correlation_hm' || input.tabs == 'cor_ft'",
                     column(1),
                     column(10,
                            sidebarwell2(
                                actionButton("hm_col_switcher", label = "Change heatmap color", class = "sidebar-button")
                            )
                     )
    ),
    

    br(),
    br()
    #column(1),
    #column(10,
    #       radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE),
    #       downloadButton('genreport', 'featurerate')
    #)
        )
