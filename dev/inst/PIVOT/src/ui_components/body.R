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

#################################### Body Module ########################################
body <- dashboardBody(
    tags$script('
                Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
                var el = $("#" + x);
                el.replaceWith(el = el.clone(true));
                var id = "#" + x + "_progress";     
                $(id).css("visibility", "hidden");
                });
                '),
    
    tabItems(
        tabItem(tabName = "data",
                fluidRow(
                    column(width = 8,
                           tabBox( width = NULL,
                                   id = "input_tabset",
                                   ##################### File input module ###################
                                   tabPanel(
                                       tags$b("File"),
                                       value = "file_in",
                                       wellPanel(
                                           fluidRow(
                                               column(6,
                                                      selectInput("file_format", label = "Input file type", choices = list("Counts Directory" = "dir", "Counts Table" = "single", "PIVOT State" = "state"),selected = "single")
                                               ),
                                               column(6, 
                                                      uiOutput("proc_method_ui")
                                               )
                                           ),
                                           uiOutput("deseq_threshold_ui"),
                                           uiOutput("norm_text_ui"),
                                           fluidRow(
                                               column(6,
                                                      uiOutput("gene_length_ui")
                                               ),
                                               column(6,
                                                      uiOutput("norm_details_ui")
                                               )
                                           )
                                       ),
                                       hr(),
                                       wellPanel(
                                           ##### count file input module #####
                                           conditionalPanel(
                                               "input.file_format == 'dir'",
                                               wellPanel(
                                                   fluidRow(
                                                       column(5, shinyFiles::shinyDirButton('data_folder', 'Select Data Folder', 'Please select a folder', FALSE, class = "btn-info")),
                                                       column(2, 
                                                              a(id = "directory_help_btn", icon("question-circle")),
                                                              shinyBS::bsModal(id = "directory_help", title = "Input tips", trigger = "directory_help_btn", size = "large", list(
                                                                  tags$li("Accepts folder containing HTSeq/featureCounts/VERSE output."),
                                                                  tags$li("Counts files must contain the same feature set (Same number of rows, same rownames, multiple species not allowed)."),
                                                                  tags$li("You can use file filter to select files containing certain keywords"),
                                                                  tags$li("Example count file:"),
                                                                  br(),
                                                                  img(src = "exp_count_file.png", width = 150)
                                                              ))      
                                                       ),
                                                       column(5, verbatimTextOutput("data_folder_show"))
                                                   ),
                                                   shinyBS::tipify(
                                                       textInput("file_search", label = "File filter", value = ""), 
                                                       title = "use keywords (e.g. exon.cnt) to include desired count files only", placement = "bottom", options = list(container = "body")
                                                   ),
                                                   uiOutput("select_data")
                                               )
                                           ),
                                           
                                           ##### Single file module #####
                                           conditionalPanel(
                                               "input.file_format == 'single'",
                                               wellPanel(
                                                   fluidRow(
                                                       column(8, fileInput('file_single', 'Choose counts file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                       column(4, 
                                                              a(id = "input_help_btn", icon("question-circle")),
                                                              shinyBS::bsModal(id = "input_help", title = "Input tips", trigger = "input_help_btn", size = "large", list(
                                                                  tags$li("PIVOT only accepts .csv or .txt input."),
                                                                  tags$li("Feature names or sample names may be slightly modified to be valid R row/column names.")),
                                                                  tags$li("Rows must be features and columns must be samples."),
                                                                  tags$li("Negative values are not allowed."),
                                                                  tags$li("Example:"),
                                                                  img(src = "input_exp.png", width = 500)
                                                              )
                                                       )
                                                   ),
                                                   checkboxInput('header_ct', 'Header', TRUE),
                                                   radioButtons('sep_ct', 'Separator', c(Comma=',', Semicolon=';', Tab='\t', Space = ' '), selected = ',', inline = TRUE),
                                                   fluidRow(
                                                       column(6, selectInput('row_ct', 'Row names', choices = list("automatic" = "automatic", "first column" = "firstcol", "numbers" = "numbers"), selected = "firstcol"))
                                                   ),
                                                   radioButtons('quote_ct', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"', inline = TRUE)
                                               )
                                           ),
                                           
                                           # Threshold
                                           conditionalPanel(condition = "input.file_format == 'dir' || input.file_format == 'single'",
                                                            wellPanel(
                                                                fluidRow(
                                                                    column(6, radioButtons("input_threshold_type", "Choose pre-filtering type:", choices = c("Row Mean" = "mean", "Row Sum" = "sum"), inline = T)),
                                                                    column(6,uiOutput("input_threshold_ui"))
                                                                )
                                                            ),
                                                            
                                                            # ERCC isolation
                                                            wellPanel(
                                                                fluidRow(
                                                                    column(12, shinyBS::tipify(checkboxInput("ercc_isolation", label = "Only analyze ERCC in ERCC module.", value = T), title = "ERCC will not appear in modules like heatmap, PCA, etc. This exclusion of ERCC happens after DESeq normalization.", placement = "bottom", options = list(container = "body")))
                                                                )
                                                            )
                                           ),
                                           
                                           # upload state
                                           conditionalPanel(condition = "input.file_format == 'state'",
                                                            fileInput('uploadState', 'Load PIVOT state:',  accept = ".rda"),
                                                            uiOutput("refreshOnUpload"),
                                                            tags$li("Only valid PIVOT .rda file can be accepted."),
                                                            tags$li("You can save state using the system panel at top right."),
                                                            tags$li("Session will immediately switch to the loaded state.")
                                           ),
                                           
                                           # upload non-counts table
                                           conditionalPanel(condition = "input.file_format == 'customtable'",
                                                            fileInput('upload_customtable', 'Load Table:',  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                                            tags$li("PIVOT supports data otherthan sequencing counts."),
                                                            tags$li("You can upload e.g. PCA/t-sne projection tables for secondary analysis."),
                                                            tags$li("Negative, non-interger values are allowed."),
                                                            br(),
                                                            wellPanel(
                                                                checkboxInput('header_cs', 'Header', TRUE),
                                                                radioButtons('sep_cs', 'Separator', c(Comma=',', Semicolon=';', Tab='\t', Space = ' '), selected = ',', inline = TRUE),
                                                                fluidRow(
                                                                    column(6, selectInput('row_cs', 'Row names', choices = list("automatic" = "automatic", "first column" = "firstcol", "numbers" = "numbers"), selected = "firstcol"))
                                                                ),
                                                                radioButtons('quote_cs', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"', inline = TRUE)
                                                            )
                                           )
                                           
                                       ),
                                       
                                       # Submit button 
                                       fluidRow(
                                           column(8),
                                           column(2,
                                                 uiOutput("input_submit_btn_ui")
                                            ),
                                           column(2,htmlOutput("data_submitted_img"))
                                       )
                                   ),
                                   
                                   
                                   ####################### Group info module #####################
                                   tabPanel(
                                       tags$b("Design Info"),
                                       value = "group_in",
                                       conditionalPanel(condition = "output.data_submitted_img",
                                            wellPanel(
                                                fluidRow(
                                                    column(8, selectInput("add_group_way", label = NULL, choices = list("Manually add design info" = 1, "Upload a design info file" = 2), selected = 1)),
                                                    column(4, 
                                                           a(id = "design_help_btn", icon("question-circle")),
                                                           
                                                           shinyBS::bsModal(id = "design_help", title = "About design info", trigger = "design_help_btn", size = "large", list(
                                                               tags$li("Specify group information (conditions used for differential expression analysis), or batch information (different experiments)."),
                                                               tags$li("Adding this information will not affect DESeq and other normalization results, or any analysis that does not involve statistical testing between conditions."),
                                                               tags$li("This information is required for differential expression analysis, classification and coloring plots by group/batch."))
                                                           )
                                                    )
                                                ),
                                                
                                                uiOutput("design_detail_ui")
                                            )
                                       ),
                                       conditionalPanel(condition = "!output.data_submitted_img",
                                            tags$p("Please submit your data first.")
                                       ),
                                       hr(),
                                       ##### Manual adding group module #####
                                       conditionalPanel(
                                           condition = "output.data_submitted_img&& input.add_group_way == 1",
                                           fluidRow(
                                               column(5,
                                                      uiOutput("manual_add_group_ui"),
                                                      uiOutput("manual_add_batch_ui")
                                               ),
                                               column(7, 
                                                      DT::dataTableOutput("group_table_show"),
                                                      fluidRow(
                                                          column(6,
                                                                 downloadButton('downloadGroup', 'Download', class = "btn btn-success")
                                                          ),
                                                          column(6,
                                                                 actionButton('clear_group_1', label = "Clear design", icon = icon("times"), class = "btn btn-danger")
                                                          )
                                                      ),
                                                      hr(),
                                                      fluidRow(
                                                          column(6, actionButton('submit_design_manual', label = "Submit Design", class = "btn btn-primary")),
                                                          column(6, uiOutput("grp_submitted_img1"))
                                                      )
                                                )        
                                           )
                                       ),
                                       
                                       ##### Upload group info module #####
                                       conditionalPanel(
                                           condition = "input.add_group_way == 2",
                                           fluidRow(
                                               column(5,
                                                      wellPanel(
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
                                                          checkboxInput('group_header', 'Header', TRUE),
                                                          radioButtons('group_sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), selected = ','),
                                                          radioButtons('group_quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), selected = '"')
                                                      )
                                               ),
                                               column(7,
                                                      DT::dataTableOutput("group_table_show2"),
                                                      br(),
                                                      actionButton('clear_group_2', label = "Clear design", icon = icon("times"), class = "btn btn-danger"),
                                                      hr(),
                                                      checkboxInput("sample_reorder", "Reorder samples according to the design table.", value = F),
                                                      fluidRow(
                                                          column(6, actionButton('submit_design_upload', label = "submit design", class = "btn btn-primary")),
                                                          column(6, uiOutput("grp_submitted_img2"))
                                                      )
                                               )
                                           )
                                       )
                                   ),
                                   
                                   #################### feature filtering module ###################
                                   tabPanel(
                                       tags$b("Feature Filter"),
                                       value = "feature_in",
                                       uiOutput("filter_ui")
                                   ),
                                   
                                   #################### Data subsetting module ###################
                                   tabPanel(
                                       tags$b("Data Subsetter"),
                                       value = "sample_in",
                                       uiOutput("subset_ui")
                                   )
                           )
                    ),
                    
                    column(width = 4,
                           uiOutput("data_pv_ui")
                    )
                ),
                
                fluidRow(
                    box(
                        width = 12,
                        title = "Quick View of Chosen Data",
                        status = "info",
                        solidHeader = T,
                        fluidRow(
                            valueBoxOutput_custom("input_file_box", width = 2, style = "padding-right:2px; padding-left: 10px;"),
                            valueBoxOutput_custom("feature_number_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("feature_percent_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("sample_number_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("group_number_box", width = 2, style = "padding-right:2px; padding-left: 2px;"),
                            valueBoxOutput_custom("batch_number_box", width = 2, style = "padding-right:10px; padding-left: 2px;")
                        )
                    )
                ),
                hr(),
                br()
        ),
        
        ########### Show Data Management Module #########
        tabItem(tabName = "data_man",
                uiOutput("data_man_ui"),
                hr(),
                br()
        ),
        
        
        ########### Show Data Table Module #########
        tabItem(tabName = "table",
                uiOutput("table_box_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "data_distribution",
                uiOutput("distribution_box_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "data_dispersion",
                uiOutput("dispersion_box_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "quality",
                uiOutput("quality_ui"),
                hr(),
                br()
        ),
        
        ################################# Analysis Output Module ##############################
        # Rank frequency
        tabItem(tabName = "rankfreq",
                uiOutput("rankfreq_box_ui"),
                hr(),
                br()
        ),
        
        # ERCC
        tabItem(tabName = "ercc",
                
                uiOutput("ercc_ui"),
                
                hr(),
                br()
        ),
        
        # Differential expression analysis
        # correlation scatterplot
        tabItem(tabName = "scde",
                uiOutput("scde_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "monocle_init",
                uiOutput("monocle_init_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "monocle_state",
                uiOutput("monocle_state_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "monocle_de",
                uiOutput("monocle_de_ui"),
                hr(),
                br()
        ),

        
        tabItem(tabName = "deseq",
                uiOutput("deseq_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "mww",
                uiOutput("mww_ui"),
                hr(),
                br()
        ),
        
        # hierarchical clustering
        tabItem(tabName = "hclust",
                uiOutput("hclust_ui"),
                hr(),
                br()
        ),
        
        # k-means
        tabItem(tabName = "kmeans",
                uiOutput("km_boxes"),
                hr(),
                br()
        ),
        
        # Community detection
        tabItem(tabName = "community",
                uiOutput("mst_ui")
        ),
        
        # correlation scatterplot
        tabItem(tabName = "pairwise",
                uiOutput("pairwise_box"),
                
                hr(),
                br()
        ),
        
        # Sample correlation heatmap
        tabItem(tabName = "correlation_hm",
                uiOutput("cor_hm_ui"),
                hr(),
                br()
        ),
        
        # Sample correlation heatmap
        tabItem(tabName = "cor_ft",
                uiOutput("cor_ft_ui"),
                hr(),
                br()
        ),
        
        # feature heatmap
        tabItem(tabName = "heatmap",
                uiOutput("hm_ui"),
                hr(),
                br()
        ),
        
        # PCA
        tabItem(tabName = "pca",
                uiOutput("pca_ui"),
                hr(),
                br()
        ),
        
        # T-SNE
        tabItem(tabName = "tsne",
                uiOutput("tsne_ui"),
                hr(),
                br()
        ),
        
        # penalizedLDA
        tabItem(tabName = "plda",
                uiOutput("plda_ui"),
                hr(),
                br()
        ),
        
        # Network visulization
        tabItem(tabName = "network",
                uiOutput("network_ui"),
                hr(),
                br()
        ),
        
        # Transdifferentiation Calculator
        tabItem(tabName = "transdiff",
                uiOutput("transdiff_ui"),
                hr(),
                br()
        ),


        # machine learning with caret
        tabItem(tabName = "caret",
                tags$h3("Training"),
                tags$p("This module requires you have less than 500 features in the current dataset. Some modeling method require additional packages to be installed. In such cases, you will have to select yes (1) or no (2) in the background R session. "),
                
                fluidRow(
                    column(4,
                           box(
                               width = NULL,
                               uiOutput("caret_train_gp_ui"),
                               uiOutput("caret_train_method_ui"),
                               actionButton('caret_train_gp_btn', label = "Train", class = "btn btn-info"),
                               hr(),
                               tags$b("Training Data"),
                               DT::dataTableOutput("caret_training_tbl")
                           )   
                    ),
                    column(4,
                           box(title = "Modeling Result",
                               width = NULL,
                               verbatimTextOutput("caret_model_result")
                           )   
                    ),
                    column(4,
                           box(title = "Feature Coefficient",
                               width = NULL,
                               DT::dataTableOutput("caret_model_coef")
                           )   
                    )
                ),
                
                fluidRow(
                    column(12,
                           hr(),
                           tags$h3("Testing")
                    )
                ),
                fluidRow(
                    column(4,
                           box(
                               width = NULL,
                               uiOutput("caret_test_gp_ui"),
                               actionButton('caret_test_gp_btn', label = "Test", class = "btn btn-info"),
                               hr(),
                               tags$b("Testing Data"),
                               DT::dataTableOutput("caret_test_tbl")
                           )   
                    ),
                    column(4,
                           box(title = "Test result (confusionMatrix)",
                               width = NULL,
                               verbatimTextOutput("caret_confusionmatrix")
                           )   
                    ),
                    column(4,
                           box(title = "Test result (assignment)",
                               width = NULL,
                               DT::dataTableOutput("caret_test_result_tbl"),
                               downloadButton("download_caret_test_result", label = "Download", class = "btn btn-success")
                           )
                    )
                ),
                box(
                    width = 12,
                    title = "Citation",
                    status = "primary",
                    tags$ol(
                        tags$li("Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, the R Core Team, Michael
  Benesty, Reynald Lescarbeau, Andrew Ziem, Luca Scrucca, Yuan Tang and Can Candan. (2015). caret: Classification and Regression Training. R package version 6.0-58.
                                http://CRAN.R-project.org/package=caret.", class = "citation")
                    )
                ),
                img(src="caret.png",height = 200)
        ),
        
        tabItem(tabName = "venn",
                uiOutput("venn_ui"),
                hr(),
                br()
        ),
        
        tabItem(tabName = "report",
                uiOutput("report_ui")

        ),
        
        
        tabItem(tabName = "manual_file",
                includeMarkdown("./manual/manual_file.Rmd")
        ),
        
        
        tabItem(tabName = "about",
                includeMarkdown("./manual/about.Rmd"),
                fluidRow(
                    HTML("
                               <div class = 'kimlab_footer'>
                                   <div class='kimlab_container'>
                                   <div class='kim_footer-3'>
                                   <a href='http://www.sas.upenn.edu'><img src='http://kim.bio.upenn.edu/wiki.media/images/penn-logo.png' class='kim_footer-logo'/></a><br/><br/>
                                   <p>&copy; 2015 J. Kim | All rights reserved</p>
                                   </div>
                                   <div class='kim_footer-3'>
                                   <address>
                                   <a href='http://www.bio.upenn.edu'><strong>Biology Department</strong></a><br/>
                                   <a href='http://www.upenn.edu'><strong>University of Pennsylvania</strong></a><br/>
                                   103I Lynch Laboratory<br/>
                                   433 S University Avenue<br/>
                                   Philadelphia, PA 19104 USA<br/>
                                   </address>
                                   </div>
                                   <div class='kim_footer-3'>
                                   <p>
                                   <strong>off:</strong> 
                                   <a href='tel:+12157465187'>(215) 746-5187</a><br/>
                                   <strong>lab:</strong> <a href='tel:+12158988395'>(215) 898-8395</a><br/>
                                   </p><br/>
                                   <p>
                                   <strong>email:</strong> 
                                   <a href='mailto:junhyong@sas.upenn.edu'>junhyong@sas.upenn.edu</a>
                                   </p>
                                   </div>
                                   </div>
                                   </div>"
                    )
                )
        )
    ),
    
    ################### JAVAScript ##################
    # Alert module
    singleton(tags$script(type="text/javascript", "
        $(document).ready(function() {
        Shiny.addCustomMessageHandler('showalert', function(message) {
          alert(message);
        });
        });
    ")),
    singleton(tags$script(type="text/javascript", "
        $(document).ready(function() {
                          Shiny.addCustomMessageHandler('closeModal', function(message) {
                                $('#modal').modal('hide');
                          });
                          });
                          ")),
    tags$script(
        HTML('
             Shiny.addCustomMessageHandler(
             type = "jsCode"
             ,function(message) {
             Shiny.onInputChange("deleteConfirmChoice",eval(message.value));
             })
             ')
    ),
    tags$head(
        tags$script(src = "js/session.js"),
        tags$script(src = "js/custom.js")
    )
)

