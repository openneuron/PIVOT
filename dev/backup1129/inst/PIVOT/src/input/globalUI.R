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


# The image indicating file is submitted
output$data_submitted_img <- renderUI({
    if(is.null(r_data$raw)) return()
    img(src = "button_ok.png", width = 35, height = 35)
})

output$input_submit_btn_ui <- renderUI({
    switch(input$file_format,
           dir = actionButton(inputId = "submit_dir", label = "Submit", class = "btn btn-primary"),
           single = actionButton(inputId = "submit_single", label = "Submit", class = "btn btn-primary"),
           customtable = actionButton(inputId = "submit_cs", label = "Submit", class = "btn btn-primary")
    ) 
})

output$input_threshold_ui <- renderUI({
    switch(input$input_threshold_type,
           mean = numericInput("min_cnt_avg", label = "Row Mean Threshold", value = 0, min = 0, max = 5000, step = 1),
           sum = numericInput("min_cnt_sum", label = "Row Sum Threshold", value = 0, min = 0, max = 5000, step = 1)
    ) 
})

output$data_pv_ui <- renderUI({
    ########## Show data preview #########
    if(input$input_tabset %in% c("file_in")) {
        if(!is.null(r_data$glb.raw)){
            box(width = NULL,
                title = "Dataset for Analysis",
                status = "success",
                solidHeader = T,
                fluidRow(
                    column(12,
                           DT::dataTableOutput("data_inprocess")
                    )
                )
            )
        } else if(input$file_format == 'single' && is.null(r_data$glb.raw)) {
            ########## Show loaded cnt module (single file input) #########
            box(width = NULL,
                title = "Loaded File Preview",
                status = "warning",
                solidHeader = T,
                fluidRow(
                    column(12,
                           DT::dataTableOutput("cnt_tbl_original")
                    )
                )
            )
        }
    } else if(input$input_tabset == "feature_in") {
        box(width = NULL,
            title = "Feature Statistics",
            status = "info",
            solidHeader = T,
            fluidRow(
                column(12,
                       DT::dataTableOutput("input_feature_stats_tbl"),
                       downloadButton("download_feature_stats_tbl", "Download", class = "btn btn-success")
                )
            )
        )
    } else if(input$input_tabset %in% c("sample_in","group_in")) {
        box(width = NULL,
            title = "Sample Statistics",
            status = "primary",
            solidHeader = T,
            fluidRow(
                column(12,
                       DT::dataTableOutput("input_sample_stats_tbl"),
                       downloadButton("download_sample_stats_tbl", "Download", class = "btn btn-success")
                )
            )
        )
    }
})


output$download_feature_stats_tbl <- downloadHandler(
    filename = "feature_stats_tbl.csv",
    content = function(file) {
        ftbl <-r_data$feature_meta
        ftbl <- ftbl[, -which(names(ftbl) %in% c("use_for_ordering", "cap_name", "STRING_id"))]
        write.csv(ftbl, file)
    }
)

# A copy of the above table to be put in feature filter tab
output$input_feature_stats_tbl <- DT::renderDataTable({
    if(is.null(r_data$feature_meta)) return()
    ftbl <-r_data$feature_meta
    ftbl <- ftbl[, -which(names(ftbl) %in% c("use_for_ordering", "cap_name", "STRING_id"))]
    DT::datatable(ftbl, selection = 'single', rownames = FALSE, options = list(
        scrollX = T, scrollY = "500px", lengthMenu = c(20, 50, 100)
    )
    )
})

# tab switch control
observeEvent(input$report_save_sc, {
    updateTabItems(session, "tabs", "report")
})

observeEvent(input$data_load_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "file_in")
})

observeEvent(input$group_load_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "group_in")
})

observeEvent(input$feature_filter_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "feature_in")
})

observeEvent(input$data_subset_sc, {
    updateTabItems(session, "tabs", "data")
    updateTabsetPanel(session, "input_tabset", "sample_in")
})

observeEvent(input$contact_author, {
    updateTabItems(session, "tabs", "about")
})

observeEvent(input$manual_sc, {
    updateTabItems(session, "tabs", "manual_file")
})

observeEvent(input$version_sc, {
    updateTabItems(session, "tabs", "about")
})







