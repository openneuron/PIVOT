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



##################### Quick View Module ######################

output$input_file_box <- renderInfoBox({
    if(is.null(r_data$file_path) || is.null(r_data$input_type)) return(
        valueBox(value = "", "Input", icon = icon("credit-card"), color = "olive")
    ) else {
        if(r_data$input_type == "single"){
            valueBox(value = r_data$file_path$name, "Input file", icon = icon("credit-card"), color = "olive")
        } 
        else if(r_data$input_type == "dir") {
            valueBox(value = r_data$file_path[[length(r_data$file_path)]], "Input folder", icon = icon("credit-card"), color = "olive")
        }
    }
})

# show how many feature is left after filtratrion
output$feature_number_box <- renderValueBox({
    if(is.null(r_data$raw)) {
        valueBox(
            0, "selected features", icon = icon("align-justify"), color = "yellow"
        )
    } else {
        valueBox(
            nrow(r_data$raw), "selected features", icon = icon("align-justify"), color = "yellow"
        )
    }
})

output$feature_percent_box <- renderValueBox({
    valueBox(
        paste0(round(nrow(r_data$raw)/nrow(r_data$glb.raw), digits = 3) * 100, "%"), "of all expressed features", icon = icon("pie-chart"), color = "maroon"
    )
})

output$sample_number_box <- renderValueBox({
    if(is.null(r_data$raw)) {
        valueBox(
            0, "selected samples", icon = icon("cubes"), color = "green"
        )
    } else {
        valueBox(
            ncol(r_data$raw), "selected samples", icon = icon("cubes"), color = "green"
        )
    }
})

output$group_number_box <- renderValueBox({
    valueBox(
        length(unique(r_data$group)), "specified groups", icon = icon("group"), color = "teal"
    )
})

output$batch_number_box <- renderValueBox({
    valueBox(
        length(unique(r_data$batch)), "specified batches", icon = icon("th-large"), color = "aqua"
    )
})



# The feature info list in header
output$featureMenu <- renderMenu({
    if(is.null(r_data$raw)) {
        sample_num <- 0
    } else {
        sample_num <- ncol(r_data$raw)
    }
    dropdownMenu(type = "notifications",
                 top_msg = "Information about the data.",
                 notificationItem(
                     text = paste(nrow(r_data$raw), "selected features"),
                     icon("align-justify")
                 ),
                 notificationItem(
                     text = paste0(round(nrow(r_data$raw)/nrow(r_data$glb.raw), digits = 3) * 100, "% ", "of all expressed features"),
                     icon("pie-chart"),
                     status = "success"
                 ),
                 notificationItem(
                     text = paste0(sample_num, " selected samples"),
                     icon = icon("cubes"),
                     status = "warning"
                 ),
                 notificationItem(
                     text = paste0(length(unique(r_data$group)), " specified groups"),
                     icon = icon("group"),
                     status = "info"
                 ),
                 notificationItem(
                     text = paste0(length(unique(r_data$batch)), " specified batches"),
                     icon = icon("th-large"),
                     status = "info"
                 )
    )
})

