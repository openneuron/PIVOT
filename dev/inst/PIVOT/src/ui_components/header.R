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


dbHeader <- dashboardHeader(
    title = "PIVOT",
    dropdownMenu(type = "support",
                 top_msg = "PIVOT Support",
                 systemItem(
                     title = "Technical Support",
                     text = actionButton("bug_Sc","General qustions, bug reports.", class = "btn_no_border"),
                     icon = icon("wrench"),
                     href = "mailto:qinzhu@outlook.com"
                 ),
                 systemItem(
                     title = "PIVOT Scriptors",
                     text =  actionButton("contact_author", "Contact script authors of PIVOT.",  class = "btn_no_border"),
                     icon = icon("group")
                 ),
                 
                 systemItem(
                     title = "PIVOT Manual",
                     text = actionButton("manual_sc", "Check out manuals of PIVOT.",  class = "btn_no_border"),
                     icon = icon("graduation-cap")
                 ),
                 systemItem(
                     title = "Version Info",
                     text = actionButton("version_sc","PIVOT, version: 1.10",  class = "btn_no_border"),
                     icon = icon("compass")
                 ),
                 systemItem(
                     title = "Kim Lab",
                     text = actionButton("lab_site_sc","Junhyong Kim Lab, University of Pennsylvania",  class = "btn_no_border"),
                     icon = icon("graduation-cap"),
                     href = "http://kim.bio.upenn.edu/"
                 )
    ),
    
    dropdownMenuOutput("featureMenu"),
    dropdownMenu(type = "system",
                 top_msg = "System Control",
                 tags$li(tags$span(
                     icon("save", class = "text-primary"), 
                     custom_downloadbtn("state_save_sc","Save State", class = "btn btn-default btn_no_border shiny-bound-input", style = "margin-left:8px"),
                     style ="padding:10px;border-bottom:1px solid #f4f4f4;background-color:transparent; display:block;"
                 )
                 ),
                 systemItem(
                     text = actionButton("report_save_sc","Save Report", class = "btn_no_border"),
                     icon = icon("pencil"),
                     status = "warning"
                 ),
                 systemItem(
                     text = actionButton("data_load_sc","Load Data", class = "btn_no_border"),
                     icon = icon("cloud-upload"),
                     status = "success"
                 ),
                 systemItem(
                     text = actionButton("group_load_sc","Group Info", class = "btn_no_border"),
                     icon = icon("group"),
                     status = "info"
                 ),
                 systemItem(
                     text = actionButton("feature_filter_sc","Filter Feature", class = "btn_no_border"),
                     icon = icon("filter"),
                     status = "primary"
                 ),
                 systemItem(
                     text = actionButton("data_subset_sc","Subset Data", class = "btn_no_border"),
                     icon = icon("columns"),
                     status = "primary"
                 ),
                 systemItem(
                     text = actionButton("session_clear_sc","Clear Session", class = "btn_no_border", onclick = "window.location.reload();"),
                     icon = icon("desktop"),
                     status = "danger"
                 ),
                 tags$li(tags$span(
                     icon("desktop", class = "text-primary"), 
                     tags$a(id = "new_session", href = "./", target = "_blank", "New Session", class = "btn btn-default btn_no_border shiny-bound-input", style = "margin-left:5px")
                 ), style ="padding:10px;border-bottom:1px solid #f4f4f4;background-color:transparent; display:block;"
                 ),
                 systemItem(
                     text = actionButton("exit_and_save","EXIT PIVOT", class = "btn_no_border", onclick = "setTimeout(function(){window.close();}, 100); "),
                     icon = icon("sign-out"),
                     status = "danger"
                 )
    )
)


