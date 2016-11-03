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


################################# This file is NOT USED #####################################
###### loading module #####
# This shows the data when it first loaded
cs_tbl_loaded <- reactive({
    inFile <- input$upload_customtable
    if (is.null(inFile))
        return(NULL)
    if(input$row_cs == "automatic")
        read.csv(inFile$datapath, header=input$header_cs, sep=input$sep_cs, quote=input$quote_cs)
    else if(input$row_cs == "firstcol")
        read.csv(inFile$datapath, header=input$header_cs, sep=input$sep_cs, quote=input$quote_cs, row.names = 1)
    else
        read.csv(inFile$datapath, header=input$header_cs, sep=input$sep_cs, quote=input$quote_cs, row.names = NULL)
})

output$cs_tbl_original <- DT::renderDataTable({
    if(is.null(input$upload_customtable)) return()
    validate(
      need(!is.null(cs_tbl_loaded()) && ncol(cnt_tbl_loaded()) > 1, "Please correct your input format. Try options on the left panel until you see your data here."),
      errorClass = "myErrorClass1"
    )
    DT::datatable(cs_tbl_loaded(), options = list(scrollX = TRUE, scrollY = "400px", lengthMenu = c(20, 50, 100)))
})

