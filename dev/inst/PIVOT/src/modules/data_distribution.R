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




# UI
output$distribution_box_ui <-renderUI({
    list(
        enhanced_box(
            title = "Data Distribution",
            id = "data_distribution",
            status = "primary",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            get_pdf = T,
            register_analysis= T,

            tags$p("Many analysis modules require the normalized counts to be roughly log-normal distributed. You can compare your data distribution (black line) with the log-normal ditribution (red line)."),
            plotOutput("data_distribution_plt")
        ),
        enhanced_box(
            title = "Rank Frequency Plot",
            id = "rank_frequency",
            status = "warning",
            width = 12,
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            get_pdf = T,
            register_analysis= T,
            
            wellPanel(
                fluidRow(
                    column(4, checkboxInput("rf_logX", label = "Log Scale for X", value = T)),
                    column(4, checkboxInput("rf_logY", label = "Log Scale for Y", value = T))
                )
            ),
            plotOutput("rankfreq_plt", height = "600px")
        )
    )
})



output$data_distribution_plt <- renderPlot({
    if(is.null(r_data$df)) return()
    L <- log(r_data$df)
    # Standardize each gene, so that they are all on the same scale, Then melt # the data with plyr so we can plot it easily'
    melted_dens_df <- reshape2::melt(t(scale(t(L))))
    # Plot the distribution of the standardized gene expression values.
    qplot(value, geom = "density", data = melted_dens_df) + stat_function(fun = dnorm, size = 0.5, color = "red") + xlab("Standardized log (normalized counts)") + ylab("Density")
})


