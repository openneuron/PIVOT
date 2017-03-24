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


#' Launch PIVOT
#' 
#' @description 
#' Launch PIVOT in the default browser.
#' 
#' @author 
#' Qin Zhu \email{qinzhu@@outlook.com},
#' Stephen Fisher \email{safisher@@sas.upenn.edu},
#' Hannah Dueck \email{hdueck@@mail.med.upenn.edu},
#' Sarah Middleton \email{s.a.middlet@@gmail.com},
#' Mugdha Khaladkar \email{mugdhak@mail.med.upenn.edu},
#' Young-Ji Na \email{youngji@@sas.upenn.edu},
#' Junhyong Kim \email{junhyong@@sas.upenn.edu}
#' 
#' Maintainer: Junhyong Kim \email{junhyong@@sas.upenn.edu}
#' 
#' @details 
#' This program is based on a collection of R scripts written by members of Junhyong Kim Lab at University of Pennsylvania.
#'  
#' Its goal is to facilitate fast and interactive RNA Sequencing data analysis and visualization.
#' 
#' You may not use this file except in compliance with the Kim Lab License located at
#'  
#' \url{http://kim.bio.upenn.edu/software/LICENSE}
#' 
#' @importFrom plotly as.widget
#' 
#' @export
pivot <- function() {
    shiny::runApp(system.file("PIVOT", package='PIVOT'))
}
