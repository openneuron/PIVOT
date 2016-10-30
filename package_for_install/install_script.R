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

failed_pkg <- list()

usePackage <- function(p) 
{
    if (!is.element(p, installed.packages()[,1]))
        install.packages(p, dep = TRUE, repos='http://cran.us.r-project.org', type = 'binary')
    if(!require(p, character.only = TRUE)) {
        failed_pkg[[length(failed_pkg) + 1]] <- p
    }
}

useDevPackage <- function(path, p, warn = T)
{
    if (!is.element(p, installed.packages()[,1]))
        install_github(path)
    if(!require(p, character.only = TRUE) && warn) {
        failed_pkg[[length(failed_pkg) + 1]] <- p
    }
}

useLocalPackage <- function(p, ver = "", type = 'source') {
    path <- paste0(p, "_", ver, ifelse(type == 'source',".tar.gz",".tgz"))
    if (!is.element(p, installed.packages()[,1]) || packageVersion(p) != ver || p == 'PIVOT')
        install.packages(path, repos = NULL, type=type, dependencies=TRUE, INSTALL_opts = c('--no-lock'))
    if(!require(p, character.only = TRUE)) {
        failed_pkg[[length(failed_pkg) + 1]] <- p
    }
}

useBiocPackage <- function(p){
    if (!is.element(p, installed.packages()[,1]))
    biocLite(p, suppressUpdates = T, suppressAutoUpdate = T)
    if(!require(p, character.only = TRUE)) {
        failed_pkg[[length(failed_pkg) + 1]] <- p
    }
}

installAll <- function() {
    
	if(R.version$major != 3 || R.version$minor < 2.3) {
		print("Please update R to >= 3.2.3, then retry installation.")
		stop()
	}

    cranPkg <- c('shinydashboard', 'devtools', 'shinyAce', 'shinyBS', 'rmarkdown', 'Rtsne', 'shiny', 'shinyFiles', 'RColorBrewer', 'networkD3', 'd3heatmap', 'reshape2', 'dendextend', 'gplots', 'ggplot2', 'ggvis', 'DT', 'plyr', 'dplyr', 'threejs', 'pryr', 'lubridate', 'boot', 'MatrixModels', 'caret', 'httr', 'flexmix', 'brew', 'Rook', 'rjson', 'Cairo', 'RMTstat', 'extRemes', 'igraph', 'VGAM', 'irlba', 'matrixStats',    'combinat', 'fastICA', 'grid', 'parallel', 'methods', 'visNetwork', 'VennDiagram', 'plotly', 'penalizedLDA','qlcMatrix')
    
    for(pkg in cranPkg) {
        usePackage(pkg)
    }
    
    # Dec 11, 2015: Seems normal installation for SCDE broke because this package failed to be installed from source. Use binary seems fix the problem.
    if (!is.element('pbkrtest', installed.packages()[,1])) 
        install.packages('pbkrtest',  dep = TRUE, repos='http://cran.us.r-project.org', type = 'binary')
    if(!require('pbkrtest', character.only = TRUE)) {
        failed_pkg[[length(failed_pkg) + 1]] <- 'pbkrtest'
    }
    
    source("https://bioconductor.org/biocLite.R")
    biocLite(ask = F, suppressUpdates = T, suppressAutoUpdate = T)
    biocPkg <- c('BiocInstaller', 'lfa', 'edgeR', 'S4Vectors', 'DESeq2', 'pcaMethods', 'vsn', 'STRINGdb', 'HSMMSingleCell')
    for(pkg in biocPkg) {
        useBiocPackage(pkg)
    }
    
    useDevPackage('vqv/ggbiplot','ggbiplot')
    useDevPackage('gaborcsardi/pkgconfig','pkgconfig')
    useDevPackage('cole-trapnell-lab/monocle-release@monocle2', 'monocle')
    useDevPackage('satijalab/seurat','Seurat')

    
    if (!is.element('scde', installed.packages()[,1])) {
        install.packages("https://github.com/hms-dbmi/scde/archive/1.99.1.tar.gz", repos = NULL, type='source', dependencies=TRUE, INSTALL_opts = c('--no-lock'))
    }
    if(!require('scde', character.only = TRUE)) {
        failed_pkg[[length(failed_pkg) + 1]] <- 'scde'
    }

    useLocalPackage('PIVOT', '1.10', type = 'source')
    if(length(failed_pkg)) {
        msg<-do.call(paste,c(failed_pkg, sep = ", "))
        print(paste("Package", msg, "failed to be installed. Please try manually install missing packages and run the installer again."))
    } else if(require('PIVOT', character.only = TRUE)) {
        print("PIVOT has been successfully installed. Please restart R first. Then use command 'pivot()' to launch.")
    } else {
        print("Please restart a fresh R session and try install again.")
    }
}



