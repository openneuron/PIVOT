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


# Mugdhak's modified DESeq script

estimateSizeFactorsForMatrix_MK <- function( counts, locfunc = median, geoMeans, controlGenes, threshold = 0.7)
{
    if (missing(geoMeans)) {
        i = 0
        numGenes = 0
        loggeomeans = c()
        
        s_th <- threshold * ncol(counts)
        
        loggeomeans <- apply(counts, 1, function(crow) {
            if(length(crow[crow!=0]) < s_th){
                return(-Inf)
            } else {
                mean(log(crow[crow!=0]))
            }
        })
        
    } else {
        if (length(geoMeans) != nrow(counts)) {
            stop("geoMeans should be as long as the number of rows of counts")
        }
        loggeomeans <- log(geoMeans)
    }
    
    numGenes <- sum(is.finite(loggeomeans))
    
    if (all(is.infinite(loggeomeans))) {
        stop("every gene contains at least one zero, cannot compute log geometric means")
    }
    sf <- if (missing(controlGenes)) {
        apply(counts, 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeans)[is.finite(loggeomeans) & cnts > 0]))
        })
    } else {
        if (!is.numeric(controlGenes) | is.logical(controlGenes)) {
            stop("controlGenes should be either a numeric or logical vector")
        }
        loggeomeansSub <- loggeomeans[controlGenes]
        apply(counts[controlGenes,], 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeansSub)[is.finite(loggeomeansSub) & cnts > 0]))
        })
    }
    return(list(sf = sf, numGenes = numGenes, threshold = threshold))
}



estimateSizeFactorsForMatrix_groupwise <- function( counts, group, locfunc = median, geoMeans, controlGenes, threshold = 0.7)
{
    if (missing(geoMeans)) {
        i = 0
        numGenes = 0
        loggeomeans = c()
        
        
        gp_num <- length(unique(group))
        
        det_rate<-as.data.frame(t(apply(counts, 1, function(crow) {table(group[which(crow > 0)])/table(group)})))
        
        not_to_keep <- which(rowSums(det_rate >= threshold) != gp_num)
        
        loggeomeans <- apply(counts, 1, function(crow) { mean(log(crow[crow!=0])) })
        
        loggeomeans[not_to_keep] <- -Inf
             
    } else {
        if (length(geoMeans) != nrow(counts)) {
            stop("geoMeans should be as long as the number of rows of counts")
        }
        loggeomeans <- log(geoMeans)
    }
    
    numGenes <- sum(is.finite(loggeomeans))
    
    if (all(is.infinite(loggeomeans))) {
        stop("every gene contains at least one zero, cannot compute log geometric means")
    }
    sf <- if (missing(controlGenes)) {
        apply(counts, 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeans)[is.finite(loggeomeans) & cnts > 0]))
        })
    } else {
        if (!is.numeric(controlGenes) | is.logical(controlGenes)) {
            stop("controlGenes should be either a numeric or logical vector")
        }
        loggeomeansSub <- loggeomeans[controlGenes]
        apply(counts[controlGenes,], 2, function(cnts) {
            exp(locfunc((log(cnts) - loggeomeansSub)[is.finite(loggeomeansSub) & cnts > 0]))
        })
    }
    return(list(sf = sf, numGenes = numGenes, threshold = threshold))
}

