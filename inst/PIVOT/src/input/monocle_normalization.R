# 

estimateSizeFactorsForMatrix <- function (counts, locfunc = median, round_exprs = TRUE, method = "mean-geometric-mean-total") 
{
    if (isSparseMatrix(counts)) {
        estimateSizeFactorsForSparseMatrix(counts, locfunc = locfunc, 
                                           round_exprs = round_exprs, method = method)
    }
    else {
        estimateSizeFactorsForDenseMatrix(counts, locfunc = locfunc, 
                                          round_exprs = round_exprs, method = method)
    }
}


isSparseMatrix <- function (x) 
{
    class(x) %in% c("dgCMatrix", "dgTMatrix")
}

estimateSizeFactorsForDenseMatrix1 <- function (counts, locfunc = median, round_exprs = TRUE, method = "mean-geometric-mean-total") 
{
    CM <- counts
    if (round_exprs) 
        CM <- round(CM)
    if (method == "weighted-median") {
        log_medians <- apply(CM, 1, function(cell_expr) {
            log(locfunc(cell_expr))
        })
        weights <- apply(CM, 1, function(cell_expr) {
            num_pos <- sum(cell_expr > 0)
            num_pos/length(cell_expr)
        })
        sfs <- apply(CM, 2, function(cnts) {
            norm_cnts <- weights * (log(cnts) - log_medians)
            norm_cnts <- norm_cnts[is.nan(norm_cnts) == FALSE]
            norm_cnts <- norm_cnts[is.finite(norm_cnts)]
            exp(mean(norm_cnts))
        })
    }
    else if (method == "median-geometric-mean") {
        log_geo_means <- rowMeans(log(CM))
        sfs <- apply(CM, 2, function(cnts) {
            norm_cnts <- log(cnts) - log_geo_means
            norm_cnts <- norm_cnts[is.nan(norm_cnts) == FALSE]
            norm_cnts <- norm_cnts[is.finite(norm_cnts)]
            exp(locfunc(norm_cnts))
        })
    }
    else if (method == "median") {
        row_median <- apply(CM, 1, median)
        sfs <- apply(Matrix::t(Matrix::t(CM) - row_median), 2, 
                     median)
    }
    else if (method == "mode") {
        sfs <- estimate_t(CM)
    }
    else if (method == "geometric-mean-total") {
        cell_total <- apply(CM, 2, sum)
        sfs <- log(cell_total)/mean(log(cell_total))
    }
    else if (method == "mean-geometric-mean-total") {
        cell_total <- apply(CM, 2, sum)
        sfs <- cell_total/exp(mean(log(cell_total)))
    }
    sfs[is.na(sfs)] <- 1
    sfs
}
