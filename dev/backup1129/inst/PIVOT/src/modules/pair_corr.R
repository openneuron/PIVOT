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


# Pairwise Scatterplot

output$pairwise_box <- renderUI({
    if(is.null(data0())) return()
    
    if(!is.null(r_data$group)) {
        groups <- as.list(unique(r_data$group))
        names(groups) <- paste0("Plot group ", unique(r_data$group))
        groups$"Plot all samples" <- "all"
        selectui <- selectInput("group_for_corr", NULL, choices = groups)
    } else {
        selectui <- NULL
    }

    list(
        enhanced_box(
            width = 12,
            title = "Pairwise correlations", 
            id = "pair_corr",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            reportable = T,
            get_html = T,
            register_analysis= T,
            
            fluidRow(
                column(6, selectui),
                column(6, actionButton("run_corr", "Run", class = "btn btn-info"))
            ),
            
            plotOutput("pair_corr_plt", height = "800px"), 
            hr(),
            tags$p("This plot shows pairwise comparison between your samples.
            The x and y axis of each plot show log10 RPM estimates in the cell corresponding to a given column and row respectively.
            The set of smoothed scatter plots on the lower left shows the overall correspondence between the transcript abundances estimated in two given cells.
            The upper right corner shows three-component mixture model, separating genes that “drop-out” in one of the cells (green component shows drop/out events in the column cell, red component shows drop-out events in the row cell). The correlated component is shown in blue. 
            The percent of genes within each component is shown in the legend.")
        ),
        box(
            width = 12,
            title = "Citation",
            status = "primary",
            tags$ol(
                tags$li("Kharchenko, P. V., Silberstein, L., & Scadden, D. T. (2014). Bayesian approach to single-cell differential expression analysis. Nature methods, 11(7), 740-742.", class = "citation"),
                tags$li("Peter Kharchenko and Jean Fan (2015). scde: Single Cell Differential Expression. R package version 1.99.0. http://pklab.med.harvard.edu/scde/index.html", class = "citation")
            )
        )
    )
    
})

paircorr <- reactiveValues()
paircorr$plt <- NULL

pair_corr <- function(df) {
    # get min and max count values for axis range.
    rangeMin = min(df)
    rangeMax = max(df)
    
    # featurerate color scale from black (0) to red (1) used to represent coorelations.
    colorFunction = colorRamp(c("black", "red"))
    # colorFunction() expects values from 0 to 1.
    zMatrix = colorFunction(seq(0,1,by=.01))
    # zColors goes from 1 to 100.
    zColors = sort(rgb(zMatrix[,1], zMatrix[,2], zMatrix[,3], maxColorValue=255))
    labelSize=1
    title="Pairwise Correlations"
    # Modified from R pairs() documentation	
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
        usr = par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r = abs(cor(x, y))
        txt = format(c(r, 0.123456789), digits=digits)[1]
        txt = paste(prefix, txt, sep="")
        
        if (FALSE) {
            # color text based on r value and change size of text also based on r value (larger text for larger r value).
            if (missing(cex.cor)) cex.cor = labelSize/strwidth(txt)
            text(0.5, 0.5, txt, cex=cex.cor*r, col=zColors[r*100])
        } else {
            # color text based on r value (red is r=1).
            text(0.5, 0.5, txt, cex=labelSize, col=zColors[r*100])
        }
    }
    par(mar = c(0,0,0,0))
    pairs(df, pch=".", cex.labels=labelSize, main=title, upper.panel=panel.cor, ylim=c(rangeMin,rangeMax), xlim=c(rangeMin,rangeMax))
}

# SCDE version
# a slight modification of pairs that passes i/j indices to the panel methods
pairs.extended <- function (x, labels, panel = points, ...,
                            lower.panel = panel, upper.panel = panel,
                            diag.panel = NULL, text.panel = textPanel,
                            label.pos = 0.5 + has.diag/3,
                            cex.labels = NULL, font.labels = 1,
                            row1attop = TRUE, gap = 1)
{
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) {
        text(x, y, txt, cex = cex, font = font)
    }
    
    localAxis <- function(side, x, y, xpd, bg, col = NULL, main, oma, ...) {
        ## Explicitly ignore any color argument passed in as
        ## it was most likely meant for the data points and
        ## not for the axis.
        if(side %%2 ==  1) { Axis(x, side = side, xpd = NA, ...) }
        else { Axis(y, side = side, xpd = NA, ...) }
    }
    
    localPlot <- function(..., main, oma, font.main, cex.main) { plot(...) }
    localLowerPanel <- function(..., main, oma, font.main, cex.main) { lower.panel(...) }
    localUpperPanel <- function(..., main, oma, font.main, cex.main) { upper.panel(...) }
    localDiagPanel <- function(..., main, oma, font.main, cex.main) { diag.panel(...) }
    
    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for(i in seq_along(names(x))) {
            if(is.factor(x[[i]]) || is.logical(x[[i]])) {
                x[[i]] <- as.numeric(x[[i]])
            }
            if(!is.numeric(unclass(x[[i]]))) {
                stop("non-numeric argument to 'pairs'")
            }
        }
    } else if(!is.numeric(x)) {
        stop("non-numeric argument to 'pairs'")
    }
    panel <- match.fun(panel)
    if((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) {
        lower.panel <- match.fun(lower.panel)
    }
    if((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) {
        upper.panel <- match.fun(upper.panel)
    }
    if((has.diag  <- !is.null( diag.panel)) && !missing( diag.panel)) {
        diag.panel <- match.fun( diag.panel)
    }
    
    if(row1attop) {
        tmp <- lower.panel
        lower.panel <- upper.panel
        upper.panel <- tmp
        tmp <- has.lower
        has.lower <- has.upper
        has.upper <- tmp
    }
    
    nc <- ncol(x)
    if (nc < 2) stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels)) {
            labels <- paste("var", 1L:nc)
        }
    } else if(is.null(labels)) {
        has.labs <- FALSE
    }
    oma <- if("oma" %in% nmdots) {
        dots$oma
    } else {
        NULL
    }
    main <- if("main" %in% nmdots) {
        dots$main
    } else {
        NULL
    }
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main)) {
            oma[3L] <- 6
        }
    }
    opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))
    
    for (i in if(row1attop) 1L:nc else nc:1L)
        for (j in 1L:nc) {
            localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, type = "n", ...)
            if(i ==  j || (i < j && has.lower) || (i  >  j && has.upper) ) {
                box()
                if(i ==  1  && (!(j %% 2) || !has.upper || !has.lower ))
                    localAxis(1 + 2*row1attop, x[, j], x[, i], ...)
                if(i ==  nc && (  j %% 2  || !has.upper || !has.lower ))
                    localAxis(3 - 2*row1attop, x[, j], x[, i], ...)
                if(j ==  1  && (!(i %% 2) || !has.upper || !has.lower ))
                    localAxis(2, x[, j], x[, i], ...)
                if(j ==  nc && (  i %% 2  || !has.upper || !has.lower ))
                    localAxis(4, x[, j], x[, i], ...)
                mfg <- par("mfg")
                if(i ==  j) {
                    if (has.diag) localDiagPanel(as.vector(x[, i]), i = i, ...)
                    if (has.labs) {
                        par(usr = c(0, 1, 0, 1))
                        if(is.null(cex.labels)) {
                            l.wid <- strwidth(labels, "user")
                            cex.labels <- max(0.8, min(2, .9 / max(l.wid)))
                        }
                        text.panel(0.5, label.pos, labels[i],
                                   cex = cex.labels, font = font.labels)
                    }
                } else if(i < j)
                    localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), i = i, j = j, ...)
                else
                    localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), i = i, j = j, ...)
                if (any(par("mfg")  !=  mfg))
                    stop("the 'panel' function made a new plot")
            } else {
                par(new = FALSE)
            }
        }
    if (!is.null(main)) {
        font.main <- if("font.main" %in% nmdots) {
            dots$font.main
        } else {
            par("font.main")
        }
        cex.main <- if("cex.main" %in% nmdots) {
            dots$cex.main
        } else {
            par("cex.main")
        }
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}

papply <- function(...,n.cores=detectCores()) {
    if(n.cores>1) {
        # bplapply implementation
        if(is.element("parallel", installed.packages()[,1])) {
            mclapply(...,mc.cores=n.cores)
        } else {
            # last resort
            bplapply(... , BPPARAM = MulticoreParam(workers = n.cores))
        }
    } else { # fall back on lapply
        lapply(...);
    }
}


calculate.crossfit.models <- function(counts, groups, min.count.threshold = 4, nrep = 1, verbose = 0, min.prior = 1e-5, n.cores = 12, save.plots = TRUE, zero.lambda = 0.1, old.cfm = NULL, threshold.segmentation = FALSE, threshold.prior = 1-1e-6, max.pairs = 1000, min.pairs.per.cell = 10) {
    names(groups) <- colnames(counts)
    # enumerate cross-fit pairs within each group
    cl <- do.call(cbind, tapply(colnames(counts), groups, function(ids) {
        cl <- combn(ids, 2)
        min.pairs.per.cell <- min(length(ids)*(length(ids)-1)/2, min.pairs.per.cell)
        if(verbose) {
            cat("number of pairs: ", ncol(cl), "\n")
        }
        if(ncol(cl) > max.pairs) {
            if(verbose) {
                cat("reducing to a random sample of ", max.pairs, " pairs\n")
            }
            
            # make sure there's at least min.pairs.per.cell pairs for each cell
            cl <- cl[, unique(c(sample(1:ncol(cl), max.pairs),
                                unlist(lapply(ids, function(id) sample(which(colSums(cl == id) > 0), min.pairs.per.cell)))))]
        }
        cl
    }))
    
    orl <- c()
    if(!is.null(old.cfm)) {
        # check which pairs have already been fitted in compared in old.cfm
        pn1 <- unlist(apply(cl, 2, function(ii) paste(ii, collapse = ".vs.")))
        pn2 <- unlist(apply(cl, 2, function(ii) paste(rev(ii), collapse = ".vs."))) ### %%% use rev() to revert element order
        vi <- (pn1 %in% names(old.cfm)) | (pn2 %in% names(old.cfm))
        cl <- cl[, !vi, drop = FALSE]
        orl <- old.cfm[names(old.cfm) %in% c(pn1, pn2)]
    }
    if(verbose) {
        cat("total number of pairs: ", ncol(cl), "\n")
    }
    
    if(dim(cl)[2] > 0) {
        if(verbose)  message(paste("cross-fitting", ncol(cl), "pairs:"))
        rl <- papply(seq_len(ncol(cl)), function(cii) {
            ii <- cl[, cii]
            df <- data.frame(c1 = counts[, ii[1]], c2 = counts[, ii[2]])
            vi <- which(rowSums(df) > 0, )
            if(!threshold.segmentation) {
                if(verbose) {
                    message("fitting pair [", paste(ii, collapse = " "), "]")
                }
                mo1 <- FLXMRglmCf(c1~1, family = "poisson", components = c(1), mu = log(zero.lambda))
                mo2 <- FLXMRnb2glmC(c1~1+I(log(c2+1)), components = c(2))
                mo3 <- FLXMRnb2glmC(c2~1+I(log(c1+1)), components = c(2))
                mo4 <- FLXMRglmCf(c2~1, family = "poisson", components = c(3), mu = log(zero.lambda))
                m1 <- mc.stepFlexmix(c1~1, data = df[vi, ], k = 3, model = list(mo1, mo2, mo3, mo4), control = list(verbose = verbose, minprior = min.prior), concomitant = FLXPmultinom(~I((log(c1+1)+log(c2+1))/2)+1), cluster = cbind(df$c1[vi]<= min.count.threshold, df$c1[vi] > min.count.threshold & df$c2[vi] > min.count.threshold, df$c2[vi]<= min.count.threshold), nrep = nrep)
                
                # reduce return size
                m1@posterior <- lapply(m1@posterior, function(m) {
                    rownames(m) <- NULL
                    return(m)
                })
                #rownames(m1@concomitant@x) <- NULL
                m1@concomitant@x <- matrix()
                m1@model <- lapply(m1@model, function(mod) {
                    mod@x <- matrix()
                    mod@y <- matrix()
                    #rownames(mod@x) <- NULL
                    #rownames(mod@y) <- NULL
                    return(mod)
                })
                
                #parent.env(environment(m1@components[[1]][[1]]@logLik)) <- globalenv()
                #parent.env(environment(m1@components[[1]][[2]]@logLik)) <- globalenv()
                #parent.env(environment(m1@components[[2]][[1]]@logLik)) <- globalenv()
                #parent.env(environment(m1@components[[2]][[2]]@logLik)) <- globalenv()
                
                names(vi) <- NULL
                pm <- posterior(m1)[, c(1, 3)]
                rownames(pm) <- NULL
                cl <- clusters(m1)
                names(cl) <- NULL
                gc()
            } else {
                # use min.count.threshold to quickly segment the points
                cl <- rep(2, length(vi))
                cl[df[vi, 1]<min.count.threshold] <- 1
                cl[df[vi, 2]<min.count.threshold] <- 3
                cl[df[vi, 1]<min.count.threshold & df[vi, 2]<min.count.threshold] <- 0
                names(cl) <- NULL
                pm <- cbind(ifelse(cl == 1, threshold.prior, 1-threshold.prior), ifelse(cl == 3, threshold.prior, 1-threshold.prior))
                rownames(pm) <- NULL
            }
            rli <- list(ii = ii, clusters = cl, posterior = pm, vi = vi)
            #message("return object size for pair [", paste(ii, collapse = " "), "] is ", round(object.size(rli)/(1024^2), 3), " MB")
            return(rli)
        }, n.cores = round(n.cores/nrep))
        #, mc.preschedule = threshold.segmentation) # mclapply function has preschedule
        names(rl) <- apply(cl, 2, paste, collapse = ".vs.")
        # clean up invalid entries
        rl <- rl[!unlist(lapply(rl, is.null))]
        rl <- rl[unlist(lapply(rl, is.list))]
        #names(rl) <- unlist(lapply(rl, function(d) paste(d$ii, collapse = ".vs.")))
    } else {
        rl <- c()
    }
    
    if(!is.null(old.cfm)) rl <- c(rl, orl)

    return(rl)
}


scde_pair_plot <- function(counts, groups, rl, ids) {
    #require(Cairo) require(RColorBrewer)
    cl <- combn(ids, 2)
    group <- as.character(groups[ids[1]])
    # log-scale hist
    t.pairs.panel.hist <- function(x, i = NULL, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        vi <- x > 0
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- log10(h$counts)
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "gray60", ...)
    }
    t.pairs.smoothScatter.spearman <- function(x, y, i = NULL, j = NULL, cex = 0.8, ...) {
        vi <- x > 0 | y > 0
        smoothScatter(x[vi], y[vi], add = TRUE, useRaster = TRUE, ...)
        legend(x = "bottomright", legend = paste("sr = ", round(cor(x[vi], y[vi], method = "spearman"), 2), sep = ""), bty = "n", cex = cex)
    }
    # component assignment scatter
    t.panel.component.scatter <- function(x, y, i, j, cex = 0.8, ...) {
        if(!is.null(rl[[paste(ids[i], "vs", ids[j], sep = ".")]])) {
            m1 <- rl[[paste(ids[i], "vs", ids[j], sep = ".")]]
            # forward plot
            vi <- which(x > 0 | y > 0)
            ci <- vi[m1$clusters == 1]
            if(length(ci) > 3) {
                points(x[ci], y[ci], pch = ".", col = densCols(x[ci], y[ci], colramp = colorRampPalette(RColorBrewer::brewer.pal(9, "Reds")[-(1:3)])), cex = 2)
            }
            
            ci <- vi[m1$clusters == 3]
            if(length(ci) > 3) {
                points(x[ci], y[ci], pch = ".", col = densCols(x[ci], y[ci], colramp = colorRampPalette(RColorBrewer::brewer.pal(9, "Greens")[-(1:3)])), cex = 2)
            }
            ci <- vi[m1$clusters == 2]
            if(length(ci) > 3) {
                points(x[ci], y[ci], pch = ".", col = densCols(x[ci], y[ci], colramp = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues")[-(1:3)])), cex = 2)
            }
            legend(x = "topleft", pch = c(19), col = "blue", legend = paste("sr = ", round(cor(x[ci], y[ci], method = "spearman"), 2), sep = ""), bty = "n", cex = cex)
            legend(x = "bottomright", pch = c(rep(19, 3)), col = c("red", "blue", "green"), legend = paste(round(unlist(tapply(m1$clusters, factor(m1$clusters, levels = c(1, 2, 3)), length))*100/length(vi), 1), "%", sep = ""), bty = "n", cex = cex)
            
        } else if(!is.null(rl[[paste(ids[i], "vs", ids[j], sep = ".")]])) {
            m1 <- rl[[paste(ids[j], "vs", ids[i], sep = ".")]]
            # reverse plot
            vi <- which(x > 0 | y > 0)
            ci <- vi[m1$clusters == 3]
            if(length(ci) > 3) {
                points(x[ci], y[ci], pch = ".", col = densCols(x[ci], y[ci], colramp = colorRampPalette(RColorBrewer::brewer.pal(9, "Reds")[-(1:3)])), cex = 2)
            }
            
            ci <- vi[m1$clusters == 1]
            if(length(ci) > 3) {
                points(x[ci], y[ci], pch = ".", col = densCols(x[ci], y[ci], colramp = colorRampPalette(RColorBrewer::brewer.pal(9, "Greens")[-(1:3)])), cex = 2)
            }
            ci <- vi[m1$clusters == 2]
            if(length(ci) > 3) {
                points(x[ci], y[ci], pch = ".", col = densCols(x[ci], y[ci], colramp = colorRampPalette(RColorBrewerbrewer.pal(9, "Blues")[-(1:3)])), cex = 2)
            }
            legend(x = "topleft", pch = c(19), col = "blue", legend = paste("sr = ", round(cor(x[ci], y[ci], method = "spearman"), 2), sep = ""), bty = "n", cex = cex)
            legend(x = "bottomright", pch = c(rep(19, 3)), col = c("red", "blue", "green"), legend = paste(round(unlist(tapply(m1$clusters, factor(m1$clusters, levels = c(3, 2, 1)), length))*100/length(vi), 1), "%", sep = ""), bty = "n", cex = cex)
        } else {
            #message(paste("ERROR: unable to find model for i = ", i, "j = ", j))
            message(paste("INFO: cross-fit plots: skipping model for i = ", i, "j = ", j, " (increase max.pairs parameter if needed"))
        }
    }
    #pdf(file = paste(group, "crossfits.pdf", sep = "."), width = 3*length(ids), height = 3*length(ids))
    pairs.extended(log10(counts[, ids]+1), lower.panel = t.pairs.smoothScatter.spearman, upper.panel = t.panel.component.scatter, diag.panel = t.pairs.panel.hist, cex = 1.5)
    #return(list(plot = p, group = group, width = 250*length(ids), height = 250*length(ids)))
}


output$pair_corr_plt <- renderPlot({
    if(is.null(input$run_corr) || input$run_corr == 0) return()
    isolate({
        if(is.null(r_data$raw)) return ()
        withProgress(message = 'Processing', value = 0.5, {
            if(is.null(r_data$group)) {
                groups <- rep("samples", ncol(r_data$raw))
            } else {
                groups <- r_data$group
            }
            tryCatch({
                r_data$cfm<-calculate.crossfit.models(r_data$raw, groups, n.cores = 1, threshold.segmentation = TRUE, verbose = 1)
            }, error = function(e) {
                return()
            })

            if(is.null(input$group_for_corr) || input$group_for_corr == "all") {
                ids <- colnames(r_data$raw)
                groups <- rep("samples", ncol(r_data$raw))
            } else {
                ids <- names(r_data$group)[which(r_data$group == input$group_for_corr)]
                groups <- r_data$group
            }
            
            scde_pair_plot(r_data$raw, groups, r_data$cfm, ids)
        })
    })
})

