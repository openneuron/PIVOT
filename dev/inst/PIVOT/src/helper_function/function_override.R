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

`%>%` <- magrittr::`%>%`

plot_ly <- plotly::plot_ly
as.widget <- plotly::as.widget

renderPlotly <- function (expr, env = parent.frame(), quoted = FALSE) 
{
    if (!quoted) {
        expr <- substitute(expr)
    }
    expr <- call("as.widget", expr)
    htmlwidgets::shinyRenderWidget(expr, plotly::plotlyOutput, env, quoted = TRUE)
}



withMathJax2 <- function (...)
{
    path <- "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))),
            ..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}

tagAssert <- function(tag, type = NULL, class = NULL, allowUI = TRUE) {
    if (!inherits(tag, "shiny.tag")) {
        print(tag)
        stop("Expected an object with class 'shiny.tag'.")
    }
    
    # Skip dynamic output elements
    if (allowUI &&
        (hasCssClass(tag, "shiny-html-output") ||
         hasCssClass(tag, "shinydashboard-menu-output"))) {
        return()
    }
    
    if (!is.null(type) && tag$name != type) {
        stop("Expected tag to be of type ", type)
    }
    
    if (!is.null(class)) {
        if (is.null(tag$attribs$class)) {
            stop("Expected tag to have class '", class, "'")
            
        } else {
            tagClasses <- strsplit(tag$attribs$class, " ")[[1]]
            if (!(class %in% tagClasses)) {
                stop("Expected tag to have class '", class, "'")
            }
        }
    }
}

hasCssClass <- function(tag, class) {
    if (is.null(tag$attribs) || is.null(tag$attribs$class))
        return(FALSE)
    
    classes <- strsplit(tag$attribs$class, " +")[[1]]
    return(class %in% classes)
}

# Modify dashboard dropdown menu
dropdownMenu <- function (..., type = c("messages", "notifications", "tasks", "system", "support"), top_msg = NULL, icon = NULL, .list = NULL) 
{
    type <- match.arg(type)
    items <- c(list(...), .list)
    lapply(items, tagAssert, type = "li")
    dropdownClass <- ifelse(type %in% c("system","support"), paste0("dropdown ", "notifications", "-menu"), paste0("dropdown ", type, "-menu"))
    if (is.null(icon)) {
        icon <- switch(type, messages = shiny::icon("envelope"), 
                       notifications = shiny::icon("list-alt"), tasks = shiny::icon("tasks"), system = shiny::icon("cog"), support = shiny::icon("question-circle"))
    }
    dropdown_style <- ifelse(type%in% c("messages","support"), "width:290px", "width:200px;")
    tags$li(class = dropdownClass, a(href = "#", class = "dropdown-toggle",`data-toggle` = "dropdown", icon, NULL), tags$ul(class = "dropdown-menu", style = dropdown_style, tags$li(class = "header", top_msg), tags$li(tags$ul(class = "menu", style = "list-style-type: none;max-height: 800px;", items))))
}

systemItem <- function (title = NULL, text, icon = NULL, status = "success", href = NULL) 
{
    tagAssert(icon, type = "i")
    icon <- tagAppendAttributes(icon, class = paste0("text-", status))
    if(!is.null(title)) {
        title <- tags$b(title)
        text <- tags$p(text, style="text-indent: 2px;")
    }

    tags$li(a(icon, title, text, href = href))
}


custom_downloadbtn <- function (outputId, label = "Download", class = NULL, style = NULL) 
{
    tags$a(id = outputId, class = paste("shiny-download-link", class), style = style, href = "", target = "_blank", label)
}

valueBoxOutput_custom <- function (outputId, width = 4, style = NULL) 
{
    shiny::uiOutput(outputId, class = paste0("col-sm-", width), style = style)
}

infoBoxOutput_custom <- function (outputId, width = 4, style = NULL) 
{
    shiny::uiOutput(outputId, class = paste0("col-sm-", width), style = style)
}

sidebarwell <- function (...) 
{
    div(class = "well well_sidebar", ...)
}

sidebarwell2 <- function (...) 
{
    div(class = "well well_sidebar2", ...)
}


formatNoSci <- function(x) {
    if (is.null(x)) return(NULL)
    format(x, scientific = FALSE, digits = 15)
}

`%AND%` <- function(x, y) {
    if (!is.null(x) && !is.na(x))
        if (!is.null(y) && !is.na(y))
            return(y)
    return(NULL)
}

numericInput_1 <- function (inputId, label, value, min = NA, max = NA, step = NA, 
          width = NULL) 
{
    inputTag <- tags$input(id = inputId, type = "number", class = "form-control", 
                           value = formatNoSci(value), style = "display:inline-block; width:60%;")
    if (!is.na(min)) 
        inputTag$attribs$min = min
    if (!is.na(max)) 
        inputTag$attribs$max = max
    if (!is.na(step)) 
        inputTag$attribs$step = step
    div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: 100%;"), label %AND% 
            tags$label(label, `for` = inputId, style = "display:inline_block"), inputTag)
}

validColors <- c("red", "yellow", "aqua", "blue", "light-blue", "green",
                 "navy", "teal", "olive", "lime", "orange", "fuchsia",
                 "purple", "maroon", "black")

validateColor <- function(color) {
    if (color %in% validColors) {
        return(TRUE)
    }
    
    stop("Invalid color: ", color, ". Valid colors are: ",
         paste(validColors, collapse = ", "), ".")
}

validStatuses <- c("primary", "success", "info", "warning", "danger")

validateStatus <- function(status) {
    
    if (status %in% validStatuses) {
        return(TRUE)
    }
    
    stop("Invalid status: ", status, ". Valid statuses are: ",
         paste(validStatuses, collapse = ", "), ".")
}

"%OR%" <- function(a, b) if (!is.null(a)) a else b

enhanced_box <- function (..., title = NULL, id = NULL, footer = NULL, status = NULL, solidHeader = FALSE, 
                          background = NULL, width = 6, height = NULL, collapsible = FALSE, 
                          reportable = FALSE, get_pdf = FALSE, get_html = FALSE, register_analysis = FALSE,
                          collapsed = FALSE) 
{
    boxClass <- "box"
    if (solidHeader || !is.null(background)) {
        boxClass <- paste(boxClass, "box-solid")
    }
    if (!is.null(status)) {
        boxClass <- paste0(boxClass, " box-", status)
    }
    if (collapsible && collapsed) {
        boxClass <- paste(boxClass, "collapsed-box")
    }
    if (!is.null(background)) {
        validateColor(background)
        boxClass <- paste0(boxClass, " bg-", background)
    }
    style <- NULL
    if (!is.null(height)) {
        style <- paste0("height: ", shinydashboard::validateCssUnit(height))
    }
    titleTag <- NULL
    if (!is.null(title)) {
        titleTag <- h3(class = "box-title", title)
    }
    
    collapseTag <- NULL
    if (collapsible) {
        buttonStatus <- status %OR% "default"
        collapseIcon <- if (collapsed) 
            "plus"
        else "minus"
        
        collapseTag <- tags$button(class = paste0("btn btn-box-tool"),
                                   `data-widget` = "collapse", 
                                   shiny::icon(collapseIcon)) 
                                                                       
    }
    
    reportTag <- NULL
    pdfTag <- NULL
    htmlTag <- NULL
    registerTag <- NULL
    
    if(get_pdf && !is.null(id)) {
        pdfTag <- tags$a(id = paste0(id, "_pdf"), class = "btn btn-box-tool shiny-download-link", 
                         `data-toggle`="tooltip", `data-original-title`="PDF",
                         href = "", target = "_blank", icon("file-pdf-o"), NULL)
    }
    
    if(get_html && !is.null(id)) {
        htmlTag <- tags$a(id = paste0(id, "_html"), class = "btn btn-box-tool shiny-download-link",
                         `data-toggle`="tooltip", `data-original-title`="HTML",
                         href = "", target = "_blank", icon("file-picture-o"), NULL)
    }
    
    if(reportable && !is.null(id)) {
        reportTag <- tags$button(class = paste0("btn btn-box-tool action-button shiny-bound-input"), 
                                 `data-toggle`="tooltip", `data-original-title`="Add to report",
                                 href=paste0(id,'_report'), id=paste0(id,'_report'), shiny::icon("clipboard"))
    }
    
    if(register_analysis && !is.null(id)) {
        registerTag <- tags$button(class = paste0("btn btn-box-tool action-button shiny-bound-input"), 
                                 `data-toggle`="tooltip", `data-original-title`="Register results",
                                 href=paste0(id,'_reg'), id=paste0(id,'_reg'), shiny::icon("magnet"))
    }
    
    headerTag <- NULL
    if (!is.null(titleTag) || !is.null(collapseTag)) {
        boxtools <- div(class = "box-tools pull-right", registerTag, reportTag, pdfTag, htmlTag, collapseTag)
        headerTag <- div(class = "box-header", titleTag, boxtools)
    }
    div(class = if (!is.null(width)) 
        paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
            style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
                div(class = "box-footer", footer)))
}


# Modal confirm button

modalDialog = function(id, header = "Confirmation", body = "Are you sure?", footer = list(actionButton("confirmDlgOkBtn", "OK"))){
    div(id = id, class = "modal fade",
        div(class = "modal-dialog modal-sm",
            div(class = "modal-content",
                div(class = "modal-header",
                    tags$button(type = "button", class = "close", 'data-dismiss' = "modal", 'aria-hidden' = "true", HTML('&times;')),
                    tags$h4(class = "modal-title", header)
                ),
                div(class = "modal-body",
                    tags$p(body)
                ),
                div(class = "modal-footer",
                    tagList(footer)
                )
            )
        )
    )
}

modalTriggerButton = function(inputId, target, label, icon = NULL, class = "btn action-button btn-danger btn_leftAlign"){
    if (!is.null(icon)) 
        buttonContent <- list(icon, label)
    else buttonContent <- label
    tags$button(id = inputId, type = "button", class = class, 'data-toggle' = "modal", 'data-target' = target,
                buttonContent)
}

# Feature plot function
feature_super_plot <- function(df, selected_gene, plot_group = "group", style = "box", legend_pos = "top"){
    if(is.null(style)) return()
    
    g1 <- ggplot(df, aes_string(x=plot_group, y="expression_level")) + 
        geom_point(position=position_jitter(w=0.1,h=0), size = 3, aes_string(colour = plot_group, group = plot_group)) + 
        ggtitle(paste0("Expression level of gene ", selected_gene))
    
    
    g1 <- g1 + scale_y_log10(breaks=c(25,100,400)) +
        theme(text = element_text(size=15), legend.position=legend_pos) + guides(fill=FALSE, alpha = F) 
    
    if(style == "box") {
        g1 <- g1 + geom_boxplot(aes_string(fill = plot_group, alpha = 0.2)) 
    } else if(style == "violin") {
        g1 <- g1 + geom_violin(aes_string(fill = plot_group, alpha = 0.2), trim = F)
    }
    return(g1)
}

