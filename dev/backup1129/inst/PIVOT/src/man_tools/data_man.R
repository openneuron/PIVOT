
# Data management

output$data_man_ui <- renderUI({
    
    if(is.null(r_data$glb.raw)) return(tags$p("Please upload your data first."))
    
    ################### Data management module ###################
    list(
        box(
            title = "Data Management",
            width = 12,
            status = "info",
            solidHeader = T,
            collapsible = T,
            fluidRow(
                column(12, DT::dataTableOutput("data_history"))
            ),
            
            hr(),
            
            fluidRow(
                column(6),
                column(3, actionButton("switch_data", "Switch to selected dataset", class = "btn-info"))
                #column(3, uiOutput("dtbl_delete_ui"))
            )
        ),
        
        box(
            title = "Data Map",
            width = 12,
            status = "success",
            solidHeader = T,
            collapsible = T,
            #radioButtons("data_net_type", NULL, inline = T, choices =  c("Static" = "static", "Interactive" = "interactive")),
            visNetworkOutput("data_inter_net", width = "100%", height = "600px"),
            fluidRow(
                column(6),
                column(3, uiOutput("dmap_control_ui")),
                column(3, uiOutput("dmap_delete_ui"))
            )
        )
    )
})


output$data_net_ui <- renderUI({
    if(input$data_net_type == "static") {
        plotOutput("data_man_net", width = "100%", height = "600px")
    } else {
        list(
            visNetworkOutput("data_inter_net", width = "100%", height = "600px"),
            uiOutput("dmap_control_ui")
        )
    }
})

# Observe the data changes, add logs each time the data has been modified

update_history <- function(r_data, parent, action_type, action, lists, norm_method, norm_params){
    if(is.null(r_data$history)) {
        r_data$history <- list()
        r_data$his_num <- 0
    } 
    
    if(is.na(parent)) {
        name = "Input data"
    } else {
        r_data$his_num <- r_data$his_num + 1
        name = paste("Subset", r_data$his_num)
    }

    
    r_data$history[[length(r_data$history) + 1]] <- list(
        name = as.character(name),
        time = lubridate::now(), 
        parent_data = as.character(parent),
        action_type = action_type,
        action = action, 
        lists = lists,
        norm_method = as.character(norm_method),
        norm_params = norm_params,
        feature_num = length(r_data$feature_list),
        sample_num = length(r_data$sample_name)
        )
    his_tbl <- plyr::ldply(r_data$history, function(x){data.frame(x[c(1,2,3,4,5,7,9,10)])})
    his_tbl$is_activated <- c(rep("N", nrow(his_tbl) - 1), "Y")
    r_data$his_tbl <- his_tbl
}


# search the last subsetting node

search_subset_node <- function(tbl) {
    cur_p = tbl$name[which(tbl$is_activated == "Y")] # Current node
    # Find the subset node upstream (including) the current node
    
    act_t = tbl$action_type[which(as.character(tbl$name) == as.character(cur_p))] 
    while(!act_t %in% c("Subset", "Input") ){
        cur_p = tbl$parent_data[which(as.character(tbl$name) == as.character(cur_p))]
        act_t = tbl$action_type[which(as.character(tbl$name) == as.character(cur_p))] 
    }
    return(as.character(cur_p))
}



output$data_history <- DT::renderDataTable({
    if(is.null(r_data$history)) return()
    
    DT::datatable(r_data$his_tbl,selection = 'single', rownames = FALSE, options = list(
        scrollX = T, scrollY = "500px"))
})


observe({
    if(is.null(r_data$his_tbl)) return()
    tbl <- r_data$his_tbl
    
    if(!is.null(r_data$reg_tbl)) {
        tbl <- rbind(tbl, r_data$reg_tbl)
    }

    tbl$label <- tbl$name
    tbl$name <- ifelse(is.na(tbl$is_activated), paste(as.character(tbl$name), tbl$time), as.character(tbl$name))
    
    tbl_nodes <- tbl %>% dplyr::select(name, time, feature_num, sample_num, is_activated, label)
    tbl_nodes <- subset(tbl_nodes, !duplicated(tbl_nodes[,1])) 
    
    tbl_edges <- apply(tbl, 1, function(row) {
        if(!is.na(row[3])){
            return(c("from" = row[3], "to" = row[1], "label" = ifelse(row[4] == "analysis", NA, row[5])))
        }
    })
    
    if(is.null(tbl_edges)) return()
    
    tbl_edges <- as.data.frame(t(as.data.frame(tbl_edges[!sapply(tbl_edges, is.null)])))
    
    colnames(tbl_edges) <- c("from", "to", "label")
    
    r_data$his_nodes <- tbl_nodes
    r_data$his_edges <- tbl_edges
})

output$data_man_net <- renderPlot({
    if(is.null(r_data$his_nodes)) return()
    tbl_nodes <- r_data$his_nodes
    tbl_edges <- r_data$his_edges
    total_feature_num <- nrow(r_data$glb.df)
    total_sample_num <- ncol(r_data$glb.df)
    feature_percent<-data.frame(num1 = tbl_nodes$feature_num, num2 = total_feature_num - tbl_nodes$feature_num)
    feature_percent<- as.list(as.data.frame(t(feature_percent)))
    
    g <- graph_from_data_frame(tbl_edges, directed=F, vertices=tbl_nodes)
    
    V(g)$color <- ifelse(is.na(tbl_nodes$is_activated), "purple", "yellow")
    V(g)$pie.color <- ifelse(tbl_nodes$is_activated == "Y", list(c("darkorange1", "gold")), list(topo.colors(2)))
    
    V(g)$size <- ifelse(is.na(tbl_nodes$sample_num), 70, 50 * tbl_nodes$sample_num[match(V(g)$name, tbl_nodes$name)] / total_sample_num)
    V(g)$label.cex <- ifelse(is.na(tbl_nodes$sample_num), 0.8, 1.2 * tbl_nodes$sample_num[match(V(g)$name, tbl_nodes$name)] / total_sample_num)
    
    E(g)$label.cex <- 0.5
    E(g)$width <- ifelse(is.na(tbl_edges$label), 1, 2)
    E(g)$weight <- ifelse(is.na(tbl_edges$label), 0.5, 2)
    E(g)$color <- ifelse(is.na(tbl_edges$label), "grey", "black")
    
    
    V(g)$shape <- ifelse(is.na(tbl_nodes$sample_num), "rectangle", "pie")
    
    V(g)$label.color <- "white"
    V(g)$pie <- feature_percent
    V(g)$label.family <- "sans"
    V(g)$frame.color <- "white"
    plot(g, layout = layout.fruchterman.reingold)
})

output$data_inter_net <- renderVisNetwork({
    if(is.null(r_data$his_nodes)) return()
    vis_nodes <- r_data$his_nodes
    vis_edges <- r_data$his_edges
    
    vis_nodes <- vis_nodes %>% 
        dplyr::mutate(group = ifelse(is.na(vis_nodes$is_activated), "Analysis", ifelse(vis_nodes$is_activated == "Y", "Dataset_active", "Dataset"))) %>%
        dplyr::mutate(title = ifelse(is.na(vis_nodes$is_activated), 
                                     paste("Created at:", time), 
                                     paste(vis_nodes$feature_num, "features,", vis_nodes$sample_num, "samples. Created at:", time))) %>%
        dplyr::select(id = name, label, group, title)
    
    vis_edges <- vis_edges %>% 
        dplyr::mutate(width = ifelse(is.na(vis_edges$label), 1, 5)) %>%
        dplyr::mutate(color = ifelse(is.na(vis_edges$label), "grey", "black")) %>%
        dplyr::select(from, to, title = label, width, color) 
    
    
    visNetwork(vis_nodes, vis_edges) %>%
        visGroups(groupname = "Dataset", shape = "icon", icon = list(code = "f200", size = 75)) %>%
        visGroups(groupname = "Dataset_active", shape = "icon", icon = list(code = "f200", size = 75, color = "orange")) %>%
        visGroups(groupname = "Analysis", shape = "icon", icon = list(code = "f080", size = 30, color = "red")) %>%
        addFontAwesome() %>%
        visLegend() %>%
        visOptions(nodesIdSelection = TRUE) %>% 
        visInteraction(navigationButtons = TRUE)
})

output$dmap_control_ui<- renderUI({
    if(is.null(input$data_inter_net_selected) || is.null(r_data$his_nodes)) return()
    s <- which(r_data$his_nodes$name == input$data_inter_net_selected)
    if(!length(s)) return()
    if(is.na(r_data$his_nodes$is_activated[s])) {
        downloadButton("dmap_report", "Download Analysis Report", class = "btn-success")
    } else if(r_data$his_nodes$is_activated[s] == "N"){
        actionButton("dmap_switch", paste("Switch to Dataset:", input$data_inter_net_selected), class = "btn-info")
    } 
})

output$dmap_delete_ui<- renderUI({
    if(is.null(input$data_inter_net_selected) || is.null(r_data$his_nodes)) return()
    s <- which(r_data$his_nodes$name == input$data_inter_net_selected)
    if(!length(s)) return()
    if(is.na(r_data$his_nodes$is_activated[s])) {
        actionButton("dmap_del_report", "Delete Analysis Report", class = "btn-danger")
    } else if(r_data$his_nodes$name[s] != "Input data"){
        list(
            modalTriggerButton("dmap_del_subset", "#deleteConfirmDlg", paste("Delete", input$data_inter_net_selected)),
            modalDialog(id="deleteConfirmDlg", 
                        header = "Are you sure?",
                        body = "Note this will also remove all analysis reports linked to this subset.",
                        footer=list(
                            modalTriggerButton("deleteConfirmDlgBtn", "#deleteConfirmDlg", "Delete"),
                            tags$button(type = "button", class = "btn btn-primary", 'data-dismiss' = "modal", "Cancel")
                        )
            )
        )
    } 
})

output$dtbl_delete_ui<- renderUI({
    if(is.null(r_data$his_tbl) || is.null(input$data_history_row_last_clicked)) return()
    s <- which(r_data$his_tbl$name == input$data_history_row_last_clicked)
    if(length(s)){
        if(r_data$his_tbl$name[s] != "Input data"){
            list(
                modalTriggerButton("dtbl_del_subset", "#deleteConfirmDlg2", paste("Delete", input$data_history_row_last_clicked)),
                modalDialog(id="deleteConfirmDlg2", 
                            header = "Are you sure?",
                            body = "Note this will also remove all analysis reports linked to this subset.",
                            footer=list(
                                modalTriggerButton("deleteConfirmDlgBtn2", "#deleteConfirmDlg2", "Delete"),
                                tags$button(type = "button", class = "btn btn-primary", 'data-dismiss' = "modal", "Cancel")
                            )
                )
            )
        } 
    }
})


switch_to_dataset <- function(r_data, s) {
    clear_results()
    
    r_data$feature_list <- r_data$history[[s]]$lists[[1]]
    r_data$sample_name <- r_data$history[[s]]$lists[[2]]
    
    if(!is.null(r_data$glb.group)) {
        r_data$group <- factor(r_data$glb.group[r_data$sample_name])
    }
    
    if(!is.null(r_data$glb.batch)) {
        r_data$batch <- factor(r_data$glb.batch[r_data$sample_name])
    }
    
    r_data$raw <- r_data$glb.raw[r_data$feature_list, r_data$sample_name]
    r_data$norm_param <- r_data$history[[s]]$norm_params
    
    #fix this
    r_data$df <- r_data$glb.df[r_data$feature_list, r_data$sample_name]
    
    is_activated <- rep("N", nrow(r_data$his_tbl))
    is_activated[s] <- "Y"
    r_data$his_tbl$is_activated <- is_activated
    
    # Reset metadata
    r_data <- init_meta(r_data)
    return(r_data)
}

observeEvent(input$switch_data, {
    if(is.null(r_data$his_tbl)) return()
    s = input$data_history_row_last_clicked
    
    if (length(s)) {    
        s <- which(r_data$his_tbl$name == s)
        r_data <- switch_to_dataset(r_data, s)
    } else {
        return()
    }
})

# Graph: switch to selected dataset

observeEvent(input$dmap_switch, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    if (length(s)) {    
        r_data <- switch_to_dataset(r_data, s)
    } else {
        return()
    }
})

# Graph: download html report of selected dataset

output$dmap_report <- downloadHandler(
    filename = function(){
        paste0(input$data_inter_net_selected, ".html")
    },
    content = function(file) {
        if(is.null(r_data$reg_tbl)) return()
        tbl <- r_data$reg_tbl
        tbl$id <- paste(as.character(tbl$name), tbl$time)
        
        s <- which(tbl$id == input$data_inter_net_selected)
        if (length(s)) {    
            rawHTML <- r_data$reg_history[[s]]$lists
            writeLines(rawHTML, file)
        }
    }
) 


###### Delete nodes ######
# Delete report

observeEvent(input$dmap_del_report, {
    if(is.null(r_data$reg_tbl)) return()
    tbl <- r_data$reg_tbl
    tbl$id <- paste(as.character(tbl$name), tbl$time)
    s <- which(tbl$id == input$data_inter_net_selected)
    if (length(s)) {   
        # Remove this record from r_data$reg_history 
        r_data$reg_history <- r_data$reg_history[-s] 
        # Remove this record from r_data$reg_tbl
        r_data$reg_tbl <- r_data$reg_tbl[-s,]
    }
})

# Delete data subset (data map)
observeEvent(input$dmap_del_subset, {
    
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    if (length(s)) {    
        session$sendCustomMessage(type = 'dialogContentUpdate',
                                  message = list(id = "deleteConfirmDlg",
                                                 message = paste0('Are you sure to delete this data subset?')))
    } 
})

observeEvent(input$deleteConfirmDlgBtn, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    if (length(s)) {    
        if(!is.null(r_data$reg_tbl) && nrow(r_data$reg_tbl) > 0) {
            a_ids <- which(r_data$reg_tbl$parent_data == input$data_inter_net_selected)
            if(length(a_ids)) {
                r_data$reg_history <- r_data$reg_history[-a_ids] 
                r_data$reg_tbl <- r_data$reg_tbl[-a_ids,]
            }
        }
        
        # If the selected node is currently active, reset active node to input data
        if(r_data$his_tbl$is_activated[s] == "Y") {
            r_data <- switch_to_dataset(r_data, 1)
        }
        
        # Remove the current subset node
        r_data$history <- r_data$history[-s]
        r_data$his_tbl <- r_data$his_tbl[-s,]
    }
})



# Delete data subset (data tbl)
observeEvent(input$dtbl_del_subset, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_history_row_last_clicked)
    if (length(s)) {    
        session$sendCustomMessage(type = 'dialogContentUpdate',
                                  message = list(id = "deleteConfirmDlg2",
                                                 message = paste0('Are you sure to delete this data subset?')))
    } 
})

observeEvent(input$deleteConfirmDlgBtn2, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_history_row_last_clicked)
    if (length(s)) {    
        if(!is.null(r_data$reg_tbl) && nrow(r_data$reg_tbl) > 0) {
            a_ids <- which(r_data$reg_tbl$parent_data == input$data_inter_net_selected)
            
            r_data$reg_history <- r_data$reg_history[-a_ids] 
            r_data$reg_tbl <- r_data$reg_tbl[-a_ids,]
        }
        
        # If the selected node is currently active, reset active node to input data
        if(r_data$his_tbl$is_activated[s] == "Y") {
            r_data <- switch_to_dataset(r_data, 1)
        }
        
        # Remove the current subset node
        r_data$history <- r_data$history[-s]
        r_data$his_tbl <- r_data$his_tbl[-s,]
    }
})




