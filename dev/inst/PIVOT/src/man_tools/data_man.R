
# Data management

output$data_man_ui <- renderUI({
    
    if(is.null(r_data$glb.raw)) return(tags$p("Please upload your data first."))
    
    ################### Data management module ###################
    
    content_rename <- list(
        textInput("dmap_rename_new", NULL, value = "", placeholder = "type new name here"),
        actionButton("dmap_rename_btn", "Rename", class = "btn-warning")
    )
    
    content_notes <- list(
        textInput("dmap_notes_content", NULL, value = "", placeholder = "type notes here"),
        actionButton("dmap_notes_btn", "Add", class = "btn-info")
    )
    
    list(
        box(
            title = "Data Management Table",
            width = 12,
            status = "info",
            solidHeader = T,
            collapsible = T,
            fluidRow(
                column(12, DT::dataTableOutput("data_history"))
            ),
            downloadButton("download_his_tbl", "Download", class = "btn btn-success"),
            # This chunk no longer necessary, just use the map switch/delete
            hr(),
            tags$p("This table shows the details of each dataset. To switch or delete dataset, please use the data map below.")    
            #fluidRow(
            #    column(6),
            #    column(3, actionButton("switch_data", "Switch to selected dataset", class = "btn-info"))
                #column(3, uiOutput("dtbl_delete_ui"))
            #)
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
                column(9, 
                       actionButton("dmap_rename_modal_trigger", label = "Rename Node", class = "btn-warning btn_leftAlign"),
                       shinyBS::bsModal(id = "dmap_rename_modal", "Enter the new name:", "dmap_rename_modal_trigger", size = "small", content_rename),
                       actionButton("dmap_notes_modal_trigger", label = "Add Notes", class = "btn-info btn_leftAlign"),
                       shinyBS::bsModal(id = "dmap_notes_modal", "Add notes:", "dmap_notes_modal_trigger", size = "medium", content_notes),
                       uiOutput("dmap_delete_ui")
                ),
                column(3, 
                       uiOutput("dmap_control_ui")
                )
            )
        )
    )
})


# Observe the data changes, add logs each time the data has been modified

update_history <- function(r_data, parent, action_type, action, lists, norm_method, norm_param){
    if(is.null(r_data$history)) {
        r_data$history <- list()
        r_data$his_num <- 0
    } 
    
    if(is.na(parent)) {
        name = "Input Data"
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
        norm_param = norm_param,
        feature_num = length(r_data$feature_list),
        sample_num = length(r_data$sample_name),
        user_notes = ""
        )
    his_tbl <- plyr::ldply(r_data$history, function(x){data.frame(x[c(1,2,3,4,5,7,9,10,11)])})
    his_tbl$is_activated <- c(rep("N", nrow(his_tbl) - 1), "Y")
    his_tbl$name <- as.character(his_tbl$name) # change factor to string so good to replace
    his_tbl$parent_data <- as.character(his_tbl$parent_data) # same as above
    his_tbl$user_notes <- as.character(his_tbl$user_notes)
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

output$download_his_tbl <- downloadHandler(
    filename = "data_history_table.csv",
    content = function(file) {
        if(is.null(r_data$history)) return()
        write.csv(r_data$his_tbl, file)
    }
)

observe({
    if(is.null(r_data$his_tbl)) return()
    tbl <- r_data$his_tbl
    
    if(!is.null(r_data$reg_tbl)) {
        tbl <- rbind(tbl, r_data$reg_tbl)
    }

    tbl$label <- tbl$name
    tbl$name <- ifelse(is.na(tbl$is_activated), paste(as.character(tbl$name), tbl$time), as.character(tbl$name))
    
    tbl_nodes <- tbl %>% dplyr::select(name, time, feature_num, sample_num, is_activated, label, user_notes)
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
    
    title_content <- paste0("<p>", 
                            ifelse(is.na(vis_nodes$is_activated), 
                                   paste("Created at:", vis_nodes$time), 
                                   paste(vis_nodes$feature_num, "features,", vis_nodes$sample_num, "samples. Created at:", vis_nodes$time)),
                            ifelse(vis_nodes$user_notes == "", "", paste0("<br>", vis_nodes$user_notes)), "</p>")
    vis_nodes <- vis_nodes %>% 
        dplyr::mutate(group = ifelse(is.na(vis_nodes$is_activated), "Analysis", ifelse(vis_nodes$is_activated == "Y", "Dataset_active", "Dataset"))) %>%
        dplyr::mutate(title = title_content) %>%
        dplyr::select(id = name, label, group, title)
    
    vis_edges <- vis_edges %>% 
        dplyr::mutate(width = ifelse(is.na(vis_edges$label), 1, 5)) %>%
        dplyr::mutate(color = ifelse(is.na(vis_edges$label), "grey", "black")) %>%
        dplyr::select(from, to, title = label, width, color) 
    
    visNetwork(vis_nodes, vis_edges) %>%
        visNodes(labelHighlightBold = T) %>%
        visGroups(groupname = "Dataset", shape = "icon", icon = list(code = "f200", size = 75)) %>%
        visGroups(groupname = "Dataset_active", shape = "icon", icon = list(code = "f200", size = 75, color = "orange")) %>%
        visGroups(groupname = "Analysis", shape = "icon", icon = list(code = "f080", size = 30, color = "red")) %>%
        addFontAwesome() %>%
        visLegend() %>%
        visOptions(nodesIdSelection = TRUE) %>% 
        visInteraction(navigationButtons = TRUE) %>%
        visEvents(doubleClick = "function(properties){alert('selected nodes: '+ properties.nodes);}")
})


output$dmap_control_ui<- renderUI({
    if(is.null(input$data_inter_net_selected) || is.null(r_data$his_nodes)) return()
    s <- which(r_data$his_nodes$name == input$data_inter_net_selected)
    if(!length(s)) return()
    if(is.na(r_data$his_nodes$is_activated[s])) {
        downloadButton("dmap_report", "Download Analysis Report", class = "btn-success btn_rightAlign")
    } else if(r_data$his_nodes$is_activated[s] == "N"){
        actionButton("dmap_switch", paste("Switch to Dataset:", input$data_inter_net_selected), class = "btn-primary btn_rightAlign")
    } 
})

output$dmap_delete_ui<- renderUI({
    if(is.null(input$data_inter_net_selected) || is.null(r_data$his_nodes)) return()
    s <- which(r_data$his_nodes$name == input$data_inter_net_selected)
    if(!length(s)) return()
    if(is.na(r_data$his_nodes$is_activated[s])) {
        actionButton("dmap_del_report", "Delete Analysis Report", class = "btn-danger btn_leftAlign")
    } else if(s != 1){
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
        if(s != 1){
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
    
    r_data$raw <- r_data$glb.raw[r_data$feature_list, r_data$sample_name]
    r_data$norm_param <- r_data$history[[s]]$norm_param
    r_data$df <- r_data$history[[s]]$lists[[3]] # This might take some memory, but for now it's the best solution
    
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
    
    if (length(s) == 1) {    
        s <- which(r_data$his_tbl$name == s)
        withProgress(message = 'Switching dataset, please wait...', value = 50, {
            r_data <- switch_to_dataset(r_data, s)
        })
    } else {
        return()
    }
})

# Graph: switch to selected dataset

observeEvent(input$dmap_switch, {
    if(is.null(r_data$his_tbl)) return()
    s <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    if (length(s) == 1) {    
        withProgress(message = 'Switching dataset, please wait...', value = 50, {
            r_data <- switch_to_dataset(r_data, s)
        })
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


###### Rename nodes ######
observeEvent(input$dmap_rename_btn, {
    if(is.null(input$data_inter_net_selected) || input$data_inter_net_selected == "") {
        session$sendCustomMessage(type = "showalert", "Please select a node on the data map.")
        return()
    }
    
    if(is.null(input$dmap_rename_new) || input$dmap_rename_new == "" || nchar(trimws(input$dmap_rename_new)) == 0) {
        session$sendCustomMessage(type = "showalert", "Please enter a valid name.")
        return()
    }
    
    new_name <- trimws(input$dmap_rename_new)
    
    s1 <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    
    if(length(s1)) {        # update dataset history
        if(new_name %in% as.character(r_data$his_tbl$name)){ # For data, duplicated name are not allowed
            session$sendCustomMessage(type = "showalert", "Name has been taken by existing datasets.")
            return()
        }
        r_data$history[[s1]]$name <- new_name
        r_data$his_tbl$name[s1] <- new_name
        # Also need to update the parent relationship for BOTH data and analysis 
        # For data
        p_idx <- which(r_data$his_tbl$parent_data == input$data_inter_net_selected)
        r_data$his_tbl$parent_data[p_idx] <- new_name
        for(idx in p_idx) {
            r_data$history[[idx]]$parent_data <- new_name
        }
        # For analysis
        p_idx <- which(r_data$reg_tbl$parent_data == input$data_inter_net_selected)
        r_data$reg_tbl$parent_data[p_idx] <- new_name
        for(idx in p_idx) {
            r_data$reg_history[[idx]]$parent_data <- new_name
        }
        toggleModal(session, "dmap_rename_modal", toggle = "close")
    } else {
        s2 <- which(paste(r_data$reg_tbl$name, r_data$reg_tbl$time) == input$data_inter_net_selected)
        if(length(s2)) {        # update analysis history
            # Analysis can take same names because the data node name used for linking is name+time
            r_data$reg_history[[s2]]$name <- new_name
            r_data$reg_tbl$name[s2] <- new_name
            toggleModal(session, "dmap_rename_modal", toggle = "close")
        }
    }
})


###### Add Notes #######
observeEvent(input$dmap_notes_btn, {
    if(is.null(input$data_inter_net_selected) || input$data_inter_net_selected == "") {
        session$sendCustomMessage(type = "showalert", "Please select a node on the data map.")
        return()
    }
    
    if(is.null(input$dmap_notes_content)) {
        return()
    }
    
    new_notes <- trimws(input$dmap_notes_content)
    
    s1 <- which(r_data$his_tbl$name == input$data_inter_net_selected)
    
    if(length(s1)) {        # update dataset history
        r_data$history[[s1]]$user_notes <- new_notes
        r_data$his_tbl$user_notes[s1] <- new_notes
        toggleModal(session, "dmap_notes_modal", toggle = "close")
    } else {
        s2 <- which(paste(r_data$reg_tbl$name, r_data$reg_tbl$time) == input$data_inter_net_selected)
        if(length(s2)) {        # update analysis history
            r_data$reg_history[[s2]]$user_notes <- new_notes
            r_data$reg_tbl$user_notes[s2] <- new_notes
            toggleModal(session, "dmap_notes_modal", toggle = "close")
        }
    }
})

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
            withProgress(message = 'Switching dataset, please wait...', value = 50, {
            r_data <- switch_to_dataset(r_data, 1)
            })
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
            withProgress(message = 'Switching dataset, please wait...', value = 50, {
            r_data <- switch_to_dataset(r_data, 1)
            })
        }
        
        # Remove the current subset node
        r_data$history <- r_data$history[-s]
        r_data$his_tbl <- r_data$his_tbl[-s,]
    }
})




