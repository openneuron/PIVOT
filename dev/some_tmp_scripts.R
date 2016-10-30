 # Add group info to global
if("group" %in% design_in_tbl) {
    
    sorted_group<-df_tmp$Group[match(colnames(r_data$glb.raw), df_tmp$Sample)]
    names(sorted_group) <- colnames(r_data$glb.raw)
    
    # Reset the group for analysis
    if(length(unique(sorted_group)) < 2) {
        session$sendCustomMessage(type = "showalert", "Seems only 1 group there!")
        return()
    } else {
        r_data$glb.group <- sorted_group
        r_data$group <- factor(sorted_group[which(names(sorted_group) %in% r_data$sample_name)])
    }
}

# Add batch info to global
if("batch" %in% design_in_tbl) {
    
    sorted_batch<-df_tmp$Group[match(colnames(r_data$glb.raw), df_tmp$Sample)]
    names(sorted_batch) <- colnames(r_data$glb.raw)
    
    # Reset the group for analysis
    if(length(unique(sorted_batch)) < 2) {
        session$sendCustomMessage(type = "showalert", "Seems only 1 batch there!")
        return()
    } else {
        r_data$glb.batch <- sorted_batch
        r_data$batch <- sorted_batch[which(names(sorted_batch) %in% r_data$sample_name)]
    }
}