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



######### caret #########

caret_data <- reactiveValues()
caret_data$model <- NULL
caret_data$cv_result <- NULL
caret_data$train_group <- NULL
caret_data$train_data <- NULL
caret_data$test_data <- NULL
caret_data$test_group <- NULL
caret_data$test_result <- NULL

# load method table
caret_model_tbl <- read.csv("src/built_in_files/caret_model_tbl.csv")
# training method 
output$caret_train_method_ui <- renderUI({
    model_tbl <- caret_model_tbl %>% 
        dplyr::filter(BB == 1) %>% 
        dplyr::select(Model, method.Argument.Value)
    all_methods <- as.list(model_tbl$method.Argument.Value)
    names(all_methods) <- model_tbl$Model
    selectInput("caret_model_method", label = "Modeling method",  choices = all_methods, selected = "glm")
})

# Training and Cross-validation
output$caret_train_gp_ui <- renderUI({
    caret_train_gp_choices <- as.list(unique(r_data$group))
    names(caret_train_gp_choices) <- unique(r_data$group)
    selectizeInput('caret_train_gp', label = "Training Groups", choices = caret_train_gp_choices, multiple = TRUE, selected = caret_train_gp_choices)
})



observeEvent(input$caret_train_gp_btn,{
    if(is.null(data0()) || is.null(r_data$group) || length(unique(r_data$group)) < 2) return ()
    if(nrow(data0()) > 500) {
        session$sendCustomMessage(type = "showalert", "Please filter features to less than 500.")
        return()
    }
    
    if(is.null(input$caret_train_gp)){
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return()
    }
    error_I <- 0
    
    withProgress(message = 'Processing', value = 0.8, {
        selected_group <- input$caret_train_gp
        sample_list <- r_data$sample_name
        group_list <- r_data$group
        names(group_list) <- sample_list
        train_group <- group_list[which(group_list %in% selected_group)]
        train_sample <- names(train_group)
        caret_data$train_group <- factor(unname(train_group))
        caret_data$train_data<-t(data0())[train_sample,]
        set.seed(input$idv_seed)
        fitControl <- caret::trainControl(## 10-fold CV
            method = "repeatedcv",
            number = input$caret_cv_fold,
            ## repeated ten times
            repeats = input$caret_cv_repeat)
        
        tryCatch({
            caret_data$model <- caret::train(x = caret_data$train_data, y = caret_data$train_group, method = input$caret_model_method, trControl = fitControl)
        }, 
        error = function(e){
            session$sendCustomMessage(type = "showalert", "Modeling failed!")
            error_I <<- 1
        }
        )
    })
    
    if(error_I) return()
    #caret_data$cv_result <- caret.cv(caret_data$train_data, caret_data$train_group, k = input$caret_k, prob = TRUE)
})

output$caret_training_tbl <- DT::renderDataTable({
    if(is.null(data0()) || is.null(r_data$group) || length(unique(r_data$group)) < 2) return ()
    DT::datatable(caret_data$train_data, options = list(scrollX = TRUE, scrollY = "350px", lengthMenu = c(20, 50, 100)))
})

output$caret_model_result <- renderPrint({
    caret_data$model
})

output$caret_model_coef <- DT::renderDataTable({
    if(is.null(caret_data$model)) return ()
    error_I <- 0
    tryCatch({
        a <- as.data.frame(caret_data$model$finalModel$coefficients)
        a <- a %>% tibble::rownames_to_column()
        colnames(a) <- c("feature", "coefficients")
        tbl <- a[order(abs(a$coefficients), decreasing = T),]
    }, error = function(e) {
        error_I <<- 1
    })
    
    if(error_I) return(data.frame(error_message = c("This information is not available.")))
    
    return(tbl)
})


# Testing

output$caret_test_gp_ui <- renderUI({
    test_gp_choices <- as.list(unique(r_data$group))
    names(test_gp_choices) <- unique(r_data$group)
    selectizeInput('caret_test_gp', label = "Testing Groups", choices = test_gp_choices, multiple = TRUE, selected = test_gp_choices)
})

observeEvent(input$caret_test_gp_btn, {
    if(is.null(data0()) || is.null(r_data$group) || length(unique(r_data$group)) < 2) return ()
    if(is.null(caret_data$model)) {
        session$sendCustomMessage(type = "showalert", "Please train the model first!")
        return ()
    } 
    if(is.null(input$caret_test_gp)){
        session$sendCustomMessage(type = "showalert", "Give me something!")
        return()
    }
    
    withProgress(message = 'Processing', value = 0.8, {
        selected_group <- input$caret_test_gp
        sample_list <- r_data$sample_name
        group_list <- r_data$group
        names(group_list) <- sample_list
        test_group <- group_list[which(group_list %in% selected_group)]
        test_sample <- names(test_group)
        caret_data$test_group <- unname(test_group)
        caret_data$test_data<-t(data0())[test_sample,]
        
        caret_data$test_result <- stats::predict(caret_data$model, newdata = caret_data$test_data)
    })
    
})

output$caret_test_tbl <- DT::renderDataTable({
    if(is.null(caret_data$test_data)) return ()
    DT::datatable(caret_data$test_data, options = list(scrollX = TRUE, scrollY = "350px", lengthMenu = c(20, 50, 100)))
})

caret_result_tbl <- reactive({
    cbind(sample = rownames(caret_data$test_data), actual_group = as.character(caret_data$test_group), assigned_group = as.character(caret_data$test_result))
})

output$caret_test_result_tbl <- DT::renderDataTable({
    if(is.null(caret_data$test_result)) return ()
    DT::datatable(caret_result_tbl(), options = list(scrollX = TRUE, scrollY = "450px", lengthMenu = c(20, 50, 100)))
})

output$download_caret_test_result <- downloadHandler(
    filename = "caret_test_result.csv",
    content = function(file) {
        write.csv(caret_result_tbl(), file, row.names = F)
    }
)

output$caret_confusionmatrix <- renderPrint({
    if(is.null(caret_data$test_result)) return ()
    error_I <- 0
    tryCatch({
        cm<-confusionMatrix(caret_data$test_result, caret_data$test_group)
    }, error = function(e) {
        error_I <<- 1
    })
    
    if(error_I) return("Confusion matrix not available.")
    return(cm)
})
