options(shiny.maxRequestSize=50*1024^2) 
library(data.table)
library(corrplot)
library(mlbench)
library(InformationValue)
options(scipen = 999)

data = data.table(read.csv('train.csv'))
# data = data.table(input$data_file)

shinyServer(function(input, output, clientData, session) {
	
	mydata <- reactive({
		
			if(is.null(input$data_file)){
				data
			
			} else {
				try(data.table(read.csv(input$data_file$datapath)),silent = T)
			}
		
		
	})
	output$target_variable = renderUI({
	
		if (is.null(mydata()))	return(selectInput("target_variable", "Target variable:",  c("")), selected = "")
		# validate(need(!is.null(mydata())&(mydata()$target_variable != '')&levels(as.factor(mydata()[[input$target_variable]])) %in% c('0','1')), "Please select Target variable with '0' and '1' values/levels.")
		
		selectInput("target_variable", "Target variable:", c("", colnames(mydata())), selected = "")
		})
	output$ID_column = renderUI({
		
		if (is.null(mydata()))	return(selectInput("ID_column", "ID column:",  c("")), selected = "")
		selectInput("ID_column", "ID column:",  c("", colnames(mydata())), selected = "")
	})
# 	output$selectUI <- renderUI({ 
# 		selectInput("target_class", "Target class:", levels(as.factor(data[[input$target_variable]])), selected = "")
# 	})
# 		observeEvent(	dataInput(),{
# 		progress <- shiny::Progress$new()
# 		progress$set(message = "Tuning the model...", value = 0)
# 		# Close the progress when this reactive exits (even if there's an error)
# 		on.exit({ progress$close() })
# 		})
		dataInput <- reactive({
			
			
			try(robust_scoring(mydata(),target_variable = input$target_variable,ID_column = input$ID_column, training_perc = input$training_perc, good_perc = input$good_perc),silent = T)
		})
		inputCutoff =  reactive({
			try(ifelse(dataInput()$prob$prob > input$cut_off,1,0),silent = T)
		})
		inputCutoffVal =  reactive({
			try(input$cut_off,silent = T)
		})
	
	output$data <- renderDataTable({
		progress <- shiny::Progress$new(session)
		on.exit(progress$close())
		
		progress$set(message = 'Uploading the dataset...',
					 detail = 'Stay tuned!')
	
		mydata()

	}, options = list(pageLength = 20))
	

	output$woe_table <- renderDataTable({
		if(class(dataInput()) != 'list')	return()
		# 		validate(need(input$first_epicenter_name != "", "Please specify both epicenters and click 'Run Calc' button."))
		# 		validate(need(input$second_epicenter_name != "", "Please specify both epicenters and click 'Run Calc' button."))
		
		
		# m = robust_scoring(data,input$target_variable)
		
		# res = data.table(rbindlist(m$woe_table))
		res = data.table(rbindlist(dataInput()$woe_table))
		res

	}, options = list(pageLength = 20))
	
	output$scorecard <- renderDataTable({
		if(class(dataInput()) != 'list')	return()
		
		res = data.table(dataInput()$scorecard)
		res

	}, options = list(pageLength = 20))
	
	output$reg_coef <- renderDataTable({
		if(class(dataInput()) != 'list')	return()
		res = 	cbind(rownames(summary(dataInput()$glm)$coefficients ),summary(dataInput()$glm)$coefficients)
		
	}, options = list(pageLength = 20))
# 	output$reg_coef <- renderPrint({
# 	
# 		print(summary(dataInput()$glm))
# 	})
	
	output$corr_matrix <- renderPlot({
		if(class(dataInput()) != 'list')	return()
		corrplot(dataInput()$corr_matrix, method = "number")
	})
	
	output$ROC <- renderPlot({
		   progress <- shiny::Progress$new(session)
		    on.exit(progress$close())
		
	       progress$set(message = 'Building the model...',
		                 detail = 'Stay tuned!')
		if(class(dataInput()) != 'list')	return()
		plot(dataInput()$roc)
		title(main = sprintf('ROC CURVE. AUC = %f, Gini = %f',as.numeric(dataInput()$auc),2*as.numeric(dataInput()$auc)-1), ,line = 3)
	})
	
	output$probabilities <-  renderDataTable({
		if(class(dataInput()) != 'list')	return()
		res = dataInput()$prob
		res = res[,`:=`(PredictedClass = inputCutoff())]
		res
		
	}, options = list(pageLength = 20))
	
	output$error_matrix <- renderDataTable({
		if(class(dataInput()) != 'list')	return()
		res = dataInput()$prob
		res = table(res$prob > inputCutoffVal(), res$TARGET)
		res = cbind(c('Predicted Class = 0','PredictedClass = 1'),res)
		colnames(res) = (c('','Target Class = 0','Target Class = 1'))
		res
		
	}, options = list(pageLength = 20))
	
	output$downloadtable <- downloadHandler(#### called from UI
        filename = function() {paste('scorecard_',format(Sys.time(), "%Y%M%d_%H%M%S"),'.csv', sep='')},
        content = function (file){
			if(class(dataInput()) != 'list') {
				write.csv(data.table(a= c(1,2),b = c(3,4)))
			} else {
			res = data.table(dataInput()$scorecard)
			write.csv(res,file)
			}
		}
	)
    
	
	
	session$onSessionEnded(function() {
		stopApp()
	})
	

})
