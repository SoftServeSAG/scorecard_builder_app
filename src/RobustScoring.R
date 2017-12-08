#' @export
robust_scoring = function(data, target_variable = 'TARGET',ID_column = 'ID',training_perc = 50, good_perc = 0) {
  
	library(data.table)

	
	dt = copy(data)

	
	library(mlbench)
	library(caret)
	library(e1071)
	
	
	library(InformationValue)
	library(ggplot2)
	
	
	library(woe)
	
	#source('asplit.R')
	
	if(target_variable == 'TARGET'){
		dt[,':='(TARGET = factor(TARGET))]
	} else {
		if('TARGET' %in% colnames(dt)){dt=dt[,!c('TARGET'),with=F]}
		dt$TARGET = factor(dt[[target_variable]])
		tar = dt[[target_variable]]
		dt = dt[,!c(target_variable),with=F]
	}
	
	# summary(dt$TARGET)
	
	dt2 = copy(dt)
	ID = dt[[ID_column]]
	dt=dt[,!c(ID_column),with=F]
	

	
	for (col in colnames(dt)){
		if (col != 'TARGET'){
			if(length(unique(dt[[col]]))>1) {dt[[col]] = asplit(dt[[col]])}
			if(length(unique(dt[[col]]))==1){dt=dt[,!c(col),with=F]}	
		}
		
	}
	
	size = nrow(dt)
	for (col in colnames(dt)){
		if (col != 'TARGET' & !prod(summary(dt[[col]]) / size < .97)){
			
			dt=dt[,!c(col),with=F]
		}
		
	}
	
	woe_table = list()
	for (col in colnames(dt)){
		if (col != 'TARGET'){
			woe_table_tmp = woe(Data=dt,col,FALSE,"TARGET",5,Bad=0,Good=1)
			woe_table[[col]] = woe_table_tmp
			# sum(woe_table$IV)
		}
	}
	
	
	woe_table = list()
	var_good = c()
	for (col in colnames(dt)){
		if (col != 'TARGET'){
			woe_table_tmp = woe(Data=dt,col,FALSE,"TARGET",5,Bad=0,Good=1)
			woe_table_tmp$attribute = col
			if(sum(	woe_table_tmp$IV) <  0.02) dt=dt[,!c(col),with=F]
			if(sum(	woe_table_tmp$IV) >= 0.02) {
				var_good = c(var_good,col)
				setcolorder(woe_table_tmp,c('attribute',colnames(woe_table_tmp)[-length(colnames(woe_table_tmp))]))
				woe_table[[col]] = woe_table_tmp
			}
		}
	}
	
	res = list()
	res$woe_table  = woe_table
	
# 	summary(dt$TARGET)
# 	var_good
# 	
	
	dt_new = subset(dt2,,c(var_good,'TARGET')) 
	dt_new = dt_new[, lapply(.SD, as.numeric), by = 'TARGET']
	TARGET = dt$TARGET

	
	res$corr_matrix = cor(dt_new[,!c('TARGET'),with = F])
	# View(corr_matrix)
	
	library(corrplot)
	# corr_matrix = cor(dt_new[,!c('TARGET'),with = F])
# 	par(cex=0.4)
# 	corrplot(corr_matrix, method = "number",tl.cex = 2,cl.cex=2)
# # 	par(cex=1)
# 	abs(0.95-corr_matrix)<0.05
# 	
# 	colnames((abs(0.95-corr_matrix)<0.05))
		
# 	if('num_var35' %in% var_good) {
# 		dt = dt[,!c('num_var35'),with = F]
# 		var_good = var_good[var_good!='num_var35']
# 		woe_table = woe_table[woe_table!='num_var35']
# 	}
	var_bad = findCorrelation(res$corr_matrix, 0.90, names = T)
	var_good = var_good[!var_good %in% var_bad]
	woe_table = woe_table[!names(woe_table) %in% var_bad]
	
	dt_new = subset(dt2,,c(var_good,'TARGET')) 
	dt_new = dt_new[, lapply(.SD, as.numeric), by = 'TARGET']
	res$corr_matrix = cor(dt_new[,!c('TARGET'),with = F])
	
	
	
	# for (col in colnames(training)){
	# 	if (col != 'TARGET'){
	# 		if(length(unique(training[[col]]))>1) {training[[col]] = asplit(training[[col]])}
	# 		if(length(unique(training[[col]]))==1){training=training[,!c(col),with=F]}	
	# 	}
	# 	
	# }
	
	dt = cbind(ID,dt)
	
	if(good_perc != 0){
		set.seed(11)
		good_subset = dt[TARGET == '1']
		bad_subset = dt[TARGET == '0']
		if(nrow(good_subset) / size <= training_perc * good_perc / 10000){
			set.seed(11)
			inTraining = createDataPartition(seq_len(size), p = training_perc / 100 , list = FALSE)
			training = dt[inTraining]
			testing = dt[-inTraining]
		} else {
			set.seed(2)
			inTrainingGood = createDataPartition(seq_len(nrow(good_subset)), p = training_perc * good_perc * size / nrow(good_subset) / 10000, list = FALSE)
			TrainGood = good_subset[inTrainingGood]
			TestGood = good_subset[-inTrainingGood]
			set.seed(11)
			inTrainingBad = createDataPartition(seq_len(nrow(bad_subset)), p = training_perc * (100 - good_perc) * size / nrow(bad_subset) / 10000, list = FALSE)
			TrainBad = bad_subset[inTrainingGood]
			TestBad = bad_subset[-inTrainingGood]
			
			training =  rbind(TrainGood, TrainBad)
			testing = rbind(TestBad, TestGood)
		}
		
		
	} else {
		set.seed(11)
		inTraining = createDataPartition(seq_len(size), p = training_perc / 100 , list = FALSE)
		training = dt[inTraining]
		testing = dt[-inTraining]
	}
	
	# training$TARGET  = as.numeric(training$TARGET) -1
	
	
	hit.glm = glm(training$TARGET ~ .,data = training[,!c('TARGET','ID'),with=F], family = binomial(link = "logit"))
	# rocplot(hit.glm,diag=TRUE,pred.prob.labels=FALSE,prob.label.digits=3,AUC=TRUE)
	# summary(hit.glm)
	res$glm = hit.glm
	library(pROC)
	
	score_def = 500
	odds_def = 20
	points_to_double = 20
	
	factor_sc = points_to_double / log(2)
	offset = score_def - points_to_double * log(odds_def) / log(2)
	
	coef_model = data.table(t(coef(hit.glm)))
	a_intercept = as.numeric(coef_model[,'(Intercept)'])
	coef_model = coef_model[,!c('(Intercept)'), with = F]
	coef_model = t(coef_model)
	count_vars = length(coef_model)
	
	scorecard = (-coef_model - a_intercept / count_vars) * factor_sc + offset / count_vars
	scorecard = data.table(Attribute = rownames(scorecard), Score = as.numeric(round(ifelse(scorecard>0,scorecard,0))))
	
	res$scorecard = scorecard
	
	prob=predict.glm(hit.glm, newdata = testing[,!c('TARGET','ID'),with=F],type=c("response"))
	
	prob1=predict(hit.glm,type=c("response"))
	
	training$prob=prob1
	testing$prob = prob
	
	dt3 = rbind(training,testing)
	
	# g = roc(TARGET ~ prob, data = testing )
	g = roc(TARGET ~ prob, data = dt3 )
	# g = roc(TARGET ~ prob, data = training )
	# plot(g) 
	# auc(training$TARGET, training$prob)
	res$roc = g
	res$auc =	auc(testing$TARGET, testing$prob)
	
# 	sum(prob[testing$TARGET==1]>0.1)
# 	summary(hit.glm)
	
	# anova(hit.glm, test="Chisq")
	
	res$error_matrix_test = table(prob>0.11,testing$TARGET)
	
	res$error_matrix_train = table(prob1>0.11,training$TARGET)
	
# 	hist(log(prob),15)
# 	
# 	alp = 500 - (50 * log(20) / log(10))
# 	bet = 50 / log (10)
# 	as.vector(hit.glm$coefficients)
# 	
# 	score = alp + as.vector(hit.glm$coefficients) * bet
# 	score = score - score[1]
# 	score
# 	
# 	testing$odds = prob/
# 		
# 		
# 		
# 		
	res$data_amended = rbind(testing,training)
	res$prob = dt3[,list(ID,TARGET,prob)]
    
    return(res)
}

#' @export
asplit = function(vec, vec_ref = c('1'),if_ref=0,num=5){
	if (!if_ref) {
		vec_ref = vec	
	} 
		
	if(class(vec) %in% c('numeric','integer')){
		seq = c(0,seq_len(num)/num)
		quan = unique(quantile(vec_ref,probs =seq,na.rm = T))
		fact = cut(vec, quan, labels = NULL,include.lowest = T, right = T, dig.lab = 3,ordered_result = F)
		return(fact)
	} else {
		return(as.factor(vec))
	}
	
}
