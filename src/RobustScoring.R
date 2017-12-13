#' @export
robust_scoring = function(data, target_variable = 'TARGET',ID_column = 'ID',training_perc = 50, good_perc = 0) {
  
	library(data.table)
  library(mlbench)
  library(caret)
  library(e1071)
  library(InformationValue)
  library(ggplot2)
  library(woe)
  library(polycor)
  library(dplyr)
  library(gdata)
	
	dt = copy(data)
	dt = dt[complete.cases(dt), ]
	for (col in colnames(dt)){
	    dt = dt[!is.na(dt[[col]])]
	    if (class(dt[[col]]) %in% c('character','factor')){
	      if (sum(dt[[col]] != '') / nrow(dt) < 0.8) {
	        dt[,(col) := NULL] 
	      } else {
	        dt = dt[dt[[col]] != '']
	      }
	    }
	}

	dt = drop.levels(dt)
	
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
	    if(length(levels(dt[[col]])) > 8) {dt=dt[,!c(col),with=F]}	
	  }
	  
	}
	
	for (col in colnames(dt)){
		if (col != 'TARGET'){
			if(length(unique(dt[[col]]))>1) {dt[[col]] = asplit(dt[[col]])}
			if(length(unique(dt[[col]]))==1){dt=dt[,!c(col),with=F]}	
		}
		
	}
	
	size = nrow(dt)
	for (col in colnames(dt)){
		if (col != 'TARGET' & (!prod(summary(dt[[col]]) / size < .97))){
			
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
	# dt_new = dt_new[, lapply(.SD, as.numeric), by = 'TARGET']
	TARGET = dt$TARGET

	
	res$corr_matrix = hetcor(data.frame(dt_new[,!c('TARGET'),with = F]))
	 
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
	var_bad = findCorrelation(res$corr_matrix$correlations, 0.90, names = T)
	var_good = var_good[!var_good %in% var_bad]
	woe_table = woe_table[!names(woe_table) %in% var_bad]
	
	dt_new = subset(dt2,,c(var_good,'TARGET')) 
	dt_new = dt_new[, lapply(.SD, as.numeric), by = 'TARGET']
	res$corr_matrix = hetcor(data.frame(dt_new[,!c('TARGET'),with = F]))	
	
	
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
		training = copy(dt[inTraining])
		testing = copy(dt[-inTraining])
	}
	
	# training$TARGET  = as.numeric(training$TARGET) -1
	
	# for (col in colnames(dt)){
	#   if (col != 'TARGET'){
	#     training[[col]] = factor(training[[col]])
	#     levels(testing[[col]]) = levels(training[[col]])
	#   }
	# }
	
	hit.glm = glm(training$TARGET ~ . - TARGET - ID-1,data = training, family = binomial(link = "logit"))
	# rocplot(hit.glm,diag=TRUE,pred.prob.labels=FALSE,prob.label.digits=3,AUC=TRUE)
	# summary(hit.glm)
	res$glm = hit.glm
	library(pROC)
	
	score_def = 500
	odds_def = 10
	points_to_double = 20
	
	factor_sc = points_to_double / log(2)
	offset = score_def - points_to_double * log(odds_def) / log(2)
	
	coef_model = data.table(t(coef(hit.glm)))
	# a_intercept = as.numeric(coef_model[,'(Intercept)'])
	# coef_model = coef_model[,!c('(Intercept)'), with = F]
  coef_model = t(coef_model)
	# count_vars = length(coef_model)
	# count_vars = 6
	scorecard = coef_model * factor_sc 
	# scorecard = data.table(Attribute = rownames(scorecard), Score = as.numeric(round(ifelse(scorecard>0,scorecard,0))))
	scorecard = data.table(Attribute = rownames(scorecard), Score = as.numeric(scorecard))
	sc = GenerateScorecard(training,scorecard)
	count_vars = length(unique(sc$Characteristic))
	sc[, Score := round(ifelse(Score + offset / count_vars>0,Score + offset / count_vars,0))]
	# res$scorecard = scorecard
	
	prob=predict.glm(hit.glm, newdata = remove_missing_levels(hit.glm,testing),type=c("response"))
	
	prob1=predict(hit.glm,type=c("response"))
	
	training$prob=prob1
	testing$prob = prob
	
	res$scorecard = sc
	
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
  if (length(unique(vec)) < 6) {
    return(as.factor(vec))
  }	else if(class(vec) %in% c('numeric','integer')){
		seq = c(0,seq_len(num)/num)
		quan = unique(quantile(vec_ref,probs =seq,na.rm = T))
		fact = cut(vec, quan, labels = NULL,include.lowest = T, right = T, dig.lab = 3,ordered_result = F)
		return(fact)
	} else {
		return(as.factor(vec))
	}
	
}

remove_missing_levels <- function(fit, test_data) {
  
  # https://stackoverflow.com/a/39495480/4185785
  
  # drop empty factor levels in test data
  test_data %>%
    droplevels() %>%
    as.data.frame() -> test_data
  
  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$contrasts))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    map(fit$contrasts, function(x) names(unmatrix(x))) %>%
      unlist() -> factor_levels
    factor_levels %>% str_split(":", simplify = TRUE) %>%
      extract(, 1) -> factor_levels
    
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$xlevels))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }
  
  # Select column names in test data that are factor predictors in
  # trained model
  
  predictors <- names(test_data[names(test_data) %in% factors])
  
  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA
  
  for (i in 1:length(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data %>%
        droplevels() -> test_data
      # issue warning to console
      message(sprintf(paste0("Setting missing levels in '%s', only present",
                             " in test data but missing in train data,",
                             " to 'NA'."),
                      var))
    }
  }
  return(test_data)
}

GenerateScorecard = function(training,scorecard) {
  
  card = data.table(
    Characteristic = character(),
    Attribute = character()
  )
  
  
  for (col in colnames(training)){
    if (!col %in% c('TARGET','ID','prob')) {
      tmp = data.table(cbind(rep(col,length(unique(training[[col]]))),levels(training[[col]])))
      colnames(tmp) = c('Characteristic','Attribute')
      card = rbindlist(list(card,tmp),use.names = T)
    }
  }
  
  card[,`:=`(Score = 0, CA = paste0(Characteristic,Attribute))]
  
  setkeyv(card, 'CA')
  setkeyv(scorecard, 'Attribute')
  card[scorecard, Score := i.Score]
  card
  
  card[,SumScore := sum(Score),by = 'Characteristic']
  card = card[SumScore != 0]
  
  card[,`:=`(SumScore = NULL, CA = NULL)]
  card
}