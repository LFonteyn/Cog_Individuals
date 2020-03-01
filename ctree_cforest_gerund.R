library (party)
library (rattle)
library(Hmisc)
library(pROC)
library(MLmetrics)
library(varImp)

#data set 
gerundata <- file.choose()
df = read.csv(gerundata, header=TRUE)

#Build conditional inference tree with gerund as dependent variable
ctree_type <- ctree(gerund ~ generation + age + author + det + func + verb_type + genre, data=df)
plot(ctree_type, type = "extended", drop_terminal = TRUE)

df$age_c = df$age-mean(df$age)

ctree_type <- ctree(gerund ~ generation + age_c + author + det + func + verb_type + genre, data=df)
plot(ctree_type, type = "extended", drop_terminal = TRUE)

ctree_type <- ctree(gerund ~ generation + age + author + det + func + verb_type + genre + gen_age, data=df)
df$det_func <- as.factor(paste(df$det, df$func, sep=":"))
df$gen_age <- as.integer(df$generation)*df$age

#abbreviated ctree
ctree_short <- ctree(gerund ~ generation + age + author + det + func + verb_type + genre, data=df, controls=ctree_control(maxdepth = 4, minbucket = 5))
plot(ctree_short, type = "extended", drop_terminal = TRUE)

#model fit simple
ctree_type.pred <- unlist(treeresponse(ctree_type))[c(FALSE, TRUE)]
somers2(ctree_type.pred, as.numeric(df$gerund)- 1)
table(predict(ctree_type), df$gerund)

(2606+9504)/nrow(df) #accuracy

#model fit complex
df = sample(df) # shuffle dataset
smp_size <- floor(0.75 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

ctree_train <- ctree(gerund ~ generation + age + author + det + func + verb_type + genre, data=train)
plot(ctree_train)
preds <- predict(ctree_train, newdata=test)

#auc residuals
roc.results <- roc(as.numeric(test$gerund)-1, as.numeric(preds)-1)
auc(roc.results)
#f1-score
F1_Score(as.numeric(test$gerund)-1, as.numeric(preds)-1) 
#f1-score macro weighted
F1_Score_macro_weighted(as.numeric(test$gerund)-1, as.numeric(preds)-1)
#f1-score micro
F1_Score_micro(as.numeric(test$gerund)-1, as.numeric(preds)-1) 
#accuracy
Accuracy(as.numeric(test$gerund)-1, as.numeric(preds)-1)


#Build conditional inference forest with gerund as dependent variable
cforest_type <- cforest(
  gerund ~ generation + age + author + det + func + verb_type + genre, data=df, 
  controls=cforest_control(mtry = 7, ntree=1000)
)
#Varimp
varimp(cforest_type)
var <- varImpAUC(cforest_type) #more robust towards class imbalance

#PLOT VARIMP
var <- var[order(var)]
varr = as.data.frame(var)
varr$functions = factor(rownames(varr), levels=rownames(varr))
ggplot(varr, aes(
  x=var, y=functions)) + 
  geom_point() + theme_minimal() + 
  ylab("") + xlab("")

#model residuals RF
cforest_train <- cforest(
  gerund ~ generation + age + author + det + func + verb_type + genre, 
  data=train, controls=cforest_control(mtry = 6, ntree=1000)
)
preds.rf <- predict(cforest_train, newdata=test)
roc.results.rf <- roc(as.numeric(test$gerund)-1, as.numeric(preds.rf)-1)
auc(roc.results.rf) 
F1_Score(as.numeric(test$gerund)-1, as.numeric(preds.rf)-1)
F1_Score_micro(as.numeric(test$gerund)-1, as.numeric(preds.rf)-1)
Accuracy(as.numeric(test$gerund)-1, as.numeric(preds.rf)-1)
