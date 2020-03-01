library (party)
library (rattle)
library(Hmisc)
library(pROC)
library(MLmetrics)
library(varImp)

#data individuals
gerundata <- file.choose()
df = read.csv(gerundata, header=TRUE)

for (author in unique(df$author)) {
  t <- ctree(gerund ~ age + det + func + verb_type + genre, data=df[df$author == author,])
  pdf(paste("[INSERT YOUR PATH", author, ".pdf", sep=""), width=12)
  plot(t, type = "extended", drop_terminal = TRUE)
  dev.off()
}

compute_imp <- function(author, return_table=T) {
  d <- df[df$author == author,]
  for (level in levels(d$det)) {
    d[,level] <- ifelse(d$det == level, 1, 0)
  }
  for (level in levels(d$func)) {
    d[,toupper(level)] <- ifelse(d$func == level, 1, 0)
  }
  for (level in levels(d$genre)) {
    d[,level] <- ifelse(d$genre == level, 1, 0)
  }
  for (level in levels(d$verb_type)) {
    d[,level] <- ifelse(d$verb_type == level, 1, 0)
  }
  levels(d$func) <- toupper(levels(d$func))
  det_factors <- paste(as.vector(levels(d$det)), collapse=" + ")
  func_factors <- paste(as.vector(levels(d$func)), collapse=" + ")
  genre_factors <- paste(as.vector(levels(d$genre)), collapse=" + ")
  verb_factors <- paste(as.vector(levels(d$verb_type)), collapse=" + ")
  formula = as.formula(paste("gerund ~ age", det_factors, func_factors, genre_factors, verb_factors,
                             sep=" + "))
  f <- cforest(formula, data=d)
  var <- varimpAUC(f) #more robust towards class imbalance
  var <- var[order(var)]
  varr = as.data.frame(var)
  if (return_table) {
    return(varr)
  } else {
    return(rownames(varr))
  }
}

varrs = sapply(unique(df$author), FUN=compute_imp)
vals <- rev(apply(varrs, 1, function (r) max(table(r)) / length(unique(df$author)))[15:24])
vnames <- rev(apply(varrs, 1, function (r) rownames(table(r))[which.max(table(r))])[15:24])
for (i in 1:length(vnames)) {
  vnames[i] <- paste(i, "-", vnames[i])
}
plot(vals, xaxt="n", pch=19, ylab="fraction of authors for which predictor is ranked at position n", xlab="ranked list of predictors")
axis(1, at=1:10, labels=vnames)

plotimp <- function(author) {
  varr <- compute_imp(author)
  varr <- tail(varr, 10)
  varr$functions = factor(rownames(varr), levels=rownames(varr))
  p <- ggplot(varr, aes(
    x=var, y=functions)) + 
    geom_point() + xlim(0, 0.2) + theme_minimal() + 
    ylab("") + xlab("") + ggtitle(author)
  p
}

plots <- lapply(unique(df$author), FUN = plotimp)
plot_grid(plotlist=plots, ncol = 5)
