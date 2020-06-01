tpr <- function(cm) {if (is.nan(p <- cm[2,2]/(cm[2,2] + cm[1,2]))) 1 else p}
fpr <- function(cm) {if (is.nan(p <- cm[2,1]/(cm[2,1] + cm[1,1]))) 1 else p}
precision <- function(cm) {if (is.nan(p <- cm[2,2]/(cm[2,2] + cm[2,1]))) 1 else p}
recall <- tpr
f.measure <- function(cm) {1/mean(c(1/precision(cm), 1/recall(cm)))}
confmat <- function(pred.y, true.y)
{table(pred.y, true.y, dnn=c("predicted","true"))}


# per-class 1 vs rest confusion matrix

confmat01 <- function(pred.y, true.y)
{
  `names<-`(lapply(levels(true.y),
                    function(d)
                    {
                      cm <- confmat(factor(as.integer(pred.y==d), levels = 0:1),
                                    factor(as.integer(true.y==d), levels = 0:1))
                    }),levels(true.y))
                      
}

