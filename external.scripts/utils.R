# Function created by Stephen Turner
# see http://gettinggeneticsdone.blogspot.de/2011/02/summarize-missing-data-for-all.html
# for more information

###
# Summarize how much missing data exists in each variable of a data frame.
# The result is a data.frame
###
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}