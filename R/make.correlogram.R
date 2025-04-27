# function to test correlations with p-value adjustment and return correlogram plot
# adapted from https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/#correlation-test

make.correlogram <- function(data,
                      method = c("kendall", 
                                 "spearman",
                                 "pearson"),
                      use = c("complete.obs",
                              "pairwise.complete.obs"),
                      sig.level = 0.05, # alpha significance level
                      order = c("original", "AOE", "FPC", "hclust", "alphabet"),
                      diag = FALSE,
                      tri.option = c("upper", "lower", "full"), 
                      tl.srt = 90,
                      tl.cex=tl.cex,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0),
                      adjust="BH",
                      height=2000,
                      width=2000) {
  
  # load the corrplot library
  if(!is.element('corrplot', installed.packages()[,1])) stop('corrplot is required but not installed')
  require(corrplot)
  
  # omit any NA values if complete observations is specified & convert to matrix
  data=if(use == "complete.obs")
    as.matrix(na.omit(data))
  # otherwise, simply convert to matrix
  else as.matrix(data)
  
  # create the correlation matrix
  correlation_matrix=cor(data, 
                         use=use, 
                         method = method)
  
  # perform the significant test with confidence intervals
  correlation_p=corrplot::cor.mtest(correlation_matrix,
                                    conf.level=(1-sig.level))
  
  # extract the unadjusted P values
  P_matrix=P.unadj=correlation_p$p
  
  # then we can extract just the lower triangle worth of P values (unadjusted)
  p=P_matrix[lower.tri(P_matrix)]
  
  # then we adjust the p values
  adj.p=p.adjust(p, method = adjust)
  
  # and assign the adjusted P values to our P value matrix
  P_matrix[lower.tri(P_matrix)]=adj.p
  # copy the lower triangle with the adjusted P values to the upper triangle
  P_matrix[upper.tri(P_matrix)]=t(P_matrix)[upper.tri(P_matrix)]
  # also replace the values on the diagonal with 2 to avoid messing up the rounding step later
  diag(P_matrix)=NA
  
  colnames(P_matrix)=rownames(P_matrix)=colnames(correlation_matrix)
  
  
  diag(P.unadj)=NA
  colnames(P.unadj)=rownames(P.unadj)=colnames(correlation_matrix)
  
  pairwise_combinations=correlation_matrix
  pairwise_combinations= tibble::rownames_to_column(data.frame(pairwise_combinations), "rows")
  pairwise_combinations=pairwise_combinations %>%
    rename_with(~ str_remove(.,"X."), everything())
  pairwise_combinations = reshape2::melt(pairwise_combinations, id.vars="rows")
  pairwise_combinations$correlation=paste(pairwise_combinations$rows, pairwise_combinations$variable)
  
  # extract all the results in a nice little list
  result=list(comparison=pairwise_combinations$correlation,
              correlation = correlation_matrix, 
              P_adj = P_matrix, 
              P_unadj = P.unadj)
  
  # define the colours for the plot
  col=colorRampPalette(c("deepskyblue2", "skyblue2", "#FFFFFF", "orchid1","magenta1"))
  
  png(paste0(getwd(), "/correlogram.png"), height=2000, width=2000)
  # make the plot
  cplot=corrplot::corrplot(correlation_matrix,
                           method="color", # circle, square, ellipse, number, shade, color, pie
                           col=col(200), 
                           number.font = number.font,
                           mar = mar, 
                           number.cex = number.cex,
                           type = tri.option, 
                           order = order,
                           addCoef.col = "black", # color by correlation coefficient
                           tl.col = "black", 
                           tl.srt = tl.srt, # rotation of text labels
                           tl.cex=tl.cex,
                           
                           # combine with significance level
                           p.mat = P_matrix, 
                           sig.level = sig.level,
                           insig = "blank",
                           
                           # hide correlation coefficients on the diagonal
                           diag = diag
            )
  dev.off()
  data.table::fwrite(result, "correlation_results.csv")
}
