# added pairwise p-value adjustment option
# added automatic saving of pairwise p-value results CSV

make.autograph.fdr <- function(dat,
                           x.axis,
                           y.axis,
                           
                           colour.by = x.axis,
                           y.axis.label = y.axis,
                           grp.order = NULL,
                           colours = NULL,
                           
                           my_comparisons = NULL,
                           Pairwise_test = "wilcox.test",
                           Pairwise_adjustment="BH",
                           Variance_test="kruskal.test",
                           
                           title = paste0(y.axis),
                           subtitle = NULL,
                           filename = paste0(y.axis, ".pdf"),
                           
                           violin = TRUE,
                           scale = "lin", # can be "lin" or "sci"
                           dot.size = 3,
                           width = 5,
                           height = 5,
                           max.y = 1.7,
                           
                           path = getwd())
{
  ### Install any non-installed libraries
  if(!is.element('Spectre', installed.packages()[,1])) stop('Spectre is required but not installed')
  if(!is.element('ggplot2', installed.packages()[,1])) stop('ggplot2 is required but not installed')
  if(!is.element('data.table', installed.packages()[,1])) stop('data.table is required but not installed')
  if(!is.element('ggpubr', installed.packages()[,1])) stop('ggpubr is required but not installed')
  
  ### Require packages
  require(Spectre)
  require(ggplot2)
  require(data.table)
  require(ggpubr)
  
  ### Library test
  if(!is.null(colours)){
    if(length(unique(dat[[colour.by]])) != length(colours)){
      stop('The length of factors you want to colour the plot by does not match the number of colours you have provided.')
    }
  }
  
  ### Set up colours and other settings
  message(paste0("AutoGraph for `", y.axis.label, " - ", y.axis, "` started"))
  
  message("AutoGraph - setup started")
  
  spectral.list <- colorRampPalette(brewer.pal(11,"Spectral"))(50)
  spectral.list <- rev(spectral.list)
  colour.scheme <- colorRampPalette(c(spectral.list))
  
  ### Max and min values
  dat <- data.table::as.data.table(dat)
  
  max_y_value <- max(dat[,y.axis, with = FALSE], na.rm = TRUE)
  max_y_value_p40 <- max_y_value*max.y
  
  min_y_value<- min(dat[,y.axis, with = FALSE], na.rm = TRUE)
  
  ###
  Xaxis <- dat[[x.axis]] <- as.factor(dat[[x.axis]])
  
  if(!is.null(grp.order)){
    Xaxis <- dat[[x.axis]] <- factor(dat[[x.axis]], levels = grp.order)
  }
  
  
  
  ###  
  message("AutoGraph - setup complete")
  
  ### Plotting
  
  message("AutoGraph - plotting started")
  
  p <- ggplot2::ggplot(dat, ggplot2::aes(x=Xaxis, y=dat[[y.axis]])) #, fill=Species)) + # can use fill = Species -- makes coloured by Species, with block outline of point -- NEED FILL for violin plot to be filled
  
  ## POINTS
  p <- p + ggplot2::geom_point(ggplot2::aes(fill=as.factor(dat[[colour.by]]),
                                            colour = as.factor(dat[[colour.by]])),
                               shape = 21,
                               stroke = 0,
                               size = dot.size,
                               position = ggplot2::position_jitter(width = 0.1, height = 0))
  ## VIOLIN
  if(violin == TRUE){
    message("AutoGraph - adding violin plot")
    p <- p + geom_violin(ggplot2::aes(fill=as.factor(dat[[colour.by]]),
                                      colour = as.factor(dat[[colour.by]])),
                         trim=TRUE,
                         show.legend = FALSE,
                         alpha = 0.1)
  }
  
  
  ## COLOUR CONTROL
  p <- p + ggplot2::scale_fill_manual(name = colour.by, values = colours)
  p <- p + ggplot2::scale_color_manual(name = colour.by, values = colours)
  
  p <- p + ggplot2::ggtitle(title)
  p <- p + ggplot2::labs(x= paste0(x.axis),
                         y = y.axis.label,
                         subtitle = subtitle) # colnames(data)[3] would return the name -- use similar for loop -- maybe data$dose
  
  
  ## PRISM -- SEM (from https://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean)
  
  # SEM
  p <- p + ggplot2::stat_summary(
    fun.max=function(i) mean(i) + sd(i)/sqrt(length(i)), # 0.975
    fun.min=function(i) mean(i) - sd(i)/sqrt(length(i)), # 0.975
    geom="errorbar", width=0.5, size = 1)
  
  p <- p + ggplot2::stat_summary(fun=mean,
                                 fun.min = mean,
                                 fun.max = mean, geom="crossbar",
                                 width = 0.7,
                                 size = 0.5) # add a large point for the mean

  sub=gsub(" ", "",colnames(dat), fixed = TRUE)
  sub=gsub("--", "", sub)
  sub.dat=dat
  colnames(sub.dat)=sub
  i=y.axis
  i=gsub(" ", "", i, fixed = TRUE)
  i=gsub("--", "", i)
  
  formula = paste(i, "~", x.axis)
  
  increase=(max_y_value*0.15)
  
  # VARIANCE
  # MORE THAN TWO GROUPS: pairwise comparison with overall anova/Kruskal-Wallis result
  message("AutoGraph - adding adjusted p values")
  if(!is.null(my_comparisons)){
    if(!is.null(Pairwise_test)){
      anno.tab.pairwise=ggpubr::compare_means(as.formula(formula),
                                              data = sub.dat,
                                              method=Pairwise_test, 
                                              p.adjust.method = "BH") %>%
        dplyr::mutate(y_pos=c(max_y_value+increase,
                              max_y_value+increase+increase,
                              max_y_value+increase+increase+increase))

      p=p+ ggpubr::stat_pvalue_manual(
        anno.tab.pairwise,
        xmin="group1",
        xmax="group2",
        y.position = "y_pos",
        label="p.adj"
      )
        
      
      }
  }
  if (!is.null(Variance_test)) {
    p <- p + stat_compare_means(method = Variance_test, 
                                label.y = max_y_value_p40, size = 4)
  }
  
  message("AutoGraph - adjusting theme")
  #coord_fixed(ratio = 1) + # determines size ratio of the plot -- smaller increases width
  p <- p + ggplot2::theme_classic(base_size = 30) # can be theme_classic(), theme_bw()
  
  ## THEMES
  p <- p + ggplot2::theme(legend.position = "right", # can be "left" "right" "top" "bottom" "none
                          legend.text = ggplot2::element_text(colour="black",size=10,angle=0,hjust=0,vjust=0,face="bold"),
                          legend.title = ggplot2::element_text(colour="black",size=10,angle=0,hjust=0,vjust=0,face="bold"),
                          
                          axis.text.x = ggplot2::element_text(colour="black",size=12,angle=45,hjust=1,vjust=1,face="bold"),
                          axis.text.y = ggplot2::element_text(colour="black",size=12,angle=0,hjust=1,vjust=0,face="bold"),
                          axis.title.x = ggplot2::element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="bold"),
                          axis.title.y = ggplot2::element_text(colour="black",size=12,angle=90,hjust=.5,vjust=1,face="bold"),
                          
                          # Title
                          plot.title = ggplot2::element_text(lineheight=.8, face="bold", hjust = 0, size = 12), # hjust = 0.5 to centre
                          plot.subtitle = ggplot2::element_text(size=10, #åhjust=0.5,
                                                                face="italic", color="black"),
                          axis.line = ggplot2::element_line(colour = 'black', size = 1),
                          axis.ticks = ggplot2::element_line(colour = "black", size = 1)
  )
  
  ### End construction of 'p'
  
  ## SCALES
  if(scale == "lin"){
    p <- p + ggplot2::scale_y_continuous(limits = c(0, max_y_value_p40))
  }
  
  if(scale == "sci"){
    p <- p + ggplot2::scale_y_continuous(labels = scales::scientific, limits = c(0, max_y_value_p40))
  }
  
  
  ## View plot
  p
  
  ## Save
  ggplot2::ggsave(plot = p, filename = paste0(filename), width = width, height = height, path = path) # width 3.6 default, height 5 default
  message(paste0("AutoGraph for `", y.axis.label, " - ", y.axis, "` saved to disk"))
  
  data.table::fwrite(anno.tab.pairwise, file=paste0(getwd(), "/", y.axis, ".csv"))
}
