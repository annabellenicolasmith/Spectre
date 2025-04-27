# run flowSOM with the option to extract MST

run.flowsom.mst <- function(dat,
                            use.cols, # names of columns to cluster
                            xdim = 14,
                            ydim = 14,
                            meta.k = 'auto',
                            max.meta = 20,
                            clust.seed = 42,
                            meta.seed = 42,
                            clust.name = "FlowSOM_cluster",
                            meta.clust.name = "FlowSOM_metacluster",
                            mem.ctrl = FALSE){
  
  
  
  ### Require packages
  # require(flowCore)
  require(Biobase)
  require(FlowSOM)
  require(data.table)
  
  
  ### Prepare starting and using data
  message("Preparing data")
  
  dat.start <- dat
  clustering.cols <- use.cols
  
  ### Check selected columns are numeric
  
  if(any(unlist(lapply(dat[, use.cols, with = FALSE], is.numeric)) == FALSE)) {
    stop('Non-numeric column selected for analysis. Check use.cols parameter.')
  }
  
  dat <- dat[,use.cols, with = FALSE]
  
  ### Create flowFrame metadata (column names with descriptions) plus flowFrame
  
  metadata <- data.frame(name=dimnames(dat)[[2]], desc=paste('column',dimnames(dat)[[2]],'from dataset'))
  dat.ff <- new("flowFrame",
                exprs=as.matrix(dat), # in order to create a flow frame, data needs to be read as matrix
                parameters=Biobase::AnnotatedDataFrame(metadata))
  
  #head(flowCore::exprs(dat.ff))
  dat_FlowSOM <- dat.ff
  
  rm(dat)
  rm(dat.ff)
  
  
  ### Run FlowSOM clustering
  
  message("Starting FlowSOM")
  
  FlowSOM_cols <- clustering.cols # clustering cols
  set.seed(clust.seed) # set seed for reproducibility
  
  ## run FlowSOM (initial steps prior to meta-clustering)
  FlowSOM_out <- FlowSOM::ReadInput(dat_FlowSOM, transform = FALSE, scale = FALSE)
  
  FlowSOM_out <- FlowSOM::BuildSOM(FlowSOM_out,
                                   colsToUse = FlowSOM_cols,
                                   xdim = xdim,
                                   ydim = ydim)
  
  FlowSOM_out <- FlowSOM::BuildMST(FlowSOM_out)
  
  ## extract cluster labels (pre meta-clustering) from output object
  labels_pre <- FlowSOM_out$map$mapping[, 1]
  
  flowsom.res.original <- labels_pre
  
  ## save ORIGINAL cluster labels
  flowsom.res.original <- data.frame("labels_pre" = labels_pre)
  colnames(flowsom.res.original)[grepl('labels_pre',colnames(flowsom.res.original))] <- clust.name
  
  dat.start <- cbind(dat.start, flowsom.res.original)   # Add results to dat
  
  ### Metaclustering
  
  if(meta.k != 0) {
    
    ## Auto number of MCs
    if(meta.k == 'auto'){
      FlowSOM_out_meta <- FlowSOM::MetaClustering(FlowSOM_out$map$codes,
                                                  method="metaClustering_consensus",
                                                  max=max.meta,
                                                  seed=meta.seed)
    }
    
    ## Define number of MCs
    if(meta.k != 'auto'){
      FlowSOM_out_meta <- FlowSOM::metaClustering_consensus(FlowSOM_out$map$codes, k = meta.k, seed = meta.seed)
    }
    
    labels <- FlowSOM_out_meta[labels_pre]
    flowsom.res.meta <- data.frame("labels" = labels)
    colnames(flowsom.res.meta)[grepl('labels',colnames(flowsom.res.meta))] <- meta.clust.name
    
    message("Binding metacluster labels to starting dataset")
    dat.start <- cbind(dat.start, flowsom.res.meta)       # Add results to dat
  }
  
  ### Return
  message("Binding cluster labels to starting dataset")
  dat.start <- data.table::as.data.table(dat.start) # Make dat a data.table for future manipulation
  df.list=list(dat.start, FlowSOM_out)
  return(df.list)
  
}

# return the cell.dat table from the flowsom results list
flowsom.mst.results.df=function(df.list){
  data.table=as.data.table(df.list[[1]]) 
  return(data.table)
}

# return the flosom object from the flowsom results list to make mst graph
flowsom.mst.results.mst=function(df.list){
  mst=df.list[[2]]
  return(mst)
}
