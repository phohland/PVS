#' k+1 levels determination for hRELSA reference data
#'
#' The \code{hrelsa_levels} function calculates kmeans-based hRELSA levels based on a reference data set.
#'
#' @param refset normalized data from a reference set
#' @param mypath path specification for saving clustering outputs
#' @param filename filename for the output *.tiff
#' @param bsl baseline object from relsa_baselines function
#' @param drops character vector of variables to drop
#' @param turns variables with inverted development under severity (i.e. temperature might rise or drop depending on the actual animal model)
#' @param relsaNA codes how to handle NaN values during calculations (default is NA)
#' @param k the number of k clusters (user defined, default is 4)
#' @param showScree show the Scree plot to estimate the optimal number of clusters
#' @param customCol custom color string for the k clusters
#' @param seed random seed, if set to NULL, no seeding for kmeans is used
#' @param myYlim limits of the y-axis for the clustering
#' @param showPlot shows the ordered data and colored cluster levels
#' @param saveTiff yes/no whether to save the output as *.tiff file (if no, mypath is obsolete)
#'
#' @return \code{levels} data.frame with k+1 levels determined by k-means
#'
#' @import dichromat
#' @import graphics
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices tiff dev.off
#' @importFrom stats kmeans complete.cases var
#' @export
#'

hrelsa_levels <- function(refset, mypath = NULL, filename=NULL, bsl=bsl, drops=NULL, turns=NULL, zvars=NULL, relsaNA=NA, k=4,
                         showScree=FALSE, customCol=NULL, seed=123, myYlim=c(0,1.4), showPlot=FALSE, saveTiff=FALSE){
  
  #####  RELSA score all data
  df <-NULL
  for(i in 1:length(unique(refset$id))){
    animal       <- i
    R            <- hrelsa(refset, bsl, a=animal, drop=drops, turnvars=turns, zvars=zvars, relsaNA=relsaNA)$relsa
    C            <- refset[refset$id==unique(refset$id)[animal],"condition"]
    df           <- rbind(df, data.frame(id=unique(refset$id)[animal], day=R$day, condition=C, relsa=R$rms))
  }
  
  ##### order data
  a    <- df$rms
  b    <- a[order(a)]
  b    <- b[complete.cases(b)] # remove NAs
  
  # show scree plot
  if(showScree){
    set.seed(123)
    wss  <- (length(b)-1)*sum(var(b) )
    for (i in 2:15) wss[i] <- sum(kmeans(b,
                                         centers=i)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares", pch=19, cex.lab=1.4, cex.axis=1.4)
    abline(v=k, col="red", lwd=2, lty=2)
  }
  
  ##### kmeans with k-clusters with or without seeding
  if(length(seed)==0){
    cl  <- kmeans(b,k)
  }else{
    set.seed(seed)
    cl  <- kmeans(b,k, iter.max =100)
  }
  
  
  # generate color index for k clusters or use custom color
  if(length(customCol)==0){
    mypalette <- colorRampPalette(c("red", "green"))
    mypalette <- mypalette(k)
    mycol     <- NULL
    for(j in 1:k){
      mycol[cl$cluster==j] <- mypalette[j]
    }
  }else{
    mypalette <- customCol
    mycol     <- NULL
    for(j in 1:k){
      mycol[cl$cluster==j] <- mypalette[j]
    }
  }
  
  
  
  # find levels
  l      <- c()
  clname <- c()
  for(j in 1:k){
    l[j]      <- max(b [cl$cluster==j ])
    clname[j] <- paste("level",j, sep="")
  }
  clname      <- append(clname, paste("level",k+1, sep=""))
  
  # Cluster level definition
  levels           <- t(matrix(sort(c(l,max(b)))))
  colnames(levels) <- clname
  levels           <- as.data.frame(levels)
  
  # Plot and/or export the cluster levels
  if(saveTiff){
    f_name  <- paste(filename,".tiff",sep="")
    mypath  <- paste(mypath, f_name,sep="")
    tiff(mypath, width = 1100, height = 1100, units = "px", pointsize = 8,res = 400, compression = "lzw")
  }else {}
  
  if(showPlot){
    # plotting the clusters
    plot(b, pch=19, ylab="RELSA score", ylim=myYlim)
    points(b, col=mycol, pch=19)
    for(j in 1:k){
      abline(h=levels[j], lwd=1, lty=2)
    }
    
  }else{}
  
  if(saveTiff){
    dev.off()
  }else{}
  
  return(levels=levels)
  
}