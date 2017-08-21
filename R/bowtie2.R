
bowtie2 <- function(bt2Index,samOutput,seq1,...,seq2=NULL,interleaved=FALSE,overwrite=FALSE){
 bt2Index <-trimws(as.character(bt2Index))
 samOutput<-trimws(as.character(samOutput))

 seq1<-trimws(as.character(seq1))


 if(!is.null(seq2)){
  seq2<-trimws(as.character(seq2))
  if(length(seq1)!=length(seq2)){
   stop("The lengths of arguments `seq1` and `seq2` should be the same length")
  }
 }
 paramlist<-trimws(as.character(list(...)))
 paramArray<-c()
 if(length(paramlist)>0){
  for(i in 1:length(paramlist)){
   paramArray<-c(paramArray,strsplit(paramlist[i],"\\s+")[[1]])
  }
 }


 if(interleaved){
  if(length(seq1)>1){
   stop("Argumnet `seq1` has to be a SINGLE file path rather than a vector of paths")
  }
  if(!is.null(seq2)){
   stop("Argumnet `seq2` has to be NULL when interleaved=TRUE")
  }
 }


 checkFileExist(seq1,"seq1")
 checkFileExist(seq2,"seq2")
 checkPathExist(bt2Index,"bt2Index")
 checkFileExist(paste0(bt2Index,".1.bt2"),"bt2Index")
 checkFileExist(paste0(bt2Index,".2.bt2"),"bt2Index")
 checkFileExist(paste0(bt2Index,".3.bt2"),"bt2Index")
 checkFileExist(paste0(bt2Index,".4.bt2"),"bt2Index")
 checkFileExist(paste0(bt2Index,".rev.1.bt2"),"bt2Index")
 checkFileExist(paste0(bt2Index,".rev.2.bt2"),"bt2Index")
 checkFileCreatable(samOutput,"samOutput",overwrite)

 argvs = c("-x",bt2Index)
 seq1<-paste0(seq1,collapse = ",")
 if(is.null(seq2)){
  if(interleaved){
   argvs <- c(argvs,"--interleaved",seq1)
  }else{
   argvs <- c(argvs,"-U",seq1)
  }
 }else{
  seq2<-paste0(seq2,collapse = ",")
  argvs <- c(argvs,"-1",seq1,"-2",seq2)
 }

 argvs <- c("bowtie2-align-s",paramArray,argvs,"-S",samOutput)

 bowtie2Mapping(argvs = argvs)
 print(argvs)
}





bowtie2_build <- function(reference,bt2Index,...,overwrite=FALSE){
 reference<- trimws(as.character(reference))
 bt2Index <- trimws(as.character(bt2Index))

 paramlist<-trimws(as.character(list(...)))
 paramArray<-c()
 if(length(paramlist)>0){
  for(i in 1:length(paramlist)){
   paramArray<-c(paramArray,strsplit(paramlist[i],"\\s+")[[1]])
  }
 }

 checkFileExist(reference,"reference")
 checkPathExist(bt2Index,"bt2Index")
 checkFileCreatable(paste0(bt2Index,".1.bt2"),"bt2Index",overwrite)
 checkFileCreatable(paste0(bt2Index,".2.bt2"),"bt2Index",overwrite)
 checkFileCreatable(paste0(bt2Index,".3.bt2"),"bt2Index",overwrite)
 checkFileCreatable(paste0(bt2Index,".4.bt2"),"bt2Index",overwrite)
 checkFileCreatable(paste0(bt2Index,".rev.1.bt2"),"bt2Index",overwrite)
 checkFileCreatable(paste0(bt2Index,".rev.2.bt2"),"bt2Index",overwrite)

 argvs <- c("bowtie2-build-s",paramArray,reference,bt2Index)

 print(argvs)
 bowtie2Build(argvs = argvs)
}

bowtie2_version <- function(){
 invisible(bowtie2Mapping(argvs = c("bowtie2-align-s","--version")))
}

bowtie2_usage <- function(){
 bowtie2Mapping(argvs = c("bowtie2-align-s","-h"))
}

bowtie2_build_usage <- function() {
 bowtie2Build(argvs = c("bowtie2-build-s","-h"))
}


