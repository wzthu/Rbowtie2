identify_adapters <- function(file1,file2,...,overwrite = FALSE){
 file1<-trimws(as.character(file1))
 file2<-trimws(as.character(file2))
 checkFileExist(file1,"file1")
 checkFileExist(file2,"file2")
 checkFileCreatable(paste0(file1,".adapter"),"file1",overwrite)
 checkFileCreatable(paste0(file2,".adapter"),"file2",overwrite)
 paramlist<-trimws(as.character(list(...)))
 paramArray<-c()
 if(length(paramlist)>0){
  for(i in 1:length(paramlist)){
   paramArray<-c(paramArray,strsplit(paramlist[i],"\\s+")[[1]])
  }
 }
 argvs<-c("AdapterRemoval","--identify-adapters","--file1",
         file1,"--file2",file2,paramArray);
 print(argvs)
 removeAdapter(argvs);
 adapter1tb<-read.table(paste0(file1,".adapter"));
 adapter2tb<-read.table(paste0(file2,".adapter"));
 adapter<-list(adapter1=as.character(adapter1tb[1,1]),adapter2=as.character(adapter2tb[1,1]));
 return(adapter)
}

remove_adapters <- function(file1,...,adapter1 = NULL,output1 = NULL,file2 = NULL,adapter2 = NULL,output2 = NULL,
                            basename = NULL,interleaved = FALSE,overwrite = FALSE){
 file1<-trimws(as.character(file1))
 if(!is.null(adapter1)){
  adapter1<-trimws(as.character(adapter1))
 }
 if(!is.null(output1)){
  output1<-trimws(as.character(output1))
 }
 if(!is.null(file2)){
  if(interleaved){
   stop("Argumnet `seq2` has to be NULL when interleaved=TRUE")
  }else{
   file2<-trimws(as.character(file2))
   if(length(file1)!=length(file2)){
    stop("The lengths of arguments `file1` and `file2` should be the same length")
   }
  }
 }
 if(!is.null(output2)){
  output2<-trimws(as.character(output2))
 }
 if(!is.null(basename)){
  basename<-trimws(as.character(basename))
 }

 paramlist<-trimws(as.character(list(...)))
 paramArray<-c()
 if(length(paramlist)>0){
  for(i in 1:length(paramlist)){
   paramArray<-c(paramArray,strsplit(paramlist[i],"\\s+")[[1]])
  }
 }
 checkFileExist(file1,"file1")
 checkFileExist(file2,"file2")
 checkFileCreatable(output1,"output1",overwrite)
 checkFileCreatable(output2,"output2",overwrite)
 checkPathExist(basename,"basename")

 argvs<-c("AdapterRemoval","--file1",file1)

 if(!is.null(adapter1)){
  argvs<-c(argvs,"--adapter1",adapter1)
 }
 if(!is.null(output1)){
  argvs<-c(argvs,"--output1",output1)
 }
 if(!is.null(file2)){
  argvs<-c(argvs,"--file2",file2)
 }
 if(!is.null(output2)){
  argvs<-c(argvs,"--output2",output2)
 }
 if(!is.null(basename)){
  argvs<-c(argvs,"--basename",basename)
 }
 argvs<-c(argvs,paramArray)
 print(argvs)
 removeAdapter(argvs)


}


