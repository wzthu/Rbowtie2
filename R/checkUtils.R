checkFileExist <- function(filePath,argname){
 if(!is.null(filePath)){
  for(i in 1:length(filePath)){
   if(!file.exists(filePath[i])){
    stop(sprintf("For argument `%s`, file does not exist: `%s`",argname,filePath[i]))
   }
  }
 }
}
checkPathExist <- function(filePath,argname){
 if(!is.null(filePath)){
  if(!dir.exists(dirname(filePath))){
   stop(paste("For argument `%s`,path does not exist: `%s`",argname,filePath))
  }
 }
}
checkFileCreatable <- function(filePath,argname,overwrite){
 if(!is.null(filePath)){
  if(file.exists(filePath)){
   if(overwrite){
    warning(sprintf("For argument `%s`, file exist:%s. It will be overwrited",argname,filePath));
   }else{
    stop(springf("For argument `%s`,file exist: %s. Use 'overwrite=TRUE' to overwrite",argname,filePath));
   }
  }else if(!file.create(filePath)){
   stop(paste("For argument `%s`, cannot create file `%s`, No such file or directory or permission denied",argname,filePath));
  }else{
   unlink(filePath)
  }
 }
}


