checkIndexType <- function(filePath){
    if (!file.exists(paste0(filePath,".1.bt2"))){
        if (!file.exists(paste0(filePath,".1.bt2l"))){
            return("ERROR")
        }
        else{
            return("LARGE")
        }
    }
    else{
        return("SMALL")
    }
} 




checkFileExist <- function(filePath,argname){
    if(!is.null(filePath)){
        for(i in seq_along(filePath)){
            if(!file.exists(filePath[i])){
                stop(sprintf("For argument `%s`, file does not exist: `%s`",
                             argname,filePath[i]))
            }
        }
    }
}
checkPathExist <- function(filePath,argname){
    if(!is.null(filePath)){
        if(!dir.exists(dirname(filePath))){
            stop(sprintf("For argument `%s`,path does not exist: `%s`",
                         argname,filePath))
        }
    }
}
checkFileCreatable <- function(filePath,argname,overwrite){
    if(!is.null(filePath)){
        if(file.exists(filePath)){
            if(overwrite){
                warning(sprintf(paste0("For argument `%s`, file exist:%s. ",
                                       "It will be overwrited"),
                                       argname,filePath));
            }else{
                stop(sprintf(paste0("For argument `%s`,file exist: %s. ",
                                    "Use 'overwrite=TRUE' to overwrite"),
                             argname,filePath));
            }
        }else if(!file.create(filePath)){
            stop(sprintf(paste0("For argument `%s`, cannot create file `%s`.",
                                "\nNo such directory or permission denied."),
                         argname,filePath));
            stop("")
        }else{
            unlink(filePath)
        }
    }
}

checkAddArgus<- function(pattern,...){
    paramlist<-trimws(as.character(list(...)))
    if(length(paramlist)>0){
        paramlist<-paste(paramlist,collapse = " ")
        paramArray <- strsplit(paramlist,"\\s+")[[1]]
        fixed<-grepl(pattern,paramArray)
        if(sum(fixed)>0){
            invalidp<-paste0(paramArray[fixed],collapse = " ")
            stop(sprintf(paste0("Argument(s) `%s` are invalid for additional ",
                                "argument. Please set them as fixed arguments."),
                         invalidp))
        }
        return(paramArray)
    }else{
        return(NULL)
    }
}

