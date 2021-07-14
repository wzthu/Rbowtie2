#'@name checkIndexType
#'
#'@title Determine bowtie2 index type
#'
#'@description This is an internal function that is not meant to be used outside
#'of the package. It determines whether the given index library comprises of 
#'small indexes (.bt2) or large indexes (.bt2l)
#'
#'@param filePath \code{Character} scalar. The path where bowtie2 index files 
#'are located. Include the basename of the index files at the end of the path
#'(i.e. path_to_index_dir/index_basename). Do not include the bowtie2 index file 
#'extension (.bt2 or .bt2l)
#'
#'@details The function first tries to determine whether 
#'path_to_index_dir/index_basename.1.bt2 exists and if it doesn't exist then it
#'tries to determine whether path_to_index_dir/index_basename.1.bt2l exists. 
#'If neither of those files exist then there is an issue with the index files 
#'that must be addressed.
#'
#'@return \code{Character} scalar. Returns either "SMALL" if the .bt2 file is found,
#'"LARGE" if the .bt2l file is found, or "ERROR" if neither the .bt2 nor .bt2l file 
#'is found.

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

#'@name checkFileExist
#'
#'@title Check if file exists 
#'
#'@description This is an internal function that is not meant to be used outside
#'of the package. It determines whether a specific file exists at the end of the
#'path given to the function.
#'
#'@param filePath \code{Character} scalar. Path to file of interest.
#'
#'@param argname \code{Character} scalar. Name of the argument passed.
#'
#'@author Zheng Wei
#'
#'@return Stops the function if the file does not exist.


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


#'@name checkPathExist
#'
#'@title Check if path exists 
#'
#'@description This is an internal function that is not meant to be used outside
#'of the package. It determines whether the path passed to the function exists.
#'
#'@param filePath \code{Character} scalar. Path of interest.
#'
#'@param argname \code{Character} scalar. Name of the argument passed.
#'
#'@author Zheng Wei
#'
#'@return Stops the function if the path does not exist.


checkPathExist <- function(filePath,argname){
    if(!is.null(filePath)){
        if(!dir.exists(dirname(filePath))){
            stop(sprintf("For argument `%s`,path does not exist: `%s`",
                         argname,filePath))
        }
    }
}

#'@name checkFileCreateable
#'
#'@title Check if file is creatable
#'
#'@description This is an internal function that is not meant to be used outside
#'of the package. It determines whether the file at the end of the path can
#'be created. 
#'
#'@param filePath \code{Character} scalar. Path to file to be created.
#'
#'@param argname \code{Character} scalar. Name of the argument passed.
#'
#'@param overwrite \code{Logical} Will warn user of overwriting if set \code{TRUE}.
#'
#'@author Zheng Wei
#'
#'@return Indicates whether the file can be created at the path given.


checkFileCreatable <- function(filePath,argname,overwrite){
    if(!is.null(filePath)){
        if(file.exists(filePath)){
            if(overwrite){
                warning("For argument ", argname, 
                        ", file exist: ", filePath,". It will be overwrited")
            }else{
                stop("For argument ", argname,
                ", file exist: ", filePath,". Use 'overwrite=TRUE' to overwrite")
            }
        }else if(!file.create(filePath)){
            stop("For argument ", argname, " cannot create file ", filePath,
                 "\nNo such directory or permission denied.")
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
            stop("Argument(s) ", invalidp, " are invalid for additional ",
                 "argument. Please set them as fixed arguments.")
        }
        return(paramArray)
    }else{
        return(NULL)
    }
}

