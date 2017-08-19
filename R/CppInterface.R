remove_adapters_call <- function(inputFile1,adapter1,outputFile1,basename,
                                  inputFile2=NULL,adapter2=NULL,outputFile2=NULL,paramlist=NULL){

    argv<-c("AdapterRemoval","--file1",inputFile1, "--adapter1",adapter1,
            "--output1",outputFile1,"--basename", basename );
    if(!is.null(inputFile2)){
        argv<-c(argv,"--file2",inputFile2, "--adapter2",adapter2,"--output2",outputFile2);
    }
    argv<-c(argv,paramlist)
    print(argv)
    return(removeAdapter(argv));
}

bowtie2_call<- function(bowtie2Index,samOutput, fastqInput1, fastqInput2=NULL,paramlist=NULL){
    if(is.null(fastqInput2)){
        argv<-c("bowtie2-align-s",paramlist,"-x",bowtie2Index,"-U",fastqInput1,"-S",samOutput)
    }else{
        argv<-c("bowtie2-align-s",paramlist,"-x",bowtie2Index,"-1",fastqInput1,"-2",fastqInput2,"-S",samOutput)
    }
    return(bowtie2Mapping(argv))
}

bowtie2_build_call<- function(genomeFastaInput, bt2_base,paramlist=NULL){
    argv<-c("bowtie2-build-s",paramlist,genomeFastaInput,bt2_base)

    return(bowtie2Build(argv))
}
