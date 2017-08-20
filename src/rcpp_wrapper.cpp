

#include <string>
#include <iostream>
#include "adapterremoval/adrm_interface.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "bowtie2/bowtie2_interface.h"
#include "RcoutRcerr.h"
using ar.interface_adapterremoval_main;

// [[Rcpp::export]]
int removeAdapter(Rcpp::CharacterVector argvs) {
	int argc=argvs.size();
	char **argv = new char* [argc];
	for(int i=0;i<argc;i++){
		int len = argvs[i].size();
		argv[i] = new char[len+1];
		strcpy(argv[i],(char *)(Rcpp::as<std::string>(argvs[i])).c_str());
    }

    return interface_adapterremoval_main(argc,argv);
}


// [[Rcpp::export]]
int bowtie2Mapping(Rcpp::CharacterVector argvs) {
  int argc=argvs.size();
  char **argv = new char* [argc];
  for(int i=0;i<argc;i++){
    int len = argvs[i].size();
    argv[i] = new char[len+1];
    strcpy(argv[i],(char *)(Rcpp::as<std::string>(argvs[i])).c_str());

  }
  return interface_bowtie_main(argc, (const char **)argv);
}

// [[Rcpp::export]]
int bowtie2Build(Rcpp::CharacterVector argvs) {
  int argc=argvs.size();
  char **argv = new char* [argc];
  for(int i=0;i<argc;i++){
    int len = argvs[i].size();
    argv[i] = new char[len+1];
    strcpy(argv[i],(char *)(Rcpp::as<std::string>(argvs[i])).c_str());

  }
  return interface_bowtie_build_main(argc, (const char **)argv);
}


