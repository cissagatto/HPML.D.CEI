##############################################################################
# COMPLETE CHAINS HPML                                                       #  
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# PhD Elaine Cecilia Gatto | Prof. PhD. Ricardo Cerri | Prof. PhD. Mauri     #
# Ferrandin | Prof. PhD. Celine Vens | PhD. Felipe Nakano Kenji              #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium               #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
##############################################################################


###########################################################################
#
###########################################################################
FolderRoot = "~/Complete-Chains-HPML"
FolderScripts = "~/Complete-Chains-HPML/R"



###############################################################################
# Runs for all datasets listed in the "datasets.csv" file
# n_dataset: number of the dataset in the "datasets.csv"
# number_cores: number of cores to paralell
# number_folds: number of folds for cross validation
# delete: if you want, or not, to delete all folders and files generated
########################################################################
executa <- function(parameters){
  
  
  ##########################################################################
  # Workspace
  FolderRoot = "~/Complete-Chains-HPML"
  FolderScripts = "~/Complete-Chains-HPML/R"
  
  ##########################################################################
  # LOAD LIBRARIES
  setwd(FolderScripts)
  source("libraries.R")
  
  setwd(FolderScripts)
  source("utils.R")
  
  
  ##########################################################################
  if(parameters$number.cores == 0){
    
    cat("\n\n############################################################")
    cat("\n# RUN: Zero is a disallowed value for number_cores. Please #")
    cat("\n# choose a value greater than or equal to 1.               #")
    cat("\n############################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(parameters$number.cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(parameters$number.cores==1){
      cat("\n\n########################################################")
      cat("\n# RUN: Running Sequentially!                           #")
      cat("\n########################################################\n\n")
    } else {
      cat("\n\n######################################################################")
      cat("\n# RUN: Running in parallel with ", parameters$number.cores, " cores! #")
      cat("\n######################################################################\n\n")
    }
  }
  
  cl = cl
  
  retorno = list()
  
  cat("\n\n########################################################")
  cat("\n# RUN: Get labels                                      #")
  cat("\n########################################################\n\n")
  arquivo = paste(parameters$Folders$folderNamesLabels, "/" ,
                  dataset.name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  parameters$NamesLabels = namesLabels
  
  
  cat("\n\n###############################################################")
  cat("\n# RUN: Get the label space                                    #")
  cat("\n###############################################################\n\n")
  timeLabelSpace = system.time(resLS <- labelSpace(parameters))
  parameters$resLS = resLS
  
  
  cat("\n\n####################################################")
  cat("\n# RUN: Get all partitions                          #")
  cat("\n####################################################\n\n")
  timeAllPartitions = system.time(resAP <- get.all.partitions(parameters))
  parameters$All.Partitions = resAP
  
  setwd(parameters$Folders$folderTested)
  write.csv(resAP$all.total.labels,"total-labels-per-cluster.csv", 
            row.names = FALSE)
  
  
  cat("\n\n#####################################################")
  cat("\n# RUN: Compute Label Atributes                      #")
  cat("\n#####################################################\n\n")
  timeCLA = system.time(resCLA <- compute.labels.attributes(parameters))
  parameters$Labels.Attr = resCLA
  
  
  if(parameters$implementation =="python"){
    
    
    setwd(FolderScripts)
    source("test-python-silho.R")
    
    
    cat("\n\n##########################################")
    cat("\n# RUN: PYTHON ECC CLASSIFIER               #")
    cat("\n############################################\n\n")
    
    
    cat("\n\n############################################")
    cat("\n# RUN: PYTHON Build and test partitions      #")
    cat("\n##############################################\n\n")
    timeBuild = system.time(resBT <- build.python(parameters))
    
    
    cat("\n\n#############################################")
    cat("\n# RUN: PYTHON Evaluation Fold                 #")
    cat("\n###############################################\n\n")
    timeAvalia = system.time(resEval <- evaluate.python(parameters))
    
    
    cat("\n\n############################################")
    cat("\n# RUN: PYTHON Gather Evaluation              #")
    cat("\n##############################################\n\n")
    timeGather = system.time(resGE <- gather.evaluated.python(parameters))
    
    
    cat("\n\n############################################")
    cat("\n# RUN: PYTHON Save Runtime                   #")
    cat("\n##############################################\n\n")
    Runtime = rbind(timeLabelSpace, timeAllPartitions, timeCLA,
                    timeBuild, timeAvalia, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(Runtime, paste(parameters$dataset.name,
                             "-run-runtime-python.csv", sep=""),
              row.names = FALSE)
    
  } else {
    cat("\n NOTHING")
  }
  
  
  cat("\n\n#################################################")
  cat("\n# RUN: Stop Parallel                              #")
  cat("\n###################################################\n\n")
  parallel::stopCluster(cl)
  
  
  gc()
  cat("\n\n###################################################")
  cat("\n# RUN: END                                          #")
  cat("\n#####################################################\n\n")
  
}


##########################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com           #
# Thank you very much!                                                   #
##########################################################################
