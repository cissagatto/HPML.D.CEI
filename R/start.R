cat("\n\n##########################################################")
cat("\n# START: COMPLETE CHAINS OF HYBRID PARTITIONS            #")
cat("\n##########################################################\n\n")


##############################################################################
# COMPLETE CHAINS HPML                                                       #
# Internal and External Chains - clusters and labels                         #  
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


cat("\n##########################################")
cat("\n# START: SET WORKSPACE PATH              #")
cat("\n##########################################\n\n")
FolderRoot = "~/Complete-Chains-HPML"
FolderScripts = "~/Complete-Chains-HPML/R"


# cat("\n##########################################")
# cat("\n# START: Changing R LIB USER PATH        #")
# cat("\n##########################################\n\n")
# Sys.setenv("R_LIBS_USER" = "/home/biomal/R/x86_64-pc-linux-gnu-library/4.2")
# /home/biomal/miniconda3/envs/AmbienteTeste/lib/R/library


# cat("\n##########################################")
# cat("\n# START: Changing R LIB PATH             #")
# cat("\n##########################################\n\n")

# cat("\n========================================\n\n\n")
# .libPaths()
# cat("\n\n\n")
# .libPaths("/home/biomal/R/x86_64-pc-linux-gnu-library/4.2")
# cat("\n\n\n")
# .libPaths()
# cat("\n========================================\n\n\n")

# cat("\n========================================")
# Sys.getenv()
# cat("\n========================================\n\n\n")

# installed.packages(lib.loc = "/home/biomal/miniconda3/envs/AmbienteTeste/lib/R/library")


cat("\n##########################################")
cat("\n# START: LOADING SOURCES                 #")
cat("\n##########################################\n\n")


setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")

setwd(FolderScripts)
source("run.R")


cat("\n##########################################")
cat("\n# START: Setting R Options               #")
cat("\n##########################################\n\n")
options(java.parameters = "-Xmx64g")  # Java Options
options(show.error.messages = TRUE)   # Error Messages
options(scipen=20)                    # Number of places after the comma


cat("\n##########################################")
cat("\n# START: Opening datasets info file      #")
cat("\n##########################################\n\n")
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-original.csv"))


cat("\n##########################################")
cat("\n# START: Get arguments from command line #")
cat("\n##########################################\n\n")
args <- commandArgs(TRUE)


# config.file = "/home/biomal/Complete-Chains-HPML/config-files-0/python/jaccard/ward.D2/silho/co-pjws-GpositiveGO.csv"

config.file <- args[1]


if(file.exists(config.file)==FALSE){
  cat("\n##############################################################")
  cat("\n# START: Missing Config File! Verify the following path:     #")
  cat("\n# ", config.file, "                                          #")
  cat("\n##############################################################\n\n")
  break
} else {
  cat("\n##############################################")
  cat("\n# START: Configuration file properly loaded  #")
  cat("\n##############################################\n\n")
}



cat("\n######################################################################")
cat("\n# START: Read Parameters                                             #\n")
config = data.frame(read.csv(config.file))
print(config)
cat("\n######################################################################\n\n")


#####################################
# creating a parameters list
parameters = list()
#####################################


# DATASET PATH
dataset.path = toString(config$Value[1])
dataset.path = str_remove(dataset.path, pattern = " ")
parameters$path.dataset = dataset.path

# TEMPORARTY PATH - FOLDER RESULTS 
folder.results = toString(config$Value[2])
folder.results = str_remove(folder.results, pattern = " ")
parameters$folder.results = folder.results

# PARTITIONS
partitions.path = toString(config$Value[3])
partitions.path = str_remove(partitions.path, pattern = " ")
parameters$path.partitions = partitions.path

# IMPLEMENTATION
implementation = toString(config$Value[4])
implementation = str_remove(implementation, pattern = " ")
parameters$implementation = implementation

# SIMILARITY
similarity = toString(config$Value[5])
similarity = str_remove(similarity, pattern = " ")
parameters$similarity = similarity

# DENDROGRAM
dendrogram = toString(config$Value[6])
dendrogram = str_remove(dendrogram, pattern = " ")
parameters$dendrogram = dendrogram

# CRITERIA
criteria = toString(config$Value[7])
criteria = str_remove(criteria, pattern = " ")
parameters$criteria = criteria

# dataset.name
dataset.name = toString(config$Value[8])
dataset.name = str_remove(dataset.name, pattern = " ")
parameters$dataset.name = dataset.name

# DATASET_NUMBER
number.dataset = as.numeric(config$Value[9])
parameters$number.dataset = number.dataset

# number.folds
number.folds = as.numeric(config$Value[10])
parameters$number.folds = number.folds

# number.cores
number.cores = as.numeric(config$Value[11])
parameters$number.cores = number.cores

# DATASET_INFO
ds = datasets[number.dataset,]
parameters$dataset.info = ds


cat("\n################################################################\n")
print(ds)
cat("\n################################################################\n")


cat("\n##########################################")
cat("\n# START: Creating Temp Folder            #")
cat("\n##########################################\n\n")
if (dir.exists(folder.results) == FALSE) {dir.create(folder.results)}


cat("\n#############################")
cat("\n# START: Get directories    #")
cat("\n#############################\n\n")
diretorios <- directories(parameters)


#####################################
parameters$Folders = diretorios
#####################################


cat("\n####################################################################")
cat("\n# START: Checking the DATASET tar.gz file                          #")
cat("\n####################################################################\n\n")
str00 = paste(dataset.path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){
  
  cat("\n##########################################################################")
  cat("\n# START: The tar.gz file for the dataset to be processed does not exist! #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file!     #")
  cat("\n# The path entered was: ", str00, "                                      #")
  cat("\n######################################################################\n\n")
  break
  
} else {
  
  cat("\n####################################################################")
  cat("\n# START: tar.gz file of the DATASET loaded correctly!              #")
  cat("\n####################################################################\n\n")
  
  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderDatasets, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }
  
  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderDatasets, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderDatasets, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }
  
  #APAGANDO
  str03 = paste("rm ", diretorios$folderDatasets, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }
  
}

cat("\n####################################################################")
cat("\n# START: Checking the PARTITIONS tar.gz file                       #")
cat("\n####################################################################\n\n")
str00 = paste(partitions.path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){
  
  cat("\n##########################################################################")
  cat("\n# START: The tar.gz file for the partitions to be processed does not exist! #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file!     #")
  cat("\n# The path entered was: ", str00, "                                      #")
  cat("\n##########################################################################\n\n")
  break
  
} else {
  
  cat("\n##################################################################")
  cat("\n# START: tar.gz file of the PARTITION loaded correctly!          #")
  cat("\n##################################################################\n\n")
  
  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderBPSC , sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }
  
  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderBPSC, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderBPSC, "/ ",
                sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }
  
  #APAGANDO
  str03 = paste("rm ", diretorios$folderBPSC, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }
  
}



cat("\n####################################################################")
cat("\n# START: EXECUTE                                                   #")
cat("\n####################################################################\n\n")
timeFinal <- system.time(results <- executa(parameters))
result_set <- t(data.matrix(timeFinal))
setwd(diretorios$folderTested)
write.csv(result_set, "Final-Runtime.csv")

x.minutos = (1 * as.numeric(result_set[3]))/60
setwd(diretorios$folderTested)
write(x.minutos, "minutos.txt")

# The definition of ‘user’ and ‘system’ times is from your OS. 
# Typically it is something like

# The ‘user time’ is the CPU time charged for the execution of 
# user instructions of the calling process. 

# The ‘system time’ is the CPU time charged for execution by 
# the system on behalf of the calling process.

# Times of child processes are not available on Windows and will 
# always be given as NA

# The first two entries are the total user and system CPU times 
# of the current R process and any child processes on which it has 
# waited, and the third entry is the ‘real’ elapsed time since the 
# process was started. 


cat("\n####################################################################")
cat("\n# START: DELETING DATASETS AND PARTITIONS FOLDERS                  #")
cat("\n####################################################################\n\n")
print(system(paste("rm -r ", diretorios$folderDatasets, sep="")))
print(system(paste("rm -r ", diretorios$folderBestPartitions, sep="")))


# cat("\n####################################################################")
# cat("\n# Compress folders and files                                       #")
# cat("\n####################################################################\n\n")
# str_a <- paste("tar -zcf ", diretorios$folder.results, "/", dataset.name,
#                "-", similarity, "-results-bps-e.tar.gz ",
#                diretorios$folder.results, sep = "")
# print(system(str_a))



if(parameters$implementation == "python"){
  
  cat("\n####################################################################")
  cat("\n# START: COPY TO GOOGLE DRIVE                                      #")
  cat("\n####################################################################\n\n")
  origem = diretorios$folderTested
  destino = paste("nuvem:Complete-Chains-HPML/", 
                  parameters$implementation, "/",
                  parameters$similarity,  "/",
                  parameters$dendrogram,  "/",
                  parameters$criteria,  "/",
                  parameters$dataset.name, sep="")
  comando = paste("rclone -P copy ", origem, " ", destino, sep="")
  cat("\n", comando, "\n")
  a = print(system(comando))
  a = as.numeric(a)
  if(a != 0) {
    stop("Erro RCLONE")
    quit("yes")
  }
  
  # cat("\n############################################################")
  # cat("\n# START: Copy to root folder                               #")
  # cat("\n############################################################\n\n")
  # 
  # folderO = paste(FolderRoot, "/Output", sep="")
  # if(dir.exists(folderO)==FALSE){dir.create(folderO)}
  # 
  # folderC = paste(folderO, "/Python", sep="")
  # if(dir.exists(folderC)==FALSE){dir.create(folderC)}
  # 
  # folderS = paste(folderC, "/", similarity, sep="")
  # if(dir.exists(folderS)==FALSE){dir.create(folderS)}
  # 
  # str_b <- paste("cp -r ", diretorios$folder.results, " ", 
  #                folderS, sep = "")
  # print(system(str_b))
  
} else { 
  
}


cat("\n####################################################################")
cat("\n# START: DELETE                                                    #")
cat("\n####################################################################\n\n")
str_c = paste("rm -r ", diretorios$folder.results, sep="")
print(system(str_c))

rm(list = ls())
gc()


cat("\n\n##########################################################")
cat("\n# START: COMPLETE CHAINS OF HYBRID PARTITION END         #")
cat("\n##########################################################")
cat("\n\n")


#############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com              #
# Thank you very much!                                                      #
#############################################################################