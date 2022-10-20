#feed in all arguments through single quotes
#--url, --user_id, --name are the parameters

#feature set: 
# code is a mix of R and python
# feed url / user id / or name of researcher
# spits out graphs of citations per year, number of papers published per year
# spits out graph of which journals published in each year
# spits out csv of the number of papers published in each journal
# spits out plot of the number of papers published in each journal 
# stores data in a folder on your computer 
	#(NEED WAY TO STORE DATA NOT WITH INTERMEDIATE PRODUCTS)
	#tmp directory and results directory (for pdf and table csv final)
	#store in a folder with the subjectid
#

library(tidyverse)
library(stringi)

args = commandArgs(trailingOnly=TRUE)

hh <- paste(unlist(args),collapse=' ')
listoptions <- unlist(strsplit(hh,'--'))[-1]
options.args <- sapply(listoptions,function(x){
         unlist(strsplit(x, ' '))[-1]
        }, simplify=FALSE)
options.names <- sapply(listoptions,function(x){
  option <-  unlist(strsplit(x, ' '))[1]
})
names(options.args) <- unlist(options.names)

if (names(options.args)[1]=="url"){

	id <- options.args[[1]]

	id <- gsub(".*user=", "", id)

	start_pos <- str_locate(stringi::stri_reverse(id), "[[:punct:]]+") %>% as_tibble() %>% dplyr::slice(1) %>% pull(start)

	if (!is.na(start_pos)){

		id <- substr(id, 1, nchar(id) - start_pos)
	}

	system(paste0('python3 analysis.py --subjectid ', id))

	system(paste0('Rscript plot.R ', id))

}

if (names(options.args)[1]=="user_id"){

	id <- options.args[[1]]

	system(paste0('python3 analysis.py --subjectid ', id))

	system(paste0('Rscript plot.R ', id))

}

if (names(options.args)[1]=="name"){

	id <- options.args[[1]]

	id <- paste0(id, collapse=" ")

	subjectid <- system(paste0("python3 analysis.py --name '", id, "'"), intern=TRUE)

	system(paste0('Rscript plot.R ', subjectid))

}



