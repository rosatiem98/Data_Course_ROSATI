library(tidyverse)
library(seqinr)
library(Biostrings)
library(msa)
library(phangorn)
library(ShortRead)
memory.limit()
#Practice with sreads

reads=readFasta("plant_ITS (1).fasta")
readssequences=sread(reads) 
substr(readssequences,1,5)
table(substr(readssequences,1,5))

myreads=reads[substr(readssequences,1,5)=="TAACA"]

myreads

dict=DNAStringSet(substr(readssequences,1,5))

hits=vcountPattern("TAACA", dict,
                   max.mismatch = 1, with.indels = TRUE)
sum(hits)

sread(reads[hits])


myreads2=reads[substr(readssequences,1,5)=="GTCCA"]

myreads2

dict=DNAStringSet(substr(readssequences,1,5))

hits2=vcountPattern("GTCCA", dict, 
                   max.mismatch = 1, with.indels = TRUE)

sum(hits2)
sread(reads[hits2])

#Alignmet practice/example
mySequencefile <- system.file("examples", "exampleAA.fasta", package="msa")
mySequences <- readAAStringSet(mySequencefile)
mySequences

myFirstAlignment <- msa(mySequences)
myFirstAlignment

print(myFirstAlignment, show="complete")

msaPrettyPrint(myFirstAlignment, y=c(164,213), output="asis",
               showNames = "none", showLogo = "none", askForOverwrite = FALSE)

#Using own data
mySequencefile <- system.file("plant_ITS (1).fasta", package = "Biostrings")
file.exists("plant_ITS (1).fasta")
dna <- readDNAStringSet("./plant_ITS (1).fasta")

smalldna <- dna[c(1:50)]

trial <- msa(smalldna)
#remove long sequences
trial
library(ggmsa)
align <- ggmsa(smalldna)
align
#saveRDS
saveRDS(align, "./align.RDS")
alignment <- readRDS("./align.RDS")

msaPrettyPrint(trial, output="pdf", askForOverwrite = FALSE)

tmpfile <- tempfile(pattern = "msa", tmpdir = ".", fileext = ".pdf")
msaPrettyPrint(tmpfile, output="pdf", askForOverwrite = FALSE)

