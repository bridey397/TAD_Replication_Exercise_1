#### Welcome ####

#All code is in this one giant R script, denoted by chapters. 
#Chapters 1-5 are about preparing the datasets.
#Start at chapter 6 if you want to work with the already prepared datasets.

#### 1 Set Up Datasets ####

#Set Working Directory
setwd("/Users/steverathje/Desktop/MotivatedTweeting2")

#Load in Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "tidygraph", "rtweet", "ggraph", "tidytext", "stopwords", "sentimentr", "lubridate", "textfeatures", "wordcloud", "RColorBrewer", "quanteda", "quanteda.dictionaries", "syuzhet", "Rcpp", "quanteda", "tidyverse", "MASS", "lmtest", "QuantPsyc", "pastecs", "psych", "tidytext", "plyr", "jtools", "dotwhisker", "cowplot", "meta", "metafor", "metaviz")
ipak(packages)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)

## Read in Facebook datasets
dem <- read_csv("CrowdtangleData/DemocratData.csv")
repub <- read_csv("CrowdtangleData/RepublicanData.csv")
demhouse <- read_csv("CrowdtangleData/DemocratHouseFull.csv")
demsenate <- read_csv("CrowdtangleData/DemocratSenate.csv")
repubhouse <- read_csv("CrowdtangleData/RepublicanHouseFull.csv")
repubsenate <- read_csv("CrowdtangleData/RepublicanSenateFull.csv")
demcongress <- rbind.fill(demhouse, demsenate)
repubcongress <- rbind.fill(repubhouse, repubsenate)

## Load in dictionaries
PositiveAffect <- scan("Dictionaries/PositiveAffectLIWC2007.txt", what='character', sep="\n", skipNul = TRUE)
PositiveAffect <- strsplit(PositiveAffect, "[[:space:]]+")
NegativeAffect <- scan("Dictionaries/NegativeAffectLIWC2007.txt", what='character', sep="\n", skipNul = TRUE)
NegativeAffect <- strsplit(NegativeAffect, "[[:space:]]+")
Anger <- scan("Dictionaries/AngerLIWC2007.txt", what='character', sep="\n", skipNul = TRUE)
Anger <- strsplit(Anger, "[[:space:]]+")
Sadness <- scan("Dictionaries/SadnessLIWC2007.txt", what='character', sep="\n", skipNul = TRUE)
Sadness <- strsplit(Sadness, "[[:space:]]+")
Anxiety <- scan("Dictionaries/AnxietyLIWC2007.txt", what='character', sep="\n", skipNul = TRUE)
Anxiety <- strsplit(Anxiety, "[[:space:]]+")
Moral <- scan("Dictionaries/Moral.txt", what='character', sep="\n", skipNul = TRUE)
Moral <- strsplit(Moral, "[[:space:]]+")
MoralEmotional <- scan("Dictionaries/MoralEmotional.txt", what='character', sep="\n", skipNul = TRUE)
MoralEmotional <- strsplit(MoralEmotional, "[[:space:]]+")
MoralEmotionalPositive <- scan("Dictionaries/MoralEmotionalPositive.txt", what='character', sep="\n", skipNul = TRUE)
MoralEmotionalPositive <- strsplit(MoralEmotionalPositive, "[[:space:]]+")
MoralEmotionalNegative <- scan("Dictionaries/MoralEmotionalNegative.txt", what='character', sep="\n", skipNul = TRUE)
MoralEmotionalNegative <- strsplit(MoralEmotionalNegative, "[[:space:]]+")
TopRepublican <- scan("Dictionaries/MostFamousRepublicans.txt", what='character', sep="\t", skipNul = TRUE)
TopDemocrat <- scan("Dictionaries/MostFamousDemocrats.txt", what='character', sep="\t", skipNul = TRUE)
DemocratCongress <- scan("Dictionaries/DemocratCongress.txt", what='character', sep="\t", skipNul = TRUE)
RepublicanCongress <- scan("Dictionaries/RepublicansCongress.txt", what='character', sep="\t", skipNul = TRUE)
TopRepublicanFB <- scan("Dictionaries/MostFamousRepublicansFacebook.txt", what='character', sep="\t", skipNul = TRUE)
TopDemocratFB <- scan("Dictionaries/MostFamousDemocratsFacebook.txt", what='character', sep="\n", skipNul = TRUE)
DemocratCongressFB <- scan("Dictionaries/DemocratCongressFB.txt", what='character', sep="\n", skipNul = TRUE)
RepublicanCongressFB <- scan("Dictionaries/RepublicansCongressFB.txt", what='character', sep="\n", skipNul = TRUE)

## Polarization Dictionary
Polarization <- scan("Dictionaries/PolarizationDictionary.txt", what='character', sep="\t", skipNul = TRUE)

## Identity Dictionaries
liberalidentity = c("socialist*", "communist*", "marxist*", "leftist*", "liberal*", "left-wing*", "progressive*", "social justice warrior", "antifa", "democrat*", "dem", "dems", "libs")
conservativeidentity = c("conservative*", "gop", "republican*", "libertarian*", "alt-right", "right-wing", "fascist*", "far-right", "far right", "repub", "repubs", "maga")

## Create Dictionaries 
group = dictionary(list(liberalidentity = liberalidentity,
                        conservativeidentity = conservativeidentity,
                        TopDemocrat = TopDemocrat, 
                        TopRepublican = TopRepublican, 
                        DemocratCongress = DemocratCongress,
                        RepublicanCongress = RepublicanCongress, 
                        Democrat = TopDemocrat,
                        Democrat = DemocratCongress,
                        Democrat = liberalidentity,
                        Republican = TopRepublican,
                        Republican = RepublicanCongress,
                        Republican = conservativeidentity,
                        Democrat2 = TopDemocrat,
                        Democrat2 = liberalidentity,
                        Republican2 = TopRepublican,
                        Republican2 = conservativeidentity, 
                        PositiveAffect = PositiveAffect, 
                        NegativeAffect = NegativeAffect, 
                        MoralEmotionalPositive = MoralEmotionalPositive,
                        MoralEmotionalNegative = MoralEmotionalNegative,
                        MoralEmotional = MoralEmotional,
                        Moral = Moral, 
                        Affect = NegativeAffect,
                        Affect = PositiveAffect,
                        Anger = Anger,
                        Anxiety = Anxiety,
                        Sadness = Sadness,
                        Polarization = Polarization))

## Create Dictionaries -- FACEBOOK NAMES ADDED
group2 = dictionary(list(liberalidentity = liberalidentity,
                        conservativeidentity = conservativeidentity,
                        TopDemocrat = TopDemocratFB, 
                        TopRepublican = TopRepublicanFB, 
                        DemocratCongress = DemocratCongressFB,
                        RepublicanCongress = RepublicanCongressFB, 
                        Democrat = TopDemocratFB,
                        Democrat = DemocratCongressFB,
                        Democrat = liberalidentity,
                        Republican = TopRepublicanFB,
                        Republican = RepublicanCongressFB,
                        Republican = conservativeidentity,
                        Democrat2 = TopDemocratFB,
                        Democrat2 = liberalidentity,
                        Republican2 = TopRepublicanFB,
                        Republican2 = conservativeidentity, 
                        PositiveAffect = PositiveAffect, 
                        NegativeAffect = NegativeAffect, 
                        MoralEmotionalPositive = MoralEmotionalPositive,
                        MoralEmotionalNegative = MoralEmotionalNegative,
                        MoralEmotional = MoralEmotional,
                        Moral = Moral, 
                        Affect = NegativeAffect,
                        Affect = PositiveAffect,
                        Anger = Anger,
                        Anxiety = Anxiety,
                        Sadness = Sadness,
                        Polarization = Polarization))

#### 2 FACEBOOK Set Up Datasets ####

#Democrat Dataset Dictionary Analysis
dem$text <- paste(dem$Message, dem$`Image Text`, dem$`Link Text`)
dem$text <- gsub("NA", "", dem$text)
liberalmedia_corpus = corpus(dem, text_field = 'text')
toks <- tokens(liberalmedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
liberalmedia_dict = dfm(toks, dictionary = group)
liberalmedia_dict_df <- quanteda::convert(liberalmedia_dict, to='data.frame')
liberalmediaplus = cbind(liberalmedia_dict_df, dem)
liberalmediaplus$doc_id <- NULL

#Conservative Dataset Dictionary Analysis
repub$text <- paste(repub$Message, repub$`Image Text`, repub$`Link Text`)
repub$text <- gsub("NA", "", repub$text)
conservativemedia_corpus = corpus(repub, text_field = 'text')
toks <- tokens(conservativemedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
conservativemedia_dict = dfm(toks, dictionary = group)
conservativemedia_dict_df <- quanteda::convert(conservativemedia_dict, to='data.frame')
conservativemediaplus = cbind(conservativemedia_dict_df, repub)
conservativemediaplus$doc_id <- NULL

#Democrat Congress Dictionary Analysis
demcongress$text <- paste(demcongress$Message, demcongress$`Image Text`, demcongress$`Link Text`)
demcongress$text <- gsub("NA", "", demcongress$text)
liberalcongress_corpus = corpus(demcongress, text_field = 'text')
toks <- tokens(liberalcongress_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
liberalcongress_dict = dfm(toks, dictionary = group)
liberalcongress_dict_df <- quanteda::convert(liberalcongress_dict, to='data.frame')
liberalcongressplus = cbind(liberalcongress_dict_df, demcongress)
liberalcongressplus$doc_id <- NULL

#Conservative Congress Dataset Dictionary Analysis
repubcongress$text <- paste(repubcongress$Message, repubcongress$`Image Text`, repubcongress$`Link Text`)
repubcongress$text <- gsub("NA", "", repubcongress$text)
conservativemedia_corpus = corpus(repubcongress, text_field = 'text')
toks <- tokens(conservativemedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
conservativecongress_dict = dfm(toks, dictionary = group)
conservativecongress_dict_df <- quanteda::convert(conservativecongress_dict, to='data.frame')
conservativecongressplus = cbind(conservativecongress_dict_df, repubcongress)
conservativecongressplus$doc_id <- NULL

#### 3 Facebook Set Up Datasets UPDATED DICTIONARIES ####

#Democrat Dataset Dictionary Analysis
dem$text <- paste(dem$Message, dem$`Image Text`, dem$`Link Text`)
dem$text <- gsub("NA", "", dem$text)
liberalmedia_corpus = corpus(dem, text_field = 'text')
toks <- tokens(liberalmedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
liberalmedia_dict = dfm(toks, dictionary = group2)
liberalmedia_dict_df <- quanteda::convert(liberalmedia_dict, to='data.frame')
liberalmediaFB = cbind(liberalmedia_dict_df, dem)
liberalmediaFB$doc_id <- NULL

#Conservative Dataset Dictionary Analysis
repub$text <- paste(repub$Message, repub$`Image Text`, repub$`Link Text`)
repub$text <- gsub("NA", "", repub$text)
conservativemedia_corpus = corpus(repub, text_field = 'text')
toks <- tokens(conservativemedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
conservativemedia_dict = dfm(toks, dictionary = group2)
conservativemedia_dict_df <- quanteda::convert(conservativemedia_dict, to='data.frame')
conservativemediaFB = cbind(conservativemedia_dict_df, repub)
conservativemediaFB$doc_id <- NULL

#Democrat Congress Dictionary Analysis
demcongress$text <- paste(demcongress$Message, demcongress$`Image Text`, demcongress$`Link Text`)
demcongress$text <- gsub("NA", "", demcongress$text)
liberalcongress_corpus = corpus(demcongress, text_field = 'text')
toks <- tokens(liberalcongress_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
liberalcongress_dict = dfm(toks, dictionary = group2)
liberalcongress_dict_df <- quanteda::convert(liberalcongress_dict, to='data.frame')
liberalcongressFB = cbind(liberalcongress_dict_df, demcongress)
liberalcongressFB$doc_id <- NULL

#Conservative Congress Dataset Dictionary Analysis
repubcongress$text <- paste(repubcongress$Message, repubcongress$`Image Text`, repubcongress$`Link Text`)
repubcongress$text <- gsub("NA", "", repubcongress$text)
conservativemedia_corpus = corpus(repubcongress, text_field = 'text')
toks <- tokens(conservativemedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
conservativecongress_dict = dfm(toks, dictionary = group2)
conservativecongress_dict_df <- quanteda::convert(conservativecongress_dict, to='data.frame')
conservativecongressFB = cbind(conservativecongress_dict_df, repubcongress)
conservativecongressFB$doc_id <- NULL

View(liberalmediaFB)

#### 4 TWITTER Set Up Datasets #### 

## Media Accounts 

## Read in datasets ##
leftcentermedia <- readRDS("Data/leftcentermedia.rds")
leftcentermedia2 <- readRDS("Data/leftcentermediaJuly1.rds")
rightcentermedia <- readRDS("Data/rightcentermedia.rds")
rightcentermedia2 <- readRDS("Data/rightcentermediaJuly1.rds")
farliberalmedia <- readRDS("Data/liberalmedia.rds")
farliberalmedia2 <- readRDS("Data/liberalmediaJuly1.rds")
farconservativemedia <- readRDS("Data/conservativemedia.rds")
facconservativemedia2 <- readRDS("Data/conservativemediaJuly1.rds")

## Combine center left and right with far left and right

liberalcenter <- rbind.fill(leftcentermedia, leftcentermedia2)
liberalmedia <- rbind.fill(leftcentermedia, leftcentermedia2, farliberalmedia, farliberalmedia2)
conservativemedia <- rbind.fill(rightcentermedia, rightcentermedia2,  farconservativemedia, facconservativemedia2)
conservativecenter <- rbind.fill(rightcentermedia, rightcentermedia2)

## Delete Duplicates 

liberalmedia = liberalmedia[!duplicated(liberalmedia$text),]
conservativemedia = conservativemedia[!duplicated(conservativemedia$text),]

#Twitter Media Analysis
conservativemedia_corpus = corpus(conservativemedia)
toks <- tokens(conservativemedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
conservativemedia_dict = dfm(toks, dictionary = group)
conservativemedia_dict_df <- quanteda::convert(conservativemedia_dict, to='data.frame')
conservativemediaTwitter = cbind(conservativemedia_dict_df, conservativemedia)
conservativemediaTwitter$doc_id <- NULL

#Liberal Analysis
liberalmedia_corpus = corpus(liberalmedia)
toks <- tokens(liberalmedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
liberalmedia_dict = dfm(toks, dictionary = group)
liberalmedia_dict_df <- quanteda::convert(liberalmedia_dict, to='data.frame')
liberalmediaTwitter = cbind(liberalmedia_dict_df, liberalmedia)
liberalmediaTwitter$doc_id <- NULL

## Congressional Accounts

#Read in Congress Tweets
congress <- readRDS("Data/congresstweets.rds")
congressRepub <- subset(congress, party == "Republican")
congressDem <- subset(congress, party == "Democrat")

#Congress Repub Dataset
congressRepubcorpus = corpus(congressRepub)
toks <- tokens(congressRepubcorpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
congressRepubdict <- dfm(toks, dictionary = group, verbose = TRUE)
congressRepub_dict_df <- quanteda::convert(congressRepubdict, to='data.frame')
congressRepubTwitter = cbind(congressRepub_dict_df, congressRepub)
congressRepubTwitter$doc_id <- NULL

#Congress Dem Dataset
congressDemcorpus = corpus(congressDem)
toks <- tokens(congressDemcorpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
congressDemdict <- dfm(toks, dictionary = group, verbose = TRUE)
congressDem_dict_df <- quanteda::convert(congressDemdict, to='data.frame')
congressDemTwitter = cbind(congressDem_dict_df, congressDem)
congressDemTwitter$doc_id <- NULL

#### 5 Save Datasets ####

saveRDS(liberalmediaplus, "Data/LiberalMediaFacebook.rds") 
saveRDS(conservativemediaplus, "Data/ConservativeMediaFacebook.rds")
saveRDS(liberalcongressplus, "Data/LiberalCongressFacebook.rds") 
saveRDS(conservativecongressplus, "Data/ConservativeCongressFacebook.rds") 
saveRDS(conservativemediaTwitter, "Data/ConservativeMediaTwitter.rds") 
saveRDS(liberalmediaTwitter, "Data/LiberalMediaTwitter.rds")
saveRDS(congressRepubTwitter, "Data/ConservativeCongressTwitter.rds")
saveRDS(congressDemTwitter, "Data/CLiberalCongressTwitter.rds") 

saveRDS(liberalmediaFB, "Data/LiberalMediaFacebookUpdated.rds")
saveRDS(conservativemediaFB, "Data/ConservativeMediaFacebookUpdated.rds")
saveRDS(liberalcongressFB, "Data/LiberalCongressFacebookUpdated.rds")
saveRDS(conservativecongressFB, "Data/ConservativeCongressFacebookUpdated.rds")

#### 6 Read in Datasets -- Start Here! ####

liberalmediaplus <- readRDS("Data/LiberalMediaFacebookUpdated.rds")
conservativemediaplus <- readRDS("Data/ConservativeMediaFacebookUpdated.rds")
liberalcongressplus <- readRDS("Data/LiberalCongressFacebookUpdated.rds")
conservativecongressplus <- readRDS("Data/ConservativeCongressFacebookUpdated.rds")
conservativemediaTwitter <- readRDS("Data/ConservativeMediaTwitter.rds")
liberalmediaTwitter <- readRDS("Data/LiberalMediaTwitter.rds")
congressRepubTwitter <- readRDS("Data/ConservativeCongressTwitter.rds")
congressDemTwitter <- readRDS("Data/CLiberalCongressTwitter.rds")

#### 7 Facebook Analysis #### 

#Add Variables
conservativemediaplus$has_URL <- ifelse(conservativemediaplus$Type == "Link", TRUE, FALSE)
conservativemediaplus$has_media <- ifelse(conservativemediaplus$Type != "Link" & conservativemediaplus$Type != "Status", TRUE, FALSE)
#Conservative Media Analysis
conservativemediaplus$shares_log <- log(conservativemediaplus$Shares + 1)
LinearModConMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
LinearModConMediaSumm <- summ(LinearModConMedia, exp = TRUE, center = TRUE, vifs = TRUE)

#Add Variables
liberalmediaplus$has_URL <- ifelse(liberalmediaplus$Type == "Link", TRUE, FALSE)
liberalmediaplus$has_media <- ifelse(liberalmediaplus$Type != "Link" & liberalmediaplus$Type != "Status", TRUE, FALSE)
#Liberal Media Analysis
liberalmediaplus$shares_log <- log(liberalmediaplus$Shares + 1)
LinearModDemMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
LinearModDemMediaSumm <- summ(LinearModDemMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModDemMediaSumm

#Add Variables
liberalcongressplus$has_URL <- ifelse(liberalcongressplus$Type == "Link", TRUE, FALSE)
liberalcongressplus$has_media <- ifelse(liberalcongressplus$Type != "Link" & liberalcongressplus$Type != "Status", TRUE, FALSE)
#Liberal Congress Analysis
liberalcongressplus$shares_log <- log(liberalcongressplus$Shares + 1)
linearModDemCongress <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
linearModDemCongresssumm <- summ(linearModDemCongress, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModDemCongresssumm 

##Conservative Congress Analysis

#Add Variables
conservativecongressplus$has_URL <- ifelse(conservativecongressplus$Type == "Link", TRUE, FALSE)
conservativecongressplus$has_media <- ifelse(conservativecongressplus$Type != "Link" & conservativecongressplus$Type != "Status", TRUE, FALSE)
conservativecongressplus$shares_log <- log(conservativecongressplus$Shares + 1)
#Analysis 
linearModCongressRepub <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
linearModCongressRepubsumm <- summ(linearModCongressRepub, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubsumm

#### 8 Facebook Analyis 2 (Don't Do This One) ####

## Conservative Media Analysis 

#Add Variables
conservativemediaFB$has_URL <- ifelse(conservativemediaFB$Type == "Link", TRUE, FALSE)
conservativemediaFB$has_media <- ifelse(conservativemediaFB$Type != "Link" & conservativemediaFB$Type != "Status", TRUE, FALSE)
#Conservative Media Analysis
conservativemediaFB$shares_log <- log(conservativemediaFB$Shares + 1)
LinearModConMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = conservativemediaFB)
LinearModConMediaSumm <- summ(LinearModConMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModConMediaSumm

## Liberal Media Analysis

#Add Variables
liberalmediaFB$has_URL <- ifelse(liberalmediaFB$Type == "Link", TRUE, FALSE)
liberalmediaFB$has_media <- ifelse(liberalmediaFB$Type != "Link" & liberalmediaFB$Type != "Status", TRUE, FALSE)
#Liberal Media Analysis
liberalmediaFB$shares_log <- log(liberalmediaFB$Shares + 1)
LinearModDemMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = liberalmediaFB)
LinearModDemMediaSumm <- summ(LinearModDemMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModDemMediaSumm

##Liberal Congress Analysis

#Add Variables
liberalcongressFB$has_URL <- ifelse(liberalcongressFB$Type == "Link", TRUE, FALSE)
liberalcongressFB$has_media <- ifelse(liberalcongressFB$Type != "Link" & liberalcongressFB$Type != "Status", TRUE, FALSE)
#Liberal Congress Analysis
liberalcongressFB$shares_log <- log(liberalcongressFB$Shares + 1)
linearModDemCongress <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = liberalcongressFB)
linearModDemCongresssumm <- summ(linearModDemCongress, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModDemCongresssumm

##Conservative Congress Analysis

#Add Variables
conservativecongressFB$has_URL <- ifelse(conservativecongressFB$Type == "Link", TRUE, FALSE)
conservativecongressFB$has_media <- ifelse(conservativecongressFB$Type != "Link" & liberalcongressFB$Type != "Status", TRUE, FALSE)
#Conservative Congress Analysis 
conservativecongressFB$likes_log <- log(conservativecongressFB$Likes + 1)
conservativecongressFB$angry_log <- log(conservativecongressFB$Angry + 1)
conservativecongressFB$shares_log <- log(conservativecongressFB$Shares + 1)
linearModCongressRepub <- glm(shares_log ~ liberalidentity + conservativeidentity + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = conservativecongressFB)
linearModCongressRepubsumm <- summ(linearModCongressRepub, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubsumm

#### 9 Twitter Anaylsis ####

## Conservative Media Analysis 

#Add Variables
conservativemediaTwitter$has_media <- is.na(conservativemediaTwitter$media_type) == FALSE
conservativemediaTwitter$has_URL <- is.na(conservativemediaTwitter$urls_url) == FALSE
#Log Transform
conservativemediaTwitter$retweet_count_log <- log(conservativemediaTwitter$retweet_count + 1)
#Model  
ConservativeTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=conservativemediaTwitter)
ConservativeTwitterModsumm <- summ(ConservativeTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
ConservativeTwitterModsumm

## Liberal Media Analysis

#Add Variables
liberalmediaTwitter$has_media <- is.na(liberalmediaTwitter$media_type) == FALSE
liberalmediaTwitter$has_URL <- is.na(liberalmediaTwitter$urls_url) == FALSE
#Log Transform
liberalmediaTwitter$retweet_count_log <- log(liberalmediaTwitter$retweet_count + 1)
liberalmediaTwitter$favorite_count_log <- log(liberalmediaTwitter$favorite_count + 1)
#Model
LiberalMediaTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=liberalmediaTwitter)
LiberalMediaTwitterModSumm <- summ(LiberalMediaTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)

## Conservative Congress Analysis 

#Create Control Variables
congressRepubTwitter$has_media <- is.na(congressRepubTwitter$media_type) == FALSE
congressRepubTwitter$has_URL <- is.na(congressRepubTwitter$urls_url) == FALSE
#Log Transform Retweet Count
congressRepubTwitter$retweet_count_log <- log(congressRepubTwitter$retweet_count + 1)
#Model
congressRepubTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=congressRepubTwitter)
congressRepubTwitterSumm <- summ(congressRepubTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressRepubTwitterSumm

## Liberal Congress Analysis 

congressDemTwitter$has_media <- is.na(congressDemTwitter$media_type) == FALSE
congressDemTwitter$has_URL <- is.na(congressDemTwitter$urls_url) == FALSE
#Log Transform Retweet Count
congressDemTwitter$retweet_count_log <- log(congressDemTwitter$retweet_count + 1)
#Model
congressDemTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=congressDemTwitter)
congressDemTwitterSumm <- summ(congressDemTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressDemTwitterSumm

#### 10 Clustered Standard Errors ####

utils::install.packages("miceadds")
devtools::install_github("alexanderrobitzsch/miceadds")
library(miceadds)
library(sandwhich)

ConTwitterCluster <- summ(ConservativeTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "screen_name")
LibTwitterCluster <- summ(LiberalMediaTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "screen_name")
ConFacebookCluster <- summ(LinearModConMedia, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
LibFacebookCluster <- summ(LinearModDemMedia, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")

ConCongressTwitterCluster <- summ(congressRepubTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
LibCongressTwitterCluster <- summ(congressDemTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
ConCongressFacebookCluster <- summ(linearModCongressRepub, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
LibCongressFacebookCluster <- summ(linearModDemCongress, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")

get_robust_se(
  LinearModConMediaSumm,
  type = "HC3",
  cluster = "Page Name",
  data = conservativemediaplus,
  vcov = NULL
)

conmediacluster <- glm.cluster(conservativemediaplus, 
            formula = shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, 
            cluster = "Page Name")
libmediacluster <- glm.cluster(liberalmediaplus, 
                               formula = shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, 
                               cluster = "Page Name")
summ(libmediacluster)

View(conservativemediaplus)
?glm.cluster()
shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`

#### Regressions Without Control Variables #### 

## Conservative Media Analysis 

#Add Variables
conservativemediaTwitter$has_media <- is.na(conservativemediaTwitter$media_type) == FALSE
conservativemediaTwitter$has_URL <- is.na(conservativemediaTwitter$urls_url) == FALSE
#Log Transform
conservativemediaTwitter$retweet_count_log <- log(conservativemediaTwitter$retweet_count + 1)
#Model  
ConservativeTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=conservativemediaTwitter)
ConservativeTwitterModsumm <- summ(ConservativeTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
ConservativeTwitterModsumm

## Liberal Media Analysis

#Add Variables
liberalmediaTwitter$has_media <- is.na(liberalmediaTwitter$media_type) == FALSE
liberalmediaTwitter$has_URL <- is.na(liberalmediaTwitter$urls_url) == FALSE
#Log Transform
liberalmediaTwitter$retweet_count_log <- log(liberalmediaTwitter$retweet_count + 1)
liberalmediaTwitter$favorite_count_log <- log(liberalmediaTwitter$favorite_count + 1)
#Model
LiberalMediaTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=liberalmediaTwitter)
LiberalMediaTwitterModSumm <- summ(LiberalMediaTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)

## Conservative Congress Analysis 

#Create Control Variables
congressRepubTwitter$has_media <- is.na(congressRepubTwitter$media_type) == FALSE
congressRepubTwitter$has_URL <- is.na(congressRepubTwitter$urls_url) == FALSE
#Log Transform Retweet Count
congressRepubTwitter$retweet_count_log <- log(congressRepubTwitter$retweet_count + 1)
#Model
congressRepubTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=congressRepubTwitter)
congressRepubTwitterSumm <- summ(congressRepubTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressRepubTwitterSumm

## Liberal Congress Analysis 

congressDemTwitter$has_media <- is.na(congressDemTwitter$media_type) == FALSE
congressDemTwitter$has_URL <- is.na(congressDemTwitter$urls_url) == FALSE
#Log Transform Retweet Count
congressDemTwitter$retweet_count_log <- log(congressDemTwitter$retweet_count + 1)
#Model
congressDemTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=congressDemTwitter)
congressDemTwitterSumm <- summ(congressDemTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressDemTwitterSumm

#Add Variables
conservativemediaplus$has_URL <- ifelse(conservativemediaplus$Type == "Link", TRUE, FALSE)
conservativemediaplus$has_media <- ifelse(conservativemediaplus$Type != "Link" & conservativemediaplus$Type != "Status", TRUE, FALSE)
#Conservative Media Analysis
conservativemediaplus$shares_log <- log(conservativemediaplus$Shares + 1)
LinearModConMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
LinearModConMediaSumm <- summ(LinearModConMedia, exp = TRUE, center = TRUE, vifs = TRUE)

#Add Variables
liberalmediaplus$has_URL <- ifelse(liberalmediaplus$Type == "Link", TRUE, FALSE)
liberalmediaplus$has_media <- ifelse(liberalmediaplus$Type != "Link" & liberalmediaplus$Type != "Status", TRUE, FALSE)
#Liberal Media Analysis
liberalmediaplus$shares_log <- log(liberalmediaplus$Shares + 1)
LinearModDemMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
LinearModDemMediaSumm <- summ(LinearModDemMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModDemMediaSumm

LinearModDemMedia2 <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + `Likes at Posting`, data = liberalmediaplus)
LinearModConMedia2 <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + `Likes at Posting`, data = conservativemediaplus)
LinearModDemMediaSumm2 <- summ(LinearModDemMedia2, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModConMediaSumm2 <- summ(LinearModConMedia2, exp = TRUE, center = TRUE, vifs = TRUE)
export_summs(LinearModDemMediaSumm2, 
             LinearModConMediaSumm2,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Media Twitter", "Conservative Media Twitter", "Liberal Media Facebook", "Conservative Media Facebook"), 
             to.file = "html", 
             file.name = "StudyOneModels.html")

#Add Variables
liberalcongressplus$has_URL <- ifelse(liberalcongressplus$Type == "Link", TRUE, FALSE)
liberalcongressplus$has_media <- ifelse(liberalcongressplus$Type != "Link" & liberalcongressplus$Type != "Status", TRUE, FALSE)
#Liberal Congress Analysis
liberalcongressplus$shares_log <- log(liberalcongressplus$Shares + 1)
linearModDemCongress <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
linearModDemCongresssumm <- summ(linearModDemCongress, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModDemCongresssumm 

##Conservative Congress Analysis

#Add Variables
conservativecongressplus$has_URL <- ifelse(conservativecongressplus$Type == "Link", TRUE, FALSE)
conservativecongressplus$has_media <- ifelse(conservativecongressplus$Type != "Link" & conservativecongressplus$Type != "Status", TRUE, FALSE)
conservativecongressplus$shares_log <- log(conservativecongressplus$Shares + 1)
#Analysis 
linearModCongressRepub <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
linearModCongressRepubsumm <- summ(linearModCongressRepub, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubsumm

#### 8 Facebook Analyis 2 (Don't Do This One) ####

## Conservative Media Analysis 

#Add Variables
conservativemediaFB$has_URL <- ifelse(conservativemediaFB$Type == "Link", TRUE, FALSE)
conservativemediaFB$has_media <- ifelse(conservativemediaFB$Type != "Link" & conservativemediaFB$Type != "Status", TRUE, FALSE)
#Conservative Media Analysis
conservativemediaFB$shares_log <- log(conservativemediaFB$Shares + 1)
LinearModConMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = conservativemediaFB)
LinearModConMediaSumm <- summ(LinearModConMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModConMediaSumm

## Liberal Media Analysis

#Add Variables
liberalmediaFB$has_URL <- ifelse(liberalmediaFB$Type == "Link", TRUE, FALSE)
liberalmediaFB$has_media <- ifelse(liberalmediaFB$Type != "Link" & liberalmediaFB$Type != "Status", TRUE, FALSE)
#Liberal Media Analysis
liberalmediaFB$shares_log <- log(liberalmediaFB$Shares + 1)
LinearModDemMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = liberalmediaFB)
LinearModDemMediaSumm <- summ(LinearModDemMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModDemMediaSumm

##Liberal Congress Analysis

#Add Variables
liberalcongressFB$has_URL <- ifelse(liberalcongressFB$Type == "Link", TRUE, FALSE)
liberalcongressFB$has_media <- ifelse(liberalcongressFB$Type != "Link" & liberalcongressFB$Type != "Status", TRUE, FALSE)
#Liberal Congress Analysis
liberalcongressFB$shares_log <- log(liberalcongressFB$Shares + 1)
linearModDemCongress <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = liberalcongressFB)
linearModDemCongresssumm <- summ(linearModDemCongress, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModDemCongresssumm

##Conservative Congress Analysis

#Add Variables
conservativecongressFB$has_URL <- ifelse(conservativecongressFB$Type == "Link", TRUE, FALSE)
conservativecongressFB$has_media <- ifelse(conservativecongressFB$Type != "Link" & liberalcongressFB$Type != "Status", TRUE, FALSE)
#Conservative Congress Analysis 
conservativecongressFB$likes_log <- log(conservativecongressFB$Likes + 1)
conservativecongressFB$angry_log <- log(conservativecongressFB$Angry + 1)
conservativecongressFB$shares_log <- log(conservativecongressFB$Shares + 1)
linearModCongressRepub <- glm(shares_log ~ liberalidentity + conservativeidentity + NegativeAffect + PositiveAffect + MoralEmotional + Polarization + has_URL + has_media + `Likes at Posting`, data = conservativecongressFB)
linearModCongressRepubsumm <- summ(linearModCongressRepub, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubsumm

#### 9 Twitter Anaylsis ####

## Conservative Media Analysis 

#Add Variables
conservativemediaTwitter$has_media <- is.na(conservativemediaTwitter$media_type) == FALSE
conservativemediaTwitter$has_URL <- is.na(conservativemediaTwitter$urls_url) == FALSE
#Log Transform
conservativemediaTwitter$retweet_count_log <- log(conservativemediaTwitter$retweet_count + 1)
#Model  
ConservativeTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=conservativemediaTwitter)
ConservativeTwitterModsumm <- summ(ConservativeTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
ConservativeTwitterModsumm

## Liberal Media Analysis

#Add Variables
liberalmediaTwitter$has_media <- is.na(liberalmediaTwitter$media_type) == FALSE
liberalmediaTwitter$has_URL <- is.na(liberalmediaTwitter$urls_url) == FALSE
#Log Transform
liberalmediaTwitter$retweet_count_log <- log(liberalmediaTwitter$retweet_count + 1)
liberalmediaTwitter$favorite_count_log <- log(liberalmediaTwitter$favorite_count + 1)
#Model
LiberalMediaTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=liberalmediaTwitter)
LiberalMediaTwitterModSumm <- summ(LiberalMediaTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)

## Conservative Congress Analysis 

#Create Control Variables
congressRepubTwitter$has_media <- is.na(congressRepubTwitter$media_type) == FALSE
congressRepubTwitter$has_URL <- is.na(congressRepubTwitter$urls_url) == FALSE
#Log Transform Retweet Count
congressRepubTwitter$retweet_count_log <- log(congressRepubTwitter$retweet_count + 1)
#Model
congressRepubTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=congressRepubTwitter)
congressRepubTwitterSumm <- summ(congressRepubTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressRepubTwitterSumm

## Liberal Congress Analysis 

congressDemTwitter$has_media <- is.na(congressDemTwitter$media_type) == FALSE
congressDemTwitter$has_URL <- is.na(congressDemTwitter$urls_url) == FALSE
#Log Transform Retweet Count
congressDemTwitter$retweet_count_log <- log(congressDemTwitter$retweet_count + 1)
#Model
congressDemTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + followers_count + is_retweet, data=congressDemTwitter)
congressDemTwitterSumm <- summ(congressDemTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressDemTwitterSumm

#### 10 Clustered Standard Errors ####

utils::install.packages("miceadds")
devtools::install_github("alexanderrobitzsch/miceadds")
library(miceadds)
library(sandwhich)

ConTwitterCluster <- summ(ConservativeTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "screen_name")
LibTwitterCluster <- summ(LiberalMediaTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "screen_name")
ConFacebookCluster <- summ(LinearModConMedia, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
LibFacebookCluster <- summ(LinearModDemMedia, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")

ConCongressTwitterCluster <- summ(congressRepubTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
LibCongressTwitterCluster <- summ(congressDemTwitterMod, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
ConCongressFacebookCluster <- summ(linearModCongressRepub, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")
LibCongressFacebookCluster <- summ(linearModDemCongress, exp = TRUE, center = TRUE, vifs = TRUE, robust = "HC3", cluster = "Page Name")

get_robust_se(
  LinearModConMediaSumm,
  type = "HC3",
  cluster = "Page Name",
  data = conservativemediaplus,
  vcov = NULL
)

conmediacluster <- glm.cluster(conservativemediaplus, 
                               formula = shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, 
                               cluster = "Page Name")
libmediacluster <- glm.cluster(liberalmediaplus, 
                               formula = shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, 
                               cluster = "Page Name")
summ(libmediacluster)

View(conservativemediaplus)
?glm.cluster()
shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`

#### Regressions Without Control Variables #### 

## Conservative Media Analysis 

#Add Variables
ConservativeTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data=conservativemediaTwitter)
ConservativeTwitterModNC <- summ(ConservativeTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
ConservativeTwitterModNC

## Liberal Media Analysis

#Add Variables
LiberalMediaTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data=liberalmediaTwitter)
LiberalMediaTwitterModNC <- summ(LiberalMediaTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
LiberalMediaTwitterModNC

## Conservative Congress Analysis 

#Create Control Variables
congressRepubTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data=congressRepubTwitter)
congressRepubTwitterNC <- summ(congressRepubTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressRepubTwitterNC

## Liberal Congress Analysis 
congressDemTwitterMod <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data=congressDemTwitter)
congressDemTwitterNC <- summ(congressDemTwitterMod, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
congressDemTwitterNC

##

#Media Analysis Facebook Liberal
LinearModConMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data = conservativemediaplus)
LinearModConMediaNC <- summ(LinearModConMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModConMediaNC

#Media Analysis Facebook Conservative
LinearModDemMedia <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data = liberalmediaplus)
LinearModDemMediaNC <- summ(LinearModDemMedia, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModDemMediaNC

#Add Variables
linearModDemCongress <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data = liberalcongressplus)
linearModDemCongressNC <- summ(linearModDemCongress, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModDemCongressNC

##Conservative Congress Analysis

#Add Variables
linearModCongressRepub <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data = conservativecongressplus)
linearModCongressRepubNC <- summ(linearModCongressRepub, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubNC

export_summs(LiberalMediaTwitterModNC,
             ConservativeTwitterModNC,
             LinearModDemMediaNC,
             LinearModConMediaNC,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Media Twitter", "Conservative Media Twitter", "Liberal Media Facebook", "Conservative Media Facebook"), 
             to.file = "html", 
             file.name = "StudyOneModelsNoControls.html")

summ

export_summs(congressDemTwitterNC,
             congressRepubTwitterNC,
             LinearModDemMediaNC,
             LinearModConMediaNC,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Congress Twitter", "Conservative Congress Twitter", "Liberal Congress Facebook", "Conservative Congress Facebook"), 
             to.file = "html", 
             file.name = "StudyTwoModelsNoControls.html")

#### 11 Export Regressions ####

export_summs(LiberalMediaTwitterModSumm, 
             ConservativeTwitterModsumm, 
             LinearModDemMediaSumm, 
             LinearModConMediaSumm, 
             statistics = "({statistic}, p = {p.value}, VIF = {VIF})",
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Media Twitter", "Conservative Media Twitter", "Liberal Media Facebook", "Conservative Media Facebook"), 
             to.file = "html", 
             file.name = "StudyOneModels.html")

export_summs(ConCongressTwitterCluster,
             LibCongressTwitterCluster, 
             ConCongressFacebookCluster,
             LibCongressFacebookCluster,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Media Twitter", "Conservative Media Twitter", "Liberal Media Facebook", "Conservative Media Facebook"), 
             to.file = "html", 
             file.name = "StudyTwoRobustStandardErrors.html")

export_summs(ConTwitterCluster,
             LibTwitterCluster, 
             ConFacebookCluster,
             LibFacebookCluster,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Media Twitter", "Conservative Media Twitter", "Liberal Media Facebook", "Conservative Media Facebook"), 
             to.file = "html", 
             file.name = "StudyTwoRobustStandardErrors.html")

export_summs(linearModDemCongresssumm, 
             linearModCongressRepubsumm, 
             congressRepubTwitterSumm,
             congressDemTwitterSumm,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Congress Twitter", "Conservative Congress Twitter", "Liberal Congress Facebook", "Conservative Congress Facebook"), 
             to.file = "html", 
             file.name = "StudyTwoModels.html")

congressDemTwitterSumm

export_summs(sharessumm, 
             commentssumm, 
             likessumm,
             lovesumm,
             wowsumm,
             hahasumm,
             sadsumm,
             angrysumm,
             retweetssumm,
             favoritessumm,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("shares", "comments", "likes", "love", "wow", "haha", "sad", "angry", "retweets", "favorites"), 
             to.file = "html", 
             file.name = "StudyOneReactionModelsConservative.html"
)

export_summs(sharessumm, 
             commentssumm, 
             likessumm,
             lovesumm,
             wowsumm,
             hahasumm,
             sadsumm,
             angrysumm,
             retweetssumm,
             favoritessumm,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("shares", "comments", "likes", "love", "wow", "haha", "sad", "angry", "retweets", "favorites"), 
             to.file = "html", 
             file.name = "StudyOneReactionModelsLiberal.html"
)

export_summs(sharessumm, 
             commentssumm, 
             likessumm,
             lovesumm,
             wowsumm,
             hahasumm,
             sadsumm,
             angrysumm,
             retweetssumm,
             favoritessumm,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("shares", "comments", "likes", "love", "wow", "haha", "sad", "angry", "retweets", "favorites"), 
             to.file = "html", 
             file.name = "StudyOneReactionModelsLiberal.html"
)

export_summs(sharessumm, 
             commentssumm, 
             likessumm,
             lovesumm,
             wowsumm,
             hahasumm,
             sadsumm,
             angrysumm,
             retweetsumm,
             favoritesumm,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("shares", "comments", "likes", "love", "wow", "haha", "sad", "angry", "retweets", "favorites"), 
             to.file = "html", 
             file.name = "StudyTwoConservativeCongress.html"
)

export_summs(sharessumm, 
             commentssumm, 
             likessumm,
             lovesumm,
             wowsumm,
             hahasumm,
             sadsumm,
             angrysumm,
             retweetsumm,
             favoritesumm,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("shares", "comments", "likes", "love", "wow", "haha", "sad", "angry", "retweets", "favorites"), 
             to.file = "html", 
             file.name = "StudyTwoLiberalCongress.html"
)

LinearModDemMedia2 <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + `Likes at Posting`, data = liberalmediaplus)
LinearModConMedia2 <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + `Likes at Posting`, data = conservativemediaplus)
LinearModDemMediaSumm2 <- summ(LinearModDemMedia2, exp = TRUE, center = TRUE, vifs = TRUE)
LinearModConMediaSumm2 <- summ(LinearModConMedia2, exp = TRUE, center = TRUE, vifs = TRUE)
export_summs(LinearModDemMediaSumm2, 
             LinearModConMediaSumm2,
             error_format = "[{conf.low}, {conf.high}]",
             modelnames = c("Liberal Media Twitter", "Conservative Media Twitter", "Liberal Media Facebook", "Conservative Media Facebook"), 
             to.file = "html", 
             file.name = "StudyOneModels.html")

#### 12 Relative Importance Analysis ####

install.packages("relaimpo")
library(relaimpo)

riconmediaT = calc.relimp(glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + is_retweet + followers_count, data=conservativemediaTwitter), type = "lmg")
rilibmediaT = calc.relimp(glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + has_URL + is_retweet + followers_count, data=liberalmediaTwitter), type = "lmg")
riconmediaF = calc.relimp(glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + `Likes at Posting`, data = conservativemediaplus), type = "lmg") 
rilibmediaF = calc.relimp(glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_media + `Likes at Posting`, data = liberalmediaplus), type = "lmg") 

riconcongressT = calc.relimp(congressRepubTwitterMod, type = "lmg") 
rilibcongressT = calc.relimp(congressDemTwitterMod, type = "lmg") 
riconcongressF = calc.relimp(linearModCongressRepub, type = "lmg") 
rilibcongressF = calc.relimp(linearModDemCongress, type = "lmg")

#### 13 Plots - Media ####

library(dotwhisker)
library(broom)
library(dplyr)
install.packages("ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

FBConCongress <- tidy(linearModCongressRepubsumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`")  %>% mutate(model = "Facebook")
FBLibCongress <- tidy(linearModDemCongresssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`") %>% mutate(model = "Facebook")
FBLibMedia <- tidy(LinearModDemMediaSumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`") %>% mutate(model = "Facebook")
FBConMedia <- tidy(LinearModConMediaSumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`")  %>% mutate(model = "Facebook")

TConMedia <- tidy(ConservativeTwitterModsumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE") %>% mutate(model = "Twitter")
TLibMedia <- tidy(LiberalMediaTwitterModSumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE") %>% mutate(model = "Twitter")
TConCongress <- tidy(congressRepubTwitterSumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE") %>% mutate(model = "Twitter")
TLibCongress <- tidy(congressDemTwitterSumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE") %>% mutate(model = "Twitter")

conmediamodels <- rbind(FBConMedia, TConMedia)
concongressmodels <- rbind(FBConCongress, TConCongress)
libmediamodels <- rbind(FBLibMedia, TLibMedia)
libcongressmodels <- rbind(FBLibCongress, TLibCongress)


## Liberal Plots
plot <- dwplot(libmediamodels, conf.level = .95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(Democrat = "Liberal (Ingroup)", Republican = "Conservative (Outgroup)", NegativeAffect = "Negative Affect", PositiveAffect = "Positive Affect", MoralEmotional = "Moral Emotional")) +
  xlim(0.7, 2.9)

two_brackets <- list(c("Identity", "Liberal (Ingroup)", "Conservative (Outgroup)"), 
                     c("Emotion", "Negative Affect", "Moral Emotional"))

libmediaplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
  ggtitle("Liberal Media") +
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  theme_apa() +
  theme(legend.position = "none") +
  #theme(legend.font.size = 12,
  #          legend.pos = "bottom") + 
  scale_colour_hue(h = c(260, 170)) 
  )} %>% 
add_brackets(two_brackets)
libmediaplot

## Conservative Plots
plot <- dwplot(conmediamodels, conf.level = .95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(Democrat = "Liberal (Outgroup)", Republican = "Conservative (Ingroup)", NegativeAffect = "Negative Affect", PositiveAffect = "Positive Affect", MoralEmotional = "Moral Emotional")) + 
  xlim(0.7, 2.9)

two_brackets <- list(c("Identity", "Liberal (Outgroup)", "Conservative (Ingroup)"), 
                     c("Emotion", "Negative Affect", "Moral Emotional"))

conservativeplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
    ggtitle("Conservative Media") +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
    theme_apa() +
    theme(legend.position = "none") +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(260, 170)) 
)} %>% 
  add_brackets(two_brackets)

plot <- plot + theme_bw() + xlab("Odds Ratio") + ylab("") +
  ggtitle("Conservative Media") +
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  theme_apa(legend.font.size = 12,
        legend.pos = "bottom") + 
  scale_colour_hue(h = c(260, 170)) 

leg <- get_legend(plot)
class(leg)

combinedmediaplots <- plot_grid(libmediaplot, 
          conservativeplot, 
          nrow = 3,
          labels = c("A", "B"),
          rel_heights = c(1, 1, .3)) + draw_grob(leg, vjust = 0.4)

combinedmediaplots

ggsave("combinedmediaplots.png", width = 7.5, height = 9)

ggsave("mediaplots.png", width = 6, height = 10)

scale_colour_hue()
two_brackets <- list(c("Identity", "Democrat", "Republican"), 
                     c("Emotion", "Negative Affect", "Moral Emotional"))

dwplot(FBConMedia, style = "distribution") 

?dwplot()

?theme

+ theme(
    legend.background = element_rect(colour="grey80"),
    legend.spacing.y = unit(-.1, 'cm'),
    legend.spacing.x = unit(-.3, 'cm'))

#### 14 Plots - Congress ####

## Liberal Plots
plot <- dwplot(libcongressmodels, conf.level = .95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(Democrat = "Liberal (Ingroup)", Republican = "Conservative (Outgroup)", NegativeAffect = "Negative Affect", PositiveAffect = "Positive Affect", MoralEmotional = "Moral Emotional")) +
  xlim(0.7, 2.9)

two_brackets <- list(c("Identity", "Liberal (Ingroup)", "Conservative (Outgroup)"), 
                     c("Emotion", "Negative Affect", "Moral Emotional"))

libcongressplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
    ggtitle("Liberal Congress") +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
    theme_apa() +
    theme(legend.position = "none") +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(260, 170)) 
)} %>% 
  add_brackets(two_brackets)
libcongressplot

## Conservative Plots
plot <- dwplot(concongressmodels, conf.level = .95, dot_args = list(size = 1.2),
               whisker_args = list(size = 1)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(Democrat = "Liberal (Outgroup)", Republican = "Conservative (Ingroup)", NegativeAffect = "Negative Affect", PositiveAffect = "Positive Affect", MoralEmotional = "Moral Emotional")) 

two_brackets <- list(c("Identity", "Liberal (Outgroup)", "Conservative (Ingroup)"), 
                     c("Emotion", "Negative Affect", "Moral Emotional"))

conservativecongressplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
    ggtitle("Conservative Congress") +
    xlim(0.7, 2.9) +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
    theme_apa() +
    theme(legend.position = "none") +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(260, 170)) 
)} %>% 
  add_brackets(two_brackets)
conservativecongressplot

plot <- plot + theme_bw() + xlab("Odds Ratio") + ylab("") +
  ggtitle("Conservative Media") +
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  theme_apa(legend.font.size = 12,
            legend.pos = "bottom") + 
  scale_colour_hue(h = c(260, 170)) 

leg <- get_legend(plot)
class(leg)

combinedcongressplots <- plot_grid(libcongressplot, 
                                conservativecongressplot, 
                                nrow = 3,
                                labels = c("A", "B"),
                                rel_heights = c(1, 1, .3)) + draw_grob(leg, vjust = 0.4)

combinedcongressplots 

ggsave("combinedcongressplots.png", width = 7.5, height = 9)

#### Combine 4 Plots in One Pane ####

combinedplots4 <- plot_grid(libmediaplot,
                            conservativeplot,
                                   libcongressplot, 
                                   conservativecongressplot, 
                                   nrow = 3,
                                   ncol = 2, 
                                   greedy = TRUE,
                                   labels = c("A", "B", "C", "D"),
                                   rel_heights = c(1, 1, .3)) + draw_grob(leg, vjust = 0.4)
combinedplots4
?plot_grid()
ggsave("combinedplots4.png", width = 11, height = 7)

combinedplotstest <- plot_grid(libmediaplot,
                               conservativeplot,
                            libmediaplotFB,
                            concongressplotFB, 
                            nrow = 3,
                            ncol = 2, 
                            labels = c("A", "B", "C", "D"),
                            rel_heights = c(1, 1, .3)) + draw_grob(leg, vjust = 0.4)
combinedplotstest
ggsave("combinedplotstest.png", width = 12, height = 7)

combinedplots42 <- plot_grid(libmediaplotFB,
                             conmediaplotReactions,
                             libcongressplotFB, 
                             concongressplotFB, 
                            nrow = 3,
                            ncol = 2, 
                            labels = c("A", "B", "C", "D"),
                            rel_heights = c(1, 1, .3)) + draw_grob(leg2, vjust = 0.4)
combinedplots42

ggsave("combinedplots42.png", width = 12, height = 8.5)

conservativeplot
libmediaplot
conservativecongressplot
conservativecongressplot

#### 15 Reaction & Correlation Plots - Media ####

install.packages("apaTables")
library(apaTables)
library(dplyr)

con <- rbind.fill(conservativemediaplus, conservativecongressplus)
lib <- rbind.fill(liberalmediaplus, liberalcongressplus)
cor.plot(con[, 29:36])
install.packages("corrplot")
install.packages("devtools")
install.packages("GGally")
library(corrplot)
library(devtools)
devtools::install_github("taiyun/corrplot", build_vignettes = TRUE)
library(corrplot)
library(GGally)
View(con[, c(31, 29:30, 32:36)])
corrplot(cor(conservativemediaplus[, c(31, 29:30, 32:36)]), method = "color", type = "full", tl.col = "black", tl.srt = 45, number.font = 0.5, sig.level = 0.95, addCoef.col = "gray")
conmediacorplot <- corrplot.mixed(cor(conservativemediaplus[, c(31, 29:30, 32:36)]), 
               tl.pos = "lt",
               tl.col = "black", 
               tl.srt = 45)
libmediacorplot <- corrplot.mixed(cor(liberalmediaplus[, c(31, 29:30, 32:36)]), 
                                  tl.pos = "lt",
                                  tl.col = "black", 
                                  tl.srt = 45)

apa.cor.table(liberalmediaplus[, c(31, 29:30, 32:36)], filename="LibMediaSharesDescriptiveTable.doc", table.number=1)
apa.cor.table(conservativemediaplus[, c(31, 29:30, 32:36)], filename="ConMediaSharesDescriptiveTable.doc", table.number=2)
apa.cor.table(conservativemediaTwitter[, c(8, 9, 12, 13, 16)], filename = "1.doc")
apa.cor.table(liberalmediaTwitter[, c(8, 9, 12, 13, 16)], filename = "2.doc")
apa.cor.table(liberalmediaplus[, c(8, 9, 12, 13, 16)], filename = "3.doc")
apa.cor.table(conservativemediaplus[, c(8, 9, 12, 13, 16)], filename = "4.doc")

apa.cor.table(conservativecongressplus[, c(31, 29:30, 32:36)], filename = "ConCongressCorrelations.doc")
apa.cor.table(liberalcongressplus[, c(31, 29:30, 32:36)], filename = "LibCongressCorrelations.doc")

apa.cor.table(congressRepubTwitter[, c(8, 9, 12, 13, 16)], filename = "1B.doc") 
apa.cor.table(congressDemTwitter[, c(8, 9, 12, 13, 16)],filename = "2B.doc") 
apa.cor.table(conservativecongressplus[, c(8, 9, 12, 13, 16)],filename = "3B.doc") 
apa.cor.table(liberalcongressplus[, c(8, 9, 12, 13, 16)],filename = "4B.doc")

c(8, 9, 12, 13, 16) 
View(conservativemediaplus)
?apa.cor.table()

grid.echo()
P2 <- grid.grab()
P1

combinedcorrplotmedia <- ggarrange(P1, P2)
                                #ncol = 2,
                                #labels = c("A", "B"))
grid.grab(combinedcorrplotmedia)
combinedcorrplotmedia

corrplot(cor(liberalmediaplus[, c(31, 29:30, 32:36)]), method = "shade", type = "full", tl.col = "black", tl.srt = 45)

#### 16 Reactions Regression Plots #### 
## Conservative Shares Analysis 

mean(conservativemediaplus$shares)
mean(liberalmediaplus$Haha)

conservativemediaplus$engagement <- mean(conservativemediaplus$Shares +
                                        conservativemediaplus$Likes +
                                        conservativemediaplus$Comments + 
                                        conservativemediaplus$Love + 
                                        conservativemediaplus$Wow + 
                                        conservativemediaplus$Haha + 
                                        conservativemediaplus$Sad + 
                                        conservativemediaplus$Angry)

#Log
conservativemediaplus$shares_log <- log(conservativemediaplus$Shares + 1)
conservativemediaplus$comments_log <- log(conservativemediaplus$Comments + 1)
conservativemediaplus$likes_log <- log(conservativemediaplus$Likes + 1)
conservativemediaplus$love_log <- log(conservativemediaplus$Love + 1)
conservativemediaplus$wow_log <- log(conservativemediaplus$Wow + 1)
conservativemediaplus$haha_log <- log(conservativemediaplus$Haha + 1)
conservativemediaplus$sad_log <- log(conservativemediaplus$Sad + 1)
conservativemediaplus$angry_log <- log(conservativemediaplus$Angry + 1)
conservativemediaplus$engagement_log <- log(conservativemediaplus$engagement + 1)
conservativemediaTwitter$retweets_log <- log(conservativemediaTwitter$retweet_count + 1)
conservativemediaTwitter$favorites_log <- log(conservativemediaTwitter$favorite_count + 1)
#Models
shares <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
comments <- glm(comments_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
likes <- glm(likes_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
love <- glm(love_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
wow <- glm(wow_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
haha <- glm(haha_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
sad <- glm(sad_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
angry <- glm(angry_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
engagement <- glm(engagement_log ~  Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativemediaplus)
retweets <- glm(retweets_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + followers_count + is_retweet, data = conservativemediaTwitter)
favorites <- glm(favorites_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + followers_count + is_retweet, data = conservativemediaTwitter)
sharessumm <- summ(shares, exp = TRUE, center = TRUE, vifs = TRUE)
commentssumm <- summ(comments, exp = TRUE, center = TRUE, vifs = TRUE)
likessumm <- summ(likes, exp = TRUE, center = TRUE, vifs = TRUE)
lovesumm <- summ(love, exp = TRUE, center = TRUE, vifs = TRUE)
wowsumm <- summ(wow, exp = TRUE, center = TRUE, vifs = TRUE)
hahasumm <- summ(haha, exp = TRUE, center = TRUE, vifs = TRUE)
sadsumm <- summ(sad, exp = TRUE, center = TRUE, vifs = TRUE)
angrysumm <- summ(angry, exp = TRUE, center = TRUE, vifs = TRUE)
engagementsumm <- summ(engagement, exp = TRUE, center = TRUE, vifs = TRUE)
retweetssumm <- summ(retweets, exp = TRUE, center = TRUE, vifs = TRUE)
favoritessumm <- summ(favorites, exp = TRUE, center = TRUE, vifs = TRUE)
conshares <- tidy(sharessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Shares")
concomments <- tidy(commentssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Comments")
conlikes <- tidy(likessumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Likes")
conlove <- tidy(lovesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Love")
conwow <- tidy(wowsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Wow")
conhaha <- tidy(hahasumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Haha")
consad <- tidy(sadsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Sad")
conangry <- tidy(angrysumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Angry")
conretweets <- tidy(retweetssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Retweets")
confavorites <- tidy(favoritessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Favorites")
conmodels <- rbind(conshares, concomments, conlikes, conlove, conhaha, conwow, consad, conangry, conretweets, confavorites)
conmodels$termtemp <- conmodels$term
conmodels$modeltemp <- conmodels$model
conmodels$term <- conmodels$modeltemp
conmodels$model <- conmodels$termtemp
#conmodels[c(1, 7, 13, 19, 25, 31, 37), 1] <- "(Intercept)"
conmodels[c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28), 1] <- "(Intercept)"

two_brackets <- list(c("Facebook", "Shares", "Angry"), 
                     c("Twitter", "Retweets", "Favorites"))

conmediaplotReactions <- {dwplot(conmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
theme_apa(legend.pos = "bottom") +
theme(legend.position = "none") + 
xlab("Change in odds of engagement") +
xlim(0.7, 3.4) +
scale_colour_hue(h = c(260, 0)) + 
ggtitle("Conservative Media")} %>%
  add_brackets(two_brackets)
conmediaplotReactions

##ggsave("conmediareactionplot.png", width = 6, height = 4)

### Liberal Shares Analysis

#Log
liberalmediaplus$shares_log <- log(liberalmediaplus$Shares + 1)
liberalmediaplus$comments_log <- log(liberalmediaplus$Comments + 1)
liberalmediaplus$likes_log <- log(liberalmediaplus$Likes + 1)
liberalmediaplus$love_log <- log(liberalmediaplus$Love + 1)
liberalmediaplus$wow_log <- log(liberalmediaplus$Wow + 1)
liberalmediaplus$haha_log <- log(liberalmediaplus$Haha + 1)
liberalmediaplus$sad_log <- log(liberalmediaplus$Sad + 1)
liberalmediaplus$angry_log <- log(liberalmediaplus$Angry + 1)
liberalmediaTwitter$retweet_count_log <- log(liberalmediaTwitter$retweet_count + 1)
liberalmediaTwitter$favorite_count_log <- log(liberalmediaTwitter$favorite_count + 1)
#Models
shares <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
comments <- glm(comments_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
likes <- glm(likes_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
love <- glm(love_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
wow <- glm(wow_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
haha <- glm(haha_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
sad <- glm(sad_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
angry <- glm(angry_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
retweets <- glm(retweet_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + followers_count + is_retweet, data = liberalmediaTwitter)
favorites <- glm(favorite_count_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + followers_count + is_retweet, data = liberalmediaTwitter)
sharessumm <- summ(shares, exp = TRUE, center = TRUE, vifs = TRUE)
commentssumm <- summ(comments, exp = TRUE, center = TRUE, vifs = TRUE)
likessumm <- summ(likes, exp = TRUE, center = TRUE, vifs = TRUE)
lovesumm <- summ(love, exp = TRUE, center = TRUE, vifs = TRUE)
wowsumm <- summ(wow, exp = TRUE, center = TRUE, vifs = TRUE)
hahasumm <- summ(haha, exp = TRUE, center = TRUE, vifs = TRUE)
sadsumm <- summ(sad, exp = TRUE, center = TRUE, vifs = TRUE)
angrysumm <- summ(angry, exp = TRUE, center = TRUE, vifs = TRUE)
retweetssumm <- summ(retweets, exp = TRUE, center = TRUE, vifs = TRUE)
favoritessumm <- summ(favorites, exp = TRUE, center = TRUE, vifs = TRUE)
shares <- tidy(sharessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Shares")
comments <- tidy(commentssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Comments")
likes <- tidy(likessumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Likes")
love <- tidy(lovesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Love")
wow <- tidy(wowsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Wow")
haha <- tidy(hahasumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Haha")
sad <- tidy(sadsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Sad")
angry <- tidy(angrysumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Angry")
retweets <- tidy(retweetssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Retweets")
favorites <- tidy(favoritessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Favorites")
libmodels <- rbind(shares, comments, likes, love, haha, wow, sad, angry, retweets, favorites)
libmodels$termtemp <- libmodels$term
libmodels$modeltemp <- libmodels$model
libmodels$term <- libmodels$modeltemp
libmodels$model <- libmodels$termtemp
#conmodels[c(1, 7, 13, 19, 25, 31, 37), 1] <- "(Intercept)"
libmodels[c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28), 1] <- "(Intercept)"

libmediaplotFB <- {dwplot(libmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
  theme_apa(legend.pos = "bottom") + 
  xlab("Change in odds of engagement") +
  theme(legend.position = "none") +
  scale_colour_hue(h = c(260, 0)) + 
  xlim(0.7, 3.4) +
  ggtitle("Liberal Media")} %>%
  add_brackets(two_brackets)
libmediaplotFB

leg2 <- get_legend(dwplot(libmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.7)) + 
                     geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
                     theme_apa(legend.pos = "bottom",
                               legend.font.size = 12) + xlab("Odds ratio") +
                     scale_colour_hue(h = c(260, 0), labels = c("Liberal Words", "Conservative Words")) + 
                     ggtitle("Liberal Media"))

?dwplot()
libmediaplotFB
combinedFBmediaplots <- plot_grid(libmediaplotFB, 
                                conmediaplotReactions, 
                                nrow = 3,
                                labels = c("A", "B"),
                                rel_heights = c(1, 1, .3)) + draw_grob(leg2, vjust = 0.4)
combinedFBmediaplots

ggsave("combinedFBmediaplots.png", width = 8, height = 10)

##Conservative Congress

#Log
liberalmediaplus$shares_log <- log(liberalmediaplus$Shares + 1)
liberalmediaplus$comments_log <- log(liberalmediaplus$Comments + 1)
liberalmediaplus$likes_log <- log(liberalmediaplus$Likes + 1)
liberalmediaplus$love_log <- log(liberalmediaplus$Love + 1)
liberalmediaplus$wow_log <- log(liberalmediaplus$Wow + 1)
liberalmediaplus$haha_log <- log(liberalmediaplus$Haha + 1)
liberalmediaplus$sad_log <- log(liberalmediaplus$Sad + 1)
liberalmediaplus$angry_log <- log(liberalmediaplus$Angry + 1)
#Models
shares <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
comments <- glm(comments_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
likes <- glm(likes_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
love <- glm(love_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
wow <- glm(wow_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
haha <- glm(haha_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
sad <- glm(sad_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
angry <- glm(angry_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalmediaplus)
sharessumm <- summ(shares, exp = TRUE, center = TRUE, vifs = TRUE)
commentssumm <- summ(comments, exp = TRUE, center = TRUE, vifs = TRUE)
likessumm <- summ(likes, exp = TRUE, center = TRUE, vifs = TRUE)
lovesumm <- summ(love, exp = TRUE, center = TRUE, vifs = TRUE)
wowsumm <- summ(wow, exp = TRUE, center = TRUE, vifs = TRUE)
hahasumm <- summ(haha, exp = TRUE, center = TRUE, vifs = TRUE)
sadsumm <- summ(sad, exp = TRUE, center = TRUE, vifs = TRUE)
angrysumm <- summ(angry, exp = TRUE, center = TRUE, vifs = TRUE)
shares <- tidy(sharessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Shares")
comments <- tidy(commentssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Comments")
likes <- tidy(likessumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Likes")
love <- tidy(lovesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Love")
wow <- tidy(wowsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Wow")
haha <- tidy(hahasumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Haha")
sad <- tidy(sadsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Sad")
angry <- tidy(angrysumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Angry")
libmodels <- rbind(shares, comments, likes, love, haha, wow, sad, angry)
libmodels$termtemp <- libmodels$term
libmodels$modeltemp <- libmodels$model
libmodels$term <- libmodels$modeltemp
libmodels$model <- libmodels$termtemp

libmodels[c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28), 1] <- "(Intercept)"

#libmediaplotFB <- dwplot(libmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
  theme_apa(legend.pos = "bottom") + xlab("Odds ratio") +
  scale_colour_hue(h = c(260, 0)) + 
  ggtitle("Conservative Media") 
#libmediaplotFB

#ggsave("libmediareactionplot.png", width = 6, height = 4)

##Conservative Congress

#Log
conservativecongressplus$shares_log <- log(conservativecongressplus$Shares + 1)
conservativecongressplus$comments_log <- log(conservativecongressplus$Comments + 1)
conservativecongressplus$likes_log <- log(conservativecongressplus$Likes + 1)
conservativecongressplus$love_log <- log(conservativecongressplus$Love + 1)
conservativecongressplus$wow_log <- log(conservativecongressplus$Wow + 1)
conservativecongressplus$haha_log <- log(conservativecongressplus$Haha + 1)
conservativecongressplus$sad_log <- log(conservativecongressplus$Sad + 1)
conservativecongressplus$angry_log <- log(conservativecongressplus$Angry + 1)
congressRepubTwitter$retweet_log <- log(congressRepubTwitter$retweet_count + 1)
congressRepubTwitter$favorites_log <- log(congressRepubTwitter$favorite_count + 1)
#Models
shares <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
comments <- glm(comments_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
likes <- glm(likes_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
love <- glm(love_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
wow <- glm(wow_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
haha <- glm(haha_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
sad <- glm(sad_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
angry <- glm(angry_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
retweet <- glm(retweet_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + is_retweet + followers_count, data = congressRepubTwitter)
favorite <- glm(favorites_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + is_retweet + followers_count, data = congressRepubTwitter)
sharessumm <- summ(shares, exp = TRUE, center = TRUE, vifs = TRUE)
commentssumm <- summ(comments, exp = TRUE, center = TRUE, vifs = TRUE)
likessumm <- summ(likes, exp = TRUE, center = TRUE, vifs = TRUE)
lovesumm <- summ(love, exp = TRUE, center = TRUE, vifs = TRUE)
wowsumm <- summ(wow, exp = TRUE, center = TRUE, vifs = TRUE)
hahasumm <- summ(haha, exp = TRUE, center = TRUE, vifs = TRUE)
angrysumm <- summ(angry, exp = TRUE, center = TRUE, vifs = TRUE)
sadsumm <- summ(sad, exp = TRUE, center = TRUE, vifs = TRUE)
retweetsumm <- summ(retweet, exp = TRUE, center = TRUE, vifs = TRUE)
favoritesumm <- summ(favorite, exp = TRUE, center = TRUE, vifs = TRUE)
shares <- tidy(sharessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Shares")
comments <- tidy(commentssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Comments")
likes <- tidy(likessumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Likes")
love <- tidy(lovesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Love")
wow <- tidy(wowsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Wow")
haha <- tidy(hahasumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Haha")
angry <- tidy(angrysumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Angry")
sad <- tidy(sadsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Sad")
retweets <- tidy(retweetsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Retweet")
favorites <- tidy(favoritesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Favorite")
concongressmodels <- rbind(shares, comments, likes, love, haha, wow, sad, angry, retweets, favorites)
concongressmodels$termtemp <- concongressmodels$term
concongressmodels$modeltemp <- concongressmodels$model
concongressmodels$term <- concongressmodels$modeltemp
concongressmodels$model <- concongressmodels$termtemp
#conmodels[c(1, 7, 13, 19, 25, 31, 37), 1] <- "(Intercept)"
concongressmodels[c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28), 1] <- "(Intercept)"

two_brackets <- list(c("Facebook", "Shares", "Angry"), 
                     c("Twitter", "Retweet", "Favorite"))

concongressplotFB <- {dwplot(concongressmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "bottom") + 
    xlab("Change in odds of engagement") +
    theme(legend.position = "none") +
    scale_colour_hue(h = c(260, 0)) + 
    xlim(0.7, 3.4) +
    ggtitle("Conservative Congress")} %>%
  add_brackets(two_brackets)
concongressplotFB

leg2 <- get_legend(dwplot(concongressmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.7)) + 
                     geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
                     theme_apa(legend.pos = "bottom",
                               legend.font.size = 12) + xlab("Odds ratio") +
                     scale_colour_hue(h = c(260, 0), labels = c("Liberal Words", "Conservative Words")) + 
                     ggtitle("Liberal Media"))

ggsave("conservativecongressreactionplot.png", width = 6, height = 4)

##Liberal Congress

#Log
liberalcongressplus$shares_log <- log(liberalcongressplus$Shares + 1)
liberalcongressplus$comments_log <- log(liberalcongressplus$Comments + 1)
liberalcongressplus$likes_log <- log(liberalcongressplus$Likes + 1)
liberalcongressplus$love_log <- log(liberalcongressplus$Love + 1)
liberalcongressplus$wow_log <- log(liberalcongressplus$Wow + 1)
liberalcongressplus$haha_log <- log(liberalcongressplus$Haha + 1)
liberalcongressplus$sad_log <- log(liberalcongressplus$Sad + 1)
liberalcongressplus$angry_log <- log(liberalcongressplus$Angry + 1)
congressDemTwitter$retweet_log <- log(congressDemTwitter$retweet_count + 1)
congressDemTwitter$favorite_log <- log(congressDemTwitter$favorite_count + 1)
#Models
shares <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
comments <- glm(comments_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
likes <- glm(likes_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
love <- glm(love_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
wow <- glm(wow_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
haha <- glm(haha_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
angry <- glm(angry_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
sad <- glm(sad_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = liberalcongressplus)
retweet <- glm(retweet_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + is_retweet + followers_count, data = congressDemTwitter)
favorite <- glm(favorite_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + is_retweet + followers_count, data = congressDemTwitter)
sharessumm <- summ(shares, exp = TRUE, center = TRUE, vifs = TRUE)
commentssumm <- summ(comments, exp = TRUE, center = TRUE, vifs = TRUE)
likessumm <- summ(likes, exp = TRUE, center = TRUE, vifs = TRUE)
lovesumm <- summ(love, exp = TRUE, center = TRUE, vifs = TRUE)
wowsumm <- summ(wow, exp = TRUE, center = TRUE, vifs = TRUE)
hahasumm <- summ(haha, exp = TRUE, center = TRUE, vifs = TRUE)
angrysumm <- summ(angry, exp = TRUE, center = TRUE, vifs = TRUE)
sadsumm <- summ(sad, exp = TRUE, center = TRUE, vifs = TRUE)
retweetsumm <- summ(retweet, exp = TRUE, center = TRUE, vifs = TRUE)
favoritesumm <- summ(favorite, exp = TRUE, center = TRUE, vifs = TRUE)
shares <- tidy(sharessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Shares")
comments <- tidy(commentssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Comments")
likes <- tidy(likessumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Likes")
love <- tidy(lovesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Love")
wow <- tidy(wowsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Wow")
haha <- tidy(hahasumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Haha")
angry <- tidy(angrysumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Angry")
sad <- tidy(sadsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Sad")
retweet <- tidy(retweetsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Retweet")
favorite <- tidy(favoritesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Favorite")
libcongressmodels <- rbind(shares, comments, likes, love, haha, wow, sad, angry, retweet, favorite)
libcongressmodels$termtemp <- libcongressmodels$term
libcongressmodels$modeltemp <- libcongressmodels$model
libcongressmodels$term <- libcongressmodels$modeltemp
libcongressmodels$model <- libcongressmodels$termtemp
#conmodels[c(1, 7, 13, 19, 25, 31, 37), 1] <- "(Intercept)"
libcongressmodels[c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28), 1] <- "(Intercept)"

libcongressplotFB <- {dwplot(libcongressmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "bottom") + 
    xlab("Change in odds of engagement") +
    theme(legend.position = "none") +
    scale_colour_hue(h = c(260, 0)) + 
    xlim(0.7, 3.4) +
    ggtitle("Liberal Congress")} %>%
  add_brackets(two_brackets)
libcongressplotFB

combinedFBcongressplots <- plot_grid(libcongressplotFB, 
                                     concongressplotFB, 
                                    nrow = 3,
                                    labels = c("A", "B"),
                                    rel_heights = c(1, 1, .3)) + draw_grob(leg2, vjust = 0.4)
combinedFBcongressplots 

ggsave("combinedcongressreactionplot.png", width = 8, height = 10)

#### 17 Tidy Text Anaylsis #### 

con <- rbind.fill(conservativemediaplus, conservativecongressplus)
lib <- rbind.fill(liberalmediaplus, liberalcongressplus)

mean(liberalmediaplus$Likes)
mean(lib$Shares)

mostpopularcon <-  conservativecongressplus %>% 
  dplyr::select(Shares, text, Republican, Democrat) %>% 
  arrange(desc(Shares)) 
  View(mostpopularcon)

conngrams <- mostpopularcon %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)

conngrams2 <- mostpopularcon %>%
  unnest_tokens(word, text, token = "ngrams", n = 1)

conngrams$word <- conngrams$bigram
conngrams$bigram <- NULL
congramtotal <- rbind(conngrams, conngrams2)

congramSort <- conngrams %>%
  group_by(word) %>%
  dplyr::summarize(n = n(),
                   avg_shares = mean(Shares), # using geometric mean
                   avg_likes = mean(Likes)) %>%
  filter(n > 100) %>%
  arrange(desc(avg_shares))

congram_corpus <- corpus(congramSort, text_field = "word")
congram_dict <- dfm(congram_corpus, dictionary = group2, stem = TRUE, verbose = TRUE)
congram_dict_df <- quanteda::convert(congram_dict, to = 'data.frame')
congramplus <- cbind(congramSort, congram_dict_df)

congramplusfinal <- subset(congramplus, congramplus$Democrat > 0 | congramplus$Republican > 0)
congramplusfinal 

congramplusfinal2 <- congramplusfinal %>%
  unnest_tokens(word, word, token = "ngrams", n = 1)

tweet_words_lib <- subset(tweet_words_dem, tweet_words_dem$Democrat > 0)
tweet_words_con <- subset(tweet_words_dem, tweet_words_dem$Republican > 0)

dplyr::select(screen_name, text, retweet_count, favorite_count, created_at, status_id) %>%
  unnest_tokens(word, text, token = "tweets") %>% # specific functionality for preserving usernames, urls, etc.
  anti_join(stop_words, by = "word")

View(mostpopularcon)

#### 18 Ingroup/Outgroup Meta-Analysis ####

install.packages("forestplot")
install.packages("metafor")
install.packages("meta")
install.packages("metaviz")
library("meta")
library(forestplot)
library(metafor)
library(metaviz)

conservativemediaplus
liberalmediaplus
conservativecongressplus
liberalcongressplus

conservativemediaTwitter
liberalmediaTwitter
congressRepubTwitter
congressDemTwitter

##Facebook Outgroup Variables
conservativemediaplus$outgroup <- ifelse((conservativemediaplus$Democrat > 0 & conservativemediaplus$Republican < 1), 1, NA)
conservativemediaplus$outgroup <- ifelse((conservativemediaplus$Republican > 0 & conservativemediaplus$Democrat < 1), 0, conservativemediaplus$outgroup)
conservativemediaplus$outgroupidentity <- ifelse((conservativemediaplus$liberalidentity > 0 & conservativemediaplus$conservativeidentity < 1), 1, NA)
conservativemediaplus$outgroupidentity <- ifelse((conservativemediaplus$conservativeidentity > 0 & conservativemediaplus$liberalidentity < 1), 0, conservativemediaplus$outgroupidentity)
conservativemediaplus$outgrouptop <- ifelse((conservativemediaplus$TopDemocrat > 0 & conservativemediaplus$TopRepublican < 1), 1, NA)
conservativemediaplus$outgrouptop <- ifelse((conservativemediaplus$TopRepublican > 0 & conservativemediaplus$TopDemocrat < 1), 0, conservativemediaplus$outgrouptop)
conservativemediaplus$outgroupcongress <- ifelse((conservativemediaplus$DemocratCongress > 0 & conservativemediaplus$RepublicanCongress < 1), 1, NA)
conservativemediaplus$outgroupcongress <- ifelse((conservativemediaplus$RepublicanCongress > 0 & conservativemediaplus$DemocratCongress < 1), 0, conservativemediaplus$outgroupcongress)

liberalmediaplus$outgroup <- ifelse((liberalmediaplus$Republican > 0 & liberalmediaplus$Democrat < 1), 1, NA)
liberalmediaplus$outgroup <- ifelse((liberalmediaplus$Democrat > 0 & liberalmediaplus$Republican < 1), 0, liberalmediaplus$outgroup)
liberalmediaplus$outgroupidentity <- ifelse((liberalmediaplus$conservativeidentity > 0 & liberalmediaplus$liberalidentity < 1), 1, NA)
liberalmediaplus$outgroupidentity <- ifelse((liberalmediaplus$liberalidentity > 0 & liberalmediaplus$conservativeidentity < 1), 0, liberalmediaplus$outgroupidentity)
liberalmediaplus$outgrouptop <- ifelse((liberalmediaplus$TopRepublican > 0 & liberalmediaplus$TopDemocrat < 1), 1, NA)
liberalmediaplus$outgrouptop <- ifelse((liberalmediaplus$TopDemocrat > 0 & liberalmediaplus$TopRepublican < 1), 0, liberalmediaplus$outgrouptop)
liberalmediaplus$outgroupcongress <- ifelse((liberalmediaplus$RepublicanCongress > 0 & liberalmediaplus$DemocratCongress < 1), 1, NA)
liberalmediaplus$outgroupcongress <- ifelse((liberalmediaplus$DemocratCongress > 0 & liberalmediaplus$RepublicanCongress < 1), 0, liberalmediaplus$outgroupcongress)

conservativecongressplus$outgroup <- ifelse((conservativecongressplus$Democrat > 0 & conservativecongressplus$Republican < 1), 1, NA)
conservativecongressplus$outgroup <- ifelse((conservativecongressplus$Republican > 0 & conservativecongressplus$Democrat < 1), 0, conservativecongressplus$outgroup)
conservativecongressplus$outgroupidentity <- ifelse((conservativecongressplus$liberalidentity > 0 & conservativecongressplus$conservativeidentity < 1), 1, NA)
conservativecongressplus$outgroupidentity <- ifelse((conservativecongressplus$conservativeidentity > 0 & conservativecongressplus$liberalidentity < 1), 0, conservativecongressplus$outgroupidentity)
conservativecongressplus$outgrouptop <- ifelse((conservativecongressplus$TopDemocrat > 0 & conservativecongressplus$TopRepublican < 1), 1, NA)
conservativecongressplus$outgrouptop <- ifelse((conservativecongressplus$TopRepublican > 0 & conservativecongressplus$TopDemocrat < 1), 0, conservativecongressplus$outgrouptop)
conservativecongressplus$outgroupcongress <- ifelse((conservativecongressplus$DemocratCongress > 0 & conservativecongressplus$RepublicanCongress < 1), 1, NA)
conservativecongressplus$outgroupcongress <- ifelse((conservativecongressplus$RepublicanCongress > 0 & conservativecongressplus$DemocratCongress < 1), 0, conservativecongressplus$outgroupcongress)

liberalcongressplus$outgroup <- ifelse((liberalcongressplus$Republican > 0 & liberalcongressplus$Democrat < 1), 1, NA)
liberalcongressplus$outgroup <- ifelse((liberalcongressplus$Democrat > 0 & liberalcongressplus$Republican < 1), 0, liberalcongressplus$outgroup)
liberalcongressplus$outgroupidentity <- ifelse((liberalcongressplus$conservativeidentity > 0 & liberalcongressplus$liberalidentity < 1), 1, NA)
liberalcongressplus$outgroupidentity <- ifelse((liberalcongressplus$liberalidentity > 0 & liberalcongressplus$conservativeidentity < 1), 0, liberalcongressplus$outgroupidentity)
liberalcongressplus$outgrouptop <- ifelse((liberalcongressplus$TopRepublican > 0 & liberalcongressplus$TopDemocrat < 1), 1, NA)
liberalcongressplus$outgrouptop <- ifelse((liberalcongressplus$TopDemocrat > 0 & liberalcongressplus$TopRepublican < 1), 0, liberalcongressplus$outgrouptop)
liberalcongressplus$outgroupcongress <- ifelse((liberalcongressplus$RepublicanCongress > 0 & liberalcongressplus$DemocratCongress < 1), 1, NA)
liberalcongressplus$outgroupcongress <- ifelse((liberalcongressplus$DemocratCongress > 0 & liberalcongressplus$RepublicanCongress < 1), 0, liberalcongressplus$outgroupcongress)

conservativemediaTwitter$outgroup <- ifelse((conservativemediaTwitter$Democrat > 0 & conservativemediaTwitter$Republican < 1), 1, NA)
conservativemediaTwitter$outgroup <- ifelse((conservativemediaTwitter$Republican > 0 & conservativemediaTwitter$Democrat < 1), 0, conservativemediaTwitter$outgroup)
conservativemediaTwitter$outgroupidentity <- ifelse((conservativemediaTwitter$liberalidentity > 0 & conservativemediaTwitter$conservativeidentity < 1), 1, NA)
conservativemediaTwitter$outgroupidentity <- ifelse((conservativemediaTwitter$conservativeidentity > 0 & conservativemediaTwitter$liberalidentity < 1), 0, conservativemediaTwitter$outgroupidentity)
conservativemediaTwitter$outgrouptop <- ifelse((conservativemediaTwitter$TopDemocrat > 0 & conservativemediaTwitter$TopRepublican < 1), 1, NA)
conservativemediaTwitter$outgrouptop <- ifelse((conservativemediaTwitter$TopRepublican > 0 & conservativemediaTwitter$TopDemocrat < 1), 0, conservativemediaTwitter$outgrouptop)
conservativemediaTwitter$outgroupcongress <- ifelse((conservativemediaTwitter$DemocratCongress > 0 & conservativemediaTwitter$RepublicanCongress < 1), 1, NA)
conservativemediaTwitter$outgroupcongress <- ifelse((conservativemediaTwitter$RepublicanCongress > 0 & conservativemediaTwitter$DemocratCongress < 1), 0, conservativemediaTwitter$outgroupcongress)

liberalmediaTwitter$outgroup <- ifelse((liberalmediaTwitter$Republican > 0 & liberalmediaTwitter$Democrat < 1), 1, NA)
liberalmediaTwitter$outgroup <- ifelse((liberalmediaTwitter$Democrat > 0 & liberalmediaTwitter$Republican < 1), 0, liberalmediaTwitter$outgroup)
liberalmediaTwitter$outgroupidentity <- ifelse((liberalmediaTwitter$conservativeidentity > 0 & liberalmediaTwitter$liberalidentity < 1), 1, NA)
liberalmediaTwitter$outgroupidentity <- ifelse((liberalmediaTwitter$liberalidentity > 0 & liberalmediaTwitter$conservativeidentity < 1), 0, liberalmediaTwitter$outgroupidentity)
liberalmediaTwitter$outgrouptop <- ifelse((liberalmediaTwitter$TopRepublican > 0 & liberalmediaTwitter$TopDemocrat < 1), 1, NA)
liberalmediaTwitter$outgrouptop <- ifelse((liberalmediaTwitter$TopDemocrat > 0 & liberalmediaTwitter$TopRepublican < 1), 0, liberalmediaTwitter$outgrouptop)
liberalmediaTwitter$outgroupcongress <- ifelse((liberalmediaTwitter$RepublicanCongress > 0 & liberalmediaTwitter$DemocratCongress < 1), 1, NA)
liberalmediaTwitter$outgroupcongress <- ifelse((liberalmediaTwitter$DemocratCongress > 0 & liberalmediaTwitter$RepublicanCongress < 1), 0, liberalmediaTwitter$outgroupcongress)

congressRepubTwitter$outgroup <- ifelse((congressRepubTwitter$Democrat > 0 & congressRepubTwitter$Republican < 1), 1, NA)
congressRepubTwitter$outgroup <- ifelse((congressRepubTwitter$Republican > 0 & congressRepubTwitter$Democrat < 1), 0, congressRepubTwitter$outgroup)
congressRepubTwitter$outgroupidentity <- ifelse((congressRepubTwitter$liberalidentity > 0 & congressRepubTwitter$conservativeidentity < 1), 1, NA)
congressRepubTwitter$outgroupidentity <- ifelse((congressRepubTwitter$conservativeidentity > 0 & congressRepubTwitter$liberalidentity < 1), 0, congressRepubTwitter$outgroupidentity)
congressRepubTwitter$outgrouptop <- ifelse((congressRepubTwitter$TopDemocrat > 0 & congressRepubTwitter$TopRepublican < 1), 1, NA)
congressRepubTwitter$outgrouptop <- ifelse((congressRepubTwitter$TopRepublican > 0 & congressRepubTwitter$TopDemocrat < 1), 0, congressRepubTwitter$outgrouptop)
congressRepubTwitter$outgroupcongress <- ifelse((congressRepubTwitter$DemocratCongress > 0 & congressRepubTwitter$RepublicanCongress < 1), 1, NA)
congressRepubTwitter$outgroupcongress <- ifelse((congressRepubTwitter$RepublicanCongress > 0 & congressRepubTwitter$DemocratCongress < 1), 0, congressRepubTwitter$outgroupcongress)

congressDemTwitter$outgroup <- ifelse((congressDemTwitter$Republican > 0 & congressDemTwitter$Democrat < 1), 1, NA)
congressDemTwitter$outgroup <- ifelse((congressDemTwitter$Democrat > 0 & congressDemTwitter$Republican < 1), 0, congressDemTwitter$outgroup)
congressDemTwitter$outgroupidentity <- ifelse((congressDemTwitter$conservativeidentity > 0 & congressDemTwitter$liberalidentity < 1), 1, NA)
congressDemTwitter$outgroupidentity <- ifelse((congressDemTwitter$liberalidentity > 0 & congressDemTwitter$conservativeidentity < 1), 0, congressDemTwitter$outgroupidentity)
congressDemTwitter$outgrouptop <- ifelse((congressDemTwitter$TopRepublican > 0 & congressDemTwitter$TopDemocrat < 1), 1, NA)
congressDemTwitter$outgrouptop <- ifelse((congressDemTwitter$TopDemocrat > 0 & congressDemTwitter$TopRepublican < 1), 0, congressDemTwitter$outgrouptop)
congressDemTwitter$outgroupcongress <- ifelse((congressDemTwitter$RepublicanCongress > 0 & congressDemTwitter$DemocratCongress < 1), 1, NA)
congressDemTwitter$outgroupcongress <- ifelse((congressDemTwitter$DemocratCongress > 0 & congressDemTwitter$RepublicanCongress < 1), 0, congressDemTwitter$outgroupcongress)

#
conMediaFacebookOutgroup <- glm(shares_log ~ outgroup + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)
conMediaFacebookOutgroupTop <- glm(shares_log ~ outgrouptop + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)
conMediaFacebookOutgroupIdentity <- glm(shares_log ~ outgroupidentity + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)
conMediaFacebookOutgroupCongress <- glm(shares_log ~ outgroupcongress + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)

libMediaFacebookOutgroup <- glm(shares_log ~ outgroup + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)
libMediaFacebookOutgroupTop <- glm(shares_log ~ outgrouptop + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)
libMediaFacebookOutgroupIdentity <- glm(shares_log ~ outgroupidentity + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)
libMediaFacebookOutgroupCongress <- glm(shares_log ~ outgroupcongress + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)

conCongressFacebookOutgroup <- glm(shares_log ~ outgroup + has_media + has_URL + `Likes at Posting`, data = conservativecongressplus)
conCongressFacebookOutgroupTop <- glm(shares_log ~ outgrouptop + has_media + has_URL + `Likes at Posting`, data = conservativecongressplus)
conCongressFacebookOutgroupIdentity <- glm(shares_log ~ outgroupidentity + has_media + has_URL + `Likes at Posting`, data = conservativecongressplus)
conCongressFacebookOutgroupCongress <- glm(shares_log ~ outgroupcongress + has_media + has_URL + `Likes at Posting`, data = conservativecongressplus)

libCongressFacebookOutgroup <- glm(shares_log ~ outgroup + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)
libCongressFacebookOutgroupTop <- glm(shares_log ~ outgrouptop + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)
libCongressFacebookOutgroupIdentity <- glm(shares_log ~ outgroupidentity + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)
libCongressFacebookOutgroupCongress <- glm(shares_log ~ outgroupcongress + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)

conMediaTwitterOutgroup <- glm(retweet_count_log ~ outgroup + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)
conMediaTwitterOutgroupTop <- glm(retweet_count_log ~ outgrouptop + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)
conMediaTwitterOutgroupIdentity <- glm(retweet_count_log ~ outgroupidentity + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)
conMediaTwitterOutgroupCongress <- glm(retweet_count_log ~ outgroupcongress + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)

libMediaTwitterOutgroup <- glm(retweet_count_log ~ outgroup + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)
libMediaTwitterOutgroupTop <- glm(retweet_count_log ~ outgrouptop + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)
libMediaTwitterOutgroupIdentity <- glm(retweet_count_log ~ outgroupidentity + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)
libMediaTwitterOutgroupCongress <- glm(retweet_count_log ~ outgroupcongress + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)

conCongressTwitterOutgroup <- glm(retweet_count_log ~ outgroup + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupTop <- glm(retweet_count_log ~ outgrouptop + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupIdentity <- glm(retweet_count_log ~ outgroupidentity + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupCongress <- glm(retweet_count_log ~ outgroupcongress + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)

libCongressTwitterOutgroup <- glm(retweet_count_log ~ outgroup + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupTop <- glm(retweet_count_log ~ outgrouptop + has_media + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupIdentity <- glm(retweet_count_log ~ outgroupidentity + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupCongress <- glm(retweet_count_log ~ outgroupcongress + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)

conmediaF <- tidy(summ(conMediaFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroup$df.null + 1), label = "Outgroup")
conmediaF2 <- tidy(summ(conMediaFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
conmediaF3 <- tidy(summ(conMediaFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
conmediaF4 <- tidy(summ(conMediaFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libmediaF <- tidy(summ(libMediaFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroup$df.null + 1), label = "Outgroup")
libmediaF2 <- tidy(summ(libMediaFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libmediaF3 <- tidy(summ(libMediaFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libmediaF4 <- tidy(summ(libMediaFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
concongressF <- tidy(summ(conCongressFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroup$df.null + 1), label = "Outgroup")
concongressF2 <- tidy(summ(conCongressFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
concongressF3 <- tidy(summ(conCongressFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
concongressF4 <- tidy(summ(conCongressFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libcongressF <- tidy(summ(libCongressFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroup$df.null + 1), label = "Outgroup")
libcongressF2 <- tidy(summ(libCongressFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libcongressF3 <- tidy(summ(libCongressFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libcongressF4 <- tidy(summ(libCongressFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
conmediaT <- tidy(summ(conMediaTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroup$df.null + 1), label = "Outgroup")
conmediaT2 <- tidy(summ(conMediaTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
conmediaT3 <- tidy(summ(conMediaTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
conmediaT4 <- tidy(summ(conMediaTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libmediaT <- tidy(summ(libMediaTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroup$df.null + 1), label = "Outgroup")
libmediaT2 <- tidy(summ(libMediaTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libmediaT3 <- tidy(summ(libMediaTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libmediaT4 <- tidy(summ(libMediaTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
concongressT <- tidy(summ(conCongressTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroup$df.null + 1), label = "Outgroup")
concongressT2 <- tidy(summ(conCongressTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
concongressT3 <- tidy(summ(conCongressTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
concongressT4 <- tidy(summ(conCongressTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libcongressT <- tidy(summ(libCongressTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroup") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroup$df.null + 1), label = "Outgroup")
libcongressT2 <- tidy(summ(libCongressTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "outgrouptop") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libcongressT3 <- tidy(summ(libCongressTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupidentity") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libcongressT4 <- tidy(summ(libCongressTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "outgroupcongress") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")

metadataset <- rbind(conmediaF, conmediaF2, conmediaF3, conmediaF4, 
                     libmediaF, libmediaF2, libmediaF3, libmediaF4,
                     concongressF, concongressF2, concongressF3, concongressF4,
                     libcongressF, libcongressF2, libcongressF3, libcongressF4,
                     conmediaT, conmediaT2, conmediaT3, conmediaT4, 
                     libmediaT, libmediaT2, libmediaT3, libmediaT4, 
                     concongressT, concongressT2, concongressT3, concongressT4,
                     libcongressT, libcongressT2, libcongressT3, libcongressT4)

metadatasetreorder <- rbind(libmediaT, libmediaT2, libmediaT3, libmediaT4, 
                            libmediaF, libmediaF2, libmediaF3, libmediaF4,
                            conmediaT, conmediaT2, conmediaT3, conmediaT4,
                            conmediaF, conmediaF2, conmediaF3, conmediaF4, 
                            libcongressT, libcongressT2, libcongressT3, libcongressT4,
                            libcongressF, libcongressF2, libcongressF3, libcongressF4,
                            concongressT, concongressT2, concongressT3, concongressT4,
                            concongressF, concongressF2, concongressF3, concongressF4)

metadatasetmain <- rbind(conmediaF, 
                     libmediaF, 
                     concongressF, 
                     libcongressF, 
                     conmediaT, 
                     libmediaT, 
                     concongressT, 
                     libcongressT)

metagen(estimate, std.error, comb.random = TRUE, method.tau = "EB", data = metadatasetmain)
metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = metadatasetreorder)
metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "EB", data = metadatasetmain)

exp(0.8408); exp(0.4421); exp(1.2396)
exp(0.6553); exp(0.4703); exp(0.8404)

metadatasetmain$liberal <- ifelse(metadatasetmain$party == "Liberal", metadatasetmain$liberal <- 1, metadatasetmain$liberal <- 0)
metadatasetmain$congress <- ifelse(metadatasetmain$type == "Congress", metadatasetmain$congress <- 1, metadatasetmain$congress <- 0)
metadatasetmain$twitter <- ifelse(metadatasetmain$platform == "Twitter", metadatasetmain$platform <- 1, metadatasetmain$platform <- 0)
res1 <- rma(estimate, std.error, data = metadatasetmain, method="EB", mods = cbind(liberal, congress, twitter)) 
?rma()
predict(res1, transf=exp, digits=2)
summary(res1)
exp(0.2964); exp(0.2454); exp(0.8383)
exp(0.1932); exp(0.3487); exp(0.7351)
exp(0.8483); exp(0.3065); exp(1.3902)

rma(estimate, std.error, data = metadatasetmain, method = "DL", mods = cbind(twitter, congress, liberal))

coef <- as.data.frame(metadataset$estimate)
lower <- as.data.frame(metadataset$conf.low)
upper <- as.data.frame(metadataset$conf.high)
coefs <- cbind(coef, lower, upper)
tabs <- cbind()
coefs

forestplot = forestplot(metadataset[, c(13, 8)], 
                        coefs,
                        xlab = "Odds being being shared or retweeted",
                        txt_gp = fpTxtGp(ticks = gpar(cex =.8),
                                         xlab = gpar(cex = .5),
                                         label = gpar(cex = .5)))

forest.meta(metagen(estimate, std.error, comb.random = TRUE, data = metadataset))

labels <- as.character(metadatasetmain[, c("dataset")])
metadatasetmaindf <- as.data.frame(metadatasetmain)
metadatasetdf <- as.data.frame(metadatasetreorder)
metadatasetdf[1:8, c("dataset")]
metadatasetdf$dataset2 <- NULL
metadatasetdf$dataset2 <- ifelse(metadatasetdf$label == "Outgroup" | metadatasetdf$label == "Outgroup (Top 100)", metadatasetdf$dataset2 <- metadatasetdf$dataset, metadatasetdf$dataset2 <- " ")

summary_table <- metadatasetdf[1:32, c("label", "dataset2")]
metadatasetdf$main <- ifelse(metadatasetdf$label == "Outgroup", metadatasetdf$main <- TRUE, metadatasetdf$main <- FALSE)
metadatasetdf$mainreverse <- metadatasetdf$main == FALSE
metadatasetdf$color <- ifelse(metadatasetdf$party == "Conservative", metadatasetdf$color <- "firebrick", metadatasetdf$color <- "steelblue4")

summary_labels <- data.frame(
  Dataset = c("All", "All"),
  Variable = c("Summary (Main Dictionaries)", "Summary (Other Dictionaries)"))
head(summary_labels)

metaplot <- viz_forest(x = metadatasetdf[1:32, c("estimate", "std.error")], 
           study_labels = metadatasetdf[1:32, c("dataset")],
           method = "DL",
           annotate_CI = T,
           study_table = summary_table[1:32, 2:1],
           summary_table = summary_labels,
           group = metadatasetdf[1:32, "mainreverse"],
           x_trans_function = exp, 
           xlab = "Odds Ratio",
           summary_label = c("Summary (Main Dictionaries)", "Summary (Other Dictionaries)"), 
           col = as.vector(metadatasetdf$color), 
           summary_col = c("slateblue4"),
           table_headers = c("Dataset", "Variable"),
           text_size = 3.8) + draw_grob(leg2, vjust = 0)

plot_grid(metaplot, ) + draw_grob(leg2, vjust = 0.4)

combinedFBmediaplots <- plot_grid(metaplot, 
                                  leg2, 
                                  nrow = 2,
                                  labels = c("A", "B"),
                                  rel_heights = c(1,.3))


leg2 <- get_legend(dwplot(libmodels, confint = .99, dot_args = list(size = 0.8), whisker_args = list(size = 0.7)) + 
                     geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
                     theme_apa(legend.pos = "side",
                               legend.font.size = 12) +
                      xlab("Odds ratio") +
                     scale_colour_manual(values = c("steelblue4", "firebrick"), labels = c("Liberal", "Conservative")) + 
                     ggtitle("Liberal Media"))

theme_apa()

ale_color_hueleg2

ggsave("metaanalysis.png", width = 15, height = 10)

c("firebrick", "steelblue4")[metadatasetdf[1:32, "mainreverse"]]

viz_forest(x = mozart[1:10, c("d", "se")], 
           group = mozart[1:10, "rr_lab"], 
           study_labels = mozart[1:10, "study_name"], 
           summary_label = c("Summary (rr_lab = no)", "Summary (rr_lab = yes)"), 
           xlab = "Cohen d",
           col = c("firebrick", "steelblue4")[mozart[1:10, "rr_lab"]],
           summary_col = c("firebrick", "steelblue4"))

class(metadatasetdf$color)

c("firebrick", "steelblue4")metadatasetdf[1:32, "mainreverse"]

ggsave("metaanalysis.png", width = 15, height = 10)

forestplot = forestplot(metadataset,
                        txt_gp = fpTxtGp(ticks = gpar(cex =.8),
                                         xlab = gpar(cex = .9),
                                         label = gpar(cex = .9)),
                        metadata,
                        new_page = TRUE,
                        zero = 0.65, 
                        boxsize = 0.13,
                        vertices = TRUE,
                        ci.vertices.height = .07, 
                        graphwidth = unit(100, 'mm'),
                        lineheight = unit(30, 'mm'),
                        xlab = "Mean Effect Size (d)", 
                        is.summary=c(rep(FALSE,4)),
                        col=fpColors(box="black",line="black", summary="black"))

#### Meta Analysis of Main Effects ####

ConMediaFB <- tidy(summ(LinearModConMedia, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook") 
LibMediaFB <- tidy(summ(LinearModDemMedia, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook")
LibCongressFB <- tidy(summ(linearModDemCongress, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook")
ConCongressFB <- tidy(summ(linearModCongressRepub, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook")

ConMediaTwitter <- tidy(summ(ConservativeTwitterMod, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter")
LibMediaTwitter <- tidy(summ(LiberalMediaTwitterMod, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter")
ConCongressTwitter <- tidy(summ(congressRepubTwitterMod, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter")
LibCongressTwitter <- tidy(summ(congressDemTwitterMod, center = TRUE, vifs = TRUE)) %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter")

meta2 <- rbind(ConMediaFB, LibMediaFB, LibCongressFB, ConCongressFB, ConMediaTwitter, LibMediaTwitter, ConCongressTwitter, LibCongressTwitter) %>%
arrange(party, term)
meta2$liberal <- ifelse(meta2$party == "Liberal", 1, 0)
meta2$twitter <- ifelse(meta2$platform == "Twitter", 1, 0)
meta2$congress <- ifelse(meta2$type == "Congress", 1, 0)

meta2[7:10, 1] <- "outgroup"
meta2[35:38, 1] <- "ingroup"
meta2[45:48, 1] <- "ingroup"
meta2[73:76, 1] <- "outgroup"

View(meta2)

meta <- metagen(estimate, std.error, sm = "OR", comb.random = TRUE, method.tau = "EB", data = subset(meta2, term == "outgroup" & congress == 1))
meta <- metagen(estimate, std.error, sm = "OR", comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "ingroup"))
meta <- metagen(estimate, std.error, sm = "OR", comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "NegativeAffect"))
meta <- metagen(estimate, std.error, sm = "OR", comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "MoralEmotional"))
meta <- metagen(estimate, std.error, sm = "OR", comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "PositiveAffect"))
res1 <- rma(estimate, std.error, data = subset(meta2, term == "outgroup"), method="DL", mods = cbind(liberal, congress, twitter)) 
View(meta2)

#Exponentiate effect sizes to make IRR
exp(meta$TE.random); exp(meta$lower.random); exp(meta$upper.random)
exp(res1[["beta"]]); exp(res1[["ci.lb"]]); exp(res1[["ci.ub"]])

meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "outgroup"))
metaoutgroup <- tidy()
metaoutgroup <- tibble(name = "Outgroup", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "ingroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Ingroup", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "NegativeAffect"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Negative Affect", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "PositiveAffect"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Positive Affect", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(estimate, std.error, comb.random = TRUE, method.tau = "DL", data = subset(meta2, term == "MoralEmotional"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Moral Emotional", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))

metaoutgroup$term <- metaoutgroup$name

plot <- dwplot(metaoutgroup, conf.level = .95,  
               dot_args = list(size = 1.2), whisker_args = list(size = 1)) 

two_brackets <- list(c("Identity", "Outgroup", "Ingroup"), 
                     c("Emotion", "Negative Affect", "Moral Emotional"))

metaplot <- {(
  plot + theme_bw() + xlab("Change in odds of share/retweet") + ylab("") +
    ggtitle("Mean Effect Sizes") +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
    theme_apa() +
    theme(legend.position = "none") +
    xlim(0.7, 2.1) +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(260, 170)) 
)} %>% 
  add_brackets(two_brackets)
metaplot

metaR1 <- libmodels %>% mutate(dataset = "Liberal Media", party = "Liberal", type = "Media")
metaR2 <- conmodels %>% mutate(dataset = "Conservative Media", party = "Conservative", type = "Media")
metaR3 <- libcongressmodels %>% mutate(dataset = "Liberal Congress", party = "Liberal", type = "Congress")
metaR4 <- concongressmodels %>% mutate(dataset = "Conservative Congress", party = "Conservative", type = "Congress")

metaR <- rbind(metaR1, metaR2, metaR3, metaR4)

View(metaR)

metaR <- metaR %>% 
  arrange(model, party)

metaR <- metaR[41:120, ]

metaR$coef <- ""
metaR$coef[1:20] <- "Outgroup"
metaR$coef[21:60] <- "Ingroup"
metaR$coef[61:80] <- "Outgroup"

metaR$term[c(19, 39, 59, 79)] <- "Retweets"
metaR$term[c(20, 40, 60, 80)] <- "Favorites"

##Outgroup
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Shares" & metaR$coef == "Outgroup"))
metaoutgroup <- tibble(name = "Shares", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Comments" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Comments", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Likes" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Likes", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Love" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Love", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Haha" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Haha", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Wow" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Wow", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Sad" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Sad", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Angry" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Angry", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Retweets" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Retweets", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Favorites" & metaR$coef == "Outgroup"))
metaoutgroup <- metaoutgroup %>% add_row(name = "Favorites", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))

##Ingroup
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Shares" & metaR$coef == "Ingroup"))
metaingroup <- tibble(name = "Shares", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Comments" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Comments", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Likes" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Likes", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Love" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Love", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Haha" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Haha", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Wow" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Wow", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Sad" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Sad", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Angry" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Angry", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Retweets" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Retweets", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))
meta <- metagen(log(estimate), std.error, comb.random = TRUE, method.tau = "DL", data = subset(metaR, metaR$term == "Favorites" & metaR$coef == "Ingroup"))
metaingroup <- metaingroup %>% add_row(name = "Favorites", estimate = exp(meta$TE.random), conf.low = exp(meta$lower.random), conf.high = exp(meta$upper.random))

metaoutgroup <- metaoutgroup %>% mutate(model = "Outgroup")
metaingroup <- metaingroup %>% mutate(model = "Ingroup")
metaReaction <- rbind(metaoutgroup, metaingroup)
metaReaction$term <- metaReaction$name

metaR$liberal <- ifelse(metaR$party == "Liberal", 1, 0)
metaR$congress <- ifelse(metaR$type == "Congress", 1, 0)

res1 <- rma(log(estimate), std.error, data = subset(metaR, metaR$term == "Angry" & metaR$coef == "Outgroup"), comb.random = TRUE, method.tau = "DL", mods = cbind(liberal, congress)) 
res1 <- rma(log(estimate), std.error, data = subset(metaR, metaR$term == "Love" & metaR$coef == "Ingroup"), comb.random = TRUE, method.tau = "DL", mods = cbind(liberal, congress)) 

#Exponentiate effect sizes to make IRR
exp(meta$TE.random); exp(meta$lower.random); exp(meta$upper.random)
exp(res1[["beta"]]); exp(res1[["ci.lb"]]); exp(res1[["ci.ub"]])
res1 <- rma(estimate, std.error, data = subset(meta2, term == "outgroup"), method="DL", mods = cbind(liberal, congress, twitter)) 
View(meta2)

#Exponentiate effect sizes to make IRR
exp(meta$TE.random); exp(meta$lower.random); exp(meta$upper.random)
exp(res1[["beta"]]); exp(res1[["ci.lb"]]); exp(res1[["ci.ub"]])

####

two_brackets <- list(c("Facebook", "Shares", "Angry"), 
                     c("Twitter", "Retweets", "Favorites"))

metaPlot <- {dwplot(metaReaction, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "none") + 
    xlab("Change in odds of engagement") +
    scale_colour_hue(h = c(0, 260)) + 
    xlim(0.8, 3.0) +
    ggtitle("Mean Effect Sizes (Reactions)")} %>%
  add_brackets(two_brackets)
metaPlot

leg2 <- get_legend(dwplot(concongressmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.7)) + 
                     geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
                     theme_apa(legend.pos = "right",
                               legend.font.size = 12) + xlab("Odds ratio") +
                     scale_colour_hue(h = c(260, 0), labels = c("Ingroup", "Outgroup")) + 
                     ggtitle("Liberal Media"))

metagrid <- plot_grid(metaplot, 
                      metaPlot,
                      leg2,
                      ncol = 3, 
                      labels = c("A", "B", " "),
                      rel_widths = c(1, 1, 0.25))
metagrid

write.table(metaReaction, file = "olstab.txt", sep = ",", quote = FALSE, row.names = F)

ggsave("MetaPlot.png", width = 14, height = 4)

#### Meta Analysis Alternate Dictionaries ####

conMediaFacebookOutgroup <- glm(shares_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)
conMediaFacebookOutgroupIdentity <- glm(shares_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)
conMediaFacebookOutgroupTop <- glm(shares_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)
conMediaFacebookOutgroupCongress <- glm(shares_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = conservativemediaplus)

libMediaFacebookOutgroup <- glm(shares_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)
libMediaFacebookOutgroupIdentity <- glm(shares_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)
libMediaFacebookOutgroupTop <- glm(shares_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)
libMediaFacebookOutgroupCongress <- glm(shares_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalmediaplus)

conCongressFacebookOutgroup <- glm(shares_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = conservativecongressplus)
conCongressFacebookOutgroupIdentity <- glm(shares_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + `Likes at Posting`, data = conservativecongressplus)
conCongressFacebookOutgroupTop <- glm(shares_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = conservativecongressplus)
conCongressFacebookOutgroupCongress <- glm(shares_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = conservativecongressplus)

libCongressFacebookOutgroup <- glm(shares_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)
libCongressFacebookOutgroupIdentity <- glm(shares_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)
libCongressFacebookOutgroupTop <- glm(shares_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)
libCongressFacebookOutgroupCongress <- glm(shares_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + `Likes at Posting`, data = liberalcongressplus)

conMediaTwitterOutgroup <- glm(retweet_count_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)
conMediaTwitterOutgroupIdentity <- glm(retweet_count_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)
conMediaTwitterOutgroupTop <- glm(retweet_count_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)
conMediaTwitterOutgroupCongress <- glm(retweet_count_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = conservativemediaTwitter)

libMediaTwitterOutgroup <- glm(retweet_count_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)
libMediaTwitterOutgroupIdentity <- glm(retweet_count_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)
libMediaTwitterOutgroupTop <- glm(retweet_count_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)
libMediaTwitterOutgroupCongress <- glm(retweet_count_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = liberalmediaTwitter)

conCongressTwitterOutgroup <- glm(retweet_count_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupIdentity <- glm(retweet_count_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupTop <- glm(retweet_count_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)
conCongressTwitterOutgroupCongress <- glm(retweet_count_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = congressRepubTwitter)

libCongressTwitterOutgroup <- glm(retweet_count_log ~ Democrat + Republican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupIdentity <- glm(retweet_count_log ~ liberalidentity + conservativeidentity + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupTop <- glm(retweet_count_log ~ TopDemocrat + TopRepublican + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)
libCongressTwitterOutgroupCongress <- glm(retweet_count_log ~ DemocratCongress + RepublicanCongress + MoralEmotional + NegativeAffect + PositiveAffect + has_media + has_URL + followers_count + is_retweet, data = congressDemTwitter)

conmediaF <- tidy(summ(conMediaFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Democrat") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroup$df.null + 1), label = "Outgroup")
conmediaF2 <- tidy(summ(conMediaFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopDemocrat") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
conmediaF3 <- tidy(summ(conMediaFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "liberalidentity") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
conmediaF4 <- tidy(summ(conMediaFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "DemocratCongress") %>% mutate(dataset = "Conservative Media Facebook", party = "Conservative", type = "Media", platform = "Facebook", n = (conMediaFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libmediaF <- tidy(summ(libMediaFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Republican") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroup$df.null + 1), label = "Outgroup")
libmediaF2 <- tidy(summ(libMediaFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopRepublican") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libmediaF3 <- tidy(summ(libMediaFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "conservativeidentity") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libmediaF4 <- tidy(summ(libMediaFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "RepublicanCongress") %>% mutate(dataset = "Liberal Media Facebook", party = "Liberal", type = "Media", platform = "Facebook", n = (libMediaFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
concongressF <- tidy(summ(conCongressFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Democrat") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroup$df.null + 1), label = "Outgroup")
concongressF2 <- tidy(summ(conCongressFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopDemocrat") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
concongressF3 <- tidy(summ(conCongressFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "liberalidentity") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
concongressF4 <- tidy(summ(conCongressFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "DemocratCongress") %>% mutate(dataset = "Conservative Congress Facebook", party = "Conservative", type = "Congress", platform = "Facebook", n = (conCongressFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libcongressF <- tidy(summ(libCongressFacebookOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Republican") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroup$df.null + 1), label = "Outgroup")
libcongressF2 <- tidy(summ(libCongressFacebookOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopRepublican") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libcongressF3 <- tidy(summ(libCongressFacebookOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "conservativeidentity") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libcongressF4 <- tidy(summ(libCongressFacebookOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "RepublicanCongress") %>% mutate(dataset = "Liberal Congress Facebook", party = "Liberal", type = "Congress", platform = "Facebook", n = (libCongressFacebookOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
conmediaT <- tidy(summ(conMediaTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Democrat") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroup$df.null + 1), label = "Outgroup")
conmediaT2 <- tidy(summ(conMediaTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopDemocrat") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
conmediaT3 <- tidy(summ(conMediaTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "liberalidentity") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
conmediaT4 <- tidy(summ(conMediaTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "DemocratCongress") %>% mutate(dataset = "Conservative Media Twitter", party = "Conservative", type = "Media", platform = "Twitter", n = (conMediaTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libmediaT <- tidy(summ(libMediaTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Republican") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroup$df.null + 1), label = "Outgroup")
libmediaT2 <- tidy(summ(libMediaTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopRepublican") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libmediaT3 <- tidy(summ(libMediaTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "conservativeidentity") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libmediaT4 <- tidy(summ(libMediaTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "RepublicanCongress") %>% mutate(dataset = "Liberal Media Twitter", party = "Liberal", type = "Media", platform = "Twitter", n = (libMediaTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
concongressT <- tidy(summ(conCongressTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Democrat") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroup$df.null + 1), label = "Outgroup")
concongressT2 <- tidy(summ(conCongressTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopDemocrat") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
concongressT3 <- tidy(summ(conCongressTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "liberalidentity") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
concongressT4 <- tidy(summ(conCongressTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "DemocratCongress") %>% mutate(dataset = "Conservative Congress Twitter", party = "Conservative", type = "Congress", platform = "Twitter", n = (conCongressTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")
libcongressT <- tidy(summ(libCongressTwitterOutgroup, center = TRUE, vifs = TRUE)) %>% filter (term == "Republican") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroup$df.null + 1), label = "Outgroup")
libcongressT2 <- tidy(summ(libCongressTwitterOutgroupTop, center = TRUE, vifs = TRUE)) %>% filter (term == "TopRepublican") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupTop$df.null + 1), label = "Outgroup (Top 100)")
libcongressT3 <- tidy(summ(libCongressTwitterOutgroupIdentity, center = TRUE, vifs = TRUE)) %>% filter (term == "conservativeidentity") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupIdentity$df.null + 1), label = "Outgroup (Identity)")
libcongressT4 <- tidy(summ(libCongressTwitterOutgroupCongress, center = TRUE, vifs = TRUE)) %>% filter (term == "RepublicanCongress") %>% mutate(dataset = "Liberal Congress Twitter", party = "Liberal", type = "Congress", platform = "Twitter", n = (libCongressTwitterOutgroupCongress$df.null + 1), label = "Outgroup (Congress)")

metadataset <- rbind(libmediaT, libmediaT2, libmediaT3, libmediaT4, 
                            libmediaF, libmediaF2, libmediaF3, libmediaF4,
                            conmediaT, conmediaT2, conmediaT3, conmediaT4,
                            conmediaF, conmediaF2, conmediaF3, conmediaF4, 
                            libcongressT, libcongressT2, libcongressT3, libcongressT4,
                            libcongressF, libcongressF2, libcongressF3, libcongressF4,
                            concongressT, concongressT2, concongressT3, concongressT4,
                            concongressF, concongressF2, concongressF3, concongressF4)

meta <- metagen(estimate, std.error, sm = "OR", comb.random = TRUE, method.tau = "DL", data = subset(metadataset, label == "Outgroup (Congress)"))

labels <- as.character(metadataset[, c("dataset")])
metadatasetmaindf <- as.data.frame(metadataset)
metadatasetdf <- as.data.frame(metadatasetreorder)
metadatasetdf[1:8, c("dataset")]
metadatasetdf$dataset2 <- NULL
metadatasetdf$dataset2 <- ifelse(metadatasetdf$label == "Outgroup" | metadatasetdf$label == "Outgroup (Top 100)", metadatasetdf$dataset2 <- metadatasetdf$dataset, metadatasetdf$dataset2 <- " ")

summary_table <- metadatasetdf[1:32, c("label", "dataset2")]
metadatasetdf$main <- ifelse(metadatasetdf$label == "Outgroup", metadatasetdf$main <- TRUE, metadatasetdf$main <- FALSE)
metadatasetdf$mainreverse <- metadatasetdf$main == FALSE
metadatasetdf$color <- ifelse(metadatasetdf$party == "Conservative", metadatasetdf$color <- "firebrick", metadatasetdf$color <- "steelblue4")

summary_labels <- data.frame(
  Dataset = c("All", "All"),
  Variable = c("Summary (Main Dictionaries)", "Summary (Other Dictionaries)"))
head(summary_labels)

metaplot <- viz_forest(x = metadatasetdf[1:32, c("estimate", "std.error")], 
                       study_labels = metadatasetdf[1:32, c("dataset")],
                       method = "DL",
                       annotate_CI = T,
                       study_table = summary_table[1:32, 2:1],
                       summary_table = summary_labels,
                       group = metadatasetdf[1:32, "mainreverse"],
                       x_trans_function = exp, 
                       xlab = "Odds Ratio",
                       summary_label = c("Summary (Main Dictionaries)", "Summary (Other Dictionaries)"), 
                       col = as.vector(metadatasetdf$color), 
                       summary_col = c("slateblue4"),
                       table_headers = c("Dataset", "Variable"),
                       text_size = 3.8) + draw_grob(leg2, vjust = 0)

#### 19 Most Popular Pages ####

gettopposts <- function(dataset) {
  rankpages <- dataset %>%
    dplyr::select(text, retweet_count) %>% 
    arrange(desc(retweet_count))
  
  toppages <- rankpages %>%
    group_by(text) %>%
    dplyr::summarize(n = n(),
                     Retweets = mean(retweet_count)) %>%
    filter(n > 1) %>%
    arrange(desc(Retweets))
}

l <- subset(conservativemediaTwitter, conservativemediaTwitter$is_retweet == FALSE) %>%
  dplyr::select(text, retweet_count) %>% 
  arrange(desc(retweet_count))
View(l)

View(gettopposts(subset(congressDemTwitter, congressDemTwitter$is_retweet == FALSE)))

gettoppages <- function(dataset) {
  rankpages <- dataset %>%
    dplyr::select(`Page Name`, text, Shares, Likes, Love, Haha, Wow, Sad, Angry) %>% 
    arrange(desc(Shares)) 
  
  toppages <- rankpages %>%
    group_by(`Page Name`) %>%
    dplyr::summarize(n = n(),
                     Shares = mean(Shares), # using geometric mean
                     Likes = mean(Likes), 
                     Love = mean(Love), 
                     Haha = mean(Haha),
                     Wow = mean(Wow),
                     Sad = mean(Sad), 
                     Angry = mean(Angry)) %>%
    filter(n > 10) %>%
    arrange(desc(Shares))
}

makeplot <- function(dataset, color) {
  dataset %>%
    head(10) %>%
    plyr::mutate(screen_name = fct_reorder(screen_name, retweets)) %>%
    ggplot(aes(screen_name, retweets)) +
    coord_flip() +
    geom_col(show.legend = TRUE, fill = color) + 
    theme_apa() 
}

gettoppagesTwitter <- function(dataset) {
  rankpages <- dataset %>%
    dplyr::select(screen_name, text, retweet_count, favorite_count, created_at, status_id) %>% 
    arrange(desc(retweet_count)) 
  
  toppages <- rankpages %>%
    group_by(screen_name) %>%
    dplyr::summarize(n = n(),
                     retweets = mean(retweet_count), # using geometric mean
                     favorites = mean(favorite_count)) %>%
    filter(n > 10) %>%
    arrange(desc(retweets))
}

CNN <- get_timeline("CNN", n = 3200)
liberalmediaTwitter2 <- rbind.fill(liberalmediaTwitter, CNN)

top <- gettoppages(conservativemediaplus)
top <- gettoppages(liberalmediaplus)
top <- gettoppages(conservativecongressplus)
top <- gettoppages(subset(liberalcongressplus, liberalcongressplus$`Page Name` != "U.S. Senator Bernie Sanders"))
top <- gettoppagesTwitter(conservativemediaTwitter)
top <- gettoppagesTwitter(rbind.fill(liberalmediaTwitter, CNN))
top <- gettoppagesTwitter(congressDemTwitter)
top <- gettoppagesTwitter(congressRepubTwitter)
top

mean(liberalmediaplus$Shares)
mean(conservativemediaplus$Shares)
mean(liberalmediaTwitter$retweet_count)
mean(conservativemediaTwitter$retweet_count)

CNN <- get_timeline("CNN", n = 3200)

View(liberalmediaTwitter)
rankcon <- conservativemediaplus %>%
dplyr::select(`Page Name`, text, Shares, Likes, Love, Haha, Wow, Sad, Angry) %>% 
  arrange(desc(Shares)) 
top

makeplot <- function(dataset, color) {
  dataset %>%
    head(10) %>%
    plyr::mutate(screen_name = fct_reorder(screen_name, retweets)) %>%
    ggplot(aes(screen_name, retweets)) +
    coord_flip() +
    geom_col(show.legend = TRUE, fill = color) + 
    theme_apa() 
}

makeplotFacebook <- function(dataset, color) {
  dataset %>%
    head(10) %>%
    plyr::mutate(`Page Name` = fct_reorder(`Page Name`, Shares)) %>%
    ggplot(aes(`Page Name`, Shares)) +
    coord_flip() +
    geom_col(show.legend = TRUE, fill = color) + 
    theme_apa() 
}

conservativemediaplus$`Page Name`

makeplot(top, "firebrick")

repubcongressplot <- top %>%
  head(10) %>%
  plyr::mutate(screen_name = fct_reorder(screen_name, retweets)) %>%
  ggplot(aes(screen_name, retweets)) +
  coord_flip() +
  geom_col(show.legend = TRUE, fill = "steelblue4") + 
  theme_apa() 

bigplot <- plot_grid(makeplotFacebook(gettoppages(conservativemediaplus), "firebrick"), 
          makeplot(gettoppagesTwitter(conservativemediaTwitter), "firebrick"),
          makeplotFacebook(gettoppages(liberalmediaplus), "steelblue4"),
          makeplot(gettoppagesTwitter(rbind.fill(liberalmediaTwitter, CNN)), "steelblue4"), 
          #
          makeplotFacebook(gettoppages(conservativecongressplus), "firebrick"), 
          makeplot(gettoppagesTwitter(congressRepubTwitter), "firebrick"),
          makeplotFacebook(gettoppages(subset(liberalcongressplus, liberalcongressplus$`Page Name` != "U.S. Senator Bernie Sanders")), "steelblue4"),
          makeplot(gettoppagesTwitter(congressDemTwitter), "steelblue4"),
          #
          ncol = 4,
          nrow = 2,
          labels = c("A", "B", "C", "D",
                     "E", "F", "G", "H"),
          rel_heights = c(1, 1))

ggsave("bigdescriptiveplot.png", width = 16, height = 8)

makeplot(gettoppages(conservativemediaTwitter), "steelblue4")

makeplotFacebook(gettoppages(conservativemediaplus), "firebrick")
+
  scale_fill_discrete(labels=c("Republican", "Democrat")) +
  theme_bw() +
  labs(title = "Most Retweeted Political Identity Terms",
       x = "Word",
       y = "Mean Retweet Count",
       fill = "Party")
repubcongressplot

hist(conservativemediaTwitter$created_at)

conservativemediaTwitter$year <- year(conservativemediaTwitter$created_at)
liberalmediaTwitter$year <- year(liberalmediaTwitter$created_at)
congressDemTwitter$year <- year(congressDemTwitter$created_at)
congressRepubTwitter$year <- year(congressRepubTwitter$created_at)

conservativemediaplus$year <- year(conservativemediaplus$Created)
liberalmediaplus$year <- year(liberalmediaplus$Created)
conservativecongressplus$year <- year(conservativecongressplus$Created)
liberalcongressplus$year <- year(liberalcongressplus$Created)
liberalcongressplus$year, xlim = c(2010, 2020), main = "hello", breaks = 5, col = "firebrick")

library("grid")
install.packages("ggplotify")
library("ggplotify")

as.ggplot(hist(conservativemediaplus$year, xlim = c(2010, 2020), main = "Conservative Media Facebook", breaks = 5, col = "firebrick"))

qplot(conservativemediaplus$year,
      geom="histogram",
      binwidth = 1,  
      main = "Conservative Media Facebook", 
      xlab = "Year",  
      ylab = "Number of Posts",
      fill=I("steelblue4"), 
      xlim=c(2009,2021)) + theme_apa()



bighist <- plot_grid(qplot(conservativemediaplus$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Conservative Media Facebook", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("firebrick"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     qplot(conservativemediaTwitter$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Conservative Media Twitter", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("firebrick"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     qplot(liberalmediaplus$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Liberal Media Facebook", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("steelblue4"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     qplot(liberalmediaTwitter$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Liberal Media Twitter", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("steelblue4"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     
                     qplot(conservativecongressplus$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Conservative Congress Facebook", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("firebrick"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     qplot(congressRepubTwitter$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Conservative Congress Twitter", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("firebrick"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     qplot(liberalcongressplus$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Liberal Congress Facebook", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("steelblue4"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     qplot(congressDemTwitter$year,
                           geom="histogram",
                           binwidth = 1,  
                           main = "Liberal Congress Twitter", 
                           xlab = "Year",  
                           ylab = "Number of Posts",
                           fill=I("steelblue"), 
                           xlim=c(2009,2021)) + theme_apa(),
                     ncol = 4,
                     nrow = 2,
                     labels = c("A", "B", "C", "D",
                                "E", "F", "G", "H"),
                     rel_heights = c(1, 1))

ggsave("bighist.png", width = 16, height = 8)


                     as.grob(hist(conservativemediaTwitter$year, xlim = c(2010, 2020), main = "Conservative Media Twitter", breaks = 5, col = "firebrick")))


                     hist(liberalmediaplus$year, xlim = c(2010, 2020), main = "hello", breaks = 5, col = "steelblue4"),
                     hist(liberalmediaTwitter$year, xlim = c(2010, 2020), main = "hello", breaks = 5, col = "steelblue4"),
                     
                     hist(liberalmediaTwitter$year, xlim = c(2010, 2020), main = "hello", breaks = 5, col = "firebrick"))
  
  
  makeplotFacebook(gettoppages(conservativemediaplus), "firebrick"), 
                     makeplot(gettoppagesTwitter(conservativemediaTwitter), "firebrick"),
                     makeplotFacebook(gettoppages(liberalmediaplus), "steelblue4"),
                     makeplot(gettoppagesTwitter(rbind.fill(liberalmediaTwitter, CNN)), "steelblue4"), 
                     #
                     makeplotFacebook(gettoppages(conservativecongressplus), "firebrick"), 
                     makeplot(gettoppagesTwitter(congressRepubTwitter), "firebrick"),
                     makeplotFacebook(gettoppages(subset(liberalcongressplus, liberalcongressplus$`Page Name` != "U.S. Senator Bernie Sanders")), "steelblue4"),
                     makeplot(gettoppagesTwitter(congressDemTwitter), "steelblue4"),
                     #
                     ncol = 4,
                     nrow = 2,
                     labels = c("A", "B", "C", "D",
                                "E", "F", "G", "H"),
                     rel_heights = c(1, 1))


#### 20 Create Limited Datasets for Sharing Purposes ####

conmediashare <- conservativemediaplus %>%
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, Likes, Shares, Comments, Love, Wow, Haha, Sad, Angry, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year) 

libmediashare <- liberalmediaplus %>% 
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, Likes, Shares, Comments, Love, Wow, Haha, Sad, Angry, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year)

concongressshare <- conservativecongressplus %>% 
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, Likes, Shares, Comments, Love, Wow, Haha, Sad, Angry, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year)

libcongressshare <- liberalcongressplus %>% 
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, Likes, Shares, Comments, Love, Wow, Haha, Sad, Angry, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year)

conmediatwittershare <- conservativemediaTwitter %>% 
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, retweet_count, favorite_count, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year)

liberalmediatwittershare <- liberalmediaTwitter %>% 
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, retweet_count, favorite_count, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year)

concongressshare <- congressRepubTwitter %>% 
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, retweet_count, favorite_count, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year)

demcongressshare <- congressDemTwitter %>% 
  dplyr::select(Democrat, Republican, NegativeAffect, PositiveAffect, MoralEmotional, retweet_count, favorite_count, has_URL, has_media, outgroup, outgroupidentity, outgroupcongress, outgrouptop, year)

save_as_csv(conmediashare, "DataShare/conservativeMediaFacebook.csv")
save_as_csv(libmediashare, "DataShare/liberalMediaFacebook.csv")
save_as_csv(concongressshare, "DataShare/conservativeCongressFacebook.csv")
save_as_csv(libcongressshare, "DataShare/liberalCongressFacebook.csv")

save_as_csv(conmediatwittershare, "DataShare/conservativeMediaTwitter.csv")
save_as_csv(liberalmediatwittershare, "DataShare/liberalMediaTwitter.csv")
save_as_csv(concongressshare, "DataShare/conservativeCongressTwitter.csv")
save_as_csv(demcongressshare, "DataShare/democratCongressTwitter.csv")
