#### Welcome ####

#All code is in this one giant R script, denoted by chapters. 
#Chapters 1-5 are about preparing the datasets.
#Start at chapter 6 if you want to work with the already prepared datasets.

#### 1 Set Up Datasets ####

#Set Working Directory
setwd("/Users/bridg/OneDrive/Documents/Georgetown/Text_as_Data")

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


## Load in dictionaries

MoralEmotional <- scan("Dictionaries/MoralEmotional.txt", what='character', sep="\n", skipNul = TRUE)
MoralEmotional <- strsplit(MoralEmotional, "[[:space:]]+")
TopRepublican <- scan("Dictionaries/MostFamousRepublicans.txt", what='character', sep="\t", skipNul = TRUE)
TopDemocrat <- scan("Dictionaries/MostFamousDemocrats.txt", what='character', sep="\t", skipNul = TRUE)
DemocratCongress <- scan("Dictionaries/DemocratCongress.txt", what='character', sep="\t", skipNul = TRUE)
RepublicanCongress <- scan("Dictionaries/RepublicansCongress.txt", what='character', sep="\t", skipNul = TRUE)
TopRepublicanFB <- scan("Dictionaries/MostFamousRepublicansFacebook.txt", what='character', sep="\t", skipNul = TRUE)
TopDemocratFB <- scan("Dictionaries/MostFamousDemocratsFacebook.txt", what='character', sep="\n", skipNul = TRUE)
DemocratCongressFB <- scan("Dictionaries/DemocratCongressFB.txt", what='character', sep="\n", skipNul = TRUE)
RepublicanCongressFB <- scan("Dictionaries/RepublicansCongressFB.txt", what='character', sep="\n", skipNul = TRUE)

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
                        MoralEmotional = MoralEmotional))

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
                         MoralEmotional = MoralEmotional))



#### Mapping dictionaries ####

### Original code:

#Conservative Congress Dataset Dictionary Analysis

#the text field for the corpus is a combination of the message of post plus any image or link text attached to the post
#repubcongress$text <- paste(repubcongress$Message, repubcongress$`Image Text`, repubcongress$`Link Text`)
#repubcongress$text <- gsub("NA", "", repubcongress$text)
#conservativemedia_corpus = corpus(repubcongress, text_field = 'text')
#toks <- tokens(conservativemedia_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
#conservativecongress_dict = dfm(toks, dictionary = group2)
#conservativecongress_dict_df <- quanteda::convert(conservativecongress_dict, to='data.frame')
#conservativecongressFB = cbind(conservativecongress_dict_df, repubcongress)
#conservativecongressFB$doc_id <- NULL

### Replication code

# Load data
conservativecongressplus <- readRDS("ConservativeCongressFacebookUpdated copy.rds")

# Filter dataset columns for replication
conservativecongress_rep = conservativecongressplus[,c("document", "User Name", "text", "Type", "Likes","Love", "Angry", "Wow", "Haha", "Sad", "Comments", "Shares", "Likes at Posting", "Page Name")]

# Create corpus
replication_corpus = corpus(conservativecongress_rep, text_field = 'text')

# Tokenize corpus
rep_toks <- tokens(replication_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, verbose = TRUE)
#rep_dict = dfm(rep_toks, dictionary = group2) #got an error that the dictionary argument of dfm() was deprecated/now defunct, need to use dfm_lookup() instead

# Change we had to make because of deprecated functionality in dfm(), added tokens_lookup()
rep_dict = dfm(tokens_lookup(rep_toks, dictionary = group2, valuetype = "glob", verbose = TRUE))

# Convert results to dataframe
rep_dict_df <- quanteda::convert(rep_dict, to='data.frame')

# Binding results back to the filtered dataset for replication
rep_conservativecongressFB = cbind(rep_dict_df, conservativecongress_rep)
rep_conservativecongressFB$doc_id <- NULL

# Look at if results match from dictionary mapping

# All results match 99.9%
# When corpus is based just on message, there is a match of more than 90%
prop.table(table(rep_conservativecongressFB$conservativeidentity == conservativecongressplus$conservativeidentity))
prop.table(table(rep_conservativecongressFB$liberalidentity == conservativecongressplus$liberalidentity))
prop.table(table(rep_conservativecongressFB$topdemocrat == conservativecongressplus$TopDemocrat))
prop.table(table(rep_conservativecongressFB$toprepublican == conservativecongressplus$TopRepublican))
prop.table(table(rep_conservativecongressFB$democratcongress == conservativecongressplus$DemocratCongress))
prop.table(table(rep_conservativecongressFB$republicancongress == conservativecongressplus$RepublicanCongress))
prop.table(table(rep_conservativecongressFB$democrat == conservativecongressplus$Democrat))
prop.table(table(rep_conservativecongressFB$republican == conservativecongressplus$Republican))
prop.table(table(rep_conservativecongressFB$democrat2 == conservativecongressplus$Democrat2))
prop.table(table(rep_conservativecongressFB$republican2 == conservativecongressplus$Republican2))
prop.table(table(rep_conservativecongressFB$moralemotional == conservativecongressplus$MoralEmotional))

# Looking at where results don't match and it equals null
rep_conservativecongressFB$Message[rep_conservativecongressFB$liberalidentity != conservativecongressplus$liberalidentity]
rep_conservativecongressFB$Message[rep_conservativecongressFB$republican != conservativecongressplus$Republican]
rep_conservativecongressFB$Message[rep_conservativecongressFB$republican2 != conservativecongressplus$Republican2]
rep_conservativecongressFB$Message[rep_conservativecongressFB$democrat != conservativecongressplus$Democrat]
rep_conservativecongressFB$Message[rep_conservativecongressFB$democrat2 != conservativecongressplus$Democrat2]


#### Logistic regression models ####


## Original code

conservativecongressplus$has_URL <- ifelse(conservativecongressplus$Type == "Link", TRUE, FALSE)
conservativecongressplus$has_media <- ifelse(conservativecongressplus$Type != "Link" & conservativecongressplus$Type != "Status", TRUE, FALSE)
conservativecongressplus$shares_log <- log(conservativecongressplus$Shares + 1)
# Analysis 
linearModCongressRepub <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
linearModCongressRepubsumm <- summ(linearModCongressRepub, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubsumm


#### Code with replicated dataset; some columns removed 

rep_conservativecongressFB$has_URL <- ifelse(rep_conservativecongressFB$Type == "Link", TRUE, FALSE)
rep_conservativecongressFB$has_media <- ifelse(rep_conservativecongressFB$Type != "Link" & rep_conservativecongressFB$Type != "Status", TRUE, FALSE)
rep_conservativecongressFB$shares_log <- log(rep_conservativecongressFB$Shares + 1)
#Analysis 
rep_linearModCongressRepub <- glm(shares_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
rep_linearModCongressRepubsumm <- summ(rep_linearModCongressRepub, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
rep_linearModCongressRepubsumm #roughly same estimates 1.72

#### Regression without control variables ####

## Original code: 

#Add Variables
linearModCongressRepub_noc <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional, data = conservativecongressplus)
linearModCongressRepubNC_nocsumm <- summ(linearModCongressRepub_noc, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubNC_nocsumm

## Replicated code

linearModCongressRepub_noc_rep <- glm(shares_log ~ democrat + republican + moralemotional, data = rep_conservativecongressFB)
linearModCongressRepubNC_noc_repsumm <- summ(linearModCongressRepub_noc_rep, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubNC_noc_repsumm

# Still similar without control variables and without positive/negative affect

# Used chatgpt to help with naming so that the coefficients fit on one line:
# Prompt: how can i change all of the coefficients in the model so that the names are lowercase

# Function to rename coefficients to lowercase in a model
rename_coefficients_lowercase <- function(model) {
  names(model$coefficients) <- tolower(names(model$coefficients))
  return(model)
}

# Apply renaming function to each model
linearModCongressRepub <- rename_coefficients_lowercase(linearModCongressRepub)
rep_linearModCongressRepub <- rename_coefficients_lowercase(rep_linearModCongressRepub)
linearModCongressRepub_noc <- rename_coefficients_lowercase(linearModCongressRepub_noc)
linearModCongressRepub_noc_rep <- rename_coefficients_lowercase(linearModCongressRepub_noc_rep)

library(stargazer)

# Formatting regression results so they appear in same table
stargazer(linearModCongressRepub,
          rep_linearModCongressRepub,
          type = "text", 
          column.labels = c("Orig. Model", "Rep. Model"),
          dep.var.labels.include = FALSE,  
          star.cutoffs = c(0.05, 0.01, 0.001),  # Significance stars
          ci = TRUE,  
          digits = 2)  

stargazer(linearModCongressRepub_noc,
          linearModCongressRepub_noc_rep,
          type = "latex", 
          title = "Logistic Regression Model Results (No Control Variables): Original vs. Replicated Data",
          column.labels = c("Orig. Model", "Rep. Model"),
          dep.var.labels.include = FALSE,  
          star.cutoffs = c(0.05, 0.01, 0.001),  
          ci = TRUE,  
          digits = 2)  

#### 16 Reactions Regression Plots #### 

## Conservative Congress

# Original code
# Log variables
conservativecongressplus$shares_log <- log(conservativecongressplus$Shares + 1)
conservativecongressplus$comments_log <- log(conservativecongressplus$Comments + 1)
conservativecongressplus$likes_log <- log(conservativecongressplus$Likes + 1)
conservativecongressplus$love_log <- log(conservativecongressplus$Love + 1)
conservativecongressplus$wow_log <- log(conservativecongressplus$Wow + 1)
conservativecongressplus$haha_log <- log(conservativecongressplus$Haha + 1)
conservativecongressplus$sad_log <- log(conservativecongressplus$Sad + 1)
conservativecongressplus$angry_log <- log(conservativecongressplus$Angry + 1)

# Models
# Commenting out any code that isn't relevant for our analysis
shares <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
comments <- glm(comments_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
likes <- glm(likes_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
love <- glm(love_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
wow <- glm(wow_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
haha <- glm(haha_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
sad <- glm(sad_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
angry <- glm(angry_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = conservativecongressplus)
#retweet <- glm(retweet_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + is_retweet + followers_count, data = congressRepubTwitter)
#favorite <- glm(favorites_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + is_retweet + followers_count, data = congressRepubTwitter)
sharessumm <- summ(shares, exp = TRUE, center = TRUE, vifs = TRUE)
commentssumm <- summ(comments, exp = TRUE, center = TRUE, vifs = TRUE)
likessumm <- summ(likes, exp = TRUE, center = TRUE, vifs = TRUE)
lovesumm <- summ(love, exp = TRUE, center = TRUE, vifs = TRUE)
wowsumm <- summ(wow, exp = TRUE, center = TRUE, vifs = TRUE)
hahasumm <- summ(haha, exp = TRUE, center = TRUE, vifs = TRUE)
angrysumm <- summ(angry, exp = TRUE, center = TRUE, vifs = TRUE)
sadsumm <- summ(sad, exp = TRUE, center = TRUE, vifs = TRUE)
#retweetsumm <- summ(retweet, exp = TRUE, center = TRUE, vifs = TRUE)
#favoritesumm <- summ(favorite, exp = TRUE, center = TRUE, vifs = TRUE)
shares <- tidy(sharessumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Shares")
comments <- tidy(commentssumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Comments")
likes <- tidy(likessumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Likes")
love <- tidy(lovesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Love")
wow <- tidy(wowsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Wow")
haha <- tidy(hahasumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Haha")
angry <- tidy(angrysumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Angry")
sad <- tidy(sadsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Sad")
#retweets <- tidy(retweetsumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Retweet")
#favorites <- tidy(favoritesumm)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE",term != "followers_count", term != "is_retweetTRUE", term != "NegativeAffect", term != "MoralEmotional", term != "PositiveAffect") %>% mutate(model = "Favorite")
concongressmodels <- rbind(shares, comments, likes, love, haha, wow, sad, angry)
concongressmodels$termtemp <- concongressmodels$term
concongressmodels$modeltemp <- concongressmodels$model
concongressmodels$term <- concongressmodels$modeltemp
concongressmodels$model <- concongressmodels$termtemp
concongressmodels$datatype <- "Original"
#conmodels[c(1, 7, 13, 19, 25, 31, 37), 1] <- "(Intercept)"
concongressmodels[c(1, 4, 7, 10, 13, 16, 19, 22), 1] <- "(Intercept)"

# Plotting logistic regression dependent variable results
concongressplotFB <- {dwplot(concongressmodels, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "bottom") + 
    xlab("Change in odds of engagement") +
    theme(legend.position = "none") +
    scale_colour_hue(h = c(0, 260)) + 
    xlim(0.7, 3.4) +
    ggtitle("Conservative Congress - Original Data")} 
#%>% add_brackets(two_brackets)
concongressplotFB


ggsave("conservativecongressreactionplot.png", width = 6, height = 4)

# Replicated code

# Log
rep_conservativecongressFB$shares_log <- log(rep_conservativecongressFB$Shares + 1)
rep_conservativecongressFB$comments_log <- log(rep_conservativecongressFB$Comments + 1)
rep_conservativecongressFB$likes_log <- log(rep_conservativecongressFB$Likes + 1)
rep_conservativecongressFB$love_log <- log(rep_conservativecongressFB$Love + 1)
rep_conservativecongressFB$wow_log <- log(rep_conservativecongressFB$Wow + 1)
rep_conservativecongressFB$haha_log <- log(rep_conservativecongressFB$Haha + 1)
rep_conservativecongressFB$sad_log <- log(rep_conservativecongressFB$Sad + 1)
rep_conservativecongressFB$angry_log <- log(rep_conservativecongressFB$Angry + 1)

# Models
shares_rep <- glm(shares_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
comments_rep <- glm(comments_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
likes_rep <- glm(likes_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
love_rep <- glm(love_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
wow_rep <- glm(wow_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
haha_rep <- glm(haha_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
sad_rep <- glm(sad_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)
angry_rep <- glm(angry_log ~ democrat + republican + moralemotional + has_URL + has_media + `Likes at Posting`, data = rep_conservativecongressFB)

sharessumm_rep <- summ(shares_rep, exp = TRUE, center = TRUE, vifs = TRUE)
commentssumm_rep <- summ(comments_rep, exp = TRUE, center = TRUE, vifs = TRUE)
likessumm_rep <- summ(likes_rep, exp = TRUE, center = TRUE, vifs = TRUE)
lovesumm_rep <- summ(love_rep, exp = TRUE, center = TRUE, vifs = TRUE)
wowsumm_rep <- summ(wow_rep, exp = TRUE, center = TRUE, vifs = TRUE)
hahasumm_rep <- summ(haha_rep, exp = TRUE, center = TRUE, vifs = TRUE)
angrysumm_rep <- summ(angry_rep, exp = TRUE, center = TRUE, vifs = TRUE)
sadsumm_rep <- summ(sad_rep, exp = TRUE, center = TRUE, vifs = TRUE)

shares_rep <- tidy(sharessumm_rep) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Shares")
comments_rep <- tidy(commentssumm_rep) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Comments")
likes_rep <- tidy(likessumm_rep)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Likes")
love_rep <- tidy(lovesumm_rep)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Love")
wow_rep <- tidy(wowsumm_rep)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Wow")
haha_rep <- tidy(hahasumm_rep)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Haha")
angry_rep <- tidy(angrysumm_rep) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Angry")
sad_rep <- tidy(sadsumm_rep)  %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`", term != "moralemotional") %>% mutate(model = "Sad")

concongressmodels_rep <- rbind(shares_rep, comments_rep, likes_rep, love_rep, haha_rep, wow_rep, sad_rep, angry_rep)
concongressmodels_rep$termtemp <- concongressmodels_rep$term
concongressmodels_rep$modeltemp <- concongressmodels_rep$model
concongressmodels_rep$term <- concongressmodels_rep$modeltemp
concongressmodels_rep$model <- concongressmodels_rep$termtemp
concongressmodels_rep$datatype <- "Replicated"

concongressmodels_rep[c(1, 4, 7, 10, 13, 16, 19, 22), 1] <- "(Intercept)"

# Plotting the results based on the replicated data
concongressplotFB_rep <- {dwplot(concongressmodels_rep, confint = .99, dot_args = list(size = 1.2), whisker_args = list(size = 0.9)) + 
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2) + 
    theme_apa(legend.pos = "bottom") + 
    xlab("Change in odds of engagement") +
    theme(legend.position = "none") +
    scale_colour_hue(h = c(0, 260)) + 
    xlim(0.7, 3.4) +
    ggtitle("Conservative Congress - Replicated Data")} 
#%>% add_brackets(two_brackets)
concongressplotFB_rep

ggsave("conservativecongressreactionplot_replicated.png", width = 6, height = 4)

# For comparison purposes, saving the results in a table and transforming into latex tables

# Original data
concongressmodels_demonly = concongressmodels %>% filter(model == "Democrat") %>% dplyr::select(term, estimate, std.error, statistic)

stargazer(concongressmodels_demonly, type = "latex", summary = FALSE,
          title = "Facebook Reaction Logistic Regression Results: Original Data",
          rownames = FALSE)

# Replicated data
concongressmodels_rep_demonly = concongressmodels_rep %>% filter(model == "democrat") %>% dplyr::select(term, estimate, std.error, statistic)

stargazer(concongressmodels_demonly, type = "latex", summary = FALSE,
          title = "Facebook Reaction Logistic Regression Results: Replicated Data",
          rownames = FALSE)

#### Odds Ratio of Sharing Plots - Conservative Congress FB Only ####

# Clean up model for plotting
FBConCongress <- tidy(linearModCongressRepubsumm) %>% filter(term != "has_mediaTRUE", term != "has_URLTRUE", term != "`Likes at Posting`")  %>% mutate(model = "Facebook")

# Updated to only include Facebook data - no Twitter since we do not have this data
concongressmodels <- FBConCongress

## Conservative Plots
plot <- dwplot(concongressmodels, conf.level = .95, dot_args = list(size = 3),
               whisker_args = list(size = 1.8)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(Democrat = "Liberal (Outgroup)", Republican = "Conservative (Ingroup)", NegativeAffect = "Negative Affect", PositiveAffect = "Positive Affect", MoralEmotional = "Moral Emotional")) 

two_brackets <- list(c("Identity", "Liberal (Outgroup)", "Conservative (Ingroup)"), 
                     c("Emotion", "Negative Affect", "Moral Emotional"))

conservativecongressplot <- {(
  plot + theme_bw() + xlab("Change in odds of share") + ylab("") +
    ggtitle("Conservative Congress") +
    xlim(0.7, 2.9) +
    geom_vline(xintercept = 1, colour = "grey60", linetype = 2, size = .7) +
    theme_apa() +
    theme(legend.position = "none") +
    #theme(legend.font.size = 12,
    #          legend.pos = "bottom") + 
    scale_colour_hue(h = c(260, 170)) 
)} %>% 
  add_brackets(two_brackets)

conservativecongressplot

ggsave("conservativecongress_odds_ratio_plot.png", height = 4, width = 6)


#### Extension - VADER ####

# Load built in VADER dictionary
library(vader)

# Sample 1000 random rows from the dataset since VADER takes too long to run on entire dataset
sampled_data <- conservativecongressplus %>% sample_n(1000)

# Create empty positive and negative sentiment lists
pos_score_list = c()
neg_score_list = c()

# Use get_vader to get positive and negative sentiment scores for each text
for (item in sampled_data$text){
  sentiment_results <- as.data.frame(get_vader(item))
  pos_score <- as.numeric(sentiment_results[3, 1])
  pos_score_list <- c(pos_score_list, pos_score)
  neg_score <- as.numeric(sentiment_results[5, 1])
  neg_score_list <- c(neg_score_list, neg_score)
}

# Add scores as columns to sampled data set
sampled_data$vader_pos_score = pos_score_list
sampled_data$vader_neg_score = neg_score_list

#Sampled Analysis with VADER
linearModCongressRepub_vader <- glm(shares_log ~ Democrat + Republican + vader_neg_score + vader_pos_score + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = sampled_data)
linearModCongressRepubsumm_vader <- summ(linearModCongressRepub_vader, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubsumm_vader

#Sampled Analysis without VADER, with LIWC
linearModCongressRepub_sampled <- glm(shares_log ~ Democrat + Republican + NegativeAffect + PositiveAffect + MoralEmotional + has_URL + has_media + `Likes at Posting`, data = sampled_data)
linearModCongressRepubsumm_sampled <- summ(linearModCongressRepub_sampled, exp = TRUE, confint = TRUE, center = TRUE, vifs = TRUE)
linearModCongressRepubsumm_sampled

# Export model summaries
stargazer(linearModCongressRepub_vader,
          linearModCongressRepub_sampled,
          title = "Table 4: VADER and LIWC Comparison with Sampled Data",
          column.labels = c('VADER Model', 'LIWC Model'),
          type = "html", out = "VADER_comparison.html")


