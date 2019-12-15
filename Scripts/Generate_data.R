#Preprocessing for all the files

#run on local or O2 mode
machine = "local"
args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0)
{
  machine = args[1]
}
ifelse(machine == "O2", path <- "/home/al311/Aging/", path <- "/Users/Alan/Desktop/Aging/")
source(file = paste(path, "Helpers.R", sep = ""))

#generate files
load(file='/Users/Alan/Desktop/Aging/nhanes_schema_merged_093016.Rdata')

#tabDesc_clean
tabDesc_clean <- tabDesc[!duplicated(tabDesc$var),]
tabDesc_clean$var[which(tabDesc_clean$var == "BPXDI1")] <- "BPXDI"
tabDesc_clean$var_desc[which(tabDesc_clean$var == "BPXDI")] <- "Diastolic: Blood pres mm Hg"
tabDesc_clean$var[which(tabDesc_clean$var == "BPXSY1")] <- "BPXSY"
tabDesc_clean$var_desc[which(tabDesc_clean$var == "BPXSY")] <- "Systolic: Blood pres mm Hg"
savRDS(tabDesc_clean, "tabDesc_clean")

#cardiac SEQN
SEQN_cardiac <- bigData$SEQN[which(bigData$UCOD %in% c("001", ""))]
savRDS(SEQN_cardiac, "SEQN_cardiac")
#cancer SEQN
SEQN_cancer <- bigData$SEQN[which(bigData$UCOD %in% c("002", ""))]
savRDS(SEQN_cancer, "SEQN_cancer")
#no trauma SEQN
SEQN_notrauma <- bigData$SEQN[which(bigData$UCOD != "004")]
savRDS(SEQN_notrauma, "SEQN_notrauma")


#demographics_var
demographics_var <- bigData %>% select(SEQN, RIAGENDR, RIDRETH1)
dummy_sex <- data.frame(dummy(demographics_var$RIAGENDR))
names(dummy_sex) <- c("female", "male")
dummy_sex <- dummy_sex[,c(2,1)]
dummy_sex <- data.frame(as.matrix(dummy_sex[,-which(names(dummy_sex) %in% c("male"))]))
names(dummy_sex) <- c("female")
dummy_sex_factor <- dummy_sex
dummy_sex_factor[] <- lapply(dummy_sex_factor, factor)
#ethnicity (delete white, use it as baseline)
demographics_var$RIDRETH1[which(demographics_var$RIDRETH1 %in% c(1,2))] <- "hisp"
demographics_var$RIDRETH1[which(demographics_var$RIDRETH1 == 3)] <- "white"
demographics_var$RIDRETH1[which(demographics_var$RIDRETH1 == 4)] <- "black"
demographics_var$RIDRETH1[which(demographics_var$RIDRETH1 == 5)] <- "other"
dummy_ethnicity <- data.frame(dummy(demographics_var$RIDRETH1))
names(dummy_ethnicity) <- c("black", "hisp", "other", "white")
dummy_ethnicity <- dummy_ethnicity[,c(4,2,1,3)]
dummy_ethnicity <- dummy_ethnicity[,-which(names(dummy_ethnicity) %in% c("white"))]
dummy_ethnicity_factor <- dummy_ethnicity
dummy_ethnicity_factor[] <- lapply(dummy_ethnicity_factor, factor)
#create sex and ethnicity interaction terms
interactions_sex_ethnicity <- data.frame(matrix(nrow = dim(demographics_var)[1], ncol = 1*3))
k = 0
for (i in 1:dim(dummy_sex)[2])
{
  for (j in 1:dim(dummy_ethnicity)[2])
  {
    k = k + 1
    name <- paste(names(dummy_sex)[i], names(dummy_ethnicity)[j], sep = ":")
    interactions_sex_ethnicity[,k] <- dummy_sex[,i]*dummy_ethnicity[,j]
    names(interactions_sex_ethnicity)[k] <- name
  }
}
demographics_var <- cbind(demographics_var[,which(names(demographics_var) %in% c("SEQN"))], dummy_sex_factor, dummy_ethnicity_factor, interactions_sex_ethnicity)
names(demographics_var)[1] <- "SEQN"
demographics_var <- demographics_var[complete.cases(demographics_var),]
savRDS(demographics_var, "demographics_var")

#other_var
other_var <- bigData %>% select(SEQN, TELOMEAN, CVDESVO2, PERMTH_EXM, MORTSTAT)
other_var <- other_var[!duplicated(other_var$SEQN),]
savRDS(other_var, "other_var")

#aging_var
aging_var <- other_var %>% select(SEQN, TELOMEAN, CVDESVO2)
aging_var$TELOMEAN <- as.vector(scale(as.numeric(aging_var$TELOMEAN)))
aging_var$CVDESVO2 <- as.vector(scale(aging_var$CVDESVO2))
savRDS(aging_var, "aging_var")

#survival_var
#get rid of deaths by trauma
survival_var <- bigData[-which(bigData$UCOD == "004"),] %>% select(SEQN, PERMTH_EXM, MORTSTAT)
survival_var <- survival_var[!duplicated(survival_var$SEQN),]
survival_var <- survival_var[complete.cases(survival_var),]
#I had to shift everything by one month to avoid non positive times for events
survival_var$PERMTH_EXM <- survival_var$PERMTH_EXM + 1
savRDS(survival_var, "survival_var")

#data_weights
bigData$weights <- ifelse(bigData$SDDSRVYR==1 | bigData$SDDSRVYR==2,(2/8)*bigData$WTMEC2YR, (1/8)*bigData$WTMEC2YR)
data_weights <- bigData %>% select(SEQN, weights)
data_weights <- data_weights[complete.cases(data_weights),]
data_weights <- data_weights[!duplicated(data_weights$SEQN),]
data_weights$weights <- as.numeric(as.character(data_weights$weights))
savRDS(data_weights, "data_weights")


#prepare data to merge BAs, and to correct for age and demographics (sex, ethnicity)
load(file='/Users/Alan/Desktop/Aging/nhanes_schema_merged_093016.Rdata')
bigData$weights <- ifelse(bigData$SDDSRVYR==1 | bigData$SDDSRVYR==2,(2/8)*bigData$WTMEC2YR, (1/8)*bigData$WTMEC2YR)
BA_comp_correctors <- bigData %>% select(SEQN, weights, RIDAGEYR, RIAGENDR, RIDRETH1)
BA_comp_correctors$RIDAGEYR <- (BA_comp_correctors$RIDAGEYR - mean(BA_comp_correctors$RIDAGEYR))/sd(BA_comp_correctors$RIDAGEYR)
#replace numbers by labels for sex and ethnicity and create dummy variables
#sex (delete male, use it as baseline)
BA_comp_correctors$RIAGENDR[which(BA_comp_correctors$RIAGENDR == 1)] <- "male"
BA_comp_correctors$RIAGENDR[which(BA_comp_correctors$RIAGENDR == 2)] <- "female"
dummy_sex <- data.frame(dummy(BA_comp_correctors$RIAGENDR))
names(dummy_sex) <- c("female", "male")
dummy_sex <- dummy_sex[,c(2,1)]
dummy_sex <- data.frame(as.matrix(dummy_sex[,-which(names(dummy_sex) %in% c("male"))]))
names(dummy_sex) <- c("female")
dummy_sex_factor <- dummy_sex
dummy_sex_factor[] <- lapply(dummy_sex_factor, factor)
#ethnicity (delete white, use it as baseline)
BA_comp_correctors$RIDRETH1[which(BA_comp_correctors$RIDRETH1 %in% c(1,2))] <- "hisp"
BA_comp_correctors$RIDRETH1[which(BA_comp_correctors$RIDRETH1 == 3)] <- "white"
BA_comp_correctors$RIDRETH1[which(BA_comp_correctors$RIDRETH1 == 4)] <- "black"
BA_comp_correctors$RIDRETH1[which(BA_comp_correctors$RIDRETH1 == 5)] <- "other"
dummy_ethnicity <- data.frame(dummy(BA_comp_correctors$RIDRETH1))
names(dummy_ethnicity) <- c("black", "hisp", "other", "white")
dummy_ethnicity <- dummy_ethnicity[,c(4,2,1,3)]
dummy_ethnicity <- dummy_ethnicity[,-which(names(dummy_ethnicity) %in% c("white"))]
dummy_ethnicity_factor <- dummy_ethnicity
dummy_ethnicity_factor[] <- lapply(dummy_ethnicity_factor, factor)
#create sex and ethnicity interaction terms
interactions_sex_ethnicity <- data.frame(matrix(nrow = dim(BA_comp_correctors)[1], ncol = 1*3))
k = 0
for (i in 1:dim(dummy_sex)[2])
{
  for (j in 1:dim(dummy_ethnicity)[2])
  {
    k = k + 1
    name <- paste(names(dummy_sex)[i], names(dummy_ethnicity)[j], sep = ":")
    interactions_sex_ethnicity[,k] <- dummy_sex[,i]*dummy_ethnicity[,j]
    names(interactions_sex_ethnicity)[k] <- name
  }
}
#interactions_sex_ethnicity$SEQN <- BA_comp_correctors$SEQN
BA_comp_correctors <- cbind(BA_comp_correctors[,which(names(BA_comp_correctors) %in% c("SEQN", "weights", "RIDAGEYR"))], dummy_sex_factor, dummy_ethnicity_factor, interactions_sex_ethnicity)
BA_comp_correctors <- BA_comp_correctors[complete.cases(BA_comp_correctors),]
savRDS(BA_comp_correctors, "BA_comp_correctors")


#format data
bigData <- readRDS(file= paste(path, "bigData.rds", sep = ""))
bigData <- bigData[!duplicated(bigData$SEQN),]
bigData <- merge(bigData, data_weights, by = "SEQN")
bigData$SEQN <- factor(bigData$SEQN)
#get rid of SDDSRVYR
bigData <- bigData[,-which(names(bigData) %in% c("SDDSRVYR"))]
#fix: get rid of redundant variables: LBXSUA, LBXSGB (why??? gives me NA coef)
bigData <- bigData[,-which(names(bigData) %in% c("LBXSUA", "LBXSGB"))]
savRDS(bigData, "bigData")
#generate clustered list of biomarkers
biomarkers <- names(bigData)[-which(names(bigData) %in% c("SEQN", "RIDAGEYR", "RIDRETH1", "RIAGENDR", "weights"))]
savRDS(biomarkers, "biomarkers")

#generate index_trainVStest
index_trainVStest <- as.vector(createDataPartition(bigData$RIDAGEYR, p = .5, list = FALSE))
index_train <- bigData$SEQN[index_trainVStest]
index_test <- bigData$SEQN[-index_trainVStest]

#generate index_1vs2
index_1vs2 <- sample.split(bigData$RIDAGEYR, SplitRatio=0.5)
index_1vs2 <- bigData$SEQN[index_1vs2]

#save indices
for (half in halves)
{
  savRDS(get(file_name("index", half)), "index", half)
}
savRDS(index_1vs2, "index", "1vs2")

#load indices
for (half in halves)
{
  assign(file_name("index", half), redRDS("index", half))
}
index_1vs2 <- redRDS("index", "1vs2")

#load files
#tabDesc_clean
tabDesc_clean <- redRDS("tabDesc_clean")
#demographics_var
demographics_var <- redRDS("demographics_var")
#other_var
other_var <- redRDS("other_var")
#aging_var
aging_var <- redRDS("aging_var")
#survival_var
survival_var <- redRDS("survival_var")
#data_weights
data_weights <- redRDS("data_weights")
#BA_comp_correctors
BA_comp_correctors <- redRDS("BA_comp_correctors")

path <- "/Users/Alan/Desktop/Aging/"
source(file = paste(path, "Helpers.R", sep = ""))

#generate all data
for (side in sides)
{
  for (age_group in age_groups)
  {
    generate_data_age(age_group, side, index_train, index_test, index_1vs2)
    generate_data_surv(age_group, side, index_train, index_test, index_1vs2)
    generate_data_surv(age_group, side, index_train, index_test, index_1vs2, subset = "cancer")
    generate_data_surv(age_group, side, index_train, index_test, index_1vs2, subset = "cardiac")
  }
}


