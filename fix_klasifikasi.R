library(plyr)
library(foreign)

##################
# desa atau kota #
##################

asal <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\bk_sc1.dta")
head(asal,3)
dk <- asal[,c("sc05","hhid14")]
dk$sc05 <- revalue(dk$sc05, c("1:Urban" = "Perkotaan", "2:Rural" = "Pedesaan"))
colnames(dk) <- c('asal','hhid14')
str(dk)
head(dk,2)

##########################
# Umur dan Jenis Kelamin #
##########################

umur_jk <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3a_cov.dta")
head(umur_jk,3)
umurjk <- umur_jk[, c("age", "sex","pidlink", "hhid14")]
umurjk$sex <- revalue(umurjk$sex, c("1:Male" = "Laki-laki", "3:Female" = "Perempuan"))
colnames(umurjk) <- c('umur', 'gender', 'pidlink', 'hhid14')
head(umurjk,3)
str(umurjk)

################################################################
# Hipertensi, Kolesterol Tinggi, dan Stroke #
################################################################

peny <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_cd3.dta")
head(peny,3)
penyakit <- peny[peny$cdtype == "A" 
                 | peny$cdtype == "M"| peny$cdtype == "H",]
head(penyakit,3)
unique(penyakit$cdtype)
penyakit <- penyakit[, c("cdtype","cd05","hhid14", "pidlink")]
levels(penyakit$cd05)
penyakit$cd05 <- revalue(penyakit$cd05, c("1:Yes" = "Ya", "3:No" = "Tidak"))
penyakit <- penyakit[penyakit$cd05 == "Ya" | penyakit$cd05 == "Tidak",]
penyakit$cd05 <- droplevels(penyakit$cd05)
head(penyakit)
penyakit$cdtype <- revalue(penyakit$cdtype, c("A" = "Hipertensi", "M" = "Kolesterol Tinggi",
                                              "H" = "Stroke"))
head(penyakit)
colnames(penyakit) <- c('jenis', 'status', 'hhid14', 'pidlink')
head(penyakit)
penyakit$jenis <- as.factor(penyakit$jenis)
str(penyakit)

##############
# Pendidikan #
##############

pendidikan <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3a_dl1.dta")
head(pendidikan,3)
edu <- pendidikan[,c("dl06","hhid14","pidlink")]
str(edu)
levels(edu$dl06)
edu$dl06 <- revalue(edu$dl06, c("2:Elementary school" = "basic", 
                                "3:Junior high general" = "basic", 
                                "4:Junior high vocational" = "basic", 
                                "5:Senior high general" = "secondary", 
                                "6:Senior high vocational" = "secondary", 
                                "11:Adult education A" = "basic", 
                                "12:Adult education B" = "basic", 
                                "13:Open university" = "higher", 
                                "15:Adult education C" = "secondary", 
                                "60:College (D1,D2,D3)" = "higher", 
                                "61:University S1" = "higher", 
                                "62:University S2" = "higher", 
                                "63:University S3" = "higher", 
                                "72:Islamic Elementary School (Madrasah Ibtidaiyah)" = "basic", 
                                "73:Islamic Junior/High School (Madrasah Tsanawiyah)" = "basic", 
                                "74:Islamic Senior/High School (Madrasah Tsanawiyah)" = "secondary"))
levels(edu$dl06)
edu <- edu[edu$dl06 == "basic" | edu$dl06 == "secondary" | edu$dl06 == "higher",]
levels(edu$dl06)
edu$dl06 <- droplevels(edu$dl06)
str(edu)
colnames(edu) <- c('pendidikan', 'hhid14', 'pidlink')
head(edu)

###########
# Merokok #
###########

smoke <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_km.dta")
head(smoke)
smoke <- smoke[smoke$km01a == "1:Yes" | smoke$km01a == "3:No",c("km01a", "hhid14", "pidlink")]
str(smoke)
smoke$km01a <- droplevels(smoke$km01a)
smoke$km01a <- revalue(smoke$km01a, c("1:Yes" = "Ya", "3:No" = "Tidak"))
colnames(smoke) <- c('merokok', 'hhid14', 'pidlink')
head(smoke)
str(smoke)

###################
# Aktivitas fisik #
###################

fisik <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_kk2.dta")
fisik <- fisik[fisik$kk02m == "1:Yes", c("kktype","hhid14", "pidlink")]
str(fisik)
fisik$kktype <- revalue(fisik$kktype, c("A" = "Berat", "B" = "Sedang", "C" = "Ringan"))
fisik$kktype <- as.factor(fisik$kktype)
str(fisik)
colnames(fisik) <- c('aktivitas', 'hhid14', 'pidlink')
head(fisik)

############
# Obesitas #
############

obe <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\bus_us.dta")
obe <- obe[,c("hhid14", "pidlink","us06", "us04")]
colnames(obe) <- c('hhid14', 'pidlink', 'bb', 'tb')
head(obe)
obe <- na.omit(obe)
sapply(obe, function(x) sum(is.na(x)))
str(obe)
obe$bmi <- obe$bb/(obe$tb/100)^2
head(obe,3)
obe$obesitas <- ifelse(obe$bmi > 25, "Ya", "Tidak")
obe <- obe[,c(1,2,6)]
obe$obesitas <- as.factor(obe$obesitas)
str(obe)

##############
# merge data #
##############

library(tidyverse)
dat <- penyakit %>%
  left_join(y=dk, by=c("hhid14")) %>%
  left_join(y=umurjk, by=c("hhid14","pidlink")) %>%
  left_join(y= edu, by=c("hhid14","pidlink")) %>%
  left_join(y=smoke, by=c("hhid14","pidlink")) %>%
  left_join(y=fisik, by=c("hhid14","pidlink")) %>%
  left_join(y=obe, by=c("hhid14","pidlink"))
dat <- dat %>% spread(jenis, status)
sapply(dat, function(x) sum(is.na(x)))
dat <- na.omit(dat)
head(dat)
nrow(dat)
length(unique(dat$pidlink))
dat <- dat %>% group_by(pidlink) %>% sample_n(1)
class(dat)
fix <- as.data.frame(dat)
head(fix,3)
fix <- fix[,-c(1,2)]
head(fix)
str(fix)
write.csv2(fix, file = "D:\\Adi\\Orbit Paper\\klasifikasi\\dat_stroke.csv")
dat <- read.csv2("D:\\Adi\\Orbit Paper\\klasifikasi\\dat_stroke.csv")
dat <- dat[,-1]
str(dat)
head(dat,2)
table(dat$Stroke)
prop.table(table(dat$Stroke))
#mengurutkan level faktor
levels(dat$pendidikan)
dat$pendidikan <- factor(dat$pendidikan, c("basic","secondary","higher"))
levels(dat$aktivitas)
dat$aktivitas <- factor(dat$aktivitas, c("Ringan", "Sedang", "Berat"))
str(dat)
#variabel Asal
#Pedesaan: 1 diganti 0, perkotaan: 2 diganti 1
#Gender
#Laki-laki: 1 diganti 0, perempuan: 2 diganti 1
#pendidikan
#basic: 1 diganti 0, secondary: 2 diganti 1, higher: 3 diganti 2
#merokok, obesitas, hipertensi, kolesterol tinggi, dan stroke
#Tidak: 1 diganti 0, Ya 2 diganti 1
#aktivitas
#ringan: 1 diganti 0, sedang: 2 diganti 1, berat: 3 diganti 2
dat[,-2] <- lapply(dat[,-2], function(x) as.numeric(x))
str(dat)
kurang1 <- function(x){
  a <- vector()
  for(i in 1:length(x)){
    a[i] = x[i]-1
  }
  return(a)
}
dat[,-2] <- lapply(dat[,-2], function(x) kurang1(x))
str(dat)
### Kasus -> Imbalanced Data

## split data 80% untuk train dan 20% untuk test
set.seed(2021)
size <- floor(0.8 * nrow(dat))
train_ind <- sample(seq_len(nrow(dat)), size = size)
train <- dat[train_ind, ]
test <- dat[-train_ind, ]
table(train$Stroke)
table(test$Stroke)
str(test)
test[,-2] <- lapply(test[,-2], function(x) as.factor(x))
str(test)
head(test)
# cek run regresi logistik
str(train)
model.rl.langsung <- glm(Stroke ~ as.factor(asal)+umur+as.factor(gender)+
               as.factor(pendidikan)+as.factor(merokok)+
               as.factor(aktivitas)+as.factor(obesitas)+
               as.factor(Hipertensi)+as.factor(Kolesterol.Tinggi),data = train, family = binomial(link="logit"))
pred <- predict(model.rl.langsung, newdata = test, type = "response")
pred <- as.factor(ifelse(pred > 0.5, 1, 0))
unique(pred)
levels(pred)
levels(pred) <- c(levels(pred), "1")
levels(pred)
levels(test$Stroke)
library(caret) #memanggil fungsi dalam package
#
#Matriks klasifikasi
c_matrix <- confusionMatrix(data = pred,
                            reference = as.factor(test$Stroke))
c_matrix
#
# Akurasi: (TP + TN) / (TP +TN + FP + FN)
#
acc <- c_matrix$overall[1]
acc

# Sensitivitas (Recall): TP / (TP + FN) --> Harus tinggi
rec <- c_matrix$byClass[1]
rec

# Presisi = TP / (TP + FP) --> Seen as Positive Prediction value
prec <- c_matrix$byClass[3]
prec

# F Score = 2 * Recall * Precision / (Recall + Precision) -> Use this to compare
# against models. A high F score is better. Combines both the results of 
# recall and Precision together.
F_score <- 2 * rec * prec / (rec + prec) [1]
names(F_score) <- 'F Score'
F_score
#
#SMOTE
library(smotefamily)
str(train)
dat.smote <- SMOTE(train[,-10],train[,10])$data
prop.table(table(dat.smote$class))
nrow(dat.smote)
library(caret)
level3 <- function(t){
  a <- vector()
  for(i in 1:length(t)){
    if(t[i] < 0.5){
      a[i] = 0
    }
    else if(t[i] >= 0.5 & t[i] < 1.5){
      a[i] = 1
    }
    else a[i] = 2
  }
  return(a)
}
level2 <- function(t){
  a <- vector()
  for(i in 1:length(t)){
    if(t[i] < 0.5){
      a[i] = 0
    }
    else a[i] = 1
  }
  return(a)
}
str(dat.smote)
dat.smote[,-c(1:3,5,7:10)] <- lapply(dat.smote[,-c(1:3,5,7:10)], function(x) as.factor(level3(x)))
str(dat.smote)
dat.smote[,-c(2,4,6,10)] <- lapply(dat.smote[,-c(2,4,6,10)], function(x) as.factor(level2(x)))
str(dat.smote)
model.smote <- glm(as.integer(class) ~.,data = dat.smote, family = binomial(link="logit"))
pred <- predict(model.smote, newdata = test, type = "response")
pred <- as.factor(ifelse(pred > 0.5, 1, 0))
c_matrix_smote <- confusionMatrix(data = pred,
                                  reference = test$Stroke)
c_matrix_smote
F_score_smote <- 2 * c_matrix_smote$byClass[1] * c_matrix_smote$byClass[3] /
  (c_matrix_smote$byClass[1] + c_matrix_smote$byClass[3])
names(F_score_smote) <- 'F_Score_Up'
F_score_smote # Better?

# down sampling techniques (undersampling)
# Like random over sampling, the random oversampling method involves sampling 
# from the overrepresented class and retrieve the same number of observations 
# as seen in the minority class
set.seed(2021)
train_down_sample <- downSample(x = train[,-10], y = as.factor(train[,10]))
str(train_down_sample)
model_down <- glm(Class ~ as.factor(asal)+umur+as.factor(gender)+
                    as.factor(pendidikan)+as.factor(merokok)+
                    as.factor(aktivitas)+as.factor(obesitas)+
                    as.factor(Hipertensi)+as.factor(Kolesterol.Tinggi),data = train_down_sample, family = binomial(link="logit"))
pred <- predict(model_down, newdata = test, type = "response")
pred <- as.factor(ifelse(pred > 0.5, 1, 0))
levels(pred)
c_matrix_down <- confusionMatrix(data = pred,
                                 reference = test$Stroke)
c_matrix_down
F_score_down <- 2 * c_matrix_down$byClass[1] * c_matrix_down$byClass[3] /
  (c_matrix_down$byClass[1] + c_matrix_down$byClass[3])
names(F_score_down) <- 'F_Score_Up'
F_score_down # Better?

# Nearmiss algorithm 
# install.packages("themis")
library(themis)
str(train)
# Need to have a variable called 'class' 
colnames(train)[10] <- 'class'
train$class <- as.factor(train$class)
# Note that the the step_nearmiss is a nearmiss-1 algorithm
set.seed(2021)
train_nearmiss_sample <- recipe(~., train) %>%
  step_nearmiss(class, under_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)
train_nearmiss_sample # a tibble is a new modern dataframe.
model_nearmiss <- glm(class ~as.factor(asal)+umur+as.factor(gender)+
                        as.factor(pendidikan)+as.factor(merokok)+
                        as.factor(aktivitas)+as.factor(obesitas)+
                        as.factor(Hipertensi)+as.factor(Kolesterol.Tinggi),
                      data = train_nearmiss_sample, family = binomial(link="logit"))
pred <- predict(model_nearmiss, newdata = test, type = "response")
pred <- as.factor(ifelse(pred > 0.5, 1, 0))
levels(pred)
library(caret)
c_matrix_nearmiss <- confusionMatrix(data = pred,
                                     reference = test$Stroke)
c_matrix_nearmiss
F_score_nearmiss <- 2 * c_matrix_nearmiss$byClass[1] * c_matrix_nearmiss$byClass[3] /
  (c_matrix_nearmiss$byClass[1] + c_matrix_nearmiss$byClass[3])
names(F_score_nearmiss) <- 'F_Score_Up'
F_score_nearmiss # Better?
#
# Note that the the step_nearmiss is a nearmiss-3 algorithm
set.seed(2021)
train_nearmiss_sample <- recipe(~., train) %>%
  step_nearmiss(class, under_ratio = 3) %>%
  prep() %>%
  bake(new_data = NULL)
train_nearmiss_sample # a tibble is a new modern dataframe.
model_nearmiss <- glm(class ~as.factor(asal)+umur+as.factor(gender)+
                        as.factor(pendidikan)+as.factor(merokok)+
                        as.factor(aktivitas)+as.factor(obesitas)+
                        as.factor(Hipertensi)+as.factor(Kolesterol.Tinggi),
                      data = train_nearmiss_sample, family = binomial(link="logit"))
pred <- predict(model_nearmiss, newdata = test, type = "response")
pred <- as.factor(ifelse(pred > 0.5, 1, 0))
levels(pred)
library(caret)
c_matrix_nearmiss <- confusionMatrix(data = pred,
                                     reference = test$Stroke)
c_matrix_nearmiss
F_score_nearmiss <- 2 * c_matrix_nearmiss$byClass[1] * c_matrix_nearmiss$byClass[3] /
  (c_matrix_nearmiss$byClass[1] + c_matrix_nearmiss$byClass[3])
names(F_score_nearmiss) <- 'F_Score_Up'
F_score_nearmiss # Better?

# Let's now do tomek.
set.seed(2021)
train_tomek_sample <- recipe(class~., train) %>%
  step_tomek(class) %>%
  prep() %>%
  bake(new_data = NULL)
nrow(train)
train_tomek_sample
#
# Note that the tomek here does not 'balance' our dataset. HOWEVER, this does
# take out 'outliers'
table(train_tomek_sample$class)
model_tomek <- glm(class ~as.factor(asal)+umur+as.factor(gender)+
                     as.factor(pendidikan)+as.factor(merokok)+
                     as.factor(aktivitas)+as.factor(obesitas)+
                     as.factor(Hipertensi)+as.factor(Kolesterol.Tinggi),
                   data = train_tomek_sample, family = binomial(link="logit"))
pred <- predict(model_tomek, newdata = test, type = "response")
pred <- as.factor(ifelse(pred > 0.5, 1, 0))
levels(pred)
levels(pred) <- c(levels(pred),"1")
c_matrix_tomek <- confusionMatrix(data = pred,
                                  reference = test$Stroke)
c_matrix_tomek
F_score_tomek <- 2 * c_matrix_tomek$byClass[1] * c_matrix_tomek$byClass[3] /
  (c_matrix_tomek$byClass[1] + c_matrix_tomek$byClass[3])
names(F_score_tomek) <- 'F_Score_Up'
F_score_tomek
