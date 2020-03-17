###########
#dodawanie danych
path <- "/Users/mikolajmalec/Desktop/WUMPROJ1"

loadeddata <- read.csv2( paste0( path, "/german_credit_data_weka_dataset.csv"), sep = ",")

###########
#przeksztalcanie na dane numeryczne i z faktorami

cleandata <- loadeddata

levels( cleandata[,1]) <- c("low","fair","high","not_have") #DM low<0<fair<200<high
levels( cleandata[,3]) <- c("all_paid","all_paid_here","paid_till_now","delay","critical")
levels( cleandata[,4]) <- c("new_car","used_car","furniture/equipment","radio/television","domestic","repairs","education","retraining","business","other") #note: 0 for vacation
levels( cleandata[,6]) <- c("low","normal","high","very_high","not_have/unknown") #DM low<100<normal<500<high<1000<very_high
levels( cleandata[,7]) <- c("unemployed","less_than_year","1-3_years","4-6_yeras","7+_years")
levels( cleandata[,9]) <- c("male_d/s","female_d/s/m","male_single","male_m/w") #d = divorsed, s = seperated, m = married, w = widowed ,#note: 0 female single
levels( cleandata[,10]) <- c("none","co-applicant","guarantor")
levels( cleandata[,12]) <- c("real_estate","building_savings","car","not_have/unknown")
levels( cleandata[,14]) <- c("bank","stores","none")
levels( cleandata[,15]) <- c("rent","own","for_free")
levels( cleandata[,17]) <- c("unskilled_non_resident","unskilled_resident","skilled_eployee","highly_qualified_employee*") #*also management, self-employed, officer
levels( cleandata[,19]) <- c("no","yes")
levels( cleandata[,20]) <- c("yes","no")
cleandata[,21] <- as.factor( as.character( loadeddata[,21]))
levels( cleandata[,21]) <- c("Good","Bad")

#########
#wstępna eksploracja
library(DataExplorer)
DataExplorer::create_report(cleandata)

########
#co chce kobieta i meszczyzna
library(rpivotTable)
rpivotTable( cleandata,
             cols = "personal",
             rows = "purpose",
             rendererName = "Heatmap",
             aggregatorName = "Count")
#wstępna analiza
#nawięcej: domestic, new car, radio/television
rpivotTable( cleandata,
             cols = "personal",
             rows = "purpose",
             rendererName = "Bar Chart",
             aggregatorName = "Count")
#warto zobaczyć procentowy rozkład: np. kobiety częsciej kupują radio/telewizje, nie kupują używanych samochodów

rpivotTable( cleandata,
             cols = "personal",
             rows = "age",
             rendererName = "Heatmap",
             aggregatorName = "Count")
#wstępna analiza
#wiek: 23-36 lat; mężaci m/f biorą młodo porzyczki; male_d/s są raczej starsi

rpivotTable( cleandata,
             cols = "customer_type",
             rows = "purpose",
             rendererName = "Heatmap",
             aggregatorName = "Count")
#warto pokazać procent przyznantch kredytow na każdą rzecz (kołowy i ile przypadków)

library(gpairs)
gpairs( cleandata, lower.pars=list(scatter="stats"), 
        outer.rot=c(90,0),
        scatter.pars=list(pch=19))
