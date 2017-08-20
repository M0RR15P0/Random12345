library(readr)
Cost_Composition_Brand_E <- read_csv("C:/Users/morrispo/Desktop/Challenge - Prediction/Cost Composition - Brand E.csv")
col_types = cols(Date = col_date(format = "%m/%d/%Y")))

# Remove Products only with Other Costs
Cost_Composition_Brand_E = Cost_Composition_Brand_E[!(Cost_Composition_Brand_E$eaOtherCost>=Cost_Composition_Brand_E$eaFOB),] 

# Remove Products with Missing Data
Cost_Composition_Brand_E = na.omit(Cost_Composition_Brand_E) 

# Take the costs only for Principal Component Analysis
PRC = prcomp(Cost_Composition_Brand_E[,c(8:13,15)],scale=TRUE) 
summary(PRC)

# Get % of Costs by Product
Cost_Composition_Brand_E_Average = Cost_Composition_Brand_E 
Cost_Composition_Brand_E_Average[,8] = Cost_Composition_Brand_E_Average[,8]/Cost_Composition_Brand_E_Average[,14]
Cost_Composition_Brand_E_Average[,9] = Cost_Composition_Brand_E_Average[,9]/Cost_Composition_Brand_E_Average[,14]
Cost_Composition_Brand_E_Average[,10] = Cost_Composition_Brand_E_Average[,10]/Cost_Composition_Brand_E_Average[,14]
Cost_Composition_Brand_E_Average[,11] = Cost_Composition_Brand_E_Average[,11]/Cost_Composition_Brand_E_Average[,14]
Cost_Composition_Brand_E_Average[,12] = Cost_Composition_Brand_E_Average[,12]/Cost_Composition_Brand_E_Average[,14]
Cost_Composition_Brand_E_Average[,13] = Cost_Composition_Brand_E_Average[,13]/Cost_Composition_Brand_E_Average[,14]
Cost_Composition_by_Product = aggregate(cbind(eaFabricCost,eaCMCost,eaTrimCost,eaArtCost,eaWashCost,eaOtherCost)~Retail_SubClass_Desc,Cost_Composition_Brand_E_Average,mean)

# Determine which Component Drives the Cost
Cost_Composition_by_Product$Category = ifelse(Cost_Composition_by_Product$eaFabricCost>0.4,"Fabric-Driven",ifelse(Cost_Composition_by_Product$eaCMCost>0.4,"Cotton-Driven","Labour-Driven"))
Cost_Composition_Brand_E = merge(Cost_Composition_Brand_E,Cost_Composition_by_Product[,c(1,8)],by.x="Retail_SubClass_Desc",by.y="Retail_SubClass_Desc",all.x=T)

# Get the Start_Date to map Cotton Index and Minimum Wage
Cost_Composition_Brand_E$Start_Date = as.Date(Cost_Composition_Brand_E$Date,"%m/%d/%Y")
library(lubridate)
Cost_Composition_Brand_E$Start_Date = format(Cost_Composition_Brand_E$Start_Date, format="%m-01-%Y")

# Join Cotton Index
library(readxl)
Economic_Data_Cotton_Prices_by_Index <- read_excel("C:/Users/morrispo/Desktop/Challenge - Prediction/Economic Data - Cotton Prices by Index.xlsx", 
col_types = c("date", "text", "numeric"))
# Use A index Cotton only, as the rest are highly correlated with it
Economic_Data_Cotton_Prices_by_Index = Economic_Data_Cotton_Prices_by_Index[Economic_Data_Cotton_Prices_by_Index$`Cotton Type`=="A index Cotton",]
Economic_Data_Cotton_Prices_by_Index = Economic_Data_Cotton_Prices_by_Index[,-2]
names(Economic_Data_Cotton_Prices_by_Index) = c("Date","Cotton Price")
Economic_Data_Cotton_Prices_by_Index$Date = paste0(substr(Economic_Data_Cotton_Prices_by_Index$Date,6,7),"-",substr(Economic_Data_Cotton_Prices_by_Index$Date,9,10),"-",substr(Economic_Data_Cotton_Prices_by_Index$Date,1,4))
Cost_Composition_Brand_E = merge(Cost_Composition_Brand_E,Economic_Data_Cotton_Prices_by_Index,by.x="Start_Date",by.y="Date",all.x=T)

# Join Currency Rate
Currency_Rate <- read_excel("~/Currency_Rate.xlsx")
library("lubridate")
Cost_Composition_Brand_E$Year = substr(Cost_Composition_Brand_E$Start_Date,7,10)
Cost_Composition_Brand_E = merge(Cost_Composition_Brand_E,Currency_Rate,by.x=c("Country","Year"),by.y=c("Country","Year"),all.x=T)

# Join Minimum Wage
Economic_Data_Minimum_Wages_by_Country <- read_excel("C:/Users/morrispo/Desktop/Challenge - Prediction/Economic Data - Minimum Wages by Country.xlsx")
Economic_Data_Minimum_Wages_by_Country$Date = paste0(substr(Economic_Data_Minimum_Wages_by_Country$Date,6,7),"-",substr(Economic_Data_Minimum_Wages_by_Country$Date,9,10),"-",substr(Economic_Data_Minimum_Wages_by_Country$Date,1,4))
Cost_Composition_Brand_E = merge(Cost_Composition_Brand_E,Economic_Data_Minimum_Wages_by_Country,by.x=c("Country","Start_Date"),by.y=c("Country","Date"),all.x=T)
Cost_Composition_Brand_E$`Cotton Price` = log10(Cost_Composition_Brand_E$`Cotton Price`)

# Segment by Product-Driven Category
Fabric_Driven = Cost_Composition_Brand_E[Cost_Composition_Brand_E$Category.y=="Fabric-Driven",]
Cotton_Driven = Cost_Composition_Brand_E[Cost_Composition_Brand_E$Category.y=="Cotton-Driven",]
Labour_Driven = Cost_Composition_Brand_E[Cost_Composition_Brand_E$Category.y=="Labour-Driven",]

# Get Regression Coefficients
Cotton_glm = glm(eaFOB~`Cotton Price`+`FX`+`Min Wage`+Retail_SubClass_Desc+0,data = Cotton_Driven, family = gaussian)
summary(Cotton_glm)
Fabric_glm = glm(eaFOB~`Cotton Price`+`FX`+`Min Wage`+Retail_SubClass_Desc+0,data = Fabric_Driven, family = gaussian)
summary(Fabric_glm)
Labour_glm = glm(eaFOB~`Cotton Price`+`FX`+`Min Wage`+Retail_SubClass_Desc+0,data = Labour_Driven, family = gaussian)
summary(Labour_glm)
