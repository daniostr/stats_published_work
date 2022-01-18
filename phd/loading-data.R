# Data Loading Script #


# Load Libraries 
library(RODBC)
library(tidyverse)
library(data.table)
library(lubridate)

# Define hebrew and open access connections
Sys.setlocale("LC_ALL", "Hebrew")
con1 <- odbcConnectAccess2007('input/Req714_Results1.accdb')
con2 <- odbcConnectAccess2007('input/Req714_Results2.accdb')
con3 <- odbcConnectAccess2007('input/Req714_Results3.accdb')
con4 <- odbcConnectAccess2007('input/Req714_Results4.accdb')
con5 <- odbcConnectAccess2007('input/Req714_Results5.accdb')
con6 <- odbcConnectAccess2007('input/Req714_Results6.accdb')
con7 <- odbcConnectAccess2007('input/Req714_Results7.accdb')
con8 <- odbcConnectAccess2007('input/Req714_Results8.accdb')
con9 <- odbcConnectAccess2007('input/Req714_ChronicDiags_CCI_Hosp.accdb')

# Load data tables
df_chemistry_er <- as.data.table(sqlFetch(con1, 'dbo_Req714_ChemistryResultsER', as.is = TRUE))
df_chemistry_er_raw <- as.data.table(sqlFetch(con1, 'dbo_Req714_ChemistryResultsER_RawData', as.is = TRUE))
df_hematology_er <- as.data.table(sqlFetch(con4, 'dbo_Req714_HematologyResultsER', as.is = TRUE))
df_hematology_er_raw<- as.data.table(sqlFetch(con4, 'dbo_Req714_HematologyResultsER_RawData', as.is = TRUE))
df_measures_er<- as.data.table(sqlFetch(con6, 'dbo_Req714_MeasuresER', as.is = TRUE))
df_chemistry_hosp_raw <- as.data.table(sqlFetch(con3, 'dbo_Req714_ChemistryResultsHosp_RawData', as.is = TRUE))
df_hematology_hosp_raw <- as.data.table(sqlFetch(con5, 'dbo_Req714_HematologyResultsHosp_RawData', as.is = TRUE))
df_measures_hosp_raw_ofek <- as.data.table(sqlFetch(con7, 'dbo_Req714_MeasuresHosp_Ofek_RawData', as.is = TRUE))
df_measures_hosp_raw_chameleon <- as.data.table(sqlFetch(con7, 'dbo_Req714_MeasuresHosp_Chameleon_RawData', as.is = TRUE))
df_hosp_and_er_details  <- as.data.table(sqlFetch(con5, 'dbo_Req714_HospAndERDetails', as.is = TRUE))
df_index <- as.data.table(sqlFetch(con6, 'dbo_Req714_Indx', as.is = TRUE))
df_demographic_static <- as.data.table(sqlFetch(con7, 'dbo_Req714_StaticDemography_New', as.is = TRUE))
df_demographic_relative <- as.data.table(sqlFetch(con8, 'dbo_Req714_RelativDemography', as.is = TRUE))
df_cci <- as.data.table(sqlFetch(con9, 'dbo_Req714_ChronicDiags_CCI', as.is = TRUE))



