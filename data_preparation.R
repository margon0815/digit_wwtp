# data preparation file 

library(dplyr)
library(readxl)
library(tibble)
library(WriteXLS)
library(ggplot2)
library(scales)
library(data.table)
library(caTools)
library(useful)

# read routine data example from excel file -------------------------------------------------------------------
filename2 = ("example_data.xlsx")

BTB <- read_excel(filename2, col_types="numeric")  # sheet = "xxx"
BTBdat <- read_excel(filename2, col_types="text")  

BTB$datum <- as.POSIXct(BTBdat$datum,tz = "UTC") # correction of date

remove(BTBdat)

#preparation of data frame

posi <-data.frame(
pos_datzeit=1,
pos_Q_d=2,          #[m3/d]
pos_T_BB=3,         #[°C]
#influent concentration of activated sludge tanks
pos_C_CSB_ZB=4,     #[mg/L]
pos_C_KN_ZB=6,      #[mg/L]
pos_C_P_ZB=5)       #[mg/L]

data <-data.frame(BTB[posi$pos_datzeit], 
                  BTB[posi$pos_Q_d], 
                  BTB[posi$pos_T_BB], 
                  BTB[posi$pos_C_CSB_ZB],
                  BTB[posi$pos_C_KN_ZB],
                  BTB[posi$pos_C_P_ZB])

# set column names based on guideline nomenclature ----
  names(data) <- c("Datum",
                   "Q_d",
                   "T_BB",
                   "C_CSB_ZB",
                   "C_KN_ZB",
                   "C_P_ZB")
   
# preparation of date ----

df_btb=data #copy of original data frame

#us of date format POSIXct for number of days of week
df_btb <- mutate(df_btb,zeit=" 00:00:00") # temporary column
df_btb$Datum <- as.POSIXct(paste(df_btb$Datum, df_btb$zeit), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") 
df_btb <- select(df_btb,-zeit) # remove temp. col.

# insert day number after date, 1=monday (%u Mo=1 1-7, %w Sun=0 0-6)
df_btb = mutate(df_btb,WT = as.numeric(format(df_btb$Datum, "%u")), .after = Datum) 

# insert year after day number 
df_btb = mutate(df_btb,Jahr = as.numeric(format(df_btb$Datum, "%Y")), .after = WT) 

# insert month after year
df_btb = mutate(df_btb,Monat = as.numeric(format(df_btb$Datum, "%m")), .after = Jahr) 

# insert day after month
df_btb = mutate(df_btb,Tag = as.numeric(format(df_btb$Datum, "%d")), .after = Monat) 

# copy of data frame

df_btb_auswahl <- df_btb

######## from her all calculations with df df_btb_auswahl ----

# calculation of ratios -------------------------------------------------

##ratio influent activated sludge tank (ZB)
df_btb_auswahl <- df_btb_auswahl %>% mutate("C_KN_ZB_2_C_CSB_ZB" = C_KN_ZB/C_CSB_ZB)
df_btb_auswahl <- df_btb_auswahl %>% mutate("C_P_ZB_2_C_CSB_ZB" = C_P_ZB/C_CSB_ZB)
df_btb_auswahl <- df_btb_auswahl %>% mutate("C_P_ZB_2_C_KN_ZB" = C_P_ZB/C_KN_ZB)

# calculation of loads ----

df_btb_auswahl <- df_btb_auswahl %>% mutate("Bd_CSB_ZB" = Q_d*C_CSB_ZB/1000)
df_btb_auswahl <- df_btb_auswahl %>% mutate("Bd_KN_ZB" = Q_d*C_KN_ZB/1000)
df_btb_auswahl <- df_btb_auswahl %>% mutate("Bd_P_ZB" = Q_d*C_P_ZB/1000)

# moving averages ----
# Temperature
df_btb_auswahl <- df_btb_auswahl %>% mutate(T_BB_2WM = runmean(T_BB, 14, endrule="NA"), .after = T_BB)

# loads
# 2 Week mean ZB
df_btb_auswahl <- df_btb_auswahl %>% mutate(Bd_CSB_ZB_2WM = runmean(Bd_CSB_ZB, 14, endrule="NA"), .after = Bd_CSB_ZB)
df_btb_auswahl <- df_btb_auswahl %>% mutate(Bd_KN_ZB_2WM = runmean(Bd_KN_ZB, 14, endrule="NA"), .after = Bd_KN_ZB)
df_btb_auswahl <- df_btb_auswahl %>% mutate(Bd_P_ZB_2WM = runmean(Bd_P_ZB, 14, endrule="NA"), .after = Bd_P_ZB)

# calculation of dry weather days ----

df_btb_auswahl <- df_btb_auswahl %>% mutate(Q21dMin = runmin(Q_d, 21, endrule="NA"), .after = Q_d)

# flag for dw based on 21d running mean
df_btb_auswahl <- df_btb_auswahl %>% mutate(TWQ21dMinflag = case_when(Q_d < Q21dMin*1.2 ~ 1, 
                                                                      Q_d == Q21dMin*1.2 ~ as.numeric(NA), 
                                                                      Q_d > Q21dMin*1.2 ~ as.numeric(NA)), .after = Q21dMin)

df_btb_auswahl <- df_btb_auswahl %>% mutate(Q_TW_21dMin = TWQ21dMinflag*Q_d, .after = TWQ21dMinflag)

# calculation of descriptive statistics ----

tmp1=df_btb_auswahl %>% summarise_all(~sum(!is.na(.)))                     # number of cells with real values

tmp2=df_btb_auswahl %>% summarise_all(~{mean(.x, na.rm = any(!is.na(.x)))})     # mean
# workaround for date and numeric format
tmp2$Datum <- as.numeric(tmp2$Datum)  
tmp3=df_btb_auswahl %>% summarise_all(~{median(.x, na.rm = any(!is.na(.x)))})   # Median
tmp3$Datum <- as.numeric(tmp3$Datum)
tmp4=df_btb_auswahl %>% summarise_all(~{sd(.x, na.rm = any(!is.na(.x)))})       # stdev
tmp4$Datum <- as.numeric(tmp4$Datum)
tmp5=df_btb_auswahl %>% summarise_all(~{min(.x, na.rm = any(!is.na(.x)))})      # Minimum
tmp5$Datum <- as.numeric(tmp5$Datum)
tmp6=df_btb_auswahl %>% summarise_all(~{max(.x, na.rm = any(!is.na(.x)))})      # Maximum
tmp6$Datum <- as.numeric(tmp6$Datum)
tmp7=df_btb_auswahl %>% summarise_all(~{quantile(.x, probs = 0.01, na.rm = TRUE)})      # Percentile
tmp7$Datum <- as.numeric(tmp7$Datum)
tmp8=df_btb_auswahl %>% summarise_all(~{quantile(.x, probs = 0.05, na.rm = TRUE)})      # Percentile
tmp8$Datum <- as.numeric(tmp8$Datum)
tmp9=df_btb_auswahl %>% summarise_all(~{quantile(.x, probs = 0.15, na.rm = TRUE)})      # Percentile
tmp9$Datum <- as.numeric(tmp9$Datum)
tmp10=df_btb_auswahl %>% summarise_all(~{quantile(.x, probs = 0.85, na.rm = TRUE)})      # Percentile
tmp10$Datum <- as.numeric(tmp10$Datum)
tmp11=df_btb_auswahl %>% summarise_all(~{quantile(.x, probs = 0.95, na.rm = TRUE)})      # Percentile
tmp11$Datum <- as.numeric(tmp11$Datum)
tmp12=df_btb_auswahl %>% summarise_all(~{quantile(.x, probs = 0.99, na.rm = TRUE)})     # Percentile
tmp12$Datum <- as.numeric(tmp12$Datum)

des_stats <- bind_rows(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11,tmp12)

des_stats = mutate(des_stats,Beschreibung = c("Anzahl Werte","Mittelwert","Median","SD","Min","Max","1%-Perz","5%-Perz","15%-Perz","85%-Perz","95%-Perz","99%-Perz"), .before = 1) 

# ratio of mean and median
des_stats <- rbind(des_stats, 
                   cbind(Beschreibung = 'Mittelwert/Median', 
                         subset(des_stats, Beschreibung == 'Mittelwert', select = -1)/
                           subset(des_stats, Beschreibung == 'Median', select = -1)))

# rel. deviation
des_stats <- rbind(des_stats, 
                   cbind(Beschreibung = 'rel. Abweichung', 
                         subset(des_stats, Beschreibung == 'SD', select = -1)/
                           subset(des_stats, Beschreibung == 'Mittelwert', select = -1)))

row.names(des_stats) <- c("Anzahl Werte","Mittelwert","Median","SD","Min","Max","1%-Perz","5%-Perz","15%-Perz","85%-Perz","95%-Perz","99%-Perz","MW/Median","rel. Abweichung")

remove(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmp9,tmp10,tmp11,tmp12)

# samples per day of week influent activated sludge tank----
aC_CSB_ZB <- df_btb_auswahl %>% group_by(WT) %>% summarise(sum(!is.na(C_CSB_ZB))) # Anzahl Werte pro Wochentag in einer Spalte
aC_P_ZB  <- df_btb_auswahl %>% group_by(WT) %>% summarise(sum(!is.na(C_P_ZB))) # Anzahl Werte pro Wochentag in einer Spalte
aC_KN_ZB  <- df_btb_auswahl %>% group_by(WT) %>% summarise(sum(!is.na(C_KN_ZB))) # Anzahl Werte pro Wochentag in einer Spalte

## insert col. names
colnames(aC_CSB_ZB) <- c("WT", "Anzahl")
colnames(aC_P_ZB) <- c("WT", "Anzahl")
colnames(aC_KN_ZB) <- c("WT", "Anzahl")

aC_CSB_ZB = mutate( aC_CSB_ZB, Parameter="CSB")
aC_P_ZB = mutate(aC_P_ZB, Parameter="P")
aC_KN_ZB = mutate(aC_KN_ZB, Parameter="KN")

DOW_ZB <- bind_rows(aC_CSB_ZB, aC_P_ZB, aC_KN_ZB)

## text for days of week
DOW_ZB$WT <- as.character(DOW_ZB$WT)
DOW_ZB$WT[DOW_ZB$WT == "1"] <-"1_Mo"
DOW_ZB$WT[DOW_ZB$WT == "2"] <-"2_Di"
DOW_ZB$WT[DOW_ZB$WT == "3"] <-"3_Mi"
DOW_ZB$WT[DOW_ZB$WT == "4"] <-"4_Do"
DOW_ZB$WT[DOW_ZB$WT == "5"] <-"5_Fr"
DOW_ZB$WT[DOW_ZB$WT == "6"] <-"6_Sa"
DOW_ZB$WT[DOW_ZB$WT == "7"] <-"7_So"

# samples per year influent activated sludge tank ----

bC_CSB_ZB <- df_btb_auswahl %>% group_by(Jahr) %>% summarise(sum(!is.na(C_CSB_ZB))) # Anzahl Werte pro Wochentag in einer Spalte
bC_P_ZB  <- df_btb_auswahl %>% group_by(Jahr) %>% summarise(sum(!is.na(C_P_ZB))) # Anzahl Werte pro Wochentag in einer Spalte
bC_KN_ZB  <- df_btb_auswahl %>% group_by(Jahr) %>% summarise(sum(!is.na(C_KN_ZB))) # Anzahl Werte pro Wochentag in einer Spalte

## insert col. names
colnames(bC_CSB_ZB) <- c("Jahr", "Anzahl")
colnames(bC_P_ZB) <- c("Jahr", "Anzahl")
colnames(bC_KN_ZB) <- c("Jahr", "Anzahl")

bC_CSB_ZB = mutate( bC_CSB_ZB, Parameter="CSB")
bC_P_ZB = mutate(bC_P_ZB, Parameter="P")
bC_KN_ZB = mutate(bC_KN_ZB, Parameter="KN")

DOY_ZB <- bind_rows(bC_CSB_ZB, bC_P_ZB, bC_KN_ZB)

save.image(file="exampletime.RData") #saving workspace for use in further scripts

# correction of date format in dataframe  ----
data$Datum <-as.Date(data$Datum, format="%d.%m.%Y", tz = "UTC")
df_btb_auswahl$Datum <-as.Date(df_btb_auswahl$Datum, format="%d.%m.%Y", tz = "UTC")

save.image(file="example.RData") #saving workspace for use in further scripts

