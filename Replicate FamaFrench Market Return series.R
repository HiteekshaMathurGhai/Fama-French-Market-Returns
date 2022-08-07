options(warn=-1)
require(data.table)
library(foreign) 
library(readxl)
library(moments)

CRSP_Stocks <- as.data.table(read.dta("Data.dta"))
CRSP_Stocks2 <- as.data.table(read.dta("VWReturns.dta"))

Part1= function(CRSP_Stocks)
{
  CRSP_Stocks2 <- CRSP_Stocks[CRSP_Stocks$shrcd == 10|CRSP_Stocks$shrcd == 11,]
  CRSP_Stocks3 <- CRSP_Stocks2[CRSP_Stocks2$exchcd==1|CRSP_Stocks2$exchcd==2|CRSP_Stocks2$exchcd==3,]
  for (col in c("shrout","ret","dlret","prc")) CRSP_Stocks3[is.na(get(col)), (col) := 0]
  
  CRSP_Stocks3$Year=as.numeric(format(as.Date(CRSP_Stocks3$date, format="%Y-%m-%d"),"%Y"))
  CRSP_Stocks3$Month=as.numeric((format(as.Date(CRSP_Stocks3$date, format="%Y-%m-%d"),"%m")))
  CRSP_Stocks3$prc = abs(CRSP_Stocks3$prc)
  
  CRSP_Stocks3$Tot_Ret = ((1+CRSP_Stocks3$ret)*(1+CRSP_Stocks3$dlret))-1
  CRSP_Stocks3$Stock_curr_MV=CRSP_Stocks3$prc*CRSP_Stocks3$shrout
  
  # sapply(CRSP_Stocks3, class) #checking data types of columns
  CRSP_Stocks3[, Stock_lag_MV := shift(Stock_curr_MV), by=permno ]
  for (col in c("Stock_lag_MV")) CRSP_Stocks3[is.na(get(col)), (col) := 0]
  CRSP_Stocks3[, MeWt := Stock_lag_MV/sum(Stock_lag_MV), by=list(Year,Month)]
  CRSP_Stocks3[, VW := sum(MeWt*Tot_Ret), by=list(Year,Month)]
  
  CRSP_Stocks_output = CRSP_Stocks3[, list(Stock_lag_MV=sum(Stock_lag_MV),Stock_Ew_Ret=mean(Tot_Ret),Stock_Vw_Ret = weighted.mean(Tot_Ret, MeWt)), by=list(Year,Month)]
  
  Monthly_CRSP_Stocks<<-CRSP_Stocks_output[order(CRSP_Stocks_output$Year),][-1]
  write.dta(Monthly_CRSP_Stocks,'Monthly_CRSP_Stocks.dta')
  return(Monthly_CRSP_Stocks)
}



Part2= function(Monthly_CRSP_Stocks,FF_mkt)
{
  FF_mkt <<- read.csv('F-F_Research_Data_Factors.csv')
  FF_mkt$Year=substr(FF_mkt$Date, start = 1, stop = 4)
  FF_mkt$Month=substr(FF_mkt$Date, start = 5, stop = 6)
  
  FF_mkt$Mkt.RF=FF_mkt$Mkt.RF/100
  FF_mkt$RF=FF_mkt$RF/100
  FF_mkt <- FF_mkt[FF_mkt$Year <2020,]
  
  CRSP_Stocks_output2=Monthly_CRSP_Stocks[order(Monthly_CRSP_Stocks$Year),]
  
  CRSP_Stocks_output2<- tail(CRSP_Stocks_output2,-6)
  
  CRSP_Stocks_output2$Year=as.numeric(CRSP_Stocks_output2$Year)
  FF_mkt$Year=as.numeric(FF_mkt$Year)
  CRSP_Stocks_output2$Month=as.numeric(CRSP_Stocks_output2$Month)
  FF_mkt$Month=as.numeric(FF_mkt$Month)
  
  M <- merge(CRSP_Stocks_output2,FF_mkt, by  = c('Year',"Month")) 
  M$Replicated_Market_minus_Rf=M$Stock_Vw_Ret-M$RF
  
  PSQ2 <- data.frame(matrix(ncol = 2, nrow = 5))
  column_names = c('Estimated FF Market Excess Return', 'Actual FF Market Excess Return')
  row_names=c('Annualized Mean', 'Annualized Standard Deviation','Annualized Sharpe Ratio', 'Skewness', 'Excess Kurtosis')
  colnames(PSQ2) <- column_names
  rownames(PSQ2)=row_names
  
  mean_monthly=mean(FF_mkt$Mkt.RF)
  mean_ann=(mean_monthly)*12
  std_monthly=sd(FF_mkt$Mkt.RF)
  std_ann=(std_monthly)*sqrt(12)
  mean_monthly1=mean(M$Replicated_Market_minus_Rf)
  mean_ann1=(mean_monthly1)*12
  std_monthly1=sd(M$Replicated_Market_minus_Rf)
  std_ann1=(std_monthly1)*sqrt(12)
  
  PSQ2['Annualized Mean','Estimated FF Market Excess Return']=mean_ann1*100
  PSQ2['Annualized Mean','Actual FF Market Excess Return']=mean_ann*100
  PSQ2['Annualized Standard Deviation','Estimated FF Market Excess Return']=std_ann1*100
  PSQ2['Annualized Standard Deviation','Actual FF Market Excess Return']=std_ann*100
  
  PSQ2['Annualized Sharpe Ratio','Estimated FF Market Excess Return']=mean_ann1/std_ann1
  PSQ2['Annualized Sharpe Ratio','Actual FF Market Excess Return']=mean_ann/std_ann
  PSQ2['Skewness','Estimated FF Market Excess Return']=skewness(M$Replicated_Market_minus_Rf)
  PSQ2['Skewness','Actual FF Market Excess Return']=skewness(FF_mkt$Mkt.RF)
  PSQ2['Excess Kurtosis','Estimated FF Market Excess Return']=(kurtosis(M$Replicated_Market_minus_Rf))-3
  PSQ2['Excess Kurtosis','Actual FF Market Excess Return']=(kurtosis(FF_mkt$Mkt.RF))-3
  return(PSQ2)
}


Part3= function(Monthly_CRSP_Stocks,FF_mkt)
{
  FF_mkt <<- read.csv('F-F_Research_Data_Factors.csv')
  FF_mkt$Year=substr(FF_mkt$Date, start = 1, stop = 4)
  FF_mkt$Month=substr(FF_mkt$Date, start = 5, stop = 6)
  
  FF_mkt$Mkt.RF=FF_mkt$Mkt.RF/100
  FF_mkt$RF=FF_mkt$RF/100
  FF_mkt <- FF_mkt[FF_mkt$Year <2020,]
  CRSP_Stocks_output2=Monthly_CRSP_Stocks[order(Monthly_CRSP_Stocks$Year),]
  
  CRSP_Stocks_output2<- tail(CRSP_Stocks_output2,-6)
  
  CRSP_Stocks_output2$Year=as.numeric(CRSP_Stocks_output2$Year)
  FF_mkt$Year=as.numeric(FF_mkt$Year)
  CRSP_Stocks_output2$Month=as.numeric(CRSP_Stocks_output2$Month)
  FF_mkt$Month=as.numeric(FF_mkt$Month)
  
  M <- merge(CRSP_Stocks_output2,FF_mkt, by  = c('Year',"Month")) 
  M$Replicated_Market_minus_Rf=M$Stock_Vw_Ret-M$RF
  
  cor=cor(M$Replicated_Market_minus_Rf,FF_mkt$Mkt.RF, method = c("pearson", "kendall", "spearman"))
  
  diff=M$Replicated_Market_minus_Rf-FF_mkt$Mkt.RF
  Mean_abs_diff=abs(diff)*10000
  library(psych)
  print(describe(Mean_abs_diff))
  md=max(Mean_abs_diff)
  
  x='Correlation'
  x1=cor
  y='Maximum absolute Difference'
  y1=md
  A.=c(x,x1)
  B.=c(y,y1)
  
  v <- rbind(A.,B.)
  v
  
  return(v)
}
Part1(CRSP_Stocks)
Part2(Monthly_CRSP_Stocks,FF_mkt)
Part3(Monthly_CRSP_Stocks,FF_mkt)
