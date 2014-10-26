#incoming

#creates data.frame for clustering
clustering <- function(data){
	#data from wW_supplier_po_measures
	#supplier_id | w_mean_day_in_window | sd_day_in_window
clusterdata <- data.frame(data)
setnames(clusterdata,1:3, c("supplier","mean","sd"))
clusterdata$sd <- ifelse(is.na(clusterdata$sd),0,clusterdata$sd)
clusterdata <- data.table(clusterdata)[, list(avg = mean(mean), avgsd = mean(sd, na.rm =TRUE)), by = supplier]
data.frame(clusterdata)[1:3]
}

clusterplot.total <- function(data){
	#input is clusterdata from function clustering
	data <- data[2:3]
	cluster2wss <- kmeans(data, 1)$totss
for (i in 2:15) cluster2wss[i] <- kmeans(data,
                                     centers=i)$tot.withinss
cluster2wss <- data.frame(cluster2wss, 1:15)
colnames(cluster2wss )<-c("ssw", "clusters")
ggplot(cluster2wss, aes(x=clusters, y = ssw))+geom_line()+geom_point()+scale_x_continuous(breaks=1:15)+xlab("number of clusters")+ylab("sum of squares within clusters")

}

clusterplot.percentage <- function(data){
	#input is clusterdata from function clustering
	data <- data[2:3]
wss <- kmeans(data,1 )$totss
for (i in 2:15) wss[i] <- (kmeans(data, i)$totss - kmeans(data,i)$tot.withinss)/kmeans(data, i)$totss

clustervexplained <- cbind(1:15, wss)
clustervexplained<-data.frame(clustervexplained[2:15,])
colnames(clustervexplained) <- c("clusters", "varexp")
ggplot(clustervexplained, aes(x = clusters, y = varexp))+geom_line()+geom_point()+scale_x_continuous(breaks=1:15)+ylab("Variance explained")+xlab("number of clusters")+scale_y_continuous(breaks = seq(0.4,1,0.1))

}





#outgoing

#sample sales channel
getsaleschannel <- function(data){
	#data: age | country
	ifelse(
		sum(c_age_sales_channeltable[c_age_sales_channeltable$age == data[1] & c_age_sales_channeltable$country == as.character(data[2]),]$Freq) == 0,
		ifelse(data[1] > 60, 
			df <- c_age_sales_channeltable_complete[c_age_sales_channeltable_complete$age == 60 & c_age_sales_channeltable_complete$country == "DE", ],
			df <- c_age_sales_channeltable_complete[c_age_sales_channeltable_complete$age == data[1] & c_age_sales_channeltable_complete$country == "DE", ])
		,df <- c_age_sales_channeltable[c_age_sales_channeltable$age == data[1] & c_age_sales_channeltable$country == as.character(data[2]), ]
		)
	
	levels(c_age_sales_channeltable$sales_channel)[sample(1:length(levels(c_age_sales_channeltable$sales_channel)), replace =TRUE, size = 1, prob = df$Freq)]
}

#sample age
getagesamples <- function(shipping_country, size){
 
	sample(18:90, size = size, replace = TRUE, prob = c_agetable[c_agetable$country == shipping_country, ]$Freq)
}

#sample basket size
getbasketsize <- function(data){
	#data: sales_channel
	sample(1:20, replace = TRUE, size = 1, prob = table_bs_sc[table_bs_sc$sales_channel == data[1],]$Freq)

}


#sample order composition. computationally expensive!
getComposition <- function (sc,month,basket ){
data <- outgoing_basketsubset[outgoing_basketsubset$sales_channel == sc &  outgoing_basketsubset$date_shipped_month == month & outgoing_basketsubset$articles_shipped == basket,c(8:17, 20,21,23:26)]
 # no NA 39,  electroniccases 34, cooperations 31, formalwearaccessories 30 
if(dim(data)[1] < 5){
data <- outgoing_basketsubset[outgoing_basketsubset$date_shipped_month==month & outgoing_basketsubset$articles_shipped==basket,c(8:17, 20,21,23:26)]
	}
data[sample(1:dim(data)[1], size = 1), ]
}

#function to get the index of every different sales channel level 
SCindex <- function(data){
apply(FUN=function(x) {rownames(data[data$sales_channel ==x,][1,])}, MARGIN=1, X=matrix(levels(data$sales_channel)))
}

#performs the complete inventory forecast
inventoryforecast <- function(length){
#getting incoming data
inc <- getCGcOnDay(openPO, "f_arrival") #orderlevel
overview <- getfutureoverview(compareoutgoingforecast.data(length))
out <- getreturndata.forecast(getfutureboxes(overview),FALSE) #slow #orderlevel
openreturns <- getreturndata.open(state128_256,FALSE)#orderlevel

shipped <- getCGcOnDay(out, "f_shipping_date") #daily
returned.f <- getCGreturnedOnDay(out, "f_return_date") #daily
returned.o <- getCGreturnedOnDay(openreturns, "date_returned") #daily

timeframe <- data.frame("date" =seq(max(outgoing_basketsubset$date_shipped),max(outgoing_basketsubset$date_shipped)+length+20, by = "days" ))
#CGreturnedOnDay
#merging returned.f and returned.o
setnames(returned.o, 1, "date")
setnames(returned.f, 1, "date")
returns <- getCGreturnedOnDay(rbind(returned.o,returned.f),"date")
#returned CGs with r

#renaming columns
#CGcOnDay
incnames <- colnames(inc)
incnames <- gsub("c_","i_",incnames)
#incoming CGs (PO) with i
setnames(inc,1:length(incnames),incnames)
setnames(inc,1,"date")
shippednames <- colnames(shipped)
shippednames <- gsub("c_","o_",shippednames)
#outgoing CGs (CO) with o
setnames(shipped,1:length(shippednames),shippednames)
setnames(shipped,1,"date")

#putting it together
timeframe<- join(timeframe, shipped)
timeframe <- join(timeframe, inc)
timeframe <- join(timeframe, returns)

#calculate flows


#CG flows
flows <- data.table(timeframe)[, list(
inflowCG1 = sum(r_Schuhe,i_Schuhe, na.rm =TRUE),
inflowCG2 = sum(r_Outdoor,i_Outdoor, na.rm =TRUE),
inflowCG3 = sum(r_Kleinaccessoires,i_Kleinaccessoires, na.rm =TRUE),
inflowCG4 = sum(r_schlafbade,i_schlafbade, na.rm =TRUE),
inflowCG5 = sum(r_Konfektion,i_Konfektion, na.rm =TRUE),
inflowCG6 = sum(r_Schuhpflege,i_Schuhpflege, na.rm =TRUE),
inflowCG7 = sum(r_tuecherschals,i_tuecherschals, na.rm =TRUE),
inflowCG8 = sum(r_Guertel,i_Guertel, na.rm =TRUE),
inflowCG9 = sum(r_Stiefel,i_Stiefel, na.rm =TRUE),
inflowCG10 = sum(r_KrawattenFliegen,i_KrawattenFliegen, na.rm =TRUE),
inflowCG11 = sum(i_Formalwearaccessories, na.rm =TRUE),
inflowCG12 = sum(i_cooperations, na.rm =TRUE),
inflowCG13 = sum(r_Oberteile,i_Oberteile, na.rm =TRUE),
inflowCG14 = sum(r_Handschuhe,i_Handschuhe, na.rm =TRUE),
inflowCG15 = 0, #electroniccases
inflowCG16 = sum(r_Kopfaccessoires,i_Kopfaccessoires, na.rm =TRUE),
inflowCG17 = sum(r_Hosen,i_Hosen, na.rm =TRUE),
inflowCG18 = sum(r_Taschen,i_Taschen, na.rm =TRUE),
inflowCG19 = sum(r_Unterbekleidung,i_Unterbekleidung, na.rm =TRUE),
outflowCG1 = sum(o_Schuhe, na.rm=TRUE),
outflowCG2 = sum(o_Outdoor, na.rm=TRUE),
outflowCG3 = sum(o_Kleinaccessoires, na.rm=TRUE),
outflowCG4 = sum(o_schlafbade, na.rm=TRUE),
outflowCG5 = sum(o_Konfektion, na.rm=TRUE),
outflowCG6 = sum(o_Schuhpflege, na.rm=TRUE),
outflowCG7 = sum(o_tuecherschals, na.rm=TRUE),
outflowCG8 = sum(o_Guertel, na.rm=TRUE),
outflowCG9 = sum(o_Stiefel, na.rm=TRUE),
outflowCG10 = sum(o_KrawattenFliegen, na.rm=TRUE),
outflowCG11 = 0, #formalwearaccessories
outflowCG12 = 0, # cooperations
outflowCG13 = sum(o_Oberteile, na.rm=TRUE),
outflowCG14 = sum(o_Handschuhe, na.rm=TRUE),
outlfowCG15 = 0, #electroniccases
outflowCG16 = sum(o_Kopfaccessoires, na.rm=TRUE),
outflowCG17 = sum(o_Hosen, na.rm=TRUE),
outflowCG18 = sum(o_Taschen, na.rm=TRUE),
outflowCG19 = sum(o_Unterbekleidung, na.rm=TRUE)), by = date ]

flows <- data.frame(flows)
flows$inflow <- rowSums(flows[2:20])
flows$outflow <- rowSums(flows[21:39])
setnames(flows, 2:20, paste("inflow ",lev[-20]))
setnames(flows, 21:39, paste("outflow ", lev[-20]))

  
  lastinventorydate <- inventory[inventory$datum==maxdate,1:21 ]
  forecastedinventory <- lastinventorydate
  for(i in 1:dim(timeframe)[1]){
    newday <- forecastedinventory[i,]
    newday[1,1] <- newday[1,1]+1
    for (j in 1:19){
      
      newday[1,1+j] <- newday[1,1+j]+flows[i,1+j]-flows[i,20+j]
    }
    forecastedinventory <- rbind(forecastedinventory, newday ) 
  }
  #rownames(forecastedinventory)<-1:dim(forecastedinventory)[1]
  setnames(forecastedinventory,2:21, lev)
rownames(forecastedinventory)<- 1:(dim(forecastedinventory)[1])

#list with 8 entries
#complete timeframe with CGs, incoming data forecasted, outgoing overview (aggregated), outgoing data foreacasted, aggregated on flows, inventory levels, length of forecast in days
list(timeframe,inc,overview,out,openreturns,flows,forecastedinventory,length)	
}


#scenario mode, similar to inventoryforecast
scenario <- function(length, amplifier, includeopenreturns=FALSE,manualreturns=FALSE,prob){
	#0<amplifier<2
	#no inc data 
	amplifier <- amplifier/100

	overview <- getfutureoverview(compareoutgoingforecast.data(length))
	overview$m <- round(overview$m * amplifier[1], 0 )
	if(manualreturns==TRUE){
			out <- getreturndata.forecast(getfutureboxes(overview),TRUE,prob)
		}else{
			out <- getreturndata.forecast(getfutureboxes(overview),FALSE)
		}

	shipped <- getCGcOnDay(out, "f_shipping_date") #daily
	returned.f <- getCGreturnedOnDay(out, "f_return_date") #daily
	

	timeframe <- data.frame("date" =seq(max(outgoing_basketsubset$date_shipped),max(outgoing_basketsubset$date_shipped)+length+20, by = "days" ))
	#CGreturnedOnDay
	#merging returned.f and returned.o

	setnames(returned.f, 1, "date")
	
	shippednames <- colnames(shipped)
	shippednames <- gsub("c_","o_",shippednames)
	#outgoing CGs (CO) with o
	setnames(shipped,1:length(shippednames),shippednames)
	setnames(shipped,1,"date")

	if(includeopenreturns==TRUE){
		if(manualreturns==TRUE){
		openreturns <- getreturndata.open(state128_256,TRUE,prob)}
		else{
			openreturns <- getreturndata.open(state128_256,FALSE)
		}
		returned.o <- getCGreturnedOnDay(openreturns, "date_returned") #daily
			setnames(returned.o, 1, "date")
		returns <- getCGreturnedOnDay(rbind(returned.o,returned.f),"date")
		timeframe<- join(timeframe, shipped)
		timeframe <- join(timeframe, returns)
		list(timeframe,overview,out,openreturns)
	}else{
		timeframe<- join(timeframe, shipped)
		timeframe <- join(timeframe, returned.f)
		list(timeframe,overview,out)
	}
	#putting it together
	

}

#UI representatino
reshape.shipped <- function(data){
b <- data.table(data)[, list("total" = sum(shipped)), by = list(date)]
                 #ADJUST
                 b.1 <- data.table(data[data$country=="DE",])[, list("total" = sum(shipped)), by = list(date)]
                 b.2 <- data.table(data[data$country=="AT",])[, list("total" = sum(shipped)), by = list(date)]
                 b.3 <- data.table(data[data$country=="CH",])[, list("total" = sum(shipped)), by = list(date)]
                 b.4 <- data.table(data[data$country=="NL",])[, list("total" = sum(shipped)), by = list(date)]
                 b.5 <- data.table(data[data$country=="DK",])[, list("total" = sum(shipped)), by = list(date)]
                 b.6 <- data.table(data[data$country=="LU",])[, list("total" = sum(shipped)), by = list(date)]
                 b.7 <- data.table(data[data$country=="SE",])[, list("total" = sum(shipped)), by = list(date)]
                 data.frame( b,"DE"= b.1$total,"AT"= b.2$total, "CH" =b.3$total, "NL" =b.4$total, "DK"=b.5$total, "LU"=b.6$total, "SE"=b.7$total)
}

#get the daily shipped data for the asked time frame
shipped.data <- function(start, end){
if(end<=maxdate){
	a <- c_shipping_table[c_shipping_table$days >= start & c_shipping_table$days<=end,1:3]
	 setnames(a, 1:3, c("date","shipped","country"))
	 x <- reshape.shipped(a)
	 x[x$total>0,]
                 
	}else{
        if(start>max(c_shipping_table$days)){
				 a <-getfutureoverview(compareoutgoingforecast.data(as.numeric(end-maxdate)))
				 setnames(a, 1:3, c("country","date","shipped"))
				 a$shipped <- as.integer(a$shipped)
	           	  x<-reshape.shipped(a)  
	           	  x[x$date>=start,]    
        	 }else{
        	 			rbind(shipped.data(start,maxdate), shipped.data(maxdate+1,end))
        	 	}
			}
	
 }
 
#!display! overview for outgoing forecast
displayfutureoverview <- function(length){
	a <-getfutureoverview(compareoutgoingforecast.data(length))
                 b <- data.table(a)[, list("total" = sum(m)), by = list(f_shipping_date)]
                 #ADJUST
                 b.1 <- data.table(a[a$country=="DE",])[, list("total" = sum(m)), by = list(f_shipping_date)]
                 b.2 <- data.table(a[a$country=="AT",])[, list("total" = sum(m)), by = list(f_shipping_date)]
                 b.3 <- data.table(a[a$country=="CH",])[, list("total" = sum(m)), by = list(f_shipping_date)]
                 b.4 <- data.table(a[a$country=="NL",])[, list("total" = sum(m)), by = list(f_shipping_date)]
                 b.5 <- data.table(a[a$country=="DK",])[, list("total" = sum(m)), by = list(f_shipping_date)]
                 b.6 <- data.table(a[a$country=="LU",])[, list("total" = sum(m)), by = list(f_shipping_date)]
                 b.7 <- data.table(a[a$country=="SE",])[, list("total" = sum(m)), by = list(f_shipping_date)]

                 data.frame( b,"DE"= b.1$total,"AT"= b.2$total, "CH" =b.3$total, "NL" =b.4$total,"DK" =b.5$total,"LU" =b.6$total,"SE" =b.7$total)
                 
}

#function starts forecasting process. merges holt winters and stl forecast
getfutureoverview <- function(data){
	#takes input from compareoutgoingforecast.data
	#gives daily shipped counts for countries
	data$stl <- ifelse(data$stl<0, 0, data$stl)
	data$holt <- ifelse(data$holt<0,0, data$holt)
	data <- ddply(data, .(day, country), summarize, m = round(mean(c(holt, stl)),0))
	data$id <- 1:dim(data)[1]
	data$f_shipping_date <- max(shipping_table$days)+data$day
	data$weekday <- weekdays(data$f_shipping_date)
	data[data$weekday == "Sonntag" | data$weekday == "Sunday",]$f_shipping_date <- data[data$weekday == "Sonntag" | data$weekday == "Sunday",]$f_shipping_date-2
	data[data$weekday == "Samstag" | data$weekday == "Saturday",]$f_shipping_date <- data[data$weekday == "Samstag" | data$weekday == "Saturday",]$f_shipping_date-1
	ddply(data, .(country, f_shipping_date), summarize, m = sum(m))
}

#UI presentation of the shipped orders in the time frame
shipped.plot <- function(start, end, week=FALSE){
	if(as.numeric(end-start)==1){qplot(y=1)+geom_text(label="                              Range too small for plotting",color = "red")}
	else{
    if(end<=maxdate){
        subset <- c_shipping_table[c_shipping_table$days >= start & c_shipping_table$days <= end,]
        setnames(subset,3,"Country")
        if(week){
        	dfw <- subset
        	dfw <- data.table(dfw)[, list(date = min(days), shipped =sum(shipped)), by = list(week, Country)]
        	ggplot(dfw, aes(x=date, y=shipped, col=Country))+geom_line()+facet_grid(Country ~. , scales="free")+xlab("date")+ylab("CO shipped")
				

        }else{
        ggplot(subset, aes(x=days, y=shipped, col=Country))+geom_line()+facet_grid(Country ~. , scales="free")+xlab("date")+ylab("CO shipped")
    	}
    }else{
        if(start>maxdate){
            d <- displayfutureoverview(as.numeric(end-maxdate))
            subset <-melt(d,id.vars = 1)
            setnames(subset,1 , "date")
            setnames(subset, "variable", "Country")
            grid <- expand.grid("date" = seq(start, end, by ="days"), "Country"=levels(c_shipping_table$Country) )
            df <- join(grid,subset)
            df$value <- ifelse(is.na(df$value),0,df$value)
            if(week){
				dfw <- df[df$Country!="total" & df$date>=start & df$date<=end,]
				dfw$week <- paste(year(dfw$date), week(dfw$date), sep = " ")
				dfw <- data.table(dfw)[, list(date = min(date), value =sum(value)), by = list(week, Country)]
				ggplot(dfw, aes(x=date, y = value, col =Country))+geom_line()+facet_grid(Country ~ ., scales="free")+xlab("date")+ylab("CO shipped")
            	}else{
            ggplot(df[df$Country!="total" & df$date>=start & df$date<=end,], aes(x=date, y=value, col = Country))+geom_line()+facet_grid(Country~.,scales="free")+xlab("date")+ylab("CO shipped")
            }
        }else{
            #mixed between
            subsethistory <- c_shipping_table[c_shipping_table$days >= start,1:3]
            setnames(subsethistory,1:3,c("date", "shipped", "Country"))
            subset <-melt(displayfutureoverview(length = end-maxdate),id.vars = 1)
            setnames(subset,1 , "date")
            setnames(subset,2, "Country")
            setnames(subset,3,"shipped")
            data <- rbind(subset[subset$Country!="total",], subsethistory)
            grid <- expand.grid("date" = seq(start, end, by ="days"), "Country"=levels(c_shipping_table$Country) )

                 df <- join(grid,data)
                 df$shipped <- ifelse(is.na(df$shipped),0,df$shipped)
             if(week){
             	dfw <- df
             	dfw$week <- paste(year(dfw$date), week(dfw$date), sep = " ")
				dfw <- data.table(dfw)[, list(date = min(date), shipped =sum(shipped)), by = list(week, Country)]
				ggplot(dfw, aes(x=date, y=shipped, col = Country))+geom_line()+facet_grid(Country~.,scales="free")+xlab("date")+ylab("CO shipped")+geom_vline(x=as.numeric(maxdate), linetype="dashed")
           	
             }else{
            ggplot(df, aes(x=date, y=shipped, col = Country))+geom_line()+facet_grid(Country~.,scales="free")+xlab("date")+ylab("CO shipped")+geom_vline(x=as.numeric(maxdate), linetype="dashed")
            }
        }
        
    }
   }
}

#next step of the inventory forecast. sampling is performed here. 
getfutureboxes <- function ( df){
#takes input from getfutureoverview
	
	df$char_country <- as.character(df$country)
	df$id <- 1:dim(df)[1]
	df <- data.table(df)[, list(f_shipping_date, country,char_country ,age = getagesamples(shipping_country = char_country, size = m)), by = id]
	df <-df[!is.na(df$age),] #if there are no shippings for a country on a day, age is NA. 
	df$id <- 1:dim(df)[1]
	df <- cbind(df, sales_channel = apply(data.frame(df)[c(5,4)], MARGIN=1,FUN=getsaleschannel))
	df <- cbind(df, articles_shipped = apply(data.frame(df)[6], MARGIN=1, FUN=getbasketsize))
	df$month <- format.Date(df$f_shipping_date, "%m")
	df$day <- as.numeric(df$f_shipping_date-max(shipping_table$days))
	df <-  join(df, df[, cbind(getComposition(sales_channel, as.character(month), articles_shipped)), by = id])
	#weekend shifted to the previous Friday
	data.frame(df)
	#df$weekday <- weekdays(df$f_shipping_date)
	
}

#
getreturntime<-function(data){
	# data: weekday | country

	#new countries do not have enough data, that is why we pick NL
	ifelse(sum(returntimetable[returntimetable$weekday==data[1] & returntimetable$shipping_country==data[2],]$Freq)<20,
		sample(1:30,prob = returntimetable[returntimetable$weekday==data[1] & returntimetable$shipping_country=="NL",]$Freq,size = 1 ),
    sample(1:30,prob = returntimetable[returntimetable$weekday==data[1] & returntimetable$shipping_country==data[2],]$Freq,size = 1 ))
}

#pick the returned articles
ss <- function(data) {
	#necessary due to unlikely cases of the lognormal distribution
	inordercg <- sum(data[1:16])
	returnpick <- data[17]
	data[17]<-ifelse(inordercg>data[17], inordercg, data[17])

	sample(x = rep(1:16, data[1:16]), replace = FALSE, size = data[17])
	}

# uses the return probabilities to create threshold in the predictions from the GLMs. 
getreturnestimations <- function(data){
	P.noreturn <- returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "N", ]$Freq
	P.fullreturn <- returnratio[returnratio$Var1 ==paste(year(today()-90),quarters(today()-90) ,sep = " ") & returnratio$Var2 == "F", ]$Freq/ sum(returnratio[returnratio$Var1 ==paste(year(today()),quarters(today()) ,sep = " ") & returnratio$Var2 != "N", ]$Freq)
	predictions <- predict(returnlm3, data, type ="response")
	data$isreturned <- ifelse(predictions <=quantile(predictions, P.noreturn), FALSE, TRUE)
	data$returnclass <- "N"
	predictions <- NULL
	predictions <- predict(returnclasslm, data[data$isreturned==TRUE,], type ="response")
	data[data$isreturned==TRUE,]$returnclass <- ifelse(predictions<=quantile(predictions, P.fullreturn),"F","P")
	data
}
#manual method for scenario mode
getreturnestimations.manual <- function(data, n, f){
	P.noreturn <- n
	P.fullreturn <- f
	predictions <- predict(returnlm3, data, type ="response")
	data$isreturned <- ifelse(predictions <=quantile(predictions, P.noreturn), FALSE, TRUE)
	data$returnclass <- "N"
	predictions <- NULL
	predictions <- predict(returnclasslm, data[data$isreturned==TRUE,], type ="response")
	data[data$isreturned==TRUE,]$returnclass <- ifelse(predictions<=quantile(predictions, P.fullreturn),"F","P")
	data
}

#samples the returned CGs given the orders and their returned articles
getreturnCG <- function(data, meanlog, sdlog){
	samples <- rlnorm(dim(data)[1],meanlog,sdlog)
	samples[samples>1]<-rlnorm(length(samples[samples>1]),meanlog,sdlog) 
	samples[samples>1]<-rlnorm(length(samples[samples>1]),meanlog,sdlog) #veery unlikely that this is also > 1
	data$percentagekept <- samples
	data$percentagekept <- ifelse(data$returnclass =="N",1,ifelse(data$returnclass=="F",0,data$percentagekept))
	data$percentagereturned <- abs(data$percentagekept-1)
	data$articles_returned <- ceiling(data$articles_shipped*data$percentagereturned)
	data$newreturnclass <- ifelse(data$articles_shipped==data$articles_returned & data$returnclass!="N", "F",as.character(data$returnclass))
	data$newreturnclass <- factor(data$newreturnclass)

	data$returnclass <- data$newreturnclass

	data$percentagekept <- NULL
	

	data <- data.frame(data)
	data <- cbind(data, data[10:25]) #add CG columns for returns
	setnames(data, 31:46, gsub("c_", "r_", colnames(data[10:25])))
	data[31:46] = 0
	subsetreturns <- data[data$articles_returned>0,]
	returnindex <- apply(X=subsetreturns[c(10:25,29)] , FUN = ss, MARGIN = 1)

	for(i in 1:dim(subsetreturns)[1]){
		for (j in 1:length(returnindex[[i]])){
			subsetreturns[i,30+returnindex[[i]][j]] <- subsetreturns[i,30+returnindex[[i]][j]]+1
		}
	}
	subsetreturns$newreturnclass <- NULL
	subsetreturns
}

#open return process
getreturndata.open <- function(data, manual=FALSE, prob){
    if(manual==TRUE){
    data<-getreturnestimations.manual(data, prob[1],prob[2])
    	}else{
    data <- getreturnestimations(data)
    }
    data$weekday <- factor(weekdays(data$date_shipped))
    data<- cbind(data, "returntime"=apply(data.frame(data$weekday,data$shipping_country), MARGIN = 1, FUN = getreturntime))
    
    data$isreturned <- ifelse(data$returntime>=data$orderage,TRUE,FALSE)
    data$returnclass <- ifelse(data$isreturned == FALSE, "N", data$returnclass)
    data$returnclass <- factor(data$returnclass)
    subs <- data[,c(1,2,11,11,12,8,18,7,62,20:29,32:33,35:38,67,69)]
     #add CG columns for returns

    subsetreturns<- getreturnCG(subs, -1.3,0.64)

    subs$percentagereturned <- 0
    subs$articles_returned <- 0
	subs <- cbind(subs, subs[10:25])
	setnames(subs, 30:45, gsub("c_", "r_", colnames(subs[10:25])))
	#adding all the columns from getreturnCG

	subs[subs$returnclass!="N",] <- subsetreturns
	subs <- join(subs, data.frame("id" = data$id, "returntime" = data$returntime))
	subs$date_returned <- subs$date_shipped + subs$returntime
	subs

}

#forecast the 
getreturndata.forecast<-function(data,manual=FALSE,prob){
	
	data <- data.frame(data)
	data$age_class <- ifelse(data$age>60, 5, ifelse(data$age>40, 4, ifelse(data$age>30,3,2)))
	#renaming...
	data$customer_age <- data$age
	data$shipping_country <- data$country

	if(manual==TRUE){
    data<-getreturnestimations.manual(data, prob[1],prob[2])
    	}else{
    data <- getreturnestimations(data)
    }
	#cleaning for next method call
	data$isreturned <- NULL
	data$customer_age <- NULL
	data$shipping_country <- NULL

#-------- to sample the returned articles after knowing size and class	
	subsetreturns <- getreturnCG(data, -1.3, 0.64)
	#adding all the columns from getreturnCG
	data$percentagereturned <- 0
	data$articles_returned <- 0
	data <- cbind(data, data[10:25]) #add CG columns for returns
	setnames(data, 30:45, gsub("c_", "r_", colnames(data[10:25])))
#--------
	
	subsetreturns$weekday <- factor(weekdays(subsetreturns$f_shipping_date))
	subsetreturns<- cbind(subsetreturns, "returntime"=apply(data.frame(subsetreturns$weekday,subsetreturns$country), MARGIN = 1, FUN = getreturntime))
	data$weekday_shipped <- factor(NA, levels = c(levels(subsetreturns$weekday), NA))
	data$returntime <- 0
	data[data$returnclass!="N",] <- subsetreturns
	data$f_return_date <- ifelse(data$returntime>0,data$f_shipping_date+data$returntime,NA)
	data$f_return_date <- as.Date(data$f_return_date)
	data$returnclass <- factor(data$returnclass)
	levels(data$returnclass) <- c("F","P","N")
	data
}

#aggregation function
getCGreturnedOnDay <- function(data, date_str){
if(date_str=="date"){
	data.table(data)[, list( 
	returnedCO = sum(returnedCO,na.rm=TRUE),
	returnedArticles = sum(returnedArticles, na.rm=TRUE),
 r_Schuhe = sum(r_Schuhe),
 r_Outdoor = sum(r_Outdoor),
 r_Kleinaccessoires = sum(r_Kleinaccessoires) ,
 r_schlafbade = sum(r_schlafbade),
 r_Konfektion = sum(r_Konfektion),
 r_Schuhpflege = sum(r_Schuhpflege),
 r_tuecherschals = sum(r_tuecherschals),
 r_Guertel = sum(r_Guertel),
 r_Stiefel = sum(r_Stiefel),
 r_KrawattenFliegen = sum(r_KrawattenFliegen),
 r_Oberteile = sum(r_Oberteile),
 r_Handschuhe = sum(r_Handschuhe),
 r_Kopfaccessoires = sum(r_Kopfaccessoires),
 r_Hosen = sum(r_Hosen),
 r_Taschen = sum(r_Taschen),
 r_Unterbekleidung = sum(r_Unterbekleidung)
	), by=date_str]}else{

data.table(data)[, list( 
	returnedCO = length(r_Schuhe),
	returnedArticles = sum(articles_returned),
 r_Schuhe = sum(r_Schuhe),
 r_Outdoor = sum(r_Outdoor),
 r_Kleinaccessoires = sum(r_Kleinaccessoires) ,
 r_schlafbade = sum(r_schlafbade),
 r_Konfektion = sum(r_Konfektion),
 r_Schuhpflege = sum(r_Schuhpflege),
 r_tuecherschals = sum(r_tuecherschals),
 r_Guertel = sum(r_Guertel),
 r_Stiefel = sum(r_Stiefel),
 r_KrawattenFliegen = sum(r_KrawattenFliegen),
 r_Oberteile = sum(r_Oberteile),
 r_Handschuhe = sum(r_Handschuhe),
 r_Kopfaccessoires = sum(r_Kopfaccessoires),
 r_Hosen = sum(r_Hosen),
 r_Taschen = sum(r_Taschen),
 r_Unterbekleidung = sum(r_Unterbekleidung)
	), by=date_str]}
}

#aggregation function
getCGcOnDay <- function(data,datestr){
	#from getfutureboxes
	if (datestr == "f_shipping_date"){
d<-data.table(data)[, list( 
	shippedCO = length(c_Schuhe), #orders on that day
	shippedArticles = sum(articles_shipped), #articles
 c_Schuhe = sum(c_Schuhe),
 c_Outdoor = sum(c_Outdoor),
 c_Kleinaccessoires = sum(c_Kleinaccessoires) ,
 c_schlafbade = sum(c_schlafbade),
 c_Konfektion = sum(c_Konfektion),
 c_Schuhpflege = sum(c_Schuhpflege),
 c_tuecherschals = sum(c_tuecherschals),
 c_Guertel = sum(c_Guertel),
 c_Stiefel = sum(c_Stiefel),
 c_KrawattenFliegen = sum(c_KrawattenFliegen),
 c_Oberteile = sum(c_Oberteile),
 c_Handschuhe = sum(c_Handschuhe),
 c_Kopfaccessoires = sum(c_Kopfaccessoires),
 c_Hosen = sum(c_Hosen),
 c_Taschen = sum(c_Taschen),
 c_Unterbekleidung = sum(c_Unterbekleidung)
	), by=datestr]}
else{
d<-data.table(data)[, list( 
	recievedPO = length(c_Schuhe), #PO partitions on that day
	recievedArticles = sum(q_sum-fq_sum), #articles
 c_Schuhe = sum(c_Schuhe),
 c_Outdoor = sum(c_Outdoor),
 c_Kleinaccessoires = sum(c_Kleinaccessoires) ,
 c_schlafbade = sum(c_schlafbade),
 c_Konfektion = sum(c_Konfektion),
 c_Schuhpflege = sum(c_Schuhpflege),
 c_tuecherschals = sum(c_tuecherschals),
 c_Guertel = sum(c_Guertel),
 c_Stiefel = sum(c_Stiefel),
 c_KrawattenFliegen = sum(c_KrawattenFliegen),
 c_Formalwearaccessories = sum(c_Formalwearaccessories),
 c_cooperations = sum(c_cooperations),
 c_Oberteile = sum(c_Oberteile),
 c_Handschuhe = sum(c_Handschuhe),
 c_Kopfaccessoires = sum(c_Kopfaccessoires),
 c_Hosen = sum(c_Hosen),
 c_Taschen = sum(c_Taschen),
 c_Unterbekleidung = sum(c_Unterbekleidung)
	), by=datestr]
}
	
		d <- data.frame(d)
		d[order(d[,1]), ]

}
#helper method to transpone a data.frame
transpone <- function(data){
    data <- data.frame(data)
    daterow <- colnames(data)[1]
    data <- data[ order(data[,1]), ]
    dates <- as.matrix(data[1])
    newdata <-data.frame(t(as.matrix(data)))
    colnames(newdata)<-dates
    newdata[-1,]
}

#outgoing forecast function that performs the STL / HoltWinters forecast
compareoutgoingforecast.data <- function (h){
#ADJUST
	DEstlforecast <- stlf(x =DEtimeseries, s.window = "periodic", h = h, robust =TRUE, method ="ets", etsmodel ="AAN")
	ATstlforecast <- stlf(x =ATtimeseries, s.window = "periodic", h = h, robust =TRUE, method ="ets", etsmodel ="AAN")
	NLstlforecast <- stlf(x =NLtimeseries, s.window = "periodic", h = h, robust =TRUE, method ="ets", etsmodel ="AAN")
	CHstlforecast <- stlf(x =CHtimeseries, s.window = "periodic", h = h, robust =TRUE, method ="ets", etsmodel ="AAN")
  
	DEholtforecast <- forecast.HoltWinters(HoltWinters(DEtimeseries), h = h)
	ATholtforecast <- forecast.HoltWinters(HoltWinters(ATtimeseries), h = h)
	NLholtforecast <- forecast.HoltWinters(HoltWinters(NLtimeseries), h = h)
	CHholtforecast <- forecast.HoltWinters(HoltWinters(CHtimeseries), h = h)
	DKstlforecast  <- data.frame(matrix(ncol=5, nrow = h,0))
	DKholtforecast  <- data.frame(matrix(ncol=5, nrow = h,0))
	LUstlforecast  <- data.frame(matrix(ncol=5, nrow = h,0))
	LUholtforecast  <- data.frame(matrix(ncol=5, nrow = h,0))
	SEstlforecast <- data.frame(matrix(ncol=5, nrow = h,0))
	SEholtforecast  <- data.frame(matrix(ncol=5, nrow = h,0))
  setnames(DKstlforecast,1:5,c("Point.Forecast",    "Lo.80"  , "Hi.80"   , "Lo.95"    ,"Hi.95"))
  setnames(DKholtforecast,1:5,c("Point.Forecast",    "Lo.80"  , "Hi.80"   , "Lo.95"    ,"Hi.95"))
	setnames(LUstlforecast,1:5,c("Point.Forecast",    "Lo.80"  , "Hi.80"   , "Lo.95"    ,"Hi.95"))
	setnames(LUholtforecast,1:5,c("Point.Forecast",    "Lo.80"  , "Hi.80"   , "Lo.95"    ,"Hi.95"))
	setnames(SEstlforecast,1:5,c("Point.Forecast",    "Lo.80"  , "Hi.80"   , "Lo.95"    ,"Hi.95"))
	setnames(SEholtforecast,1:5,c("Point.Forecast",    "Lo.80"  , "Hi.80"   , "Lo.95"    ,"Hi.95"))
  
  if(length(DKstlforecast)>15){DKstlforecast <- stlf(x =DKtimeseries, s.window = "periodic", h = h, robust =TRUE, method ="ets", etsmodel ="AAN")
                               DKholtforecast <- forecast.HoltWinters(HoltWinters(DKtimeseries), h = h)
                          }
	if(length(LUstlforecast)>15){LUstlforecast <- stlf(x =LUtimeseries, s.window = "periodic", h = h, robust =TRUE, method ="ets", etsmodel ="AAN")
	                             LUholtforecast <- forecast.HoltWinters(HoltWinters(LUtimeseries), h = h)
	                       }
	if(length(SEstlforecast)>15){SEstlforecast <- stlf(x =SEtimeseries, s.window = "periodic", h = h, robust =TRUE, method ="ets", etsmodel ="AAN")
	                             SEholtforecast <- forecast.HoltWinters(HoltWinters(SEtimeseries), h = h)
	                       }

	dfForecast <- rbind( 	data.frame(DEstlforecast, DEholtforecast), 
						 	data.frame(ATstlforecast, ATholtforecast),
 							data.frame(NLstlforecast, NLholtforecast),
							data.frame(CHstlforecast, CHholtforecast),
							data.frame(DKstlforecast, DKholtforecast),
							data.frame(LUstlforecast, LUholtforecast),
							data.frame(SEstlforecast, SEholtforecast)
							)
	colnames(dfForecast) <- c("stl", "stl80l","stl80h","stl95l","stl95h","holt","holt80l","holt80h","holt95l","holt95h")

	#ADJUST NUMBER IN SECOND PARAMETER AS WELL
	dfForecast$day <- rep(1:h, 7)
	dfForecast$country <- c(rep("DE", h), rep("AT", h), rep("NL", h ), rep("CH", h ),rep("DK",h),rep("LU",h),rep("SE",h))
	dfForecast$country <- factor(dfForecast$country)
	dfForecast
}

#decomposition of time series in GGPLOT2
decomp <- function(series,frequency,country){ 
  na.stinterp(series)  -> x   #this is for interpolation 
  ts(x,start=01,freq=frequency) -> x1 #converting into a ts object
  stl(x1,s.window="periodic") -> x2 #performing stl on the ts object
  data.frame(x,x2$time.series) -> x3 #creating a data.frame
  index(x3) -> x3$index #create an id 
  colnames(x3)[1] <- "time-series"  
  dx <- countrystarts[1, country]
  melt(x3,id.vars="index") -> x4   #melt the dataframe into long format  
  ggplot(x4,aes(index,value,group=variable)) + geom_line(alpha=0.7) + facet_grid(variable ~., scales="free")  + xlab("Weeks") + ylab("shipped Boxes") + scale_x_continuous(breaks = seq(1,max(x4$index),7), labels = seq(dx, by ="7 days", length.out = max(x4$index)/7)) +  theme(axis.text.x=element_text(angle = -45, hjust = 0))
}
#decomposition of time series for the last x days. 
decomp2 <- function(series,frequency,country,thelastxdays){ 
 diff <- length(series)-thelastxdays
  dx <- countrystarts[1, country]+diff
  i = 0
  while(as.POSIXlt(dx)$wday != 1){dx <- dx-1
  i = i + 1}
  series <- series[(diff-i):length(series)]
  na.stinterp(series)  -> x   #this is for interpolation 
  ts(x,start=01,freq=frequency) -> x1 #converting into a ts object
  stl(x1,s.window="periodic") -> x2 #performing stl on the ts object
  data.frame(x,x2$time.series) -> x3 #creating a data.frame
  index(x3) -> x3$index #create an id 
  colnames(x3)[1] <- "time-series"  
  melt(x3,id.vars="index") -> x4   #melt the dataframe into long format  
  ggplot(x4,aes(index,value,group=variable)) + geom_line(alpha=0.7) + facet_grid(variable ~., scales="free")  + xlab("Weeks") + ylab("shipped Boxes") + scale_x_continuous(breaks = seq(1,max(x4$index),7), labels = seq(dx, by ="7 days", length.out = max(x4$index)/7)) +  theme(axis.text.x=element_text(angle = -45, hjust = 0))
}
decomp2.data <- function(series, frequency, country, thelastxdays){
 diff <- length(series)-thelastxdays
 dx <- countrystarts[1, country]+diff
 i = 0
 while(as.POSIXlt(dx)$wday != 1){dx <- dx-1
                                 i = i + 1}
 series <- series[(diff-i):length(series)]
 na.stinterp(series)  -> x   #this is for interpolation 
 ts(x,start=01,freq=frequency) -> x1 #converting into a ts object
 stl(x1,s.window="periodic") -> x2 #performing stl on the ts object
 data.frame(x,x2$time.series) -> x3 #creating a data.frame
 index(x3) -> x3$index #create an id 
 colnames(x3)[1] <- "time-series"
 seq(dx, by ="1 days", length.out = max(x3$index)) -> s
 df <- cbind(x3, s)
 data.frame("day" =df$s, "time-series"=df$`time-series`, "seasonal"=df$seasonal, "trend"=df$trend, "remainder"=df$remainder)
 }


#PRESSlm <- function (data) 
#{
#    predictions <- 0
#    for (i in 1:nrow(data)) {
#        
#        newDATA <- data[-i, ]
#   		mod <- lm(formula = w_mean_day_in_window ~ cluster + windowweekday + articles, data = newDATA  )
#        pred <- predict(mod, data[i,])
#        predictions <- c(predictions, pred)
#    }
#    sum((data$w_mean_day_in_window-predictions[-1])^2)
#}
#PRESS
#lm 33083.86
#lm log 33454.92
#lm sqrt 33052.93
#lm sq 33506.87
#tree 38743.29
#tree pruned size = 6 26820.61

#PRESStree <- function (data) 
#{
#    predictions <- 0
#    for (i in 1:nrow(data)) {
#        
#        newDATA <- data[-i, ]
#   		mod <- tree(formula = w_mean_day_in_window ~ cluster + windowweekday + articles, data = newDATA  )
#   		mod <- prune.tree(mod, best = 8)
#        pred <- predict(mod, data[i,])
#        predictions <- c(predictions, pred)
#    }
#    sum((data$w_mean_day_in_window-predictions[-1])^2)
#}


