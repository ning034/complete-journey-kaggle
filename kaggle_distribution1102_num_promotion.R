{#基本设置
    setwd("D:/D/data/kaggle/")
    library(data.table)
    library(dplyr)
    #install.packages("papeR")
    library(papeR)
    library(tidyverse)
}

{#交易数据
    td<-fread("transaction_data.csv",header = T,sep=",")
    td
    gc()
    
    product<-fread("product.csv",header = T,sep=",")
    product
    length(unique(product$DEPARTMENT))
    unique(product$DEPARTMENT)
    
    #连接品类
    td <- left_join(td,product,by="PRODUCT_ID")
    td <- as.data.table(td)
    td#2595732
    
    #数据清洗-剔除极值
    summary(td$QUANTITY)#有过高
    quantile(td$QUANTITY,0.99)
    td <- subset(td,QUANTITY < 10)
    summary(td$SALES_VALUE)
    summary(td$RETAIL_DISC)#有正值
    td <- subset(td,RETAIL_DISC <= 0)#2568071，剔除了约1%的数据
    td
    summary(td$COUPON_MATCH_DISC)
    
    #销售额最高的品类
    td
    catsize <- td[,c(.(amt=sum(SALES_VALUE)),.(sku_num=length(unique(PRODUCT_ID)))),by="COMMODITY_DESC"]#SUB_COMMODITY_DESC  COMMODITY_DESC  DEPARTMENT
    catsize <- catsize[order(amt,decreasing = T),]
    catsize[1:30,]
    catname <- catsize$COMMODITY_DESC[1:30]
    
    #计算季度与年度
    td$quarter <- cut(td$WEEK_NO,breaks = 8,labels =1:8 )
    td$quarter <- as.character(td$quarter)
    td$quarter <- as.numeric(td$quarter)
    td$year <- 1
    w <- which(td$quarter > 4)
    td$year[w] <- 2
    td
    td$quarter <- paste("q",td$quarter,sep = "" )
    td
    
    #取数据
    df <- subset(td,COMMODITY_DESC %in% catname)
    df
    
    #计算份额
    cat_amt <- df[,c(.(amt=sum(SALES_VALUE)),.(PRODUCT_ID=unique(PRODUCT_ID))),by=c("quarter","COMMODITY_DESC")]
    cat_amt
    sku_amt <- df[,c(.(sku_amt=sum(SALES_VALUE))),by=c("quarter","PRODUCT_ID")]
    sku_amt
    sku_share <- left_join(sku_amt,cat_amt,by=c("quarter","PRODUCT_ID"))
    sku_share$share <- 100 * sku_share$sku_amt / sku_share$amt
    sku_share
    
    #计算铺货
    store_amt <- df[,c(.(store_amt=sum(SALES_VALUE)),.(PRODUCT_ID=unique(PRODUCT_ID))),by=c("STORE_ID","quarter")]
    store_amt
    sku_distribution <- store_amt[,c(.(sum_amt=sum(store_amt))),by=c("quarter","PRODUCT_ID")]
    sku_distribution
    year_amt <- df[,c(.(quarteramt=sum(SALES_VALUE))),by="quarter"]
    year_amt
    sku_distribution <- left_join(sku_distribution,year_amt,by="quarter")
    sku_distribution$sku_dis <- 100 * sku_distribution$sum_amt / sku_distribution$quarteramt
    sku_distribution
    summary(sku_distribution$sku_dis)
    
    #回归数据
    lm_data <- left_join(sku_distribution,sku_share,by=c("quarter","PRODUCT_ID"))
    lm_data
    lm_data$sku_dis2 <- lm_data$sku_dis ^ 2
    lm_data
    #
    
}


#接lm_data未季度平均之前
{#计算sku促销次数-只看零售折扣
    df
    #num_pro <- df[,.(num=uniqueN(DAY),sum=sum(RETAIL_DISC)),keyby=.(PRODUCT_ID,quarter,discount=ifelse(RETAIL_DISC<0,"Y","N"))]
    num_pro <- df[,.(retail_disc=sum(RETAIL_DISC),coupon_disc=sum(COUPON_MATCH_DISC)),keyby=.(PRODUCT_ID,quarter)]
    num_pro$retail_disc <- (-1) * num_pro$retail_disc
    num_pro$coupon_disc <- (-1) * num_pro$coupon_disc
    num_pro
    
    lm_data <- left_join(lm_data,num_pro,by=c("PRODUCT_ID","quarter"))
    lm_data
    
    #test
    model0 <- lm(lm_data$share ~ lm_data$sku_dis + lm_data$sku_dis2 + lm_data$num + lm_data$sum)
    summary(model0)#显示次数越多份额越高；但加入折扣总额的时候，发现折扣次数越多份额越低，但折扣额度越高份额越高。
    #说明，最好不要有太多次折扣。
    
    library(car)
    vif(model0)
    
    
    #回归数据求季度平均
    lm_data$year <- 1
    w <- which(lm_data$quarter %in% c("q5","q6","q7","q8"))
    lm_data$year[w] <- 2
    lm_data <- as.data.table(lm_data)
    new_data <- lm_data[,c(.(meandis=mean(sku_dis)),.(meanshare=mean(share)),.(mean_disnum=mean(num)),.(mean_dissum=mean(sum)),.(COMMODITY_DESC=unique(COMMODITY_DESC))),by=c("year","PRODUCT_ID")]
    new_data
    new_data$meandis2 <- new_data$meandis ^ 2
    new_data
    #write.csv(new_data,file="D:/D/data/kaggle/model_data.csv",row.names = F)
    
    #test
    model0 <- lm(new_data$meanshare ~ new_data$meandis + new_data$meandis2 + new_data$mean_disnum + new_data$mean_dissum)
    summary(model0)
    vif(model0)
    
    x <-prettify(summary(model0))
    x
    xtable(x,digits = 4)
    
    #数据集描述
    df
    catdesc <- df[,.(num_brand=uniqueN(MANUFACTURER),num_sku=uniqueN(PRODUCT_ID),sumrev=sum(SALES_VALUE)),by="COMMODITY_DESC"]
    gc()
    catdesc$sumrev <- catdesc$sumrev / 10000
    catdesc$sumrev <- round(catdesc$sumrev,2)
    catdesc
    
    lm_data
    catdesc2 <- lm_data[,.(sku_ms_avg=mean(share),sku_ms_med=median(share),sku_ms_max=max(share),retaildis_num_avg=mean(num),retaildis_num_med=median(num),retaildis_num_max=max(num)),by="COMMODITY_DESC"]
    catdesc2
    catdesc2$sku_ms_avg <- round(catdesc2$sku_ms_avg,2)
    catdesc2$sku_ms_med <- round(catdesc2$sku_ms_med,2)
    catdesc2$sku_ms_max <- round(catdesc2$sku_ms_max,2)
    catdesc2$retaildis_num_avg <- round(catdesc2$retaildis_num_avg,0)
    catdesc2
    
    newcatdesc <- left_join(catdesc,catdesc2,by="COMMODITY_DESC")
    newcatdesc
    xtable(newcatdesc)
}
