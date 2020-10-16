

{
  td<-fread("transaction_data.csv",header = T,sep=",")
  td
  
  
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
  sku_share <- df[,c(.(store_amt=sum(SALES_VALUE)),.(sku=unique(PRODUCT_ID))),by=c("STORE_ID","quarter")]
  sku_share
  
  
}