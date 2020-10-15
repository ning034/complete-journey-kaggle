####基础包加载####
{
  setwd("D:/D/data/kaggle/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
}


####导入campaign_desc.csv数据####
{
  campaign_desc<-fread("campaign_desc.csv",header = T,sep=",")
  campaign_desc
  x<-colnames(campaign_desc)
  x<-tolower(x)
  setnames(campaign_desc,x)
  campaign_desc#与类型的描述，涉及开始和结束时间以及campaign
  #问题：start_day与end_day代表什么意思？campaign是活动编号?所谓的类型描述到底是什么？
  table(campaign_desc$campaign)
}

####导入campaign_table.csv数据####
{
  campaign_table<-fread("campaign_table.csv",header = T,sep=",")
  campaign_table
  length(unique(campaign_table$household_key))
  x<-colnames(campaign_table)
  x<-tolower(x)
  setnames(campaign_table,x)
  campaign_table#与household_key有关，涉及他们类型的描述
  table(campaign_table$campaign)
}

####导入causal_data.csv数据####
{
  cd<-fread("causal_data.csv",header = T,sep=",")
  cd
  length(unique(cd$PRODUCT_ID))
  length(unique(cd$STORE_ID))
  unique(cd$display)
  length(unique(cd$WEEK_NO))
  table(cd$mailer)
  table(cd$display)
  x <- cd[,length(PRODUCT_ID),by="display"]
  x <- arrange(x,desc(V1))
  x <- paste(x$display,x$V1,sep="&")
  x <- paste(x,"\\\\",sep="")
  x
  write.csv(x,file="D:/D/data/kaggle/display.csv",row.names = F,quote = F)
  
  x <- cd[,length(PRODUCT_ID),by="mailer"]
  x <- arrange(x,desc(V1))
  x <- paste(x$mailer,x$V1,sep="&")
  x <- paste(x,"\\\\",sep="")
  x
  write.csv(x,file="D:/D/data/kaggle/mailer.csv",row.names = F,quote = F)
  
  table(cd$display,cd$mailer)
  gc()
  x<-colnames(cd)
  x<-tolower(x)
  setnames(cd,x)
  cd#与product_id store_id week_no display mailer有关
}

####导入coupon.csv数据####
{
  coupon<-fread("coupon.csv",header = T,sep=",")
  coupon
 
  x<-colnames(coupon)
  x<-tolower(x)
  setnames(coupon,x)
  coupon#与coupon_upc product_id campaign有关，涉及产品参加活动
  length(unique(coupon$product_id))
  #case,每个产品可用优惠券的数量
  coupon[product_id==27160,]
  x <- coupon[,c(length(unique(coupon_upc))),by="product_id"]
  x
  summary(x$V1)
  
  length(unique(coupon$coupon_upc))
  
  #每个活动发放的优惠券数量
  x <- coupon[,c(length(unique(coupon_upc))),by="campaign"]
  x
  summary(x$V1)
  
  #每个活动覆盖的产品数量
  x <- coupon[,c(length(unique(product_id))),by="campaign"]
  x
  summary(x$V1)
}

####导入coupon_redempt.csv数据####
{
  coupon_redempt<-fread("coupon_redempt.csv",header = T,sep=",")
  coupon_redempt
  x<-colnames(coupon_redempt)
  x<-tolower(x)
  setnames(coupon_redempt,x)
  coupon_redempt#与household_key day  coupon_upc campaign有关
  
  length(unique(coupon_redempt$household_key))
  summary(coupon_redempt$DAY)
  length(unique(coupon_redempt$COUPON_UPC))
  length(coupon_redempt$COUPON_UPC)
  
  #每个活动被兑换优惠券的种类
  x <- coupon_redempt[,c(length(unique(COUPON_UPC))),by="CAMPAIGN"]
  x
  summary(x$V1)
  x <- coupon_redempt[,c(length(COUPON_UPC)),by="CAMPAIGN"]
  x
  summary(x$V1)
  
  
}

####导入hh_demographic.csv数据####
{
  hh_demographic<-fread("hh_demographic.csv",header = T,sep=",")
  hh_demographic
  x<-colnames(hh_demographic)
  x<-tolower(x)
  setnames(hh_demographic,x)
  hh_demographic#与household_key有关，涉及他们的人口统计学变量，共801个household
  
  length(unique(hh_demographic$age_desc))
  unique(hh_demographic$age_desc)
  
  unique(hh_demographic$marital_status_code)
  hh_demographic[,c(unique(marital_status_code)),by="hh_comp_desc"]
  
  x <- unique(hh_demographic$income_desc)
  sort(x)
  
  unique(hh_demographic$homeowner_desc)
  
  unique(hh_demographic$hh_comp_desc)
  
  unique(hh_demographic$household_size_desc)
  
  unique(hh_demographic$kid_category_desc)
  
  table(hh_demographic$kid_category_desc)
  
  length(unique(hh_demographic$household_key))
}

####导入transaction_data.csv数据####
{
td<-fread("transaction_data.csv",header = T,sep=",")
td
#x<-colnames(td)
#x<-tolower(x)
#setnames(td,x)
td
summary(td)#具体的交易信息

length(unique(td$household_key))

length(unique(td$BASKET_ID))
x <- td[,c(length(unique(BASKET_ID))),by="household_key"]
summary(x$V1)

length(unique(td$DAY))
summary(td$DAY)

length(unique(td$PRODUCT_ID))
x <- unique(coupon$product_id)
y <- unique(td$PRODUCT_ID)
z <- intersect(x,y)


summary(td$QUANTITY)
quantile(td$QUANTITY,0.992)
w <- which(td$QUANTITY > 4568)
td[w,]#极端值严重，需要清洗

summary(td$SALES_VALUE)

length(unique(td$STORE_ID))

summary(td$RETAIL_DISC)
ww <- which(td$RETAIL_DISC > 0)

summary(td$TRANS_TIME)

length(unique(td$WEEK_NO))
summary(td$WEEK_NO)

summary(td$COUPON_DISC)

summary(td$COUPON_MATCH_DISC)

{#本科论文
  pd <- td[,c(.(qty=sum(QUANTITY)),.(mprice=mean(SALES_VALUE-RETAIL_DISC-COUPON_MATCH_DISC))),by=c("WEEK_NO","PRODUCT_ID")]
  pd
  num <- as.data.frame(table(pd$PRODUCT_ID))
  num <- arrange(num,desc(Freq))
  head(num)
  test <- subset(pd,PRODUCT_ID==840361)
  test
  lm1 <- lm(test$qty~test$mprice)
  summary(lm1)
}
}

####导入product.csv数据####
{
product<-fread("product.csv",header = T,sep=",")
product
#x<-colnames(product)
#x<-tolower(x)
#setnames(product,x)
#product$department<-tolower(product$department)
#product$commodity_desc<-tolower(product$commodity_desc)
#product$sub_commodity_desc<-tolower(product$sub_commodity_desc)
product

length(unique(product$PRODUCT_ID))

length(unique(product$MANUFACTURER))

length(unique(product$DEPARTMENT))
table(product$BRAND)

length(unique(product$COMMODITY_DESC))
length(unique(product$SUB_COMMODITY_DESC))

length(unique(product$CURR_SIZE_OF_PRODUCT))
w <- which(product$CURR_SIZE_OF_PRODUCT=="")
test <- product[-w,]
length(unique(test$PRODUCT_ID))
}





