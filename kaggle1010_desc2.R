####基础包加载####
{
  setwd("D:/D/data/kaggle/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(papeR)
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
  
  #每种类型的活动数量
  table(campaign_desc$description) #5-19-6
  
  #每种类型活动的开始时间、结束时间、持续时间
  campaign_desc$duration <- campaign_desc$end_day - campaign_desc$start_day
  summary(campaign_desc$duration)
  
  #箱型图
  campaign_desc$description <- as.factor(campaign_desc$description)
  par(mfrow=c(1,3))
  boxplot(start_day ~ description,data=campaign_desc,xlab="活动类型",ylab="活动开始时间(天)")
  boxplot(end_day ~ description,data=campaign_desc,xlab="活动类型",ylab="活动结束时间(天)")
  boxplot(duration ~ description,data=campaign_desc,xlab="活动类型",ylab="活动持续时间(天)")
  dev.off()
}


####导入campaign_table.csv数据####
{
  campaign_table<-fread("campaign_table.csv",header = T,sep=",")
  campaign_table
  length(unique(campaign_table$household_key))#实际中记录了1584个家庭参加了活动
  x<-colnames(campaign_table)
  x<-tolower(x)
  setnames(campaign_table,x)
  campaign_table#与household_key有关，涉及他们类型的描述
  table(campaign_table$campaign)
  campaign_table$description <- as.factor(campaign_table$description)
  
  #每个家庭参加的活动个数
  num_house <- campaign_table %>% group_by(household_key) %>% summarise(num=length(campaign))
  num_house
  summary(num_house$num)
  hist(num_house$num,breaks = seq(0.5,20,by=1),xlab="每个家庭参与活动的数量(个)",ylab="频数",main="")
  
  #每种类型活动参与家庭的个数
  type <- as.data.frame(table(campaign_table$description))
  ggplot(type,aes(Var1,Freq))+geom_bar(stat="identity",fill="#000002")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="活动类型",y="参与家庭的个数")
  #barplot(num_house$num)
  #barplot(campaign_table$campaign)
}

####导入causal_data.csv数据####
{
  cd<-fread("causal_data.csv",header = T,sep=",")#促销
  cd
  
  #展示与邮寄类型中产品的数量
  display <- cd %>% group_by(display) %>% summarise(product_num=length(unique(PRODUCT_ID)))
  display <- arrange(display,desc(product_num))
  display
  sd(display$product_num)
  
  mailer <- cd %>% group_by(mailer) %>% summarise(product_num=length(unique(PRODUCT_ID)))
  mailer <- arrange(mailer,desc(product_num))
  mailer
  ggplot(display,aes(display,product_num))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="display类型",y="产品数量")
  ggplot(mailer,aes(mailer,product_num))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="mailer类型",y="产品数量")
  
  #每个店铺中参与活动的周数
  week_no <- cd %>% group_by(STORE_ID) %>% summarise(week_num=length(unique(WEEK_NO)))
  week_no <- arrange(week_no,desc(week_num))
  week_no
  summary(week_no$week_num)
  unique(week_no$week_num)
    #为周数直方图计算标签值
  week_no$level <- cut(week_no$week_num,breaks = seq(0,100,by=10))
  week_no
  num <- table(week_no$level)
  num
    #直方图赋值标签
  hist(week_no$week_num,labels = as.character(num),xlab="周数",ylab = "店铺数量",main = "")
  table(week_no$week_num)
}

####导入coupon.csv数据####
{
  coupon<-fread("coupon.csv",header = T,sep=",")
  coupon
  
  x<-colnames(coupon)
  x<-tolower(x)
  setnames(coupon,x)
  coupon#与coupon_upc product_id campaign有关，涉及产品参加活动
  length(unique(coupon$product_id))#44133个产品，与cd中数量并不一致。说明有的产品虽然参与了促销活动，但并不一定有优惠券发送。
  
  #case,每个产品可用优惠券的数量
  coupon[product_id==27160,]
  x <- coupon[,c(length(unique(coupon_upc))),by="product_id"]
  x
  summary(x$V1)
  
  #每个优惠券可用的产品数量
  x <- coupon[,c(length(unique(product_id))),by="coupon_upc"]
  x
  summary(x$V1)
  
  length(unique(coupon$coupon_upc))
  
  #每个活动发放的优惠券种类数量
  x <- coupon[,c(length(unique(coupon_upc))),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  p1 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign编号",y="优惠券种类")
  p1
  
  #每个活动覆盖的产品种类数量
  x <- coupon[,c(length(unique(product_id))),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  x$V1 <- x$V1/100
  p2 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign编号",y="产品种类(*100)")
  p2
  
  #install.packages("cowplot")
  #library(cowplot)
  plot_grid(p1,p2,ncol=1)
  
  #每个活动发放的优惠券数量
  x <- coupon[,c(length(coupon_upc)),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  x
  p1 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign编号",y="优惠券数量")
  p1
  
  #每个活动覆盖的产品数量
  x <- coupon[,c(length(product_id)),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  x
  p2 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign编号",y="产品数量")
  p2
  
  #install.packages("cowplot")
  #library(cowplot)
  plot_grid(p1,p2,ncol=1)
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
  
  #与campaign_desc连接
  campaign_desc
  coupon_redempt
  coupon_redempt <- left_join(coupon_redempt,campaign_desc,by="campaign")
  head(coupon_redempt)
  
  #验证coupon_redempt中的优惠券兑换天数，是否在活动期内——全部都在
  x <- coupon_redempt$day >= coupon_redempt$start_day & coupon_redempt$day <= coupon_redempt$end_day
  table(x)
  nrow(coupon_redempt)
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
  
  #年龄与收入
  age_income <- table(hh_demographic$age_desc,hh_demographic$income_desc)
  age_income <- as.matrix(age_income)
  age_income

  sum_col <- apply(age_income,2,sum)
  sum_col <- matrix(sum_col,nrow=1)
  sum_col
  age_income <- rbind(age_income,sum_col)
  age_income
  
  sum_row <- apply(age_income,1,sum)
  sum_row <- as.matrix(sum_row)
  sum_row
  age_income <- cbind(age_income,sum_row)
  age_income
  
  xtable(age_income)
  
  #收入与房屋所有权比例
    #提取收入最小值
  hh_demographic
  w <- regexpr("-",hh_demographic$income_desc)
  hh_demographic$income <- substr(hh_demographic$income_desc,1,w-1)
  hh_demographic$income <- as.numeric(hh_demographic$income)
  w <- which(hh_demographic$income_desc=="Under 15K")
  hh_demographic$income[w] <- 15
  hh_demographic
  
  
  income_home <- table(hh_demographic$income_desc,hh_demographic$homeowner_desc)
  x <- apply(income_home,1,sum)
  x
  y <- income_home[,1] / x 
  y <- as.data.frame(y)
  y$income_desc <- row.names(y)
  y
  
  hh_demographic <- left_join(hh_demographic,y,by="income_desc")
  hh_demographic
  lm1 <- lm(hh_demographic$y~hh_demographic$income)
  model1 <- summary(lm1)
  xtable(model1)
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
  boxplot(td$QUANTITY)
  
  
  summary(td$SALES_VALUE)
  
  length(unique(td$STORE_ID))
  
  summary(td$RETAIL_DISC)
  ww <- which(td$RETAIL_DISC > 0)
  
  summary(td$TRANS_TIME)
  
  length(unique(td$WEEK_NO))
  summary(td$WEEK_NO)
  
  summary(td$COUPON_DISC)
  
  summary(td$COUPON_MATCH_DISC)
