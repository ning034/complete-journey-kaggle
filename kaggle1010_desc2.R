####����������####
{
  setwd("D:/D/data/kaggle/")
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(papeR)
}

####����campaign_desc.csv����####
{
  campaign_desc<-fread("campaign_desc.csv",header = T,sep=",")
  campaign_desc
  x<-colnames(campaign_desc)
  x<-tolower(x)
  setnames(campaign_desc,x)
  campaign_desc#�����͵��������漰��ʼ�ͽ���ʱ���Լ�campaign
  #���⣺start_day��end_day����ʲô��˼��campaign�ǻ���?��ν����������������ʲô��
  table(campaign_desc$campaign)
  
  #ÿ�����͵Ļ����
  table(campaign_desc$description) #5-19-6
  
  #ÿ�����ͻ�Ŀ�ʼʱ�䡢����ʱ�䡢����ʱ��
  campaign_desc$duration <- campaign_desc$end_day - campaign_desc$start_day
  summary(campaign_desc$duration)
  
  #����ͼ
  campaign_desc$description <- as.factor(campaign_desc$description)
  par(mfrow=c(1,3))
  boxplot(start_day ~ description,data=campaign_desc,xlab="�����",ylab="���ʼʱ��(��)")
  boxplot(end_day ~ description,data=campaign_desc,xlab="�����",ylab="�����ʱ��(��)")
  boxplot(duration ~ description,data=campaign_desc,xlab="�����",ylab="�����ʱ��(��)")
  dev.off()
}


####����campaign_table.csv����####
{
  campaign_table<-fread("campaign_table.csv",header = T,sep=",")
  campaign_table
  length(unique(campaign_table$household_key))#ʵ���м�¼��1584����ͥ�μ��˻
  x<-colnames(campaign_table)
  x<-tolower(x)
  setnames(campaign_table,x)
  campaign_table#��household_key�йأ��漰�������͵�����
  table(campaign_table$campaign)
  campaign_table$description <- as.factor(campaign_table$description)
  
  #ÿ����ͥ�μӵĻ����
  num_house <- campaign_table %>% group_by(household_key) %>% summarise(num=length(campaign))
  num_house
  summary(num_house$num)
  hist(num_house$num,breaks = seq(0.5,20,by=1),xlab="ÿ����ͥ����������(��)",ylab="Ƶ��",main="")
  
  #ÿ�����ͻ�����ͥ�ĸ���
  type <- as.data.frame(table(campaign_table$description))
  ggplot(type,aes(Var1,Freq))+geom_bar(stat="identity",fill="#000002")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="�����",y="�����ͥ�ĸ���")
  #barplot(num_house$num)
  #barplot(campaign_table$campaign)
}

####����causal_data.csv����####
{
  cd<-fread("causal_data.csv",header = T,sep=",")#����
  cd
  
  #չʾ���ʼ������в�Ʒ������
  display <- cd %>% group_by(display) %>% summarise(product_num=length(unique(PRODUCT_ID)))
  display <- arrange(display,desc(product_num))
  display
  sd(display$product_num)
  
  mailer <- cd %>% group_by(mailer) %>% summarise(product_num=length(unique(PRODUCT_ID)))
  mailer <- arrange(mailer,desc(product_num))
  mailer
  ggplot(display,aes(display,product_num))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="display����",y="��Ʒ����")
  ggplot(mailer,aes(mailer,product_num))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="mailer����",y="��Ʒ����")
  
  #ÿ�������в���������
  week_no <- cd %>% group_by(STORE_ID) %>% summarise(week_num=length(unique(WEEK_NO)))
  week_no <- arrange(week_no,desc(week_num))
  week_no
  summary(week_no$week_num)
  unique(week_no$week_num)
    #Ϊ����ֱ��ͼ�����ǩֵ
  week_no$level <- cut(week_no$week_num,breaks = seq(0,100,by=10))
  week_no
  num <- table(week_no$level)
  num
    #ֱ��ͼ��ֵ��ǩ
  hist(week_no$week_num,labels = as.character(num),xlab="����",ylab = "��������",main = "")
  table(week_no$week_num)
}

####����coupon.csv����####
{
  coupon<-fread("coupon.csv",header = T,sep=",")
  coupon
  
  x<-colnames(coupon)
  x<-tolower(x)
  setnames(coupon,x)
  coupon#��coupon_upc product_id campaign�йأ��漰��Ʒ�μӻ
  length(unique(coupon$product_id))#44133����Ʒ����cd����������һ�¡�˵���еĲ�Ʒ��Ȼ�����˴������������һ�����Ż�ȯ���͡�
  
  #case,ÿ����Ʒ�����Ż�ȯ������
  coupon[product_id==27160,]
  x <- coupon[,c(length(unique(coupon_upc))),by="product_id"]
  x
  summary(x$V1)
  
  #ÿ���Ż�ȯ���õĲ�Ʒ����
  x <- coupon[,c(length(unique(product_id))),by="coupon_upc"]
  x
  summary(x$V1)
  
  length(unique(coupon$coupon_upc))
  
  #ÿ������ŵ��Ż�ȯ��������
  x <- coupon[,c(length(unique(coupon_upc))),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  p1 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign���",y="�Ż�ȯ����")
  p1
  
  #ÿ������ǵĲ�Ʒ��������
  x <- coupon[,c(length(unique(product_id))),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  x$V1 <- x$V1/100
  p2 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign���",y="��Ʒ����(*100)")
  p2
  
  #install.packages("cowplot")
  #library(cowplot)
  plot_grid(p1,p2,ncol=1)
  
  #ÿ������ŵ��Ż�ȯ����
  x <- coupon[,c(length(coupon_upc)),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  x
  p1 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign���",y="�Ż�ȯ����")
  p1
  
  #ÿ������ǵĲ�Ʒ����
  x <- coupon[,c(length(product_id)),by="campaign"]
  x
  summary(x$V1)
  x <- arrange(x,V1)
  x
  p2 <- ggplot(x,aes(as.factor(campaign),V1))+geom_bar(stat="identity")+theme_bw()+theme(axis.text  =element_text(colour ="black"),panel.grid = element_blank())+labs(x="campaign���",y="��Ʒ����")
  p2
  
  #install.packages("cowplot")
  #library(cowplot)
  plot_grid(p1,p2,ncol=1)
}

####����coupon_redempt.csv����####
{
  coupon_redempt<-fread("coupon_redempt.csv",header = T,sep=",")
  coupon_redempt
  x<-colnames(coupon_redempt)
  x<-tolower(x)
  setnames(coupon_redempt,x)
  coupon_redempt#��household_key day  coupon_upc campaign�й�
  
  length(unique(coupon_redempt$household_key))
  summary(coupon_redempt$DAY)
  length(unique(coupon_redempt$COUPON_UPC))
  length(coupon_redempt$COUPON_UPC)
  
  #��campaign_desc����
  campaign_desc
  coupon_redempt
  coupon_redempt <- left_join(coupon_redempt,campaign_desc,by="campaign")
  head(coupon_redempt)
  
  #��֤coupon_redempt�е��Ż�ȯ�һ��������Ƿ��ڻ���ڡ���ȫ������
  x <- coupon_redempt$day >= coupon_redempt$start_day & coupon_redempt$day <= coupon_redempt$end_day
  table(x)
  nrow(coupon_redempt)
}

####����hh_demographic.csv����####
{
  hh_demographic<-fread("hh_demographic.csv",header = T,sep=",")
  hh_demographic
  x<-colnames(hh_demographic)
  x<-tolower(x)
  setnames(hh_demographic,x)
  hh_demographic#��household_key�йأ��漰���ǵ��˿�ͳ��ѧ��������801��household
  
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
  
  #����������
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
  
  #�����뷿������Ȩ����
    #��ȡ������Сֵ
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


####����transaction_data.csv����####
{
  td<-fread("transaction_data.csv",header = T,sep=",")
  td
  #x<-colnames(td)
  #x<-tolower(x)
  #setnames(td,x)
  td
  summary(td)#����Ľ�����Ϣ
  
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
  td[w,]#����ֵ���أ���Ҫ��ϴ
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