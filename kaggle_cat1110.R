{#��������
    setwd("D:/D/data/kaggle/")
    library(data.table)
    library(dplyr)
    #install.packages("papeR")
    library(papeR)
    library(tidyverse)
    
    td <- fread("df1110.csv",header = T)
    #��Ҫ���ĸ����̵�ĳ��Ʒ������������á�
    #������������˵��Ʒ��ѡ����ȥ�̻��Ŀ����Ծ͸���
    #����Ʒ����˵��ѡ���ĸ�����ȥ�̻���Ч�����ܾ͸��á�
    
    #�����̴���Щ�Ƕ�ѡ���̵�ȥ�̻��أ������̴���Щ�Ƕ�����Ʒ���أ�
    
    #վ�������̵ĽǶȣ�
    #���ȣ���ֱ�Ӿ��ǿ�ԭ��������ĳ��Ʒ����������������Ϊ�����ָ������жϵ�����������Ⱥ���Ŷ���Ŀ��ͻ���
    #������������ѡ��2������������ĵ��̽����̻���
    #����һ����������Ӫ�����ж��ĸ����̸��á�����Щָ������жϣ�
    #��Ϊ��������򣬺���ָ����Ӳ�Ի���ָ�꣬����ָ�������������ʣ�
    #����ָ�꣺Ʒ��������۶�;
    #����ָ�꣺�������ۿۡ��Ż�ȯ�ۿۣ�
    #����ָ�꣺Ʒ���ܡ��¡��������۶������ʱ仯��
    
    #վ�������̽Ƕȣ�Ū�����ʲô����Ӱ����������ָ�꣬��ô���Ƕ������������顣
    #����ܿ��Ƶ�����������λ�á��ܱ��������������������ѡַ��������ô�ͷ��������̾����ĶԱ��ˡ�
    #����ͼ������۶����������֮����������һ����ɸѡ�������������̣��������������̵����ڲ�Ӱ��Ʒ�����۵����ء�
    #������ǿ�����̵ĺ��ľ�������������Ʒ�Ƶ����ۣ����������Լ������档�൱�ڸ��Լ��������������
    
    #����Ͷ�룬ÿ�������ӵ�������ǣ�Ϊʲô�ء�������Ϊ�������������𣿾���������Ϊʲô�����أ�����˵�Էݶ�ûɶӰ����
}

{#ѡ��Ʒ���Ʒ��
    catsize <- td[,c(.(amt=sum(SALES_VALUE)),.(sku_num=length(unique(PRODUCT_ID)))),by="COMMODITY_DESC"]#SUB_COMMODITY_DESC  COMMODITY_DESC  DEPARTMENT
    catsize <- catsize[order(amt,decreasing = T),]
    catsize[1:30,]
    catname <- catsize$COMMODITY_DESC[1:3]
    3 / uniqueN(catsize$COMMODITY_DESC)#0.9%
    sum(catsize$amt[1:3]) / sum(catsize$amt)#11%
    
    
    #ȡ����:ѡƷ��
    df <- subset(td,COMMODITY_DESC %in% catname)
    df
    
    #ѡƷ��:69
    mf <- df[,.(amt=sum(SALES_VALUE),qty=sum(QUANTITY)),by=.(MANUFACTURER)]
    mf[order(amt,decreasing = T),]#69
    mf[order(qty,decreasing = T),]#69
}

{#����mf 69
    n <- 103
    mf69 <- df[MANUFACTURER==n,]
    mf69
    unique(mf69$COMMODITY_DESC)
    mf69[,sum(SALES_VALUE),by=.(COMMODITY_DESC)]#ѡ��SOFT DRINKS
    
    #so
    mf69 <- df[MANUFACTURER==n&COMMODITY_DESC=="SOFT DRINKS",]
    mf69
    a <- lapply(mf69,uniqueN)
    a
    
    #������������69���̻����̵ı仯
    store <- mf69[,.(store_num=uniqueN(STORE_ID),sku_num=uniqueN(PRODUCT_ID),amt=sum(SALES_VALUE),qty=sum(QUANTITY),r_disc =sum(RETAIL_DISC),m_disc=sum(COUPON_DISC)),by=.(year)]
    store
    uniqueN(mf69$STORE_ID)#310
    
    s1 <- unique(mf69[year==1,]$STORE_ID)#243
    s2 <- unique(mf69[year==2,]$STORE_ID)#230
    x <- setdiff(s1,s2)#�ڵڶ��겻���̻��ĵ���
    
    ##�鿴������̵�������۶�
    amt <- df[year==1,.(sum=sum(SALES_VALUE)/uniqueN(DAY)),by=.(STORE_ID)]
    amt$index <- "good"
    w <- which(amt$STORE_ID %in% x)
    amt$index[w] <- "bad"
    amt
    amt[,mean(sum),by=.(index)]
    amt[-w,]
    amt[w,]
    
    #���ڵ����⣺
    #�����̵ڶ��겻���̻��ĵ����У���һЩ����������ȼ����̻���������á�
    #��Ȼ����Щ������ͨ����������ֻ�и�λ�����Ҳ²�����Ϊ������Ϣ��ȫ�����Կ�������Щ����������������Ϣ���¡�
    #��ˣ���һ������Ҫ������Ʒ����������ϲ���޳���(��һ��ʼ���޳�)��ֻ�������������Ϊȫ������ݣ�
    #�������������������̻����̵�ԭ��
}