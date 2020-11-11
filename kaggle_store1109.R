{#��������
    setwd("D:/D/data/kaggle/")
    library(data.table)
    library(dplyr)
    #install.packages("papeR")
    library(papeR)
    library(tidyverse)
    
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

{#td
    td <- fread("df1110.csv",header = T)
    store <- td[,.(skunum=uniqueN(PRODUCT_ID),mfnum=uniqueN(MANUFACTURER),amt=sum(SALES_VALUE),disc=sum(RETAIL_DISC)),keyby=.(STORE_ID)]
    store
    summary(store)
    uniqueN(store$STORE_ID)
    w <- which(store$amt > mean(store$amt))#��Щ���̵����۶���������۶��ֵ
    store[w,]
    
    #ÿ�������̻�Ʒ�����ڶ��ٵ����̻�
    mf <- td[,.(storenum=uniqueN(STORE_ID)),keyby=.(MANUFACTURER)]
    mf
    summary(mf$storenum)
    w <- which(mf$storenum > mean(mf$storenum))
    mf[w,]
    
    #��mf  2 Ϊ�������ĸ����̵����۶��
    td[MANUFACTURER==2,.(amt=sum(QUANTITY)),keyby=.(STORE_ID)]
    td[MANUFACTURER==2&STORE_ID==45]
    
}

{#df
    store <- df[,.(skunum=uniqueN(PRODUCT_ID),mfnum=uniqueN(MANUFACTURER),amt=sum(SALES_VALUE),disc=sum(RETAIL_DISC)),keyby=.(STORE_ID)]
    store
    summary(store)
    uniqueN(store$STORE_ID)
    w <- which(store$amt > mean(store$amt))#��Щ���̵����۶���������۶��ֵ
    store[w,]
    
    #ÿ�������̻�Ʒ�����ڶ��ٵ����̻�
    mf <- df[,.(storenum=uniqueN(STORE_ID)),keyby=.(MANUFACTURER)]
    mf
    summary(mf$storenum)
    w <- which(mf$storenum > mean(mf$storenum))
    mf[w,]
    
    #��mf  2 Ϊ�������ĸ����̵����۶��
    df[MANUFACTURER==2,.(amt=sum(QUANTITY)),keyby=.(STORE_ID)]
    df[MANUFACTURER==2&STORE_ID==32269]
}
