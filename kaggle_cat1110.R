{#基本设置
    setwd("D:/D/data/kaggle/")
    library(data.table)
    library(dplyr)
    #install.packages("papeR")
    library(papeR)
    library(tidyverse)
    
    td <- fread("df1110.csv",header = T)
    #想要看哪个店铺的某个品类销售情况更好。
    #对于零售商来说，品牌选择它去铺货的可能性就更大。
    #对于品牌来说，选择哪个店铺去铺货的效果可能就更好。
    
    #制造商从哪些角度选择商店去铺货呢？零售商从哪些角度吸引品牌呢？
    
    #站在制造商的角度：
    #首先，最直接就是看原来店铺在某个品类的销售情况——因为看这个指标可以判断店铺中消费者群体大概都算目标客户。
    #接下来，假如选择2家销售情况差不多的店铺进行铺货。
    #经过一年或两年的运营，再判断哪个店铺更好。有哪些指标可以判断？
    #分为横向和纵向，横向指的是硬性汇总指标，纵向指的是销售增长率；
    #横向指标：品牌年度销售额;
    #控制指标：零售商折扣、优惠券折扣；
    #纵向指标：品牌周、月、季度销售额增长率变化；
    
    #站在零售商角度：弄清楚是什么因素影响横向和纵向指标，那么就是对它有利的事情。
    #如果能控制店铺所处地理位置、周边消费者收入情况等外在选址条件。那么就分析零售商竞争的对比了。
    #否则就假设销售额差不多的零售商之间外在条件一样。筛选出这样的零售商，再来分析零售商店铺内部影响品牌销售的因素。
    #帮助增强零售商的核心竞争力。增加了品牌的销售，就是增加自己的利益。相当于给自己增加溢价能力！
    
    #促销投入，每年是增加的吗？如果是，为什么呢——是因为竞争对手增加吗？竞争对手又为什么增加呢？不是说对份额没啥影响吗？
}

{#选择品类和品牌
    catsize <- td[,c(.(amt=sum(SALES_VALUE)),.(sku_num=length(unique(PRODUCT_ID)))),by="COMMODITY_DESC"]#SUB_COMMODITY_DESC  COMMODITY_DESC  DEPARTMENT
    catsize <- catsize[order(amt,decreasing = T),]
    catsize[1:30,]
    catname <- catsize$COMMODITY_DESC[1:3]
    3 / uniqueN(catsize$COMMODITY_DESC)#0.9%
    sum(catsize$amt[1:3]) / sum(catsize$amt)#11%
    
    
    #取数据:选品类
    df <- subset(td,COMMODITY_DESC %in% catname)
    df
    
    #选品牌:69
    mf <- df[,.(amt=sum(SALES_VALUE),qty=sum(QUANTITY)),by=.(MANUFACTURER)]
    mf[order(amt,decreasing = T),]#69
    mf[order(qty,decreasing = T),]#69
}

{#分析mf 69
    n <- 103
    mf69 <- df[MANUFACTURER==n,]
    mf69
    unique(mf69$COMMODITY_DESC)
    mf69[,sum(SALES_VALUE),by=.(COMMODITY_DESC)]#选择SOFT DRINKS
    
    #so
    mf69 <- df[MANUFACTURER==n&COMMODITY_DESC=="SOFT DRINKS",]
    mf69
    a <- lapply(mf69,uniqueN)
    a
    
    #看看两年来，69号铺货店铺的变化
    store <- mf69[,.(store_num=uniqueN(STORE_ID),sku_num=uniqueN(PRODUCT_ID),amt=sum(SALES_VALUE),qty=sum(QUANTITY),r_disc =sum(RETAIL_DISC),m_disc=sum(COUPON_DISC)),by=.(year)]
    store
    uniqueN(mf69$STORE_ID)#310
    
    s1 <- unique(mf69[year==1,]$STORE_ID)#243
    s2 <- unique(mf69[year==2,]$STORE_ID)#230
    x <- setdiff(s1,s2)#在第二年不再铺货的店铺
    
    ##查看两类店铺的年度销售额
    amt <- df[year==1,.(sum=sum(SALES_VALUE)/uniqueN(DAY)),by=.(STORE_ID)]
    amt$index <- "good"
    w <- which(amt$STORE_ID %in% x)
    amt$index[w] <- "bad"
    amt
    amt[,mean(sum),by=.(index)]
    amt[-w,]
    amt[w,]
    
    #现在的问题：
    #制造商第二年不再铺货的店铺中，有一些的销售情况比继续铺货的情况还好。
    #当然，这些数据中通常销售天数只有个位数。我猜测是因为数据信息不全。所以看不到这些店铺完整的销售信息导致。
    #因此，下一步，需要将店铺品类销售情况较差的剔除掉(在一开始就剔除)，只保留销售情况较为全面的数据，
    #再来分析两年中制造商换店铺的原因。
}