{#基本设置
    setwd("D:/D/data/kaggle/")
    library(data.table)
    library(dplyr)
    #install.packages("papeR")
    library(papeR)
    library(tidyverse)
    
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

{#td
    td <- fread("df1110.csv",header = T)
    store <- td[,.(skunum=uniqueN(PRODUCT_ID),mfnum=uniqueN(MANUFACTURER),amt=sum(SALES_VALUE),disc=sum(RETAIL_DISC)),keyby=.(STORE_ID)]
    store
    summary(store)
    uniqueN(store$STORE_ID)
    w <- which(store$amt > mean(store$amt))#哪些店铺的销售额大于年销售额均值
    store[w,]
    
    #每个制造商或品牌有在多少店铺铺货
    mf <- td[,.(storenum=uniqueN(STORE_ID)),keyby=.(MANUFACTURER)]
    mf
    summary(mf$storenum)
    w <- which(mf$storenum > mean(mf$storenum))
    mf[w,]
    
    #以mf  2 为例看看哪个店铺的销售额高
    td[MANUFACTURER==2,.(amt=sum(QUANTITY)),keyby=.(STORE_ID)]
    td[MANUFACTURER==2&STORE_ID==45]
    
}

{#df
    store <- df[,.(skunum=uniqueN(PRODUCT_ID),mfnum=uniqueN(MANUFACTURER),amt=sum(SALES_VALUE),disc=sum(RETAIL_DISC)),keyby=.(STORE_ID)]
    store
    summary(store)
    uniqueN(store$STORE_ID)
    w <- which(store$amt > mean(store$amt))#哪些店铺的销售额大于年销售额均值
    store[w,]
    
    #每个制造商或品牌有在多少店铺铺货
    mf <- df[,.(storenum=uniqueN(STORE_ID)),keyby=.(MANUFACTURER)]
    mf
    summary(mf$storenum)
    w <- which(mf$storenum > mean(mf$storenum))
    mf[w,]
    
    #以mf  2 为例看看哪个店铺的销售额高
    df[MANUFACTURER==2,.(amt=sum(QUANTITY)),keyby=.(STORE_ID)]
    df[MANUFACTURER==2&STORE_ID==32269]
}

