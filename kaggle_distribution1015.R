{#基本设置
    setwd("D:/D/data/kaggle/")
    library(data.table)
    library(dplyr)
    #install.packages("papeR")
    library(papeR)
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
    
    #test
    model0 <- lm(lm_data$share ~ lm_data$sku_dis + lm_data$sku_dis2)
    summary(model0)
    
    #回归数据求季度平均
    lm_data$year <- 1
    w <- which(lm_data$quarter %in% c("q5","q6","q7","q8"))
    lm_data$year[w] <- 2
    lm_data <- as.data.table(lm_data)
    new_data <- lm_data[,c(.(meandis=mean(sku_dis)),.(meanshare=mean(share)),.(COMMODITY_DESC=unique(COMMODITY_DESC))),by=c("year","PRODUCT_ID")]
    new_data
    new_data$meandis2 <- new_data$meandis ^ 2
    new_data
    #write.csv(new_data,file="D:/D/data/kaggle/model_data.csv",row.names = F)
}

{#一般参数模型#####
    model1 <- lm(new_data$meanshare ~ new_data$meandis + new_data$meandis2)
    summary(model1)
}

{#品类模型####
    
    #连接品类
    new_data
    length(unique(new_data$PRODUCT_ID))#24065
    cat_pro <- df[,c(.(PRODUCT_ID=unique(PRODUCT_ID))),by="COMMODITY_DESC"]
    length(unique(cat_pro$PRODUCT_ID))#24065
    new_data <- left_join(new_data,cat_pro,by="PRODUCT_ID")
    new_data
    
    #构建解释变量矩阵(0-1)-30个品类，2个解释变量，共60个解释变量
    m <- nrow(new_data)
    x <- matrix(0,m,60)
    x <- as.data.table(x)
    y <-c(x,new_data)
    y <- as.data.table(y)
    head(y)
    x <- as.matrix(x)#将x转化为矩阵是因为data.table结构的数据，无法用DT[,i]取得列变量
    j <-seq(1,60,by=2)
    catname <- unique(new_data$COMMODITY_DESC)
    catname <- rep(catname,each=2)
    for(i in j){
        w <- which(y$COMMODITY_DESC==catname[i])
        x[w , i] <- 1
        x[, i+1] <- x[, i]
        x[, i] <- x[, i] * y$meandis
        x[, i+1] <- x[, i+1] * y$meandis2
    }
    
    #回归
    model2 <- lm(new_data$meanshare ~ x)#问题：平方项与一次项的相关性，造成的回归共线性的问题，怎么理解？
    summary(model2) 
    
    #输出结果
    j <-seq(1,60,by=2)
    result <- c()
    result1 <- c()
    zz <- prettify(summary(model2),signif.stars = getOption("show.signif.stars"))#用到显著性的星号
    zz <- zz[-1,]
    for(i in j){
        result1 <- data.frame(cat=catname[i],acv=zz$Estimate[i],t_value=zz$`t value`[i],acv2=zz$Estimate[i+1],t_value2=zz$`t value`[i+1],star1=zz$`   `[i],star2=zz$`   `[i+1],row.names = NULL)
        result <- rbind(result,result1)
    }
    result
    result$acv <-round(result$acv,5)
    result$acv2 <- round(result$acv2,5)
    result$t_value <- round(result$t_value,1)
    result$t_value2 <- round(result$t_value2,1)
    result
    
    output2 <- result
    output2$acv2 <- format(output2$acv2,scientific = F)
    output2$acv <- paste(output2$acv,output2$star1,sep="")
    output2$acv2 <- paste(output2$acv2,output2$star2,sep="")
    output2
    output2 <- output2[,-c(6,7)]#注意acv2有的系数是科学计数法。
    xtable(output2)#输出为latex
    write.csv(output2,file = "D:/D/data/kaggle/output2.csv",row.names = F) #输出为table
    
}

{#品类特征模型####
    #算按年度算季度平均，还是直接算年度品类特征指标？
    
    #连接品牌-制造商
    new_data
    #mf <- df[,c(.(MANUFACTURER=unique(MANUFACTURER))),by=c("COMMODITY_DESC","PRODUCT_ID")]
    #mf
    #cat_data <- left_join(new_data,mf,by=c("COMMODITY_DESC","PRODUCT_ID"))
    
    
    #HHI-level
    #份额计算
    share_m <- df[,c(.(amt=sum(SALES_VALUE)),.(COMMODITY_DESC=unique(COMMODITY_DESC))),by=c("MANUFACTURER","year")]
    share_m <- share_m %>% group_by(year) %>% mutate(amt_year=sum(amt))
    share_m$share <- share_m$amt / share_m$amt_year
    share_m$share2 <- share_m$share ^ 2
    share_m
    
    #HHI计算
    hhi <- select(share_m,c(1,2,4,7))
    hhi <- as.data.table(hhi)
    hhi <- hhi[,c(.(hhi=sum(share2))),by=c("year","COMMODITY_DESC")]
    hhi$hhi <- 100 * hhi$hhi
    hhi
    
    #连接品类特征
    cat_data <- left_join(new_data,hhi,by=c("year","COMMODITY_DESC"))
    cat_data
    
    #计算品类规模
    catsize <- df[,c(.(amt=sum(SALES_VALUE))),by=c("year","COMMODITY_DESC")]
    catsize
    catsize$size <- ifelse(catsize$amt > quantile(catsize$amt,0.8),"large","medium")
    w <- which(catsize$amt < quantile(catsize$amt,0.15))
    catsize$size[w] <- "small"
    catsize <- catsize[,c(1,2,4)]
    
    #连接品类规模
    cat_data <- left_join(cat_data,catsize,by=c("year","COMMODITY_DESC"))
    cat_data
    
    #建立size的三个虚拟变量
    cat_data$large <- ifelse(cat_data$size=="large",1,0)
    cat_data$medium <- ifelse(cat_data$size=="medium",1,0)
    cat_data$small <- ifelse(cat_data$size=="small",1,0)
    
    
    #自定义品类
    catname #前面定义的前30的品类
    tolower(catname)
    food <- catname[c(2,4,5,6,8,9,11,13,14,15,18,21,27,30)]
    drink <- catname[c(1,3,7,10,16,17,19,20,23,24,25,29)]
    baby <- catname[28]
    household <- catname[26]
    other <- catname[c(12,22)]
    
    #建立自定义品类的虚拟变量
    cat_data$food <- ifelse(cat_data$COMMODITY_DESC %in% food,1,0)
    cat_data$drink <- ifelse(cat_data$COMMODITY_DESC %in% drink,1,0)
    cat_data$baby <- ifelse(cat_data$COMMODITY_DESC %in% baby,1,0)
    cat_data$household <- ifelse(cat_data$COMMODITY_DESC %in% household,1,0)
    cat_data$other <- ifelse(cat_data$COMMODITY_DESC %in% other,1,0)
    cat_data
    
    #回归数据
    acv <- cat_data$meandis * cat_data[,c(7,9:10,12:15)]#去除一个虚拟变量11和16
    acv <- as.matrix(acv)
    acv2 <- cat_data$meandis2 * cat_data[,c(7,9:10,12:15)]
    acv2 <- as.matrix(acv2)
    maineffect <- cat_data[,c(7,9:10,12:15)]
    maineffect <- as.matrix(maineffect)
    
    #回归
    model3 <- lm(cat_data$meanshare ~ maineffect + acv + acv2 )
    summary(model3)
    
    #结果输出
    zz <- prettify(summary(model3))
    zz <- zz[-1,]
    
    #结果处理
    zz$Estimate <- round(zz$Estimate,digits = 5)
    zz$Estimate <- format(zz$Estimate,scientific = F)
    zz$`t value` <- round(zz$`t value`,digits = 1)
    zz$`t value` <- format(zz$`t value`,scientific = F)
    zz$Estimate <- paste(zz$Estimate,zz$`   `,sep="")
    
    output3 <- data.frame(char=c("hhi","large","medium","food","drink","baby","household"),maineffect=zz$Estimate[1:7],tvalue=zz$`t value`[1:7],acv=zz$Estimate[8:14],tvalue1=zz$`t value`[8:14],acv2=zz$Estimate[15:21],tvalue2=zz$`t value`[15:21])
    output3
    write.csv(output3,file = "D:/D/data/kaggle/output3.csv",row.names = F)
    xtable(output3)
}


{#品牌模型####
    
    #连接品牌#即制造商
    new_data
    
}


{#伪R^2计算
    
}