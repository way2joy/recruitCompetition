features <- read.csv("features.csv")
dim(features)
summary(features)
fix(features)

stores <- read.csv("stores.csv")
dim(stores)
str(stores)
summary(stores)
boxplot(Size ~ Store, data=stores)
boxplot(Size ~ Type, data=stores)

plot(Type ~ Size, data=stores)
plot(Size ~ Type, data=stores)


submission <- read.csv("sampleSubmission.csv")
fix(submission)
rm(samplesubmission)

train <- read.csv("train.csv")
summary(train)
str(train)
boxplot(Weekly_Sales ~ Store, data=train)
boxplot(Weekly_Sales ~ Dept, data=train, subset=train$Store == 1)
train_1 = train$Store == 1
rm(train_1)

storeno <- unique(train$Store)
summary(storeno)


rb <- boxplot(Weekly_Sales ~ Dept, data=train, subset=train$Store == 1)
yi = tapply(train$Weekly_Sales[train$Store == 1], train$Dept[train$Store == 1], median)
xi = 0.3 + seq(rb$n)

zi = cbind(xi,yi)
crvalue = fivenum(zi[,2])
c1 <- crvalue[[2]][1]
c2 <- crvalue[[4]][1]
zinew1 <- zi[(zi[,2] > c2),]
zinew2 <- zi[(zi[,2] < c1),]

text(zinew1[,1], zinew1[,2], zinew1[,2], cex = 0.5, col = "blue")
text(zinew2[,1], zinew2[,2], zinew2[,2], cex = 0.5, col = "red")


test <- read.csv("test.csv")
head(test)


pdf(file='walmart_dept_sales_each_store_05.pdf',
    onefile=TRUE,
    width = 20,
    height = 15)
for (i in storeno) {
  rb <- boxplot(Weekly_Sales ~ Dept, data=train, subset=train$Store == i,
          main = paste("Walmart depart sales of Store","= (", i,")"),
          xlab = "dept number",
          ylab = "sales amount(?)",
          medcol = "red")
  
  yi = tapply(train$Weekly_Sales[train$Store == i], train$Dept[train$Store == i], median)
  xi = 0.3 + seq(rb$n)
  
  zi = cbind(xi,yi)
  
  c1 <- quantile(zi[,2], 0.05)
  c2 <- quantile(zi[,2], 0.90)
  
  zinew1 <- zi[(zi[,2] > c2),]
  zinew2 <- zi[(zi[,2] < c1),]
  
  text(zinew1[,1], zinew1[,2]*1.05, zinew1[,2], cex = 2.0, col = "blue")
  text(zinew2[,1], zinew2[,2]*0.95, zinew2[,2], cex = 2.0, col = "red")

}
dev.off()


































































