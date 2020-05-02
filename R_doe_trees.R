### DOE - Decision trees

if (!require("pacman")){
  install.packages("pacman")
  require("pacman")
} 
pacman::p_load(tree)

### https://www.datacamp.com/community/tutorials/decision-trees-R
### https://www.statmethods.net/advstats/cart.html

### https://www.tutorialspoint.com/r/r_decision_tree.htm


##### Random Forests
### https://towardsdatascience.com/random-forest-in-r-f66adf80ec9
### https://cran.r-project.org/web/packages/randomForest/randomForest.pdf

pacman::p_load(randomForest, caTools)
gwr_sb_df <- read.csv("doe-imagestacks/gwr_sb.csv")
gwr_sb_df <- as.data.frame(gwr_sb_df)
gwr_sb_stack <- stack("doe-imagestacks/gwr_sb_stack.gri")
# def2 <- gwr_sb_stack[["Deformation"]]
# def2[def2>-20]<-0
# def2[def2<=-20]<-1
gwr_sb_df$geot <- (gwr_sb_df$Deformation<=-20)
gwr_sb_df$geot <- as.factor(gwr_sb_df$geot)
gwr_sb_df$Geothermal <- as.factor(gwr_sb_df$Geothermal)
names(gwr_sb_df)[1] <- "num"

sapply(gwr_sb_df, class)
summary(gwr_sb_df)

### Should be zero for each column (no NA values)
colSums(is.na(gwr_sb_df))
data <- gwr_sb_df[c("Minerals", "Temperature", "Faults", "geot")]
sample = sample.split(data$geot, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
rf<- randomForest(geot ~ ., data = train)
pred = predict(rf, newdata=test[-4])
summary(rf)
cm = table(test[,4], pred)
print("Confusion Matrix")
print(cm)
# partialPlot(rf, data, Minerals )
# partialPlot(rf, data, Temperature )
# partialPlot(rf, data, Faults )

### https://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree


