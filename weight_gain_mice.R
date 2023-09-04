
## reading the table
library(Matrix)
mice = read.table("magentic_field.txt", head=TRUE)

## retrieving a summary of the data
summary(mice)
dim(mice)
names(mice)

## retrieving first 5 rows
head(mice)

## getting first 10 rows of weight gain
mice$Weight_gain[1:10]

## instead of getting first 10 rows, data may not be cleaned so retrieving group number
mice$Weight_gain[mice$Group == 1]

## retrieving mice in group number 2
mice$Weight_gain[mice$Group == 2]

## creating a histogram of the mice group 1 
hist(mice$Weight_gain[mice$Group == 1], xlab = 'Weight Gain', main = 'Group 1 Weight Gain')

## retrieving the median for the group 1 mice
sort(mice$Weight_gain)
median(mice$Weight_gain[mice$Group == 1])

## retrieving the median for group 2 mice
sort(mice$Weight_gain[mice$Group == 2])
median(mice$weight_gain[mice$Group == 2])

## retrieving the mean for group 1 mice
mean(mice$Weight_gain[mice$Group == 1])

## mean for group 2 mice
mean(mice$Weight_gain[mice$Group == 2])

## plotting a histogram for group 2 mice weight gain
hist(mice$Weight_gain[mice$Group == 2], xlab = 'Weight Gain', main = 'Group 2 Weight Gain')

## only printing the weight gains that are above 20 for Group 1 because this is most relevant
for(i in mice$Weight_gain[mice$Group == 1]) {
  if(i >= 20) {
    print(i)
  } 
}

## only printing weight gains that are less than 10 to see the difference between 1 and 2
for(z in mice$Weight_gain[mice$Group == 2]) {
  if(z >= 10) {
    cat(z, '')
  }
}

## plotting two for loop line plots side by side to see how they compare against
par(mfrow=c(1,2))
plot(i, type = 'l', lwd = 3, col = 'magenta')
plot(z, type = 'l', lwd = 3, col = 'cyan')

## getting min and max for each group
which.min(mice$Weight_gain[mice$Group == 1])
which.max(mice$Weight_gain[mice$Group == 1])
which.min(mice$Weight_gain[mice$Group == 2])
which.max(mice$Weight_gain[mice$Group == 2])

## plotting line plots side by side for groups 1 and 2
## we see that group 2 has more spikes so the weight gain is plenty more than group 1
plot(mice$Weight_gain[mice$Group == 1], type = "l", ylab = 'Weight Gain', xlab = 'Each Cage', col = 'blue')
plot(mice$Weight_gain[mice$Group == 2], type = "l", ylab = 'Weight Gain', xlab = 'Each Cage', col = 'purple')

## comparing the min and max for each of the groups
Group1Vec <- c(mice$Weight_gain[mice$Group == 1], min, max)
Group2Vec <- c(mice$Weight_gain[mice$Group == 2], min, max)

## plotting the vectors side by side
par(mfrow = c(1,2))
plot(Group1Vec, type = 'l', lwd = 3, col = 'navy')
plot(Group2Vec, type = 'l', lwd = 3, col = 'brown')

## creating matrices for Groups 1 and 2 and seeing how they look like
Group1Matrix <- matrix(mice$Weight_gain[mice$Group == 1], 4, 5)
Group2Matrix <- matrix(mice$Weight_gain[mice$Group == 2], 4, 5)

## looking at the matrix tables for each of the control/treatment group
t(Group1Matrix)
t(Group2Matrix)

## min and max of the group 1 matrix. Also, interquartile range for group 1
min(Group1Matrix)
IQR(mice$Weight_gain[mice$Group == 1])
max(Group1Matrix)

## min and max of the group 2 matrix. Also, interquartile range for group 2
min(Group2Matrix)
IQR(mice$Weight_gain[mice$Group == 2])
max(Group2Matrix)

## plotting the two matrices
plot(Group1Matrix, type = 'l', lwd = 3, col = 'red')
plot(Group2Matrix, type = 'l', lwd = 3, col = 'gold')

## interested in these values in the matrix and applying mean for group 1
t(Group1Matrix)
Group1Matrix[2:3, 3]
Group1Matrix[3:4, 4]
group1mean <- apply(Group1Matrix, 1, mean)

## finding the average for first and second columns of group 2
t(Group2Matrix)
group2mean <- apply(Group2Matrix, 1, mean)

## seeing histogram vs. line plot for group 1 and 2 mean
par(mfrow = c(2,2))
hist(group1mean, xlab = 'Weight Gain', main = 'Group 1 Mean', col = 'black')
hist(group2mean, xlab = 'Weight Gain', main = 'Group 2 Mean', col = 'pink')
plot(group1mean, type = 'l', lwd = 3, col = 'blue')
plot(group2mean, type = 'l', lwd = 3, col = 'red')


## comparing the first rows of Group 1 and 2
y <- c(Group1Matrix[, 1])
print(y)
x <- c(Group2Matrix[, 1])
print(x)
plot(x, xlab = 'Mice', ylab = 'Weight Gain', main = 'Group 1 and 2 1st Row', col = 'blue', cex = 2)
points(y, col = 'red', cex = 2)

## getting the quantile of groups 1 and 2
quantile(mice$Weight_gain[mice$Group == 1])
quantile(mice$Weight_gain[mice$Group == 2])

# Lastly, creating pie charts for each of the groups to complete visualization

## pie chart for group 1
label <- c("Cage1", "Cage2", "Cage3", "Cage4", "Cage5", "Cage6", "Cage7", "Cage8", "Cage9", "Cage10")
diffcolors <- c('blue', 'red', 'pink', 'gold', 'green', 'magenta', 'cyan', 'brown','black')
pie(mice$Weight_gain[mice$Group == 1], label, init.angle = 90)

##pie chart for group 2
label <- c("Cage1", "Cage2", "Cage3", "Cage4", "Cage5", "Cage6", "Cage7", "Cage8", "Cage9", "Cage10")
pie(mice$Weight_gain[mice$Group == 2], label, init.angle = 90)

