# Set the working directory to this file's location.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Install and load necessary add-on packages.
if (!require(carData)) {
  install.packages("carData")
  library(carData)
} else {
  library(carData)
}
if (!require(car)) {
  install.packages("car")
  library(carData)
} else {
  library(car)
}


# Set the seed for R's random number generator for replicability of results.
set.seed(1)
# Remove scientific notation in plots.
options(scipen = 10)


#~~~~~ DATA ~~~~~#

# Specify the folder path to where the data are located.
data.path = "data/raw/"
# Read in the data file names.
filenames <- list.files(path = data.path, full.names = FALSE)

# Create an empty list to contain each data frame corresponding to each data set.
m183dat.lst <- list()
# Create data frames for each data file and store them in the list. Some of the data has some leading/trailing
# white-space, so we use the strip.white option to deal with this. Otherwise, this causes issues with the data
# analysis (e.g., 'f' =/= 'f ', etc.).
for (i in 1:length(filenames)) {
        m183dat.lst[[i]] <- read.csv(paste(data.path, filenames[i], sep = ""), header = TRUE, strip.white = TRUE, na.strings = c("","NA"))
}

# Create a new column in each data frame recording the teaching period.
m183dat.lst[[1]]$TeachingPeriod <- "S1/2003"
m183dat.lst[[2]]$TeachingPeriod <- "S2/2003"
m183dat.lst[[3]]$TeachingPeriod <- "S2/2004"
m183dat.lst[[4]]$TeachingPeriod <- "S2/2005"

# Merge the individual data frames into a single data frame. Now that we have a column specifying the teaching
# period, we can maintain 'groups' re: teaching period.
m183dat.df <- rbind(m183dat.lst[[1]], m183dat.lst[[2]], m183dat.lst[[3]], m183dat.lst[[4]])


#~~~~~ Summary Assessment ~~~~~#

# Produce a summary of height by sex for the data overall. This also reports the number of NA values for height
# observations in the data set.
m183height.summary <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max", "NA's"))
m183height.summary <- as.data.frame(do.call(rbind, tapply(m183dat.df[, 2], m183dat.df[, 1], summary)))
row.names(m183height.summary) <- c("Female", "Male")

#~~~~~ Height vs. Teaching Period for each Sex ~~~~~#

# Create a new data frame which contains only the data for which there are no NA values in height and gender
# columns. That is, this omits all NA values from the original data frame and preserves original row numbers.
m183height.df.rm <- m183dat.df[!is.na(m183dat.df[, 2]) & !is.na(m183dat.df[, 1]), ]
# Remove the clearly extreme value from the data whose height entry is 18.
m183height.df.rm <- m183height.df.rm[!(m183height.df.rm[, 2]) == 18, ]
# Re-number rows for the cleaned data frame so that they are properly ordinal.
row.names(m183height.df.rm) <- 1:nrow(m183height.df.rm)

# Produce a summary of height by teaching period for females and store it in a data frame.
m183heightf.summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
m183heightf.summary <- as.data.frame(do.call(rbind, tapply(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f'], m183height.df.rm[, 18][m183height.df.rm[, 1] == 'f'], summary)))
row.names(m183heightf.summary) <- c("S1/2003", "S2/2003", "S2/2004", "S2/2005")
# Produce a summary of height by teaching period for males and store it in a data frame.
m183heightm.summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
m183heightm.summary <- as.data.frame(do.call(rbind, tapply(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm'], m183height.df.rm[, 18][m183height.df.rm[, 1] == 'm'], summary)))
row.names(m183heightm.summary) <- c("S1/2003", "S2/2003", "S2/2004", "S2/2005")

par(cex.axis = 0.7, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(2,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create box-plots for height grouped by teaching period for females.
boxplot(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f'] ~ as.factor(m183height.df.rm[, 18][m183height.df.rm[, 1] == 'f']),
        xlab = "", ylab = "",
        ylim = c(140, 195),
        at = c(1,2,3,4), col = grey.colors(4, start = 0.25, alpha = 0.5),
        names = c("S1/03", "S2/03", "S2/04", "S2/05"), horizontal = TRUE)
title(main = "", xlab = "Height", ylab = "Teaching Period", line = 2)
# Create overlaid density plots for height grouped by teaching period for females.
plot(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S1/2003"]),
     main = "", xlab = "", ylab = "",
     xlim = c(140, 195), ylim = c(0,0.1),
     col = "red")
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2003"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 195), ylim = c(0,0.1),
      col = "blue")
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2004"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 195), ylim = c(0,0.1),
      col = "black")
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2005"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 195), ylim = c(0,0.1),
      col = "purple")
legend("topleft", c("S1/03", "S2/03", "S2/04", "S2/05"), fill = c("red", "blue", "black", "purple"), bty = "n")
title(main = "", xlab = "Height", ylab = "Density", line = 2)
mtext(text = "Height by Teaching Period for Females", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Create box-plots for height grouped by teaching period for males.
boxplot(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm'] ~ as.factor(m183height.df.rm[, 18][m183height.df.rm[, 1] == 'm']),
        xlab = "", ylab = "",
        ylim = c(140, 210),
        at = c(1,2,3,4), col = grey.colors(4, start = 0.25, alpha = 0.5),
        names = c("S1/03", "S2/03", "S2/04", "S2/05"), horizontal = TRUE)
title(main = "", xlab = "Height", ylab = "Teaching Period", line = 2)
# Create overlaid density plots for height grouped by teaching period for males.
plot(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S1/2003"]),
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0,0.1),
     col = "red")
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2003"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 210), ylim = c(0,0.1),
      col = "blue")
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2004"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 210), ylim = c(0,0.1),
      col = "black")
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2005"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 210), ylim = c(0,0.1),
      col = "purple")
legend("topleft", c("S1/03", "S2/03", "S2/04", "S2/05"), fill = c("red", "blue", "black", "purple"), bty = "n")
title(main = "", xlab = "Height", ylab = "Density", line = 2)
mtext(text = "Height by Teaching Period for Males", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)


#~~~~~ Height vs. Sex for each Teaching Period ~~~~~#

# Create a new data frame which contains only the data for which there are no NA values in height and gender
# columns. That is, this omits all NA values from the original data frame and preserves original row numbers.
m183height.df.rm <- m183dat.df[!is.na(m183dat.df[, 2]) & !is.na(m183dat.df[, 1]), ]
# Remove the clearly extreme value from the data whose height entry is 18.
m183height.df.rm <- m183height.df.rm[!(m183height.df.rm[, 2]) == 18, ]
# Re-number rows for the cleaned data frame so that they are properly ordinal.
row.names(m183height.df.rm) <- 1:nrow(m183height.df.rm)

# Produce a summary of height by sex for S1/2003 and store it in a data frame.
m183height2003s1.summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
m183height2003s1.summary <- as.data.frame(do.call(rbind, tapply(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S1/2003"], m183height.df.rm[, 1][m183height.df.rm[, 18] == "S1/2003"], summary)))
row.names(m183height2003s1.summary) <- c("Female", "Male")
# Produce a summary of height by sex for S2/2003 and store it in a data frame.
m183height2003s2.summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
m183height2003s2.summary <- as.data.frame(do.call(rbind, tapply(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S2/2003"], m183height.df.rm[, 1][m183height.df.rm[, 18] == "S2/2003"], summary)))
row.names(m183height2003s2.summary) <- c("Female", "Male")
# Produce a summary of height by sex for S2/2004 and store it in a data frame.
m183height2004s2.summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
m183height2004s2.summary <- as.data.frame(do.call(rbind, tapply(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S2/2004"], m183height.df.rm[, 1][m183height.df.rm[, 18] == "S2/2004"], summary)))
row.names(m183height2004s2.summary) <- c("Female", "Male")
# Produce a summary of height by sex for S2/2005 and store it in a data frame.
m183height2005s2.summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
m183height2005s2.summary <- as.data.frame(do.call(rbind, tapply(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S2/2005"], m183height.df.rm[, 1][m183height.df.rm[, 18] == "S2/2005"], summary)))
row.names(m183height2005s2.summary) <- c("Female", "Male")

par(cex.axis = 0.7, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(3,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create box-plots for height grouped by sex for S1/2003.
boxplot(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S1/2003"] ~ as.factor(m183height.df.rm[, 1][m183height.df.rm[, 18] == "S1/2003"]),
        xlab = "", ylab = "",
        ylim = c(140, 210),
        at = c(1,2), col = grey.colors(2, start = 0.5, alpha = 0.5),
        names = c("Female", "Male"), horizontal = TRUE)
title(main = "", xlab = "Height", ylab = "Sex", line = 2)
# Create overlaid histograms for height grouped by sex for S1/2003.
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S1/2003"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S1/2003"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Frequency", line = 2)
box()
# Create overlaid density plots for height grouped by sex for S1/2003.
plot(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S1/2003"]),
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S1/2003"]),
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S1/2003"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S1/2003"]),
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Density", line = 2)
mtext(text = "Height by Sex for Semester 1, 2003", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Create box-plots for height grouped by sex for S2/2003.
boxplot(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S2/2003"] ~ as.factor(m183height.df.rm[, 1][m183height.df.rm[, 18] == "S2/2003"]),
        xlab = "", ylab = "",
        ylim = c(140, 210),
        at = c(1,2), col = grey.colors(2, start = 0.5, alpha = 0.5),
        names = c("Female", "Male"), horizontal = TRUE)
title(main = "", xlab = "Height", ylab = "Sex", line = 2)
# Create overlaid histograms for height grouped by sex for S2/2003.
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2003"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2003"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Frequency", line = 2)
box()
# Create overlaid density plots for height grouped by sex for S2/2003.
plot(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2003"]),
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2003"]),
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2003"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2003"]),
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Density", line = 2)
mtext(text = "Height by Sex for Semester 2, 2003", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Create box-plots for height grouped by sex for S2/2004.
boxplot(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S2/2004"] ~ as.factor(m183height.df.rm[, 1][m183height.df.rm[, 18] == "S2/2004"]),
        xlab = "", ylab = "",
        ylim = c(140, 210),
        at = c(1,2), col = grey.colors(2, start = 0.5, alpha = 0.5),
        names = c("Female", "Male"), horizontal = TRUE)
title(main = "", xlab = "Height", ylab = "Sex", line = 2)
# Create overlaid histograms for height grouped by sex for S2/2004.
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2004"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2004"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Frequency", line = 2)
box()
# Create overlaid density plots for height grouped by sex for S2/2004.
plot(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2004"]),
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[,1] == 'f' & m183height.df.rm[, 18] == "S2/2004"]),
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2004"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2004"]),
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Density", line = 2)
mtext(text = "Height by Sex for Semester 2, 2004", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Create box-plots for height grouped by sex for S2/2005.
boxplot(m183height.df.rm[, 2][m183height.df.rm[, 18] == "S2/2005"] ~ as.factor(m183height.df.rm[, 1][m183height.df.rm[, 18] == "S2/2005"]),
        xlab = "", ylab = "",
        ylim = c(140, 210),
        at = c(1,2), col = grey.colors(2, start = 0.5, alpha = 0.5),
        names = c("Female", "Male"), horizontal = TRUE)
title(main = "", xlab = "Height", ylab = "Sex", line = 2)
# Create overlaid histograms for height grouped by sex for S2/2005.
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2005"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
hist(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2005"],
     breaks = 10,
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0, 35),
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), add = TRUE)
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Frequency", line = 2)
box()
# Create overlaid density plots for height grouped by sex for S2/2005.
plot(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2005"]),
     main = "", xlab = "", ylab = "",
     xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'f' & m183height.df.rm[, 18] == "S2/2005"]),
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
lines(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2005"]),
      main = "", xlab = "", ylab = "",
      xlim = c(140, 210), ylim = c(0,0.1))
polygon(density(m183height.df.rm[, 2][m183height.df.rm[, 1] == 'm' & m183height.df.rm[, 18] == "S2/2005"]),
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
legend("topleft", c("Female", "Male"), fill = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.5), rgb(red = 1, green = 0, blue = 0, alpha = 0.5)), bty = "n")
title(main = "", xlab = "Height", ylab = "Density", line = 2)
mtext(text = "Height by Sex for Semester 2, 2005", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)


#~~~~~ Dominant Hand vs. Dominant Eye ~~~~~#

# Create a new data frame which contains only the data for which there are no NA values in dominant hand and
# dominant eye columns. That is, this omits all NA values from the original data frame and preserves original
# row numbers.
m183dom.df.rm <- m183dat.df[!is.na(m183dat.df[, 5]) & !is.na(m183dat.df[, 17]), ]
# Remove the ambiguous factors. These are factor levels for which we cannot immediately discern the appropriate 
# classification. Better to get rid of them and report than attempt to make some assumption.
m183dom.df.rm <- m183dom.df.rm[!(m183dom.df.rm[, 17]) == 'y', ]
# Re-number rows for the cleaned data frame so that they are properly ordinal.
row.names(m183dom.df.rm) <- 1:nrow(m183dom.df.rm)

# Produce a contingency table for frequency counts of dominant hand by dominant eye across all teaching periods.
m183dom.table <- table(factor(m183dom.df.rm[, 17], labels = c("Left", "Right")), factor(m183dom.df.rm[, 5], labels = c("Left", "Right")),
                       dnn = c("Dominant Eye", "Dominant Hand"))
# Produce a contingency table for frequency counts of dominant hand by dominant eye for S1/2003.
m183dom2003s1.table <- table(factor(m183dom.df.rm[, 17][m183dom.df.rm[, 18] == "S1/2003"], labels = c("Left", "Right")), factor(m183dom.df.rm[, 5][m183dom.df.rm[, 18] == "S1/2003"], labels = c("Left", "Right")),
                             dnn = c("Dominant Eye", "Dominant Hand"))
# Produce a contingency table for frequency counts of dominant hand by dominant eye for S2/2003.
m183dom2003s2.table <- table(factor(m183dom.df.rm[, 17][m183dom.df.rm[, 18] == "S2/2003"], labels = c("Left", "Right")), factor(m183dom.df.rm[, 5][m183dom.df.rm[, 18] == "S2/2003"], labels = c("Left", "Right")),
                             dnn = c("Dominant Eye", "Dominant Hand"))
# Produce a contingency table for frequency counts of dominant hand by dominant eye for S1/2003.
m183dom2004s2.table <- table(factor(m183dom.df.rm[, 17][m183dom.df.rm[, 18] == "S2/2004"], labels = c("Left", "Right")), factor(m183dom.df.rm[, 5][m183dom.df.rm[, 18] == "S2/2004"], labels = c("Left", "Right")),
                             dnn = c("Dominant Eye", "Dominant Hand"))
# Produce a contingency table for frequency counts of dominant hand by dominant eye for S1/2003.
m183dom2005s2.table <- table(factor(m183dom.df.rm[, 17][m183dom.df.rm[, 18] == "S2/2005"], labels = c("Left", "Right")), factor(m183dom.df.rm[, 5][m183dom.df.rm[, 18] == "S2/2005"], labels = c("Left", "Right")),
                             dnn = c("Dominant Eye", "Dominant Hand"))

# Produce proportions for frequency counts of dominant hand by dominant eye across all teaching periods.
m183dom.prop.table <- prop.table(m183dom.table)
# Produce proportions for frequency counts of dominant hand by dominant eye for S1/2003.
m183dom2003s1.prop.table <- prop.table(m183dom2003s1.table)
# Produce proportions for frequency counts of dominant hand by dominant eye for S2/2003.
m183dom2003s2.prop.table <- prop.table(m183dom2003s2.table)
# Produce proportions for frequency counts of dominant hand by dominant eye for S1/2003.
m183dom2004s2.prop.table <- prop.table(m183dom2004s2.table)
# Produce proportions for frequency counts of dominant hand by dominant eye for S1/2003.
m183dom2005s2.prop.table <- prop.table(m183dom2005s2.table)

par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create a mosaic plot for dominant hand by dominant eye across all teaching periods.
mosaicplot(m183dom.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Dominant Eye", ylab = "Dominant Hand", line = 2)
mtext(text = "Dominant Eye by Dominant Hand for All Teaching Periods", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a bar plot for dominant hand by dominant eye across all teaching periods.
barplot(m183dom.table, width = 1,
        names.arg = c("Left", "Right"), legend.text = c("Left", "Right"),
        beside = FALSE, horiz = FALSE,
        main = "", sub = "", xlab = "", ylab = "",
        ylim = c(0,700),
        args.legend = list(x = "topleft", bty = "n", title = "Dominant Eye"))
box()
title(main = "", xlab = "Dominant Hand", ylab = "Frequency", line = 2)
mtext(text = "Dominant Eye by Dominant Hand for All Teaching Periods", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Create mosaic plots for dominant hand by dominant eye for each teaching period.
par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create a mosaic plot for dominant hand by dominant eye for S1/2003.
mosaicplot(m183dom2003s1.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Dominant Eye", ylab = "Dominant Hand", line = 2)
mtext(text = "Dominant Hand by Dominant Eye for Semester 1, 2003", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a mosaic plot for dominant hand by dominant eye for S2/2003.
mosaicplot(m183dom2003s2.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Dominant Eye", ylab = "Dominant Hand", line = 2)
mtext(text = "Dominant Hand by Dominant Eye for Semester 2, 2003", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a mosaic plot for dominant hand by dominant eye for S2/2004.
mosaicplot(m183dom2004s2.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Dominant Eye", ylab = "Dominant Hand", line = 2)
mtext(text = "Dominant Hand by Dominant Eye for Semester 2, 2004", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a mosaic plot for dominant hand by dominant eye for S2/2005.
mosaicplot(m183dom2005s2.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Dominant Eye", ylab = "Dominant Hand", line = 2)
mtext(text = "Dominant Hand by Dominant Eye for Semester 2, 2005", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)


#~~~~~ Eye Colour vs. Bespectacled-ness ~~~~~#

# Create a new data frame which contains only the data for which there are no NA values in dominant hand and
# dominant eye columns. That is, this omits all NA values from the original data frame and preserves original
# row numbers.
m183eye.df.rm <- m183dat.df[!is.na(m183dat.df[, 6]) & !is.na(m183dat.df[, 7]), ]
# Remove the ambiguous factors. These are factor levels for which we cannot immediately discern the appropriate 
# classification. Better to get rid of them and report than attempt to make some assumption.
m183eye.df.rm <- m183eye.df.rm[!(m183eye.df.rm[, 6]) == "bl/brn" &
                                 !(m183eye.df.rm[, 6]) == "bl/green" &
                                 !(m183eye.df.rm[, 6]) == "bl/grn" &
                                 !(m183eye.df.rm[, 6]) == "blgrey" &
                                 !(m183eye.df.rm[, 6]) == "blgrn" &
                                 !(m183eye.df.rm[, 6]) == "blu/grey" &
                                 !(m183eye.df.rm[, 6]) == "grey/bl", ]
m183eye.df.rm <- m183eye.df.rm[!(m183eye.df.rm[, 7]) == 'g', ]
# Reclassify factors so that they are consistent. The factor(s) 'dkbrown' is reclassified as 'brown'.
m183eye.df.rm[m183eye.df.rm[, 6] == "dkbrown", 6] <- "brown"
# Re-number rows for the cleaned data frame so that they are properly ordinal.
row.names(m183eye.df.rm) <- 1:nrow(m183eye.df.rm)

# Produce a contingency table for frequency counts of bespectacled-ness by eye colour across all teaching periods.
m183eye.table <- table(factor(m183eye.df.rm[, 6], labels = c("Black", "Blue", "Brown", "Green", "Grey", "Hazel")), factor(m183eye.df.rm[, 7], labels = c("No", "Yes")),
                       dnn = c("Eye Colour", "Bespectacled?"))
# Produce a contingency table for frequency counts of bespectacled-ness by eye colour for S1/2003.
m183eye2003s1.table <- table(factor(m183eye.df.rm[, 6][m183eye.df.rm[, 18] == "S1/2003"], labels = c("Black", "Blue", "Brown", "Green", "Grey", "Hazel")), factor(m183eye.df.rm[, 7][m183eye.df.rm[, 18] == "S1/2003"], labels = c("No", "Yes")),
                             dnn = c("Eye Colour", "Bespectacled?"))
# Produce a contingency table for frequency counts of bespectacled-ness by eye colour for S2/2003.
m183eye2003s2.table <- table(factor(m183eye.df.rm[, 6][m183eye.df.rm[, 18] == "S2/2003"], labels = c("Black", "Blue", "Brown", "Green", "Grey", "Hazel")), factor(m183eye.df.rm[, 7][m183eye.df.rm[, 18] == "S2/2003"], labels = c("No", "Yes")),
                             dnn = c("Eye Colour", "Bespectacled?"))
# Produce a contingency table for frequency counts of bespectacled-ness by eye colour for S2/2004.
m183eye2004s2.table <- table(factor(m183eye.df.rm[, 6][m183eye.df.rm[, 18] == "S2/2004"], labels = c("Black", "Blue", "Brown", "Green", "Grey", "Hazel")), factor(m183eye.df.rm[, 7][m183eye.df.rm[, 18] == "S2/2004"], labels = c("No", "Yes")),
                             dnn = c("Eye Colour", "Bespectacled?"))
# Produce a contingency table for frequency counts of bespectacled-ness by eye colour for S2/2005.
m183eye2005s2.table <- table(factor(m183eye.df.rm[, 6][m183eye.df.rm[, 18] == "S2/2005"], labels = c("Black", "Blue", "Brown", "Green", "Grey", "Hazel")), factor(m183eye.df.rm[, 7][m183eye.df.rm[, 18] == "S2/2005"], labels = c("No", "Yes")),
                             dnn = c("Eye Colour", "Bespectacled?"))

# Produce proportions for frequency counts of bespectacled-ness by eye colour across all teaching periods.
m183eye.prop.table <- prop.table(m183eye.table)
# Produce proportions for frequency counts of bespectacled-ness by eye colour for S1/2003.
m183eye2003s1.prop.table <- prop.table(m183eye2003s1.table)
# Produce proportions for frequency counts of bespectacled-ness by eye colour for S2/2003.
m183eye2003s2.prop.table <- prop.table(m183eye2003s2.table)
# Produce proportions for frequency counts of bespectacled-ness by eye colour for S1/2003.
m183eye2004s2.prop.table <- prop.table(m183eye2004s2.table)
# Produce proportions for frequency counts of bespectacled-ness by eye colour for S1/2003.
m183eye2005s2.prop.table <- prop.table(m183eye2005s2.table)

par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create a mosaic plot for bespectacled-ness by eye colour across all teaching periods.
mosaicplot(m183eye.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Eye Colour", ylab = "Bespectacled?", line = 2)
mtext(text = "Eye Colour by Bespectacled-ness for All Teaching Periods", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a bar plot for bespectacled-ness by eye colour across all teaching periods.
barplot(m183eye.table, width = 1,
        names.arg = c("No", "Yes"), legend.text = c("Black", "Blue", "Brown", "Green", "Grey", "Hazel"),
        beside = FALSE, horiz = FALSE,
        #col = c("black", "cornflowerblue", "saddlebrown", "chartreuse4", "darkgrey", "orange4"),
        main = "", sub = "", xlab = "", ylab = "",
        ylim = c(0,500),
        args.legend = list(bty = "n", title = "Eye Colour"))
box()
title(main = "", xlab = "Bespectacled?", ylab = "Frequency", line = 2)
mtext(text = "Eye Colour by Bespectacled-ness for All Teaching Periods", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Create mosaic plots for bespectacled-ness by eye colour for each teaching period.
par(cex.axis = 1, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(1,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create a mosaic plot for eye bespectacled-ness by eye colour for S1/2003.
mosaicplot(m183eye2003s1.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Eye Colour", ylab = "Bespectacled?", line = 2)
mtext(text = "Eye Colour by Bespectacled-ness for Semester 1, 2003", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a mosaic plot for bespectacled-ness by eye colour for S2/2003.
mosaicplot(m183eye2003s2.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Eye Colour", ylab = "Bespectacled?", line = 2)
mtext(text = "Eye Colour by Bespectacled-ness for Semester 2, 2003", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a mosaic plot for bespectacled-ness by eye colour for S2/2004.
mosaicplot(m183eye2004s2.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Eye Colour", ylab = "Bespectacled?", line = 2)
mtext(text = "Eye Colour by Bespectacled-ness for Semester 2, 2004", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
# Create a mosaic plot for bespectacled-ness by eye colour for S2/2005.
mosaicplot(m183eye2005s2.table, main = "",
           sub = "", xlab = "", ylab = "",
           shade = TRUE)
title(main = "", xlab = "Eye Colour", ylab = "Bespectacled?", line = 2)
mtext(text = "Eye Colour by Bespectacled-ness for Semester 2, 2005", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)


#~~~~~ Time Travelled vs. Transport Mode ~~~~~#

# Remove the ambiguous factors. These are factor levels for which we cannot immediately discern the appropriate 
# classification. Better to get rid of them and report than attempt to make some assumption.
m183timetravel.df.rm <- m183dat.df[!(m183dat.df[, 15]) == 'b' & !(m183dat.df[, 15]) == "bus/car", ]
# Reclassify factors so that they are consistent. The factor(s) 'bike' is reclassified as 'bicycle'; the
# factor(s) 'motrbike' and 'motorcyc' are reclassified as 'motorcycle'; the factor(s) 'bus', 'bus/train',
# 'trainbus', 'trn&bus', and 'trn+bus' are reclassified as 'publictransport'. The factor(s) 'walk' are fine, so
# we leave it alone.
m183timetravel.df.rm[m183timetravel.df.rm[, 15] == "bike", 15] <- "bicycle"
m183timetravel.df.rm[m183timetravel.df.rm[, 15] == "motrbike" | m183timetravel.df.rm[, 15] == "motorcyc", 15] <- "motorcycle"
m183timetravel.df.rm[m183timetravel.df.rm[, 15] == "bus" |
                m183timetravel.df.rm[, 15] == "bus/train" |
                m183timetravel.df.rm[, 15] == "trainbus" |
                m183timetravel.df.rm[, 15] == "trn&bus" |
                m183timetravel.df.rm[, 15] == "trn+bus", 15] <- "publictransport"
# Remove the observations recorded for factor 'motorcycle'. There is a low number of observations for this
# particular factor (n = 3), so we should remove it so that it doesn't cause problems for analyses.
m183timetravel.df.rm <- m183timetravel.df.rm[!(m183timetravel.df.rm[, 15]) == "motorcycle", ]
# Re-number rows for the cleaned data frame so that they are properly ordinal.
row.names(m183timetravel.df.rm) <- 1:nrow(m183timetravel.df.rm)

# Produce a summary of time traveled by transport mode.
m183timetravel.summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
m183timetravel.summary <- as.data.frame(do.call(rbind, tapply(m183timetravel.df.rm[, 16], m183timetravel.df.rm[, 15], summary)))
row.names(m183timetravel.summary) <- c("Bicycle", "Car", "Public Transport", "Walk")

par(cex.axis = 0.7, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(2,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Create box-plots for time traveled by transport mode.
boxplot(m183timetravel.df.rm[, 16] ~ as.factor(m183timetravel.df.rm[, 15]),
        xlab = "", ylab = "",
        ylim = c(-20, 170),
        at = c(1,2,3,4), col = grey.colors(4, start = 0.25, alpha = 0.5),
        names = c("Bc", "Cr", "PTr", "Wk"), horizontal = TRUE)
title(main = "", xlab = "Time", ylab = "Mode", line = 2)
# Create overlaid density plots for time traveled by transport mode.
plot(density(m183timetravel.df.rm[, 16][m183timetravel.df.rm[, 15] == "walk"]),
     main = "", xlab = "", ylab = "",
     xlim = c(-20, 170), ylim = c(0,0.1),
     col = "red")
lines(density(m183timetravel.df.rm[, 16][m183timetravel.df.rm[, 15] == "car"]),
      main = "", xlab = "", ylab = "",
      xlim = c(-20, 170), ylim = c(0,0.1),
      col = "blue")
lines(density(m183timetravel.df.rm[, 16][m183timetravel.df.rm[, 15] == "bicycle"]),
      main = "", xlab = "", ylab = "",
      xlim = c(-20, 170), ylim = c(0,0.1),
      col = "black")
lines(density(m183timetravel.df.rm[, 16][m183timetravel.df.rm[, 15] == "publictransport"]),
      main = "", xlab = "", ylab = "",
      xlim = c(-20, 170), ylim = c(0,0.1),
      col = "purple")
legend("topleft", c("Bc", "Cr", "PTr", "Wk"), fill = c("red", "blue", "black", "purple"), bty = "n")
title(main = "", xlab = "Time", ylab = "Density", line = 2)
mtext(text = "Travel Time by Transport Mode for All Teaching Periods", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)


#~~~~~ ANALYSIS ~~~~~#

#~~~~~ Height vs. Teaching Period for each Sex ~~~~~#

# Perform a one-way ANOVA to evaluate the relationship between height and teaching period for females.
m183heightf.anova <- aov(Height[Gender == 'f'] ~ factor(TeachingPeriod[Gender == 'f']), data = m183height.df.rm)
# Perform a one-way ANOVA to evaluate the relationship between height and teaching period for males.
m183heightm.anova <- aov(Height[Gender == 'm'] ~ factor(TeachingPeriod[Gender == 'm']), data = m183height.df.rm)
# Summarise and view the fitted ANOVA model for height vs. teaching period for females.
summary(m183heightf.anova)
# Summarise and view the fitted ANOVA model for height vs. teaching period for males.
summary(m183heightm.anova)

# Compute Tukey HSD (Honest Significant Differences) for performing multiple pairwise-comparison between the
# means of the groups for the fitted ANOVA model for height vs. teaching period for females.
TukeyHSD(m183heightf.anova, conf.level = 0.95)
# Compute Tukey HSD (Honest Significant Differences) for performing multiple pairwise-comparison between the
# means of the groups for the fitted ANOVA model for height vs. teaching period for males.
TukeyHSD(m183heightm.anova, conf.level = 0.95)

# Create diagnostic plots for the fitted ANOVA models. That is, create a normal quantile-quantile plot and a
# plot of residuals vs. fitted values for each model.
par(cex.axis = 0.7, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(2,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Diagnostic plots for females ANOVA model.
plot(m183heightf.anova, which = 1, id.n = 0)
plot(m183heightf.anova, which = 2, id.n = 0)
mtext(text = "Diagnostic Plots for Height by Teaching Period for Females", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Diagnostic plots for males ANOVA model.
plot(m183heightm.anova, which = 1, id.n = 0)
plot(m183heightm.anova, which = 2, id.n = 0)
mtext(text = "Diagnostic Plots for Height by Teaching Period for Males", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Formally check the assumption of normality for the fitted ANOVA model by performing a Shapiro-Wilk test on the
# ANOVA residuals for height vs. teaching period for females.
shapiro.test(m183heightf.anova$residuals)
# Formally check the assumption of normality for the fitted ANOVA model by performing a Shapiro-Wilk test on the
# ANOVA residuals for height vs. teaching period for males.
shapiro.test(m183heightm.anova$residuals)

# Formally check the assumption of homogeneous variance between groups by performing Levene's test on the fitted
# ANOVA model for height vs. teaching period for females.
leveneTest(Height[Gender == 'f'] ~ factor(TeachingPeriod[Gender == 'f']), data = m183height.df.rm, center = median)
# Formally check the assumption of homogeneous variance between groups by performing Levene's test on the fitted
# ANOVA model for height vs. teaching period for males.
leveneTest(Height[Gender == 'm'] ~ factor(TeachingPeriod[Gender == 'm']), data = m183height.df.rm, center = median)


#~~~~~ Height vs. Sex for each Teaching Period ~~~~~#

# Perform a one-way ANOVA to evaluate the relationship between height and sex for S1/2003.
m183height2003s1.anova <- aov(Height[TeachingPeriod == "S1/2003"] ~ factor(Gender[TeachingPeriod == "S1/2003"]), data = m183height.df.rm)
# Perform a one-way ANOVA to evaluate the relationship between height and sex for S2/2003.
m183height2003s2.anova <- aov(Height[TeachingPeriod == "S2/2003"] ~ factor(Gender[TeachingPeriod == "S2/2003"]), data = m183height.df.rm)
# Perform a one-way ANOVA to evaluate the relationship between height and sex for S2/2004.
m183height2004s2.anova <- aov(Height[TeachingPeriod == "S2/2004"] ~ factor(Gender[TeachingPeriod == "S2/2004"]), data = m183height.df.rm)
# Perform a one-way ANOVA to evaluate the relationship between height and sex for S2/2005.
m183height2005s2.anova <- aov(Height[TeachingPeriod == "S2/2005"] ~ factor(Gender[TeachingPeriod == "S2/2005"]), data = m183height.df.rm)
# Summarise and view the fitted ANOVA model for height vs. sex for S1/2003.
summary(m183height2003s1.anova)
# Summarise and view the fitted ANOVA model for height vs. sex for S2/2003.
summary(m183height2003s2.anova)
# Summarise and view the fitted ANOVA model for height vs. sex for S2/2004.
summary(m183height2004s2.anova)
# Summarise and view the fitted ANOVA model for height vs. sex for S2/2005.
summary(m183height2005s2.anova)

# Compute Tukey HSD (Honest Significant Differences) for performing multiple pairwise-comparison between the
# means of the groups for the fitted ANOVA model for height vs. sex for S1/2003.
TukeyHSD(m183height2003s1.anova, conf.level = 0.95)
# Compute Tukey HSD (Honest Significant Differences) for performing multiple pairwise-comparison between the
# means of the groups for the fitted ANOVA model for height vs. sex for S2/2003.
TukeyHSD(m183height2003s2.anova, conf.level = 0.95)
# Compute Tukey HSD (Honest Significant Differences) for performing multiple pairwise-comparison between the
# means of the groups for the fitted ANOVA model for height vs. sex for S2/2004.
TukeyHSD(m183height2004s2.anova, conf.level = 0.95)
# Compute Tukey HSD (Honest Significant Differences) for performing multiple pairwise-comparison between the
# means of the groups for the fitted ANOVA model for height vs. sex for S2/2005.
TukeyHSD(m183height2005s2.anova, conf.level = 0.95)

# Create diagnostic plots for the fitted ANOVA models. That is, create a normal quantile-quantile plot and a
# plot of residuals vs. fitted values for each model.
par(cex.axis = 0.7, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(2,1), mgp = c(2,1,0), mar = c(3,4,3,4))
# Diagnostic plots for S1/2003 ANOVA model.
plot(m183height2003s1.anova, which = 1, id.n = 0)
plot(m183height2003s1.anova, which = 2, id.n = 0)
mtext(text = "Diagnostic Plots for Height by Sex for Semester 1, 2003", side = 3, line = -1, outer = TRUE, cex = 1.2, font = 2)

# Diagnostic plots for S2/2003 ANOVA model.
plot(m183height2003s2.anova, which = 1, id.n = 0)
plot(m183height2003s2.anova, which = 2, id.n = 0)
mtext(text = "Diagnostic Plots for Height by Sex for Semester 2, 2003", side = 3, line = -1, outer = TRUE, cex = 1.2, font = 2)

# Diagnostic plots for S2/2004 ANOVA model.
plot(m183height2004s2.anova, which = 1, id.n = 0)
plot(m183height2004s2.anova, which = 2, id.n = 0)
mtext(text = "Diagnostic Plots for Height by Sex for Semester 2, 2004", side = 3, line = -1, outer = TRUE, cex = 1.2, font = 2)

# Diagnostic plots for S2/2005 ANOVA model.
plot(m183height2005s2.anova, which = 1, id.n = 0)
plot(m183height2005s2.anova, which = 2, id.n = 0)
mtext(text = "Diagnostic Plots for Height by Sex for Semester 2, 2005", side = 3, line = -1, outer = TRUE, cex = 1.2, font = 2)

# Formally check the assumption of normality for the fitted ANOVA model by performing a Shapiro-Wilk test on the
# ANOVA residuals for height vs. sex for S1/2003.
shapiro.test(m183height2003s1.anova$residuals)
# Formally check the assumption of normality for the fitted ANOVA model by performing a Shapiro-Wilk test on the
# ANOVA residuals for height vs. sex for S2/2003.
shapiro.test(m183height2003s2.anova$residuals)
# Formally check the assumption of normality for the fitted ANOVA model by performing a Shapiro-Wilk test on the
# ANOVA residuals for height vs. sex for S2/2004.
shapiro.test(m183height2004s2.anova$residuals)
# Formally check the assumption of normality for the fitted ANOVA model by performing a Shapiro-Wilk test on the
# ANOVA residuals for height vs. sex for S2/2005.
shapiro.test(m183height2005s2.anova$residuals)

# Formally check the assumption of homogeneous variance between groups by performing Levene's test on the fitted
# ANOVA model for height vs. sex for S1/2003.
leveneTest(Height[TeachingPeriod == "S1/2003"] ~ factor(Gender[TeachingPeriod == "S1/2003"]), data = m183height.df.rm, center = median)
# Formally check the assumption of homogeneous variance between groups by performing Levene's test on the fitted
# ANOVA model for height vs. sex for S2/2003.
leveneTest(Height[TeachingPeriod == "S2/2003"] ~ factor(Gender[TeachingPeriod == "S2/2003"]), data = m183height.df.rm, center = median)
# Formally check the assumption of homogeneous variance between groups by performing Levene's test on the fitted
# ANOVA model for height vs. sex for S2/2004.
leveneTest(Height[TeachingPeriod == "S2/2004"] ~ factor(Gender[TeachingPeriod == "S2/2004"]), data = m183height.df.rm, center = median)
# Formally check the assumption of homogeneous variance between groups by performing Levene's test on the fitted
# ANOVA model for height vs. sex for S2/2005.
leveneTest(Height[TeachingPeriod == "S2/2005"] ~ factor(Gender[TeachingPeriod == "S2/2005"]), data = m183height.df.rm, center = median)


#~~~~~ Dominant Hand vs. Dominant Eye ~~~~~#

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors dominant hand and dominant eye for S1/2003.
chisq <- chisq.test(m183dom2003s1.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for dominant hand vs. dominant eye for S1/2003.
chisq$observed
# Expected counts of the chi-squared test for dominant hand vs. dominant eye for S1/2003.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for dominant hand vs. dominant eye for S1/2003.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for dominant hand vs. dominant eye for S1/2003.
round(chisq$residuals, 4)

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors dominant hand and dominant eye for S2/2003.
chisq <- chisq.test(m183dom2003s2.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for dominant hand vs. dominant eye for S2/2003.
chisq$observed
# Expected counts of the chi-squared test for dominant hand vs. dominant eye for S2/2003.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for dominant hand vs. dominant eye for S2/2003.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for dominant hand vs. dominant eye for S2/2003.
round(chisq$residuals, 4)

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors dominant hand and dominant eye for S2/2004.
chisq <- chisq.test(m183dom2004s2.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for dominant hand vs. dominant eye for S2/2004.
chisq$observed
# Expected counts of the chi-squared test for dominant hand vs. dominant eye for S2/2004.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for dominant hand vs. dominant eye for S2/2004.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for dominant hand vs. dominant eye for S2/2004.
round(chisq$residuals, 4)

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors dominant hand and dominant eye for S2/2005.
chisq <- chisq.test(m183dom2005s2.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for dominant hand vs. dominant eye for S2/2005.
chisq$observed
# Expected counts of the chi-squared test for dominant hand vs. dominant eye for S2/2005.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for dominant hand vs. dominant eye for S2/2005.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for dominant hand vs. dominant eye for S2/2005.
round(chisq$residuals, 4)

# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors dominant hand and dominant eye for S1/2003.
fisher.test(m183dom2003s1.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)
# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors dominant hand and dominant eye for S2/2003.
fisher.test(m183dom2003s2.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)
# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors dominant hand and dominant eye for S2/2004.
fisher.test(m183dom2004s2.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)
# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors dominant hand and dominant eye for S2/2005.
fisher.test(m183dom2005s2.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)


#~~~~~ Eye Colour vs. Bespectacled-ness ~~~~~#

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors glasses and eye colour for all teaching periods.
chisq <- chisq.test(m183eye.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for glasses vs. eye colour for all teaching periods.
chisq$observed
# Expected counts of the chi-squared test for glasses vs. eye colour for all teaching periods.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for glasses vs. eye colour for all teaching periods.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for glasses vs. eye colour for all teaching periods.
round(chisq$residuals, 4)

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors glasses and eye colour for S1/2003.
chisq <- chisq.test(m183eye2003s1.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for glasses vs. eye colour for S1/2003.
chisq$observed
# Expected counts of the chi-squared test for glasses vs. eye colour for S1/2003.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for glasses vs. eye colour for S1/2003.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for glasses vs. eye colour for S1/2003.
round(chisq$residuals, 4)

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors glasses and eye colour for S2/2003.
chisq <- chisq.test(m183eye2003s2.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for glasses vs. eye colour for S2/2003.
chisq$observed
# Expected counts of the chi-squared test for glasses vs. eye colour for S2/2003.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for glasses vs. eye colour for S2/2003.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for glasses vs. eye colour for S2/2003.
round(chisq$residuals, 4)

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors glasses and eye colour for S2/2004.
chisq <- chisq.test(m183eye2004s2.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for glasses vs. eye colour for S2/2004.
chisq$observed
# Expected counts of the chi-squared test for glasses vs. eye colour for S2/2004.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for glasses vs. eye colour for S2/2004.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for glasses vs. eye colour for S2/2004.
round(chisq$residuals, 4)

# Perform a chi-squared test of independence for to evaluate whether there is a significant association between
# the factors glasses and eye colour for S2/2005.
chisq <- chisq.test(m183eye2005s2.table, correct = TRUE)
chisq
# Observed counts of the chi-squared test for glasses vs. eye colour for S2/2005.
chisq$observed
# Expected counts of the chi-squared test for glasses vs. eye colour for S2/2005.
round(chisq$expected, 2)
# Expected counts as proportions of the chi-squared test for glasses vs. eye colour for S2/2005.
prop.table(round(chisq$expected, 2))
# Pearson residuals of the chi-squared test for glasses vs. eye colour for S2/2005.
round(chisq$residuals, 4)

# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors glasses and eye colour for all teaching periods.
fisher.test(m183eye.table, workspace = 800000,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)
# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors glasses and eye colour for S1/2003.
fisher.test(m183eye2003s1.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)
# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors glasses and eye colour for S2/2003.
fisher.test(m183eye2003s2.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)
# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors glasses and eye colour for S2/2004.
fisher.test(m183eye2004s2.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)
# Perform Fisher's exact as an alternative to the chi-squared test for small cell counts to evaluate whether
# there is a significant association between the factors glasses and eye colour for S2/2005.
fisher.test(m183eye2005s2.table,
            alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95)


#~~~~~ Time Travelled vs. Transport Mode ~~~~~#

# Perform one-way ANOVA using a model of time travelled regressed on transport mode.
m183timetravel.anova <- aov(TimeTravel ~ factor(Transport), data = m183timetravel.df.rm)
# Summarise and view the fitted ANOVA model.
summary(m183timetravel.anova)

# Compute Tukey HSD (Honest Significant Differences) for performing multiple pairwise-comparison between the
# means of the groups for the fitted ANOVA model.
TukeyHSD(m183timetravel.anova, conf.level = 0.95)

# Create diagnostic plots for the fitted ANOVA model. That is, create a normal quantile-quantile plot and a
# plot of residuals vs. fitted values for the model.
par(cex.axis = 0.7, cex.lab = 1, cex.main = 1.2, col.axis = "grey30", font.lab = 2, las = 0, mfrow = c(2,1), mgp = c(2,1,0), mar = c(3,4,3,4))
plot(m183timetravel.anova, which = 1, id.n = 0)
plot(m183timetravel.anova, which = 2, id.n = 0)
mtext(text = "Diagnostic Plots for Time Travelled by Transport Mode for All Teaching Periods", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Formally check the assumption of normality for the fitted ANOVA model by performing a Shapiro-Wilk test on the
# ANOVA residuals for time travel vs. transport mode.
shapiro.test(m183timetravel.anova$residuals)

# Formally check the assumption of homogeneous variance between groups by performing Levene's test on the fitted
# ANOVA model for time travel vs. transport mode.
leveneTest(TimeTravel ~ factor(Transport), data = m183timetravel.df.rm, center = median)

# Perform a non-parametric Kruskal-Wallis rank-sum test, which can be used when the assumptions for ANOVA are
# violated. Note that ANOVA usually performs reasonably well even when the assumptions for it are not met, but a
# violation of the assumptions can affect conclusions.
kruskal.test(TimeTravel ~ factor(Transport), data = m183timetravel.df.rm)

# Perform a pairwise Wilcoxon rank-sum test to calculate multiple pairwise comparisons between group levels.
pairwise.wilcox.test(m183timetravel.df.rm[, 16], factor(m183timetravel.df.rm[, 15]),
                     p.adjust.method = "bonferroni")


#~~~~~ Clean-up ~~~~~#

# Remove redundant variables from the work space.
rm(chisq, data.path, filenames, i, m183dat.lst)

