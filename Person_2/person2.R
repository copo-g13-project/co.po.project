
data <- read.csv(file.choose())
data_original <- data
nrow(data)
data_clean <- data[3:32, ]
rownames(data_clean) <- NULL
colnames(data_clean)[1] <- "Student_Name"
data_clean$Marks.in.Math <- as.numeric(as.character(data_clean$Marks.in.Math))
data_clean$Marks.in.Physics <- as.numeric(as.character(data_clean$Marks.in.Physics))
data_clean$Marks.in.Chemistry <- as.numeric(as.character(data_clean$Marks.in.Chemistry))
colSums(is.na(data_clean))
sum(duplicated(data_clean))
sapply(data_clean[, 2:4], function(x) sum(x < 0 | x > 100))
write.csv(data_clean, "C:/Users/Jitendra/COPO/AI-CO-PO-PROJECT/co.po.project/Person_2/Clean_data.csv", row.names = FALSE)
