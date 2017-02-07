# Read the data from CSV file
getwd()
cigar <- read.table("/Users/hazel/Desktop/nyts2013_dataset.csv", sep = ";", header = TRUE)
names(cigar)
dim(cigar)
View(cigar)

# Get samples from data for some questions 
group1 <- select(cigar, qn9:qn12, qn69:qn71)
sample0 <- sample_n(group1, 300)

r_cigar <- sample0[complete.cases(sample0),]  # if you don't want NA values
View(r_cigar)
dim(r_cigar)

# Get dissimilarity matrix and Apply fuzzy function
dissmatrix <- daisy(r_cigar, metric = "euclidean")    # change matrix: Gower, Canberra
class(dissmatrix)
fuzzyss <- fanny(dissmatrix, k = 4)        # change k value, see what happens

# Other questions that were not used for analysis
sample_row_count <- dim(sample0)[1]
full_student_data = subset(cigar, year==0)
for (i in 0:sample_row_count) {
  index <- row.names(sample0[i,])
  thestudent <- cigar[index,] 
  print(thestudent)
  full_student_data[i,] <- thestudent
}


# Percent Disagreement Method
# Find difference for 2 rows and calculate ratio 
difference <- function (person_1, person_2) {
  different_count = 0
  for (i in 1:ncol(person_1)) {
      answer1 = person_1[i]
      answer2 = person_2[i]
      if (answer1 != answer2) {
        different_count = different_count + 1
      }
  }
  return (different_count/ncol(person_1))
}


# Matrix for Percent Disagreement Method output
mainfunc <- function (abc, empty_matrix) {
  row_count = nrow(abc)
  print("Got empty matrix")
  print(nrow(empty_matrix))
  for (i in 1:row_count) {
  for (j in 1:row_count) {
    person1 = abc[i,]
    person2 = abc[j,]
    empty_matrix[i,j] <- difference(person1, person2)
    }
  }
  return (empty_matrix)
}





