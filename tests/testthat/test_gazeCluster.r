library(gazeR)
### THIS FILE PERFORMS A UNIT TEST OF THE gazeCluster() FUNCTION
### KEEP IN MIND THE UNIT TEST PROTOCOL IS STILL NOT EXHAUSTIVE.

### simulate data with two fixations
set.seed(1)
testData1 <- data.frame(time = c(1:1000), 
                        X = c(rnorm(400,100,3) , seq(200,800, length.out = 200) ,rnorm(400,900,3)),
                        Y = rnorm(1000,500,1) )
results1 <- gazeClusters(testData1,eps=50, minPts = 80, recRate = NULL) 

### using recording rate of 5 ms
recRate2 <- 5
results2 <- gazeClusters(testData1,eps=50, minPts = 80, recRate = recRate2 ) 

### data containing characters
testData2 <- testData1
testData2$X <- rep('a', nrow(testData2)) 

### start testing
test_that("Correct number of fications clusters and eye movement recrdings are identified from test data",
          {
            expect_equal(results1$fixationID, c(0,1,2))
            expect_equal(results1$fixationLength, c(200,400,400))
            expect_equal(results1$fixationLength * recRate2, results2$fixationLength )
          }
)


test_that("Errors messages for wrong/missing eye tracking data are working", 
          {
            expect_error( gazeClusters( eps=1, minPts = 5, recRate = NULL) , "dat is required")
            expect_error( gazeClusters(2, eps=1, minPts = 5, recRate = NULL), "dat should be a matrix or a data frame with three columns named: X, Y, time.")
            expect_error( gazeClusters(testData2, eps=1, minPts = 5, recRate = NULL), "dat content needs to be numeric")
          }
)

test_that("Warning message apears when fixations not found (inadequate dbscan parameters (eps, minPts)", 
          {
            expect_warning(gazeClusters(testData1,eps=300, minPts = 500, recRate = recRate2 ), 
                           "No fixations found. You may need to use other values for the eps or minPTS parameters")
          }
)