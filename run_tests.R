##install.packages("RUnit")
library('RUnit')

source('cachematrix.R')

test.suite <- defineTestSuite("cache matrix",
                              dirs = file.path("test"),
                              testFileRegexp = '^.*Test\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
