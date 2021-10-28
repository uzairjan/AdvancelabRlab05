
context("PollutionData")


test_that("This application only supports these countries: {'Sweden','Pakistan','India','Bangladesh','Afghanistan'}",{
   expect_error(PollutionData$new(list("Spain","Turkey")))
   expect_error(PollutionData$new(list("Brazil","Sweden")))
   expect_error(PollutionData$new(list()))
})

# test_that("Number of observations",{
#   pobjc<-PollutionData$new(list("Pakistan","India","Bangladesh","Sweden","Afghanistan"))
#   expect_equal(pobjc$response_list$Pakistan$nhits==0,FALSE)
#   expect_equal(pobjc$response_list$India$nhits==0,FALSE)
# })


test_that("Type of data",{
  pobjc<-PollutionData$new(list("Pakistan","India","Bangladesh","Sweden","Bangladesh","Afghanistan"))
  expect_true(is.data.frame(pobjc$getOnlySectionData(pobjc$response_list$Pakistan,c("value_pm5","category_pm25"))))
})

test_that("Parameter should be a  character list",{
  pobjc<-PollutionData$new(list("Pakistan","India","Bangladesh","Sweden","Bangladesh","Afghanistan"))
  expect_error(pobjc<-class(a$getAllSectionResponse(c("Turkey"))))
  expect_error(pobjc<-class(a$getAllSectionResponse(c(5))))
})

test_that("select correct data",{
  pobjc<-PollutionData$new(list("Pakistan","India","Bangladesh","Sweden","Bangladesh","Afghanistan"))
   pobjc<-PollutionData$new(list("Sweden"))
   expect_error(pobjc$getAllSectionResponse(sectionVector = list("value_pm5","category_pm25")))
   })


