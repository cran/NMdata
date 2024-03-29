context("egdt")

test_that("basic",{
    fileRef <- "testReference/egdt_1.rds"

    df1 <- data.frame(a=1:2,b=3:4)
    df2 <- data.frame(c=5:6,d=7:8)

    res <- egdt(as.data.table(df1),as.data.table(df2),quiet=TRUE)

    expect_equal_to_reference(res,fileRef,version=2)
})



test_that("common column names",{
    df1 <- data.frame(a=1:2,b=3:4)
    df2 <- data.frame(b=5:6,d=7:8)

    expect_error(egdt(as.data.table(df1),as.data.table(df2)))
})

