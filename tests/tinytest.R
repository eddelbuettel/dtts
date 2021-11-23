pkg <- "dtts"

if (requireNamespace("tinytest", quietly=TRUE)) {
    tinytest::test_package(pkg)
}
