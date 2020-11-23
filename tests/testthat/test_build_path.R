test_that("path buildgs correctly", {
    expect_equal(build_path('basic'), '/propertyapi/v1.0.0/property/basicprofile')
    expect_equal(build_path('detail'), '/propertyapi/v1.0.0/property/detail')
    expect_equal(build_path('address'), '/propertyapi/v1.0.0/property/address')
    expect_equal(build_path('sales'), '/propertyapi/v1.0.0/sale/snapshot')
    expect_equal(build_path('nonsense'), '/propertyapi/v1.0.0/')
    expect_equal(build_path(''), '/propertyapi/v1.0.0/')
    expect_warning(build_path(''), 'Invalid search option.')
})
