test_that("netrc", {
  
  netrc = ".netrc"
  dodsrc = ".dodsrc"
  unlink(netrc)
  unlink(dodsrc)
  expect_error(check_rc_files(dodsrc, netrc))
  expect_false(checkNetrc(netrcFile = netrc))
  expect_error(writeNetrc(login = "climateR@gmail.com", netrcFile = netrc))
  
  netrc = writeNetrc("climateR@gmail.com", "password1234", netrcFile = netrc)
  expect_error(writeNetrc("climateR@gmail.com", "password1234", netrcFile = netrc))
  expect_true(any(grepl("climateR@gmail.com", readLines(netrc))))
  expect_true(any(grepl("password1234", readLines(netrc))))
  expect_true(checkNetrc(netrcFile = netrc))

  expect_message(check_rc_files(dodsrc, netrc))
  unlink(dodsrc)
  expect_false(checkDodsrc(dodsrc, netrc))
  writeDodsrc(netrc, dodsrc)
  expect_true(checkDodsrc(dodsrc, netrc))
  
  expect_true(any(grepl(netrc, readLines(dodsrc))))
  
  f = getNetrcPath()
  expect_type(f, "character")
  f = getDodsrcPath()
  expect_type(f, "character")
  unlink(netrc)
  unlink(dodsrc)
})
