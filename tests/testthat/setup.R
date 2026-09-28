# The cache is on by default, and the mocked requests in these tests share
# URLs, so without this a later test is served an earlier one's response and
# makes no request at all. `test-cache.R` turns it back on where that is the
# point of the test.
withr::local_options(transferegovr.cache = FALSE, .local_envir = teardown_env())
