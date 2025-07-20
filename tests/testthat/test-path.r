test_that("`M()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(MZ(1:2, 1:2) |> format(),
                 "M 1,1 2,2 Z")
    expect_equal(mz(1:2, 1:2) |> format(),
                 "m 1,1 2,2 z")
    p <- affiner::as_coord2d(x = 1:2, y = 3:4)
    expect_equal(MZ(p) |> format(),
                 "M 1,3 2,4 Z")

    rlang::local_options(dee.origin_at_bottom = TRUE,
                         dee.height = 10,
                         dee.sep = " ")
    expect_equal(MZ(1:2, 1:2) |> format(),
                 "M 1 9 2 8 Z")
    expect_equal(mz(1:2, 1:2) |> format(),
                 "m 1 -1 2 -2 z")
    expect_equal(MZ(p) |> format(),
                 "M 1 7 2 6 Z")

    expect_equal(M(1.25, 1.75) |> format(),
                 "M 1.25 8.25")
    rlang::local_options(dee.digits = 0)
    expect_equal(M(1.25, 1.75) |> format(),
                 "M 1 8")
})

test_that("`CIRCLE()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_true(CIRCLE(5, 5, 2) |> inherits("dee"))
    # plot(CIRCLE(5, 5, 4), height = 10, width = 10, fill = "red")
})

test_that("`RECT()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(RECT(x = 10, y = 10, w = 6, h = 4) |> format(),
                 "M 7,8 7,12 13,12 13,8 Z")
})

test_that("`L()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(LZ(1:2, 1:2) |> format(),
                 "L 1,1 2,2 Z")
    expect_equal(lz(1:2, 1:2) |> format(),
                 "l 1,1 2,2 z")

    expect_equal(HZ(1:2) |> format(),
                 "H 1 2 Z")
    expect_equal(hz(1:2) |> format(),
                 "h 1 2 z")

    expect_equal(VZ(1:2) |> format(),
                 "V 1 2 Z")
    expect_equal(vz(1:2) |> format(),
                 "v 1 2 z")

    rlang::local_options(dee.origin_at_bottom = TRUE,
                         dee.height = 10,
                         dee.sep = " ")
    expect_equal(LZ(1:2, 1:2) |> format(),
                 "L 1 9 2 8 Z")
    expect_equal(lz(1:2, 1:2) |> format(),
                 "l 1 -1 2 -2 z")

    expect_equal(HZ(1:2) |> format(),
                 "H 1 2 Z")
    expect_equal(hz(1:2) |> format(),
                 "h 1 2 z")

    expect_equal(VZ(1:2) |> format(),
                 "V 9 8 Z")
    expect_equal(vz(1:2) |> format(),
                 "v -1 -2 z")
})

test_that("`Q()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(QZ(1:2, 1:2, 1:2, 1:2) |> format(),
                 "Q 1,1 1,1 2,2 2,2 Z")
    expect_equal(qz(1:2, 1:2, 1:2, 1:2) |> format(),
                 "q 1,1 1,1 2,2 2,2 z")
    expect_equal(TZ(1:2, 1:2) |> format(),
                 "T 1,1 2,2 Z")
    expect_equal(tz(1:2, 1:2) |> format(),
                 "t 1,1 2,2 z")

    rlang::local_options(dee.origin_at_bottom = TRUE,
                         dee.height = 10,
                         dee.sep = " ")
    expect_equal(QZ(1:2, 1:2, 1:2, 1:2) |> format(),
                 "Q 1 9 1 9 2 8 2 8 Z")
    expect_equal(qz(1:2, 1:2, 1:2, 1:2) |> format(),
                 "q 1 -1 1 -1 2 -2 2 -2 z")
    expect_equal(TZ(1:2, 1:2) |> format(),
                 "T 1 9 2 8 Z")
    expect_equal(tz(1:2, 1:2) |> format(),
                 "t 1 -1 2 -2 z")
})

test_that("`C()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(CZ(1:2, 1:2, 1:2, 1:2, 1:2, 1:2) |> format(),
                 "C 1,1 1,1 1,1 2,2 2,2 2,2 Z")
    expect_equal(cz(1:2, 1:2, 1:2, 1:2, 1:2, 1:2) |> format(),
                 "c 1,1 1,1 1,1 2,2 2,2 2,2 z")
    expect_equal(SZ(1:2, 1:2, 1:2, 1:2) |> format(),
                 "S 1,1 1,1 2,2 2,2 Z")
    expect_equal(sz(1:2, 1:2, 1:2, 1:2) |> format(),
                 "s 1,1 1,1 2,2 2,2 z")

    rlang::local_options(dee.origin_at_bottom = TRUE,
                         dee.height = 10,
                         dee.sep = " ")
    expect_equal(CZ(1:2, 1:2, 1:2, 1:2, 1:2, 1:2) |> format(),
                 "C 1 9 1 9 1 9 2 8 2 8 2 8 Z")
    expect_equal(cz(1:2, 1:2, 1:2, 1:2, 1:2, 1:2) |> format(),
                 "c 1 -1 1 -1 1 -1 2 -2 2 -2 2 -2 z")
    expect_equal(SZ(1:2, 1:2, 1:2, 1:2) |> format(),
                 "S 1 9 1 9 2 8 2 8 Z")
    expect_equal(sz(1:2, 1:2, 1:2, 1:2) |> format(),
                 "s 1 -1 1 -1 2 -2 2 -2 z")
})

test_that("`A()`", {
    do.call(rlang::local_options, dee_options(default = TRUE))
    expect_equal(AZ(1:2, 1:2, 45, 0, 0, 1:2, 1:2) |> format(),
                 "A 1,1 45,0,0 1,1 2,2 45,0,0 2,2 Z")
    expect_equal(az(1:2, 1:2, 45, 0, 0, 1:2, 1:2) |> format(),
                 "a 1,1 45,0,0 1,1 2,2 45,0,0 2,2 z")

    rlang::local_options(dee.origin_at_bottom = TRUE,
                         dee.height = 10,
                         dee.sep = " ")
    expect_equal(AZ(1:2, 1:2, 45, 0, 0, 1:2, 1:2) |> format(),
                 "A 1 1 -45 0 1 1 9 2 2 -45 0 1 2 8 Z")
    expect_equal(az(1:2, 1:2, 45, 0, 0, 1:2, 1:2) |> format(),
                 "a 1 1 -45 0 1 1 -1 2 2 -45 0 1 2 -2 z")
})
