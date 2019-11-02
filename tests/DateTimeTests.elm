module DateTimeTests exposing (..)

import DateTime exposing (DateTime)
import Expect
import Fuzz exposing (..)
import Test exposing (..)


yearFuzzer =
    intRange 2018 2022


monthFuzzer =
    intRange 1 12


dayFuzzer =
    intRange 1 31


hourFuzzer =
    intRange 0 23


minFuzzer =
    intRange 0 59


defaultDateTimeFuzzers =
    { year = yearFuzzer
    , month = monthFuzzer
    , day = dayFuzzer
    , hour = hourFuzzer
    , min = minFuzzer
    }


dateTimeFuzzer fuzzers =
    map5 DateTime fuzzers.year fuzzers.month fuzzers.day fuzzers.hour fuzzers.min


expectIsGreaterThan greater lower =
    DateTime.isGreaterThan greater lower
        |> Expect.true
            ("Expected "
                ++ DateTime.toString greater
                ++ " to be greater than "
                ++ DateTime.toString lower
            )


expectIsNotGreaterThan greater lower =
    DateTime.isGreaterThan greater lower
        |> Expect.false
            ("Expected "
                ++ DateTime.toString greater
                ++ " not to be greater than "
                ++ DateTime.toString lower
            )


isGreaterThan =
    describe "isGreaterThan"
        [ fuzz2
            (dateTimeFuzzer { defaultDateTimeFuzzers | year = constant 2019 })
            (dateTimeFuzzer { defaultDateTimeFuzzers | year = constant 2018 })
            "returns true when the year is greater"
            expectIsGreaterThan
        , fuzz2
            (dateTimeFuzzer { defaultDateTimeFuzzers | year = constant 2019 })
            (dateTimeFuzzer { defaultDateTimeFuzzers | year = constant 2019 })
            "returns true when the month is greater"
          <|
            \greater lower ->
                expectIsGreaterThan { greater | month = lower.month + 1 } lower
        , test "returns false when the date times are equal" <|
            \_ ->
                let
                    greater =
                        DateTime 2019 1 2 3 4

                    lower =
                        DateTime 2019 1 2 3 4
                in
                expectIsNotGreaterThan greater lower
        ]
