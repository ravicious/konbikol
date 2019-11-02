module TicketParserTests exposing (..)

import Array
import DateTime exposing (DateTime)
import Expect exposing (Expectation)
import Test exposing (..)
import Ticket
import TicketParser


suite : Test
suite =
    describe "TicketParser"
        [ test "parses a regular ticket correctly" <|
            \_ ->
                let
                    expectedTicket =
                        { departureStation = "Foo"
                        , arrivalStation = "Bar"
                        , departure = DateTime 2018 11 4 13 25
                        , arrival = DateTime 2018 11 4 20 22
                        , train = "TLK 12345"
                        , carriage = "13"
                        , seat = "35o"
                        }
                in
                TicketParser.parseStrings regularTicketStrings
                    |> Expect.equal (Ok expectedTicket)
        ]


regularTicketStrings =
    Array.fromList
        [ "Data wydruku: 2019-11-01 21:27:24"
        , "12345"
        , "NORMAL. : "
        , "1"
        , "ULG. : "
        , "   X : X"
        , " OF: 1"
        , "\"PKP Intercity\""
        , "Spółka Akcyjna"
        , "Przewoźnik: PKP IC"
        , "BILET INTERNETOWY"
        , "E-Wcześniej: 1xNormal"
        , "¦"
        , " ̧"
        , "Od/From "
        , ""
        , "Do/To"
        , "¦"
        , " ̧"
        , "KL./CL."
        , "04.11"
        , "*"
        , "13:25"
        , "*"
        , "Foo"
        , "*"
        , ""
        , "Bar"
        , "*"
        , "04.11"
        , "*"
        , "20:22"
        , "*"
        , "2"
        , "*"
        , "PRZEZ: Quux * Baz"
        , "SUMA PLN:"
        , "48,00 zł"
        , "130983210984"
        , "Nr biletu"
        , ": eIC13289321"
        , "(P24) 1239"
        , "Informacje o podróży:"
        , "Stacja"
        , "Data"
        , "Godzina"
        , "/Wagon"
        , "Km"
        , "Nr miejsca"
        , " (o-okno ś-środek k-korytarz)"
        , "Suma PLN"
        , "Foo"
        , "Bar"
        , "04.11"
        , "04.11"
        , "13:25"
        , "20:22"
        , "TLK 12345"
        , "13"
        , "122"
        , "35o"
        , "1 m. do siedzenia; wagon z przedziałami"
        , "32,00 zł"
        , "r4I"
        , "r4I"
        , "Podróżny: Fuuux Buuuuux"
        , "Informacja o cenie"
        , "PTU"
        , "Suma PLN"
        , "Płatność: przelewem"
        , "Opłata za przejazd:"
        , " 8%"
        , "32,00"
        , "Zapłacono i wystawiono dnia:"
        , "2018-10-21 12:12:29(11111111)"
        , "Ogółem PLN:"
        , "32,00"
        , "Bilet internetowy jest biletem imiennym i jest ważny:"
        , "a) wraz z dokumentem ze zdjęciem potwierdzającym tożsamość Podróżnego,"
        , "b) tylko w dniu, relacji, pociągu, wagonie i na miejsce na nim oznaczone."
        , "Zwrotu należności za niewykorzystany bilet dokonuje się na podstawie wniosku"
        , "złożonego przez płatnika w wyznaczonych przez 'PKP Intercity' S.A. punktach, z"
        , "wyjątkiem należności zwracanych automatycznie na zasadach określonych w"
        , "Regulaminie e-IC."
        , "Niniejszy bilet internetowy nie jest fakturą VAT."
        , "W związku z przeprowadzanymi modernizacjami sieci kolejowej, uprzejmie prosimy o"
        , "dokładne sprawdzanie rozkładu jazdy pociągów przed podróżą."
        ]
