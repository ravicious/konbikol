module Mid2021TicketFormatTests exposing (..)

import Array
import DateTime exposing (DateTime)
import Expect
import Test exposing (..)
import Ticket
import TicketParser


suite : Test
suite =
    describe "mid 2021 ticket format"
        [ test "is parsed correctly" <|
            \_ ->
                let
                    expectedTicket =
                        { departureStation = "Foo Baz"
                        , arrivalStation = "Quux"
                        , departure = DateTime 2021 7 25 15 18
                        , arrival = DateTime 2021 7 25 17 2
                        , train = "TLK 1234"
                        , carriageNumber = "17 p"
                        , seat = "29 o"
                        , travelClass = "1"
                        , carriageType = ""
                        }
                in
                TicketParser.parseStrings mid2021Ticket
                    |> Expect.equal (Ok expectedTicket)
        , test "is parsed correctly for a double ticket" <|
            \_ ->
                let
                    expectedTicket =
                        { departureStation = "Kraków Główny"
                        , arrivalStation = "Quux"
                        , departure = DateTime 2021 9 15 12 25
                        , arrival = DateTime 2021 9 15 15 29
                        , train = "TLK 12345"
                        , carriageNumber = "16 p"
                        , seat = "66 o, 67 o"
                        , travelClass = "1"
                        , carriageType = ""
                        }
                in
                TicketParser.parseStrings doubleTicket
                    |> Expect.equal (Ok expectedTicket)
        ]


mid2021Ticket =
    Array.fromList
        [ "Bilet (25.07.2021)"
        , "Foo Baz "
        , " Quux"
        , "x1"
        , "Bilet jest ważny wraz z dokumentem ze zdjęciem potwierdzającym"
        , "tożsamość. Na każde żądanie organu kontrolnego w pociągu bilet należy"
        , "przedstawić do kontroli."
        , "Informacje o podróży"
        , "xxxxx yyyyyyyy"
        , " to Twój plan podróży:"
        , "relacja"
        , "przew."
        , "poc."
        , "kl."
        , "wagon"
        , "miejsca"
        , "1)"
        , "Foo"
        , "Baz"
        , "Quux"
        , "15:18"
        , " - 17:02"
        , "25.07.2021"
        , "PKP"
        , "IC"
        , "TLK"
        , "1234"
        , "1"
        , "17"
        , " p"
        , "29"
        , " o"
        , "LEGENDA: p - przedziałowy; o - od okna"
        , "1)"
        , "Uprawnia do przejazdu ze st. Kraków Płaszów do st. Kraków Główny wybranymi tramwajami ZTP"
        , "i pociągami TLK/IC/EIC (obowiązek rezerwacji). Więcej: www.intercity.pl/krk"
        , "Na stacji odjazdu sprawdzisz z jakiego peronu odjeżdża Twój pociąg."
        , "Wybierając PKP Intercity emitujesz tylko 8,1 kg CO2 na pasażera. To ponad 3 razy mniej niż samochód (29,7 kg) i 8 razy mniej"
        , "niż samolot (71,0 kg)."
        , "Informacje rozliczeniowe"
        , "Usługa"
        , "Liczba"
        , "Ulga"
        , "Oferta"
        , "VAT (zł)"
        , "Należność (zł)"
        , "Foo Bar "
        , " Quux (123 km)"
        , "Przejazd osób"
        , "1"
        , "-"
        , "1 A-Cena bazowa"
        , "8% 2,48"
        , "43,00"
        , "Razem:"
        , "43,00"
        , "Sposób płatności: przelew"
        , "Zapłacono i wystawiono: 18.07.2021 21:13:28"
        , "Num. płatności: 98765432"
        , "Ten bilet nie jest fakturą"
        , "Sprzedawca: PKP Intercity S.A."
        , "Dokument wygenerowano: 18.07.2021 21:14:16"
        , "NR BILETU: "
        , "eIC123456789"
        , ", 1234 (P24) 4321"
        , "NR REFERENCYJNY:  123456789012"
        ]


doubleTicket =
    Array.fromList
        [ "Bilet (15.09.2021)"
        , "Kraków Główny "
        , " Quux"
        , "x2"
        , "Bilet jest ważny wraz z dokumentem ze zdjęciem potwierdzającym"
        , "tożsamość. Na każde żądanie organu kontrolnego w pociągu bilet należy"
        , "przedstawić do kontroli."
        , "Informacje o podróży"
        , "Bar Baz"
        , " to Twój plan podróży:"
        , "relacja"
        , "przew."
        , "poc."
        , "kl."
        , "wagon"
        , "miejsca"
        , "1)"
        , "Kraków"
        , "Główny"
        , "Quux"
        , "12:25"
        , " - 15:29"
        , "15.09.2021"
        , "PKP"
        , "IC"
        , "TLK"
        , "12345"
        , "1"
        , "16"
        , " p"
        , "66"
        , " o, "
        , "67"
        , " o"
        , "LEGENDA: p - przedziałowy; o - od okna"
        , "1)"
        , "Uprawnia do przejazdu ze st. Kraków Płaszów do st. Kraków Główny wybranymi tramwajami ZTP"
        , "i EIC/TLK/IC (w EZT - obowiązkowy bilet dodatkowy). Więcej: www.intercity.pl/krk"
        , "Na stacji odjazdu sprawdzisz z jakiego peronu odjeżdża Twój pociąg."
        , "Wybierając PKP Intercity emitujesz tylko 8,0 kg CO2 na pasażera. To ponad 3 razy mniej niż samochód (29,3 kg) i 8 razy mniej"
        , "niż samolot (70,0 kg)."
        , "Informacje rozliczeniowe"
        , "Usługa"
        , "Liczba"
        , "Ulga"
        , "Oferta"
        , "VAT (zł)"
        , "Należność (zł)"
        , "Kraków Główny "
        , " Quux (123 km)"
        , "Przejazd osób"
        , "2"
        , "-"
        , "1 A-Cena bazowa"
        , "8% 11,11"
        , "111,00"
        , "Razem:"
        , "111,00"
        , "Sposób płatności: przelew"
        , "Zapłacono i wystawiono: 15.09.2021 11:21:11"
        , "Num. płatności: 12345678"
        , "Ten bilet nie jest fakturą"
        , "Sprzedawca: PKP Intercity S.A."
        , "Dokument wygenerowano: 15.09.2021 11:21:12"
        , "NR BILETU: "
        , "eIC123456789"
        , ", 12345 (P24) 1234"
        , "NR REFERENCYJNY:  123456789012"
        ]
