#!/usr/bin/python
# -*- coding: utf-8 -*-

AC = "camping"
AW = "wildcamping"
AJ = "hostel"
AH = "hotel"

T0 = ""
TC = "cycle"
TT = "train"
TF = "ferry"
TP = "plane"

data = [ ("20110421", "London", None, None, None, .0, AJ, T0, 0),
         ("20110422", "London", None, None, None, .0, AJ, T0, 0),
         ("20110423", "London", None, None, None, .0, AJ, T0, 0),
         ("20110424", "London", None, None, None, .0, AJ, T0, 0),
         ("20110425", "London", "Rochester", "08:00", "20:00", 87.0, AH, TC, 1),
         ("20110426", "Rochester", "Canterbury", "09:00", "18:00", 69.0, AC, TC, 2),
         ("20110427", "Canterbury", "Dunkerque", "09:00", "20:00", 65.0, AC, TC, 3),
         ("20110428", "Dunkerque", None, None, None, .0, AC, T0, 0),
         ("20110429", "Dunkerque", "Strasbourg", None, None, .0, AJ, TT, 0),
         ("20110430", "Strasbourg", "München", None, None, .0, AJ, TT, 0),
         ("20110501", "München", None, None, None, .0, AJ, T0, 0),
         ("20110502", "München", None, None, None, .0, AJ, T0, 0),
         ("20110503", "München", "Plzen", None, None, .0, AJ, TT, 0),
         ("20110504", "Plzen", None, None, None, .0, AJ, T0, 0),
         ("20110505", "Plzen", None, None, None, .0, AJ, T0, 0),
         ("20110506", "Plzen", "Praha", None, None, .0, AJ, TT, 0),
         ("20110507", "Praha", None, None, None, .0, AJ, T0, 0),
         ("20110508", "Praha", None, None, None, .0, AJ, T0, 0),
         ("20110509", "Praha", None, None, None, .0, AJ, T0, 0),
         ("20110510", "Praha", "Týnec", None, None, .0, AC, TC, 4),
         ("20110511", "Týnec", "Borotin", None, None, .0, AH, TC, 5),
         ("20110512", "Borotin", "Sobeslav", None, None, .0, AC, TC, 6),
         ("20110513", "Sobeslav", "Ceské Budejovice", None, None, .0, AC, TC, 7),
         ("20110514", "Ceské Budejovice", "Cesky Krumlov", None, None, .0, AC, TC, 8),
         ("20110515", "Cesky Krumlov", None, None, None, .0, AC, TC, 0),
         ("20110516", "Cesky Krumlov", "Freistadt", None, None, .0, AC, TC, 9),
         ("20110517", "Freistadt", "Au", None, None, .0, AC, TC, 10),
         ("20110518", "Au", "Melk", None, None, .0, AC, TC, 11),
         ("20110519", "Melk", "Tulln", None, None, .0, AC, TC, 12),
         ("20110520", "Tulln", "Wien", None, None, .0, AC, TC, 13),
         ("20110521", "Wien", None, None, None, .0, AC, T0, 0),
         ("20110522", "Wien", None, None, None, .0, AC, T0, 0),
         ("20110523", "Wien", "Breiterbrunn", None, None, .0, AC, TC, 14),
         ("20110524", "Breiterbrunn", "Markt St. Martin", None, None, .0, AC, TC, 15),
         ("20110525", "Markt St. Martin", "Bad Tatzmannsdorf", None, None, .0, AC, TC, 16),
         ("20110526", "Bad Tatzmannsdorf", "Fürstenfeld", None, None, .0, AC, TC, 17),
         ("20110527", "Fürstenfeld", None, None, None, .0, AC, TC, 0),
         ("20110528", "Fürstenfeld", None, None, None, .0, AC, TC, 0),
         ("20110529", "Fürstenfeld", "St. Peter am Ottersbach", None, None, .0, AC, TC, 18),
         ("20110530", "St. Peter am Ottersbach", "Maribor", None, None, .0, AC, TC, 19),
         ("20110531", "Maribor", None, None, None, .0, AJ, T0, 0),
         ("20110601", "Maribor", "Rajkovec", None, None, .0, AC, TC, 20),
         ("20110602", "Rajkovec", "Prebold", None, None, .0, AC, TC, 21),
         ("20110603", "Prebold", "Kamnik", None, None, .0, AC, TC, 22),
         ("20110604", "Kamnik", "Bled", None, None, .0, AC, TC, 23),
         ("20110605", "Bled", None, None, None, .0, AC, T0, 0),
         ("20110606", "Bled", None, None, None, .0, AC, T0, 0),
         ("20110607", "Bled", "Kranjska Gora", None, None, .0, AC, TC, 24),
         ("20110608", "Kranjska Gora", "Bovec", None, None, .0, AC, TC, 25),
         ("20110609", "Bovec", "Kobarid", None, None, .0, AC, TC, 26),
         ("20110610", "Kobarid", "Ozeljian", None, None, .0, AC, TC, 27),
         ("20110611", "Ozeljian", "Monfalcone", None, None, .0, AC, TC, 28),
         ("20110612", "Monfalcone", "Lido di Venezia", None, None, .0, AC, TC, 29),
         ("20110613", "Lido di Venezia", None, None, None, .0, AC, T0, 0),
         ("20110614", "Lido di Venezia", None, None, None, .0, AC, T0, 0),
         ("20110615", "Lido di Venezia", "Ferrara", None, None, .0, AC, TC, 30),
         ("20110616", "Ferrara", None, None, None, .0, AC, T0, 0),
         ]

def pathify(date):
    return "%s/%s/%s" % chop(date)

def chop(date):
    return (date[0:4], date[4:6], date[6:8])

def get_dates():
    return [day[0] for day in data]

def print_dates():
    for date in get_dates():
        print date

if __name__ == '__main__':
    print_dates()
