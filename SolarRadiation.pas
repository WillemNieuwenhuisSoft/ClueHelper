unit SolarRadiation;

interface

uses System.Generics.Collections;

type
    TCoordinate  = class
        name : string;
        longitude : double;
        latitude : double;
        constructor Create(station: string; lon : double; lat : double);
    end;

    TSolarRadiation = class
        coordsOf : TDictionary<integer, TCoordinate>;

    protected
        function newCoordinate(station : string; longitude : double; latitude : double) : TCoordinate;
    public
        constructor Create;
    end;

implementation

constructor TCoordinate.Create(station : string; lon : double; lat : double);
begin
    name := station;
    longitude := lon;
    latitude := lat;
end;

function TSolarRadiation.newCoordinate(station : string; longitude : double; latitude : double) : TCoordinate;
var coord : TCoordinate;
begin
    coord := TCoordinate.Create(station, longitude, latitude);
    newCoordinate := coord;
end;

constructor TSolarRadiation.Create;
begin
    // coordinates based on: http://climexp.knmi.nl/KNMIData/list_dx.txt (2020)
    coordsOf := TDictionary<integer, TCoordinate>.Create;
    coordsOf.Add(210, newCoordinate('Valkenburg', 52.18, 4.42));
    coordsOf.Add(235, newCoordinate('De Kooy', 52.93, 4.78));
    coordsOf.Add(240, newCoordinate('Schiphol', 52.32, 4.78));
    coordsOf.Add(242, newCoordinate('Vlieland', 53.23, 4.92));
    coordsOf.Add(244, newCoordinate('Schermerhorn', 52.39, 5.03));
    coordsOf.Add(249, newCoordinate('Berkhout', 52.65, 4.98));
    coordsOf.Add(251, newCoordinate('Hoorn_Terschelling', 53.38, 5.35));
    coordsOf.Add(260, newCoordinate('De_Bilt', 52.10, 5.18));
    coordsOf.Add(265, newCoordinate('Soesterberg', 52.13, 5.28));
    coordsOf.Add(267, newCoordinate('Stavoren', 52.90, 5.38));
    coordsOf.Add(269, newCoordinate('Lelystad', 52.45, 5.52));
    coordsOf.Add(270, newCoordinate('Leeuwarden', 53.22, 5.75));
    coordsOf.Add(273, newCoordinate('Marknesse', 52.70, 5.88));
    coordsOf.Add(275, newCoordinate('Deelen', 52.05, 5.87));
    coordsOf.Add(277, newCoordinate('Lauwersoog', 53.42, 6.20));
    coordsOf.Add(278, newCoordinate('Heino', 52.43, 6.27));
    coordsOf.Add(279, newCoordinate('Hoogeveen', 52.75, 6.57));
    coordsOf.Add(280, newCoordinate('Eelde', 53.12, 6.58));
    coordsOf.Add(283, newCoordinate('Hupsel', 52.07, 6.65));
    coordsOf.Add(286, newCoordinate('Nieuw_Beerta', 53.20, 7.15));
    coordsOf.Add(290, newCoordinate('Twente', 52.27, 6.88));
    coordsOf.Add(310, newCoordinate('Vlissingen', 51.45, 3.6));
    coordsOf.Add(319, newCoordinate('Westdorpe', 51.22, 3.87));
    coordsOf.Add(323, newCoordinate('Wilhelminadorp', 51.53, 3.88));
    coordsOf.Add(330, newCoordinate('Hoek_van_Holland', 51.98, 4.12));
    coordsOf.Add(340, newCoordinate('Woensdrecht', 51.45, 4.35));
    coordsOf.Add(343, newCoordinate('Rotterdam Geulhaven', 51.53, 4.19));
    coordsOf.Add(344, newCoordinate('Rotterdam', 51.97, 4.45));
    coordsOf.Add(348, newCoordinate('Cabauw', 51.97, 4.92));
    coordsOf.Add(350, newCoordinate('Gilze-Rijen', 51.57, 4.93));
    coordsOf.Add(356, newCoordinate('Herwijnen', 51.85, 5.15));
    coordsOf.Add(370, newCoordinate('Eindhoven', 51.45, 5.38));
    coordsOf.Add(375, newCoordinate('Volkel', 51.65, 5.70));
    coordsOf.Add(377, newCoordinate('Ell', 51.20, 5.77));
    coordsOf.Add(380, newCoordinate('Maastricht', 50.90, 5.77));
    coordsOf.Add(391, newCoordinate('Arcen', 51.50, 6.20));
end;

{
4 OPERATIONELE IHPLEHNTATIE VAN HET PROGRAHA
Hierna volgt een beschrijving van het programma dat uit de
tien—minutengegevens van de globale straling de uurlijkse
waarden van de zonneschijn berekend.
program berekenzonneschijnduur;
uses dos, crt;
type stringé = string[6];
var dagnum : integer;
dtg : string6;
declinatie : real;
uurvak : real;
minuten : byte;
tijdvereff : real;
sinzon : real;
turbiditeit : byte;
f : text:
fname : string;
rec : string;
fr : real;
frsum : real;
qgrens : real;
qgem : real;
qmax : real; maximum straling over 10-minuten )
qmin : real; minimum straling over 1O—minuten )
qextr : real; straling aan rand van atmosfeer )
qdiff : real- diffuse straling )
qdir : real, directe straling )
ssum : real; totale zonneschijn )
station : string;
lengte : real;
breedte : real;
srec : searchrec;
ontbrekend : boolean;
aantalontbrekend : byte;
-_
/~,\,\¢\,-/~
}
{
Voor de berekening van de zonneschijnduur zijn de breedte- en
de lengtegraad van het station van belang. De breedte— en
lengtegraad van het station worden geretourneerd door
onderstaande functies.
}

{
    Het dagnummer wordt gebruikt om de declinatie van de zon en de
    tijdvereffening te berekenen.
    date format is yyyymmdd
}
function TSolarRadiation.dayofyear(date : string) : integer;
const daycumm : array[1..12] of integer =
    (0,31,59,90,120,151,181,212,243,273,304,334);
var
    year,
    month,
    day,
    code,
    daynum : integer;
begin
    val(copy(date, 1, 4), year, code);
    val(copy(date, 5, 2), month, code);
    val(copy(date, 7, 2), day, code);
    if year mod 4 = 0 then
    ( geen rekening gehouden met jaar 2000 }
    begin
    end
    else daynum:=daycumm[month]+day;
    getdaynum:=daynum;
end;


function getstation(filename : string) : string;
begin
getstation:=copy(filename,1,3)7
end:
function gethour (var rec : string) : real;
( deze functie haalt het uurvak uit het gelezen record )
integer;
integer;
integer;
integer;
integer;
var hour : byte;
result : integer;
begin
val(copy(rec,12,2),hour,result);
ethour:=hour;
9
end;
deze functie berekent het dagnummer aan de hand van de
datumtijdgroep van de tienminuten—gegevensrecords.
deze datumtijdgroep bevindt zich op de posities 5 t/m
10 van het record. het format van de dtg is yymmdd,
waarbij yy de laatste twee cijfers van het jaar zijn.
if month > 2 then daynum:=daycumm[month]+day+1
else daynum:=daycumm[month]+day
-9-
\a~n\4\4~<.4



function getminutes (var rec : string) : byte;
( deze functie haalt het tienminuten—tijdvak uit het )
( gelezen record )
var minutes :
result :
byte;
integer;
begin
val(copy(rec,14,2),minutes,result);
getminutes:=minutes+5;
end;
function getdeclination (daynum : integer) : real;
var declination : real;
pd : real;
begin
pd:=2*pi*daynum/366;
dec1ination:=0.33281-22.984*cos(pd)—0.3499O*cos(2*pd)—
O.13980*c0s(3*pd)+3.78720*sin(pd)+
‘ 0.32050*sin(2*pd)+0.07187*sin(3*pd);
declination:=dec1ination/180*pi;
getdeclination:=declination
end;
function gettimecorrection (dagnum : integer) : real;
( deze functie berekent de tijdvereffening )
var timecorr : real;
pd : real;
begin
pd:=2*pi*dagnum/366;
timecorr:=0.0072*cos(pd)-0.0528*cos(2*pd)-
0.00l2*cos(3*pd)-0.1229*sin(pd)-
0.l565*sin(2*pd)—0.0041*sin(3*pd);
gettimecorrection:=timecorr
end;
function getsinussunheight (declinatie
begin
tijdvereff
uurvak
real;
real:
real) : real
getsinussunheight:=sin(declinatie)*sin(breedte/360*2*pi)+
cos(dec1inatie)*cos(breedte/360*2*pi)*
cos((uurvak—12+tijdvereff+lengte/15)*15/360*2*pi);
end;



function getradiation(var rec : string) : real;
var radiation : longint;
result : integer;
begin
val(copy(rec,17,3),radiation,result);
getradiation:=radiation*l00/367
( omzetten J/m2 -> Watt/m2 )
if (copy(rec,2O,l) = '8') or (copy(rec,20,1) = '9') then
begin
ontbrekend := TRUE;
inc(aantalontbrekend);
end;
end;
function getmaxradiation(var rec : string) : real;
var radiation : longint;
result : integer;
begin
val(copy(rec,25,3),radiation,result);
getmaxradiation:=radiation*l00/36;
( omzetten J/m2 ~> Watt/m2 )
if (copy(rec,28,1) = '8') or (copy(rec,28,1) = '9') then
begin
ontbrekend := TRUE;
inc(aantalontbrekend);
end;
end;
function getminradiation(var rec : string) : real;
var radiation : longint;
result : integer;
begin
val(copy(rec,21,3),radiation,result);
getminradiation:=radiation*100/36;
( omzetten J/m2 —> Watt/m2 )
if (copy(rec,24,l) = '8') or (copy(rec,24,l) = '9') then
begin
ontbrekend := TRUE;
inc(aantalontbrekend);
end;
end;



funct
var q
ion getqextr(dagnum : integer) : real;
extr : real;
pd : real;
begin
end;
proce
var c
begin
pd:=2*pi*dagnum/366;
qextr:=1353+45.326*cos(pd)+
0.880l8*cos(2*pd)~0.00461*cos(3*pd);
qextr:=qextr+1.8037*sin(pd)+
0.09746*sin(2*pd)+0.l84l2*sin(3*pd);
getqextr:=qextr;
dure processrec(var rec : string);
h : char;
dtg:=copy(rec,6,6);
qgem:=getradiation(rec);
qmin:=getminradiation(rec);
qmax:=getmaxradiation(rec);
dagnum:=getdaynum(dtg);
qextr:=getqextr(dagnum);
uurvak:=gethour(rec);
minuten:=getminutes(rec);
uurvak:=uurvak+minuten/60;
declinatie:=getdeclination(dagnum);
tijdvereff:=gettimecorrection(dagnum);
if (uurvak > 3.0) and (uurvak < 21.0) then
begin
sinzon:=getsinussunheight
(declinatie,tijdvereff,uurvak)7
if sinzon < 0.05 then
fr:=0
else
if (sinzon > =0.05) and (sinzon < 0.3) then
begin
turbiditeit:=6;
qdiff:=0.2+sinzon/3;
qdir:=exp(-turbiditeit/(0.9 + 9.4*sinzon))
qqrens:=qdiff + qdir;
if qgem/(qextr * sinzon) < qgrens then
fr:=0
else
fr:=1;
end -



else
begin
turbiditeit:=10;
end;
qdiff:=0.3
qdir:=exp(Lturbiditeit/(0.9 + 9.4 * sinzon));
qgrens:=qdiff + qdir;
if qmax/(qextr * sinzon) < 0.4 then
fr. 0
else
begin
end;
if qmin/(qextr*sinzon)>qgrens then
fr. 1
else
begin
end;
if
(qmax/(qextr*sinzon) > qgrens) and
(qmax/(qextr*sinzon)-qmin/
(qextr*sinzon) < 0.1)
then
fr:=1
else
begin
end
turbiditeit:=8;
qdiff:=1.2*qmin/(qextr*sinzon);
if qdiff > 0.4 then qdiff:=0.4;
qdir:=exp(—turbiditeit/(0.9+
9.4*sinzon))7
fr:=(qgem/
(qextr*sinzon)—qdiff)/qdir;
if fr < 0.0
then fr:=0.0
else
if fr > 1.0 then
fr:=1.0;



end;
end;
begin
frsum:=frsum+fr;
ssum:=ssum+fr;
if minuten=55 then
begin
write(uurvak:2:0,' ’,frsum/6 2 1,’
' and (sinzon >= 0 O5) then
1f ontbrekend
begin
write(’In dit tijdvak ontbreken )
writeln(aantalontbrekend/3 1 0,
10"—gegeven(s)’)
end
else writeln;
frsum:=0.0;
ontbrekend := FALSE;
aantalontbrekend := 0;
end;
clrscr;
if paramcount<>1 then
begin
end;
findfirst('c:\qqqdata\'+paramstr(1),arch1ve,srec)
if doserror<>0 then
begin
end;
while
begin
writeln('Tik in : zon ???YMMDD.DAT')
writeln('waarbij III = station‘)
= laatste c1]fer van het
writeln(' Y
]aar');
writeln(' MM
writeln(' DD
halt;
writeln(’I/O-error,
diskette niet in drive of
maand.')
dag.’);
file niet gevonden'):
halt;
doserror=0 do
aantalontbrekend :=
O7
ontbrekend := FALSE;
assign(f,'c:\qqqdata\'+srec.name)
($i-)
reset(f);
($i+)
_14_



end.
end;
frsum:=0.0;
ssum:=0.0;
station:=getstation(srec.name);
breedte:=getbreedte(station);
lengte:=getlengte(station)7
writeln('Station : ',station);
write('Datum : 199'+copy(paramstr(1),4,1)+'-’);
writeln(copy(paramstr(l),5,2)+'—'+
copy(paramstr(1),7,2));
writeln;
writeln(’Uur Zen’);
repeat
read1n(f,rec);
processrec(rec);
until eof(f)7
close(f);
write(’Totaal : ',ssum/6:2:1,' uur zonneschijn.')
writeln('Druk op Enter voor vervo1g');
readln;
findnext(srec);
CIISCIF
-15-



5 VOORBEELD VAN PROGRAHMA-UITVOER
Wanneer uitgegaan wordt van de eerder gegeven file als invoer
produceert het programma de volgende uitvoer:
Station
Datum
Uur
(XI\3O'\U1>|>
9
10
ll
12
13
14
15
16
17
l8
19
20
21
Totaal : 8.8 uur zonneschijn. Druk op Enter voor vervolg
o 0 o o o o o P P o H o H o o o o o N
-u~on~.|nuu--0-Q
c>O o m o n m o o m o m 0 » w N o o S
235
1993-O3-30



}
end.
