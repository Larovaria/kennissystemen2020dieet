%concepten
%-------------------------------------------
%gezonde drempelwaardes mg per BMI
supplement(vitamineA, alfa).
supplement(vitamineB12, alfa).
supplement(vitamineD, alfa).
supplement(magnesium, alfa).
supplement(ijzer, alfa).
supplement(calcium, alfa).

%klachten die veroorzaakt worden door
klacht(vermoeidheid).
klacht(depressie).
klacht(geheugen).
klacht(hoofdpijn).
klacht(stress).
klacht(spierproblemen).
klacht(zwangerschap).

%lichaamswaardes
:- dynamic lengte/1.
:- dynamic gewicht/1.
:- dynamic geslacht/1.
:- dynamic leeftijd/1.
%-------------------------------------------


%relaties
%-------------------------------------------
%word veroorzaakt door
%klacht K word_veroorzaakt door supplement S probleem P
oorzaak(klacht(zwangerschap), vitamineA, overschot).
oorzaak(klacht(vermoeidheid), vitamineB12, tekort).
oorzaak(klacht(vermoeidheid), vitamineD, tekort).
oorzaak(klacht(vermoeidheid), magnesium, tekort).
oorzaak(klacht(vermoeidheid), ijzer, tekort).
oorzaak(klacht(depressie), vitamineD, tekort).
oorzaak(klacht(geheugen), vitamineB12, tekort).
oorzaak(klacht(stress), magnesium, tekort).
oorzaak(klacht(spierproblemen), magnesium, tekort).

%word berekend met
% lichaamsgegevens van de gebruiker
% BMI berekend met gegevens lichaamswaardes.
lichaam(lengte(L), gewicht(G), geslacht(m), leeftijd(_), BMI):-
    BMI is G/(L*L).
%-------------------------------------------



%regels
% ------------------------------------------
% wat het probleem is met een bepaald supplement S met inname X voor een
% persoon met EEN bepaald BMI
probleem(S, X, lichaam(_,_,_,_,BMI), tekort):-
    supplement(S, Y),
    X is Y*BMI.
probleem(vitamineA, X, lichaam(_,_,geslacht(v),_, BMI), overschot):-
    supplement(vitamineA, Y),
    X is Y*BMI.
%-------------------------------------------


%progamma
%-------------------------------------------
verwijderLichaam:-
    retractall(lengte(_)),
    retractall(gewicht(_)),
    retractall(geslacht(_)),
    retractall(leeftijd(_)).

initialisatie:-
  verwijderLichaam,
  asserta(lengte(1)),
  asserta(gewicht(2)),
  asserta(geslacht(m)),
  asserta(leeftijd(3)).
:-initialisatie.

lichaaminvoer("j").
lichaaminvoer("n"):-
  verwijderLichaam,
  writeln('wat is uw lengte in meter?'),
  read_line_to_string(user_input, L),
  number_string(L2, L),
  asserta(lengte(L2)),
  writeln('wat is uw gewicht in kilogrammen?'),
  read_line_to_string(user_input, W),
  number_string(W2, W),
  asserta(gewicht(W2)),
  writeln('wat is uw geslacht? m/v'),
  read_line_to_string(user_input, S),
  asserta(geslacht(S)),
  writeln('wat is uw leeftijd?'),
  read_line_to_string(user_input, A),
  number_string(A2, A),
  asserta(leeftijd(A2)),
  vraagomlichaam.

printLichaam :-
  lengte(L),
  gewicht(W),
  geslacht(S),
  leeftijd(A),
  writeln('uw lichaamswaardes zijn:'),
  format('lengte:   ~w meter ~ngewicht:  ~w kilogram ~ngeslacht: ~w ~nleeftijd: ~w jaar ~n', [L, W, S, A]),
  nl.

vraagomlichaam:-
%     repeat,
     write('\e[2J'),%tty_clear
     illustratie,
     nl,
     printLichaam,
     writeln('is dit correct? j/n'),
     read_line_to_string(user_input, Keuze),
     lichaaminvoer(Keuze).

klachteninvoer("j").
klachteninvoer("n"):-
  writeln("hier implementeren hoe de klachten opgevraagd worden").

printhuidigeklachten:-
    writeln("u heeft aangegeven dat u last heeft van: "),
    nl,
    writeln('hier implementeren hoe de klachten weergegeven worden').

vraagklachten:-
    format('~46t~72|~n'),
    printhuidigeklachten,
    nl,
    writeln('is dit correct? j/n'),
    read_line_to_string(user_input, Keuze),
    klachteninvoer(Keuze),
    format('~46t~72|~n').

printtekorten:-
  writeln("hier de implementatie van wat de tekorten zijn").

bepaaltekorten:-
  writeln("we gaan bepalen wat uw tekorten zijn"),
  printhuidigeklachten,
  writeln("op basis hiervan zijn uw tekorten"),
  printtekorten.

verwerk_keuze("0").
verwerk_keuze("1"):-
  vraagomlichaam.
verwerk_keuze("2"):-
  vraagklachten.
verwerk_keuze("3"):-
  bepaaltekorten.


kennissysteem :-
  repeat,
  write('\e[2J'),
  illustratie,
  format('~46t~72|~n'),
  writeln("kennissysteem om te bepalen waar de klachten door komen"),
  writeln('1. Voer lichaams gegevens in'),
  writeln('2. voer klachten in'),
  writeln('3. bepaal tekorten'),
  writeln('0. Afsluiten'),
  format('~46t~72|~n'),
  read_line_to_string(user_input, Keuze),
  verwerk_keuze(Keuze),
  Keuze == "0".


  %code om te checken of er al lichaamsgegevens zijn ingevoerd.
  %code die vraagt naar de klachten.
  %code die klachten vergelijkt en redeneert wat het probleem is.
  %als meer dan 1 probleem mogelijk vraag dan naar inname om onduidelijkheid te verbeteren.
%  write('ik heb slecht nieuws voor je.\n'),
%  write('je gaat dood vanwege teveel schoonmaakmiddel in je bloed.').


bmi:-
  write('wat is je lengte in meter?\n'),
  read_line_to_string(user_input, L1),
  write('hoe zwaar ben je in kilogram?\n'),
  read_line_to_string(user_input, G1),
  number_string(L, L1),
  number_string(G, G1),
  X is G/(L*L),
  write('je BMI is: '),
  write(X).

illustratie:-
  writeln(" ,--./,-."),
  writeln("/ #      \\"),
  writeln("|         |"),
  writeln("\\        /"),
  writeln(" `._,._,'").

%:- kennissysteem.
