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
  asserta(leeftijd(A2)).



printLichaam :-
  lengte(L),
  gewicht(W),
  geslacht(S),
  leeftijd(A),
  writeln('uw lichaamswaardes zijn:'),
  format('lengte:   ~w meter ~ngewicht:  ~w kilogram ~ngeslacht: ~w ~nleeftijd: ~w jaar ~n', [L, W, S, A]),
  nl.

vraagomlichaam:-
     write('\e[2J'),%tty_clear
     repeat,
     nl,
     printLichaam,
     writeln('is dit correct? j/n'),
     read_line_to_string(user_input, Keuze),
     write(Keuze),
     lichaaminvoer(Keuze),
     Keuze == "j".








kennissysteem():-
    %code om te checken of er al lichaamsgegevens zijn ingevoerd.
    %code die vraagt naar de klachten.
    %code die klachten vergelijkt en redeneert wat het probleem is.
    %als meer dan 1 probleem mogelijk vraag dan naar inname om onduidelijkheid te verbeteren.
    write('ik heb slecht nieuws voor je.\n'),
    write('je gaat dood vanwege teveel schoonmaakmiddel in je bloed.').


bmi():-
    write('wat is je lengte in meter?\n'),
    read(L),
    write('hoe zwaar ben je in kilogram?\n'),
    read(G),
    X is G/(L*L),
    write('je BMI is: '),
    write(X).
