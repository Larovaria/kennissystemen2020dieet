%concepten
%-------------------------------------------
%lijst van supplementen
supplement(vitamineA).
supplement(vitamineB12).
supplement(vitamineD).
supplement(magnesium).
supplement(ijzer).
supplement(calcium).

%klachten die veroorzaakt worden door
klacht(vermoeidheid).
klacht(depressie).
klacht(geheugen).
klacht(hoofdpijn).
klacht(stress).
klacht(spierproblemen).
klacht(zwangerschap).

%welke klachten de gebruiker wel en niet heeft
:- dynamic klachten/2.

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
  retractall(klachten(_, _)).
:-initialisatie.

%schone manier om duidelijk de vraag te onderscheiden
vraag(Keuze):-
  nl,
  read_line_to_string(user_input, Keuze),
  nl.



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
  format('~46t~72|~n'),
  lengte(L),
  gewicht(W),
  geslacht(S),
  leeftijd(A),
  writeln('uw lichaamswaardes zijn:'),
  format('lengte:   ~w meter ~ngewicht:  ~w kilogram ~ngeslacht: ~w ~nleeftijd: ~w jaar ~n', [L, W, S, A]),
  format('~46t~72|~n'),
  nl.
printLichaam:-
  writeln('er zijn  geen lichaamsgegevens ingevuld'),
  lichaaminvoer("n").

vraagomlichaam:-
     repeat,
     write('\e[2J'),%tty_clear
     illustratie,
     nl,
     printLichaam,
     writeln('is dit correct? j/n'),
     vraag(Keuze),
     lichaaminvoer(Keuze),
     Keuze=="j".

klachteninvoer("0").
klachteninvoer("1"):-
  vraagnieuweklachten.
klachteninvoer("2"):-
  retractall(klachten(_, _)),
  writeln('Nu staan er geen klachten meer in het systeem'),
  writeln('om te weten wat uw tekorten zijn moet u klachten toevoegen').

lastvan(Last):-
    findall(K1, klachten(klacht(K1), ja), Last).
geenlast(Last):-
      findall(K2, klachten(klacht(K2), nee), Last).

printhuidigeklachten:-
    nl,
    format('~46t~72|~n'),
    lastvan(Last),
    writeln("u heeft aangegeven dat u last heeft van: "),
    writeln(Last),
    nl,
    geenlast(GLast),
    writeln("u heeft aangegeven dat u geen last heeft van: "),
    writeln(GLast),
    format('~46t~72|~n').

vraagnieuweklachten:-
  ongevraagdeklachten(X),
  vraagnaarklacht(X).

vraagnaarklacht([]):-
  writeln("we hebben op dit moment niks meer om te vragen").
vraagnaarklacht([H|T]):-
  format("Heeft u last van ~w~n", [H]),
  writeln("1, Ja ik heb hier last van"),
  writeln("2. Nee ik heb hier geen last van"),
  writeln("0. Dit kan of wil ik niet zeggen"),
  vraag(Keuze),
  nieuweklacht(Keuze, H, T).

nieuweklacht("1", X, _):-
  asserta(klachten(klacht(X), ja)).
nieuweklacht("2", X, _):-
  asserta(klachten(klacht(X), nee)).
nieuweklacht("0", _, Y):-
  vraagnaarklacht(Y).

ongevraagdeklachten(X):-
  findall(K1, klachten(klacht(K1), ja), L1),
  findall(K2, klachten(klacht(K2), nee), L2),
  findall(K3, klacht(K3), L3),
  removeklachten(L3, L1, X1),
  removeklachten(X1, L2, X).

removeklachten(L1, [Rh|Rt], L2):-
  delete(L1, Rh, L),
  removeklachten(L, Rt, L2).
removeklachten(L, [], L).


vraagklachten:-
    format('~46t~72|~n'),
    printhuidigeklachten,
    nl,
    writeln('1. nog een klacht toevoegen'),
    writeln('2. dit is incorrect, wis alles'),
    writeln('0. dit is correct, ga terug'),
    vraag(Keuze),
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
  findall(Sup,supplement(Sup) , Supplementen),
  vindalletekorten(Supplementen).

vindalletekorten([]).
vindalletekorten([Sup|T]):-
  heefttekort(Sup),
  vindalletekorten(T).

zijnernogklachten:-
  ongevraagdeklachten(X),
  length(X, Y),
  Y > 0,
  writeln('er zijn nog klachten die ingevoerd kunnen worden').
zijnernogklachten:-
  writeln("u kunt uw tekorten bepalen").

%wrapper voor lekker kort typen
ksy:- kennissysteem.
kennissysteem :-
  repeat,
  write('\e[2J'),
  illustratie,
  nl,
  zijnernogklachten,
  nl,
  format('~46t~72|~n'),
  writeln("kennissysteem om te bepalen waar de klachten door komen"),
  writeln('1. Voer lichaams gegevens in'),
  writeln('2. voer klachten in'),
  writeln('3. bepaal tekorten'),
  writeln('0. Afsluiten'),
  format('~46t~72|~n'),
  vraag(Keuze),
  verwerk_keuze(Keuze),
  Keuze == "0".

%TODO
heefttekort(Sup):-
  supplement(Sup),
  findall(K, oorzaak(K, Sup, tekort), Klachten),
  heefttekorten(Klachten),
  write("U heeft mogelijk last van "),
  write(Sup),
  writeln(' tekort'),
  writeln('dit is beredeneert omdat u aangegeven dat u last heeft van'),
  writeln(Klachten),
  nl,nl.

heefttekorten([]).
heefttekorten([H|T]):-
  klachten(H, ja),
  heefttekorten(T).

illustratie:-
  writeln(" ,--./,-."),
  writeln("/ #      \\"),
  writeln("|         |"),
  writeln("\\        /"),
  writeln(" `._,._,'").

%:- kennissysteem.
