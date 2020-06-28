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
klacht(geheugenproblemen).
klacht(hoofdpijn).
klacht(stress).
klacht(spierproblemen).
klacht(zwangerschap).
klacht(botklachten).

%producten die veel van een supplement bevatten
zitin(supplement(vitamineB12), zuivel).
zitin(supplement(vitamineB12), vlees).
zitin(supplement(vitamineB12), ei).
zitin(supplement(vitamineB12), vis).
zitin(supplement(vitamineD), zuivel).
zitin(supplement(vitamineD), vlees).
zitin(supplement(vitamineD), ei).
zitin(supplement(vitamineD), vis).
zitin(supplement(calcium), zuivel).
zitin(supplement(calcium), 'bepaalde groente (bijvoorbeeld boerenkool of spinazie)').
zitin(supplement(calcium), noten).
zitin(supplement(magnesium), 'pinda\'s').
zitin(supplement(magnesium), spinazie).
zitin(supplement(magnesium), volkorenproducten).
zitin(supplement(ijzer), vlees).
zitin(supplement(ijzer), tahoe).
zitin(supplement(ijzer), boerenkool).
zitin(supplement(ijzer), noten).
zitin(supplement(ijzer), ei).
zitin(supplement(ijzer), bonen).
zitin(supplement(ijzer), volkorenproducten).
zitin(supplement(vitamineA), vlees).
zitin(supplement(vitamineA), zuivel).
zitin(supplement(vitamineA), ei).
zitin(supplement(vitamineA), vis).

%welke klachten de gebruiker wel en niet heeft
:- dynamic klachten/2.

%lichaamswaardes
:- dynamic lengte/1.
:- dynamic gewicht/1.
:- dynamic geslacht/1.
:- dynamic leeftijd/1.
:- dynamic zwanger/1.
%-------------------------------------------

%gewichtsklasse 30, 60, 80, 100
gewichtsklasse(1):-
  gewicht(G),
  G>=30,
  G<60.
gewichtsklasse(2):-
  gewicht(G),
  G>=60,
  G<80.
gewichtsklasse(3):-
  gewicht(G),
  G>=80,
  G<100.
gewichtsklasse(4):-
  gewicht(G),
  G>=100.

%lengteklasse 140 150 170 190
lengteklasse(1):-
  lengte(L),
  L>=1.40,
  L<1.50.
lengteklasse(2):-
  lengte(L),
  L>=1.50,
  L<1.70.
lengteklasse(3):-
  lengte(L),
  L>=1.70,
  L<1.90.
lengteklasse(1):-
  lengte(L),
  L>=1.90.

%leeftijdklasse 10, 30, 50, 70
leeftijdklasse(1):-
  leeftijd(L),
  L>=10,
  L<30.
leeftijdklasse(2):-
  leeftijd(L),
  L>=30,
  L<50.
leeftijdklasse(3):-
  leeftijd(L),
  L>=50,
  L<70.
leeftijdklasse(4):-
  leeftijd(L),
  L>=70.

%hoeveel iemand nodig heeft van welk supplement voor welk lichaam
%tabel te vinden in verslag is gehaald van voedingscentrum
nodig(vitamineD, '15mcg').

nodig(vitamineB12, '1.8mcg'):-
  leeftijdklasse(1).
nodig(vitamineB12, '2.4mcg').

nodig(ijzer, '18mg'):-
  geslacht("v"),
  leeftijdklasse(2).
nodig(ijzer, '18mg'):-
  geslacht("v"),
  leeftijdklasse(3).
nodig(ijzer, '8mg').

nodig(calcium, '1300mg'):-
  leeftijdklasse(1).
nodig(calcium, '1200mg'):-
  leeftijdklasse(4),
  geslacht("v").
nodig(calcium, '1000mg').

nodig(magnesium, '240mg'):-
  leeftijdklasse(1).
nodig(magnesium, '400mg'):-
  leeftijdklasse(2),
  geslacht("m").
nodig(magnesium, '310mg'):-
  leeftijdklasse(2),
  geslacht("v").
nodig(magnesium, '420mg'):-
  geslacht("m").
nodig(magnesium, '320mg'):-
  geslacht("v").

nodig(vitamineA, '600mcg'):-
  leeftijdklasse(1).
nodig(vitamineA, '900mcg'):-
  geslacht("m").
nodig(vitamineA, '700mcg').

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
oorzaak(klacht(geheugenproblemen), vitamineB12, tekort).
oorzaak(klacht(stress), magnesium, tekort).
oorzaak(klacht(spierproblemen), magnesium, tekort).
oorzaak(klacht(botklachten), calcium, tekort).
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
    retractall(leeftijd(_)),
    retractall(zwanger(_)).


lichaaminvoer("j").
lichaaminvoer("n"):-
  verwijderLichaam,
  writeln('Wat is uw lengte in meters? Decimalen graag invoeren met een punt en geen komma'),
  read_line_to_string(user_input, L),
  writeln('Wat is uw gewicht in kilogrammen?'),
  read_line_to_string(user_input, W),
  writeln('Wat is uw geslacht? m/v'),
  read_line_to_string(user_input, S),
  writeln('Wat is uw leeftijd?'),
  read_line_to_string(user_input, A),
  lichaamToevoegen(L, W, S, A).

issexe("m"):-
  asserta(zwanger("n")),
  iszwanger.
issexe("v"):-
  writeln('U heeft aangegeven een vrouw te zijn'),
  writeln('Bent u momenteel zwanger? j/n'),
  nl,
  vraag(Zwanger),
  asserta(zwanger(Zwanger)),
  iszwanger.
iszwanger:-
  zwanger("j"),
  retractall(klachten(klacht(zwangerschap), _)),
  asserta(klachten(klacht(zwangerschap), ja)).
iszwanger:-
  not(zwanger("ja")),
  retractall(klachten(klacht(zwangerschap), _)),
  asserta(klachten(klacht(zwangerschap), nee)).

lichaamToevoegen(L, W, S, A):-
  number_string(L2, L),
  asserta(lengte(L2)),
  number_string(W2, W),
  asserta(gewicht(W2)),
  issexe(S),
  asserta(geslacht(S)),
  number_string(A2, A),
  asserta(leeftijd(A2)).

lichaamToevoegen(_,_,_,_):-
  writeln('Deze waardes kunnen niet goed door het systeem verwerkt worden.'),
  writeln('Probeer het opnieuw met inachtneming van de aanwijzingen.'),
  nl,
  lichaaminvoer("n").



%schone manier om duidelijk de vraag te onderscheiden
vraag(Keuze):-
  nl,
  read_line_to_string(user_input, Keuze),
  nl.





printLichaam :-
  format('~46t~72|~n'),
  lengte(L),
  gewicht(W),
  geslacht(S),
  leeftijd(A),
  writeln('Uw lichamelijke gegevens zijn:'),
  format('lengte:   ~w meter ~ngewicht:  ~w kilogram ~ngeslacht: ~w ~nleeftijd: ~w jaar ~n', [L, W, S, A]),
  format('~46t~72|~n'),
  nl.

vraagomlichaam:-
  repeat,
  write('\e[2J'),%tty_clear
%  illustratie,
  nl,
  printLichaam,
  writeln('Zijn de gegevens correct? j/n'),
  vraag(Keuze),
  lichaaminvoer(Keuze),
  ksy.

klachteninvoer("0"):-
  ksy.
klachteninvoer("1"):-
  vraagnieuweklachten.
klachteninvoer("2"):-
  retractall(klachten(_, _)),
  iszwanger,
  writeln('Nu staan er geen klachten meer in het systeem'),
  writeln('Om te weten wat uw tekorten zijn moet u klachten toevoegen'),
  vraagklachten.

lastvan(Last):-
    findall(K1, klachten(klacht(K1), ja), Last).
geenlast(Last):-
      findall(K2, klachten(klacht(K2), nee), Last).

printhuidigeklachten:-
    nl,
%    format('~46t~72|~n'),
    lastvan(Last),
    writeln("U heeft aangegeven de volgende klachten/symptomen te hebben: "),
    schrijfopsomming(Last),
    nl,
    geenlast(GLast),
    writeln("U heeft aangegeven de volgende klachten/symptomen niet te hebben: "),
    schrijfopsomming(GLast),
    format('~46t~72|~n').

vraagnieuweklachten:-
  ongevraagdeklachten(X),
  vraagnaarklacht(X).

vraagnaarklacht([]):-
%  writeln("we hebben op dit moment niks meer om te vragen"),
  vraagklachten.
vraagnaarklacht([H|T]):-
  format("Heeft u last van ~w~n", [H]),
  writeln("1. Ja ik heb hier last van"),
  writeln("2. Nee ik heb hier geen last van"),
  nl,
  writeln("0. Dit kan of wil ik niet zeggen"),
  nl,
  vraag(Keuze),
  nieuweklacht(Keuze, H, T).

nieuweklacht("1", X, Y):-
  asserta(klachten(klacht(X), ja)),
  vraagnaarklacht(Y).
nieuweklacht("2", X, Y):-
  asserta(klachten(klacht(X), nee)),
  vraagnaarklacht(Y).
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
  repeat,
  write('\e[2J'),
%  format('~46t~72|~n'),
  printhuidigeklachten,
  nl,
  writeln('1. Symptomen/klachten invoeren.'),
  writeln('2. Dit is incorrect, wis alle ingevoerde klachten.'),
  nl,
  writeln('0. Dit is correct, ga terug naar het hoofdmenu.'),
  format('~46t~72|~n'),
  vraag(Keuze),
  klachteninvoer(Keuze).

vindalletekorten([], []).
vindalletekorten([Sup|T], [Sup|T2]):-
  heefttekort(Sup),
  vindalletekorten(T, T2).
vindalletekorten([Sup|T], T2):-
  not(heefttekort(Sup)),
  vindalletekorten(T, T2).

verwerk_keuze("0").
verwerk_keuze("1"):-
  vraagomlichaam.
verwerk_keuze("2"):-
  vraagklachten.
verwerk_keuze("3"):-
  findall(Sup,supplement(Sup) , Supplementen),
  nl,nl,nl,nl,
  vindalletekorten(Supplementen, Tekorten),
  leguit(Tekorten).

%wrapper voor lekker kort typen
ksy:- kennissysteem.
kennissysteem :-
  repeat,
%  write('\e[2J'),    weggehaald vanwege problemen met diagnose
  nl,
  illustratie,
  nl,
  format('~46t~72|~n'),
  writeln("DE BLIJE APPEL:"),
  writeln("Worden mijn klachten veroorzaakt door tekorten aan vitamines en mineralen?"),
  nl,
  writeln('1. Controleer lichamelijke gegevens'),
  writeln('2. Voer klachten in'),
  writeln('3. Bepaal tekorten'),
  nl,
  writeln('0. Afsluiten'),
  format('~46t~72|~n'),
  vraag(Keuze),
  verwerk_keuze(Keuze).



heefttekort(Sup):-
  supplement(Sup),
  findall(K, oorzaak(K, Sup, _), Klachten),
  heefttekorten(Klachten).

leguit([]):-
  ksy.
leguit([Sup|T]):-
  format('~46t~72|~n'),
  Sup = vitamineA,
  nodig(Sup, Nodig),
  findall(K, oorzaak(K, vitamineA, _), Klachten),
  heefttekorten(Klachten),
  write("U heeft mogelijk last van "),
  write(Sup),
  writeln(' overschot,'),
  writeln('omdat u heeft aangegeven dat zwanger bent'),
  nl,
  writeln('Vrouwen die zwanger zijn, hebben een hoger risico op een vitamine A overschot.'),
  writeln('Wees dus voorzichtig met het eten van voedsel waar dit in zit.'),
  nl,
  format('~w zit in voeding als:~n', [Sup]),
  findall(Bevat, zitin(supplement(Sup), Bevat), Voeding),
  schrijfopsomming(Voeding),
  nl,
  format('Voor een persoon met uw lichaamswaardes wordt ongeveer ~w van ~w aanbevolen als maximale hoeveelheid per dag.', [Nodig, Sup]),
  nl,
  leguit(T).
leguit([Sup|T]):-
  format('~46t~72|~n'),
  nodig(Sup, Nodig),
  findall(K, oorzaak(K, Sup, _), Klachten),
  heefttekorten(Klachten),
  write("U heeft mogelijk last van "),
  write(Sup),
  writeln(' tekort,'),
  writeln('omdat u heeft aangegeven dat u last heeft van'),
  parseklachten(Klachten, KL2),
  schrijfopsomming(KL2),
  nl,
  format('~w is te halen uit voeding als:~n', [Sup]),
  findall(Bevat, zitin(supplement(Sup), Bevat), Voeding),
  schrijfopsomming(Voeding),
  writeln('Indien u dit niet kunt of wilt eten kunt u ook supplementen gebruiken.'),
  nl,
  format('Een persoon met uw lichaamswaardes wordt ongeveer ~w van ~w aanbevolen per dag.', [Nodig, Sup]),
  nl,
  leguit(T).

schrijfklachten([klacht(K)]):-
  write(K),
  nl.
schrijfklachten([klacht(H)|T]):-
  write(H),
  write(', '),
  schrijfklachten(T).

heefttekorten([]).
heefttekorten([H|T]):-
  klachten(H, ja),
  heefttekorten(T).

parseklachten([],[]).
parseklachten([klacht(X)|T1],[X|T2]):-
  parseklachten(T1,T2).


/*
schrijfopsomming([]).
schrijfopsomming([X]):-
  write(X),
  write('.'),
  nl.
schrijfopsomming([X, Y]):-
  write(X),
  write(' en '),
  write(Y),
  write('.'),
  nl.
schrijfopsomming([H|T]):-
  write(H),
  write(', '),
  schrijfopsomming(T).
*/
schrijfopsomming(X):-
  writeln(X).


illustratie:-
  writeln(" ,--./,-."),
  writeln("/ #      \\"),
  writeln("|   o o   |"),
  writeln("\\    v   /"),
  writeln(" `._,._,'").


initialisatie:-
  lichaaminvoer("n"),
  retractall(klachten(_, _)),
  iszwanger,
  ksy.
:- initialisatie.
