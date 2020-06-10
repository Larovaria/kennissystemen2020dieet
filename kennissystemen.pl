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
lengte(_).
gewicht(_).
geslacht(m).
geslacht(v).
leeftijd(_).
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

