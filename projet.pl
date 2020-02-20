% Déclaration dynamique
:-dynamic(tuile/3).
:-retractall(tuile(_,_,_)).
% :-retractall(listePerso(_)).





% liste des 16 perso
personnage(girafe).
personnage(lion).
personnage(morse).
personnage(crocodile).
personnage(renard).
personnage(lapin).
personnage(loup).
personnage(chat).
personnage(chien).
personnage(canard).
personnage(oie).
personnage(elephant).
personnage(loutre).
personnage(poisson).
personnage(souris).
personnage(serpent).

% liste des 3policiers
personnage(policier1).
personnage(policier2).
personnage(policier3).

% liste des viseurs (ligne, colonne)
viseur(1,1).
viseur(1,4).
viseur(4,1).
viseur(4,4).
viseur(2,2).
viseur(2,3).
viseur(3,2).
viseur(3,3).

%  Liste armes
arme(pistolet).
arme(couteau).
arme(fusil).

% prédicat des coordonées
ligne(N):-N=<4, N>0.
colonne(N):-N=<4, N>0.

% Liste Perso
listePerso([]).
listePerso([T|Q]) :- personnage(T), listePerso(Q).

% prédicat tuile
tuile(Ligne, Colonne, ListePerso). %:- ligne(Ligne), colonne(Colonne), listePerso(ListePerso).

% prédiact déplacer => ajouter un personnage au début de la listePerso d'une tuile et le supprimer de la listePerso de la tuile d'origine
deplacer(Perso, Ligne, Colonne) :- personnage(Perso), retract(tuile(Ligne,Colonne, ListePerso)), asserta(tuile(Ligne,Colonne,[Perso|ListePerso])).

% ajouter(Perso, ListePerso, [Perso|ListePerso]).



% p(X,Y):-gkjsdngksjdngksdjb, retract(fait(Z)), NZ is Z+3, asserta(fait(NZ)), 

% memorise:- travail(X), asserta(fait(X)).




%