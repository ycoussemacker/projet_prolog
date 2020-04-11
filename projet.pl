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

% Liste Perso
listePerso([]).
listePerso([T|Q]) :- personnage(T), listePerso(Q).

% prédicat tuile (ajout à la bdd de toutes les tuiles possibles initialisées avec des listes de perso vides)
:- asserta(tuile(1,1,[])).
:- asserta(tuile(1,2,[])).
:- asserta(tuile(1,3,[])).
:- asserta(tuile(1,4,[])).
:- asserta(tuile(2,1,[])).
:- asserta(tuile(2,3,[])).
:- asserta(tuile(2,2,[])).
:- asserta(tuile(2,4,[])).
:- asserta(tuile(3,1,[])).
:- asserta(tuile(3,2,[])).
:- asserta(tuile(3,3,[])).
:- asserta(tuile(3,4,[])).
:- asserta(tuile(4,1,[])).
:- asserta(tuile(4,2,[])).
:- asserta(tuile(4,3,[])).
:- asserta(tuile(4,4,[])).

% prédiact déplacer => ajouter un personnage au début de la listePerso d'une tuile et le supprimer de la listePerso de la tuile d'origine
deplacer(Perso, Ligne, Colonne) :- personnage(Perso), supprimer(Perso), ajouter(Perso, Ligne, Colonne).

% supprime l'ancienne tuile pour la remplacer par la tuile avec la liste de perso actualisée
ajouter(Perso, Ligne, Colonne) :-  retract(tuile(Ligne,Colonne, ListePerso)), asserta(tuile(Ligne,Colonne,[Perso|ListePerso])).

% Cherche parmis toutes les tuiles qu'il possède celle qui contient le personnage et supprime le personnage de cette dernière.
supprimer(Perso) :- tuile(L,C,ListePerso), dans(Perso,ListePerso), supprimer(Perso,ListePerso, NewList), retract(tuile(L,C,_)), asserta(tuile(L,C,NewList)).% On récupère la liste des perso à partir de ligne et ccolonne pour en supprimer le Perso
supprimer(Perso, [T|Q], [T|QT]) :- Perso\=T, supprimer(Perso,Q, QT).
supprimer(Perso, [Perso|Q], QT) :- supprimer(Perso, Q, QT).
supprimer(_,[],[]).

% Renseigne si un element X est dans une liste.
dans(X,[X|_]). 
dans(X,[_|Q]) :- dans(X,Q).

% Permet l'affichage du plateau en appuyant sur le ";" dans la console ProLog
plateau(X,Y,LP):- tuile(X,Y,LP).

% Initialiser placement aléatoire des perso pour commencer le jeu
% Initialiser placement des 4 cases sniper

% Début de la partie = les scores sont nuls
pointsUtilisateur(0).
pointsOrdi(0).

% Passer du tour du joueur au tour de l'ordinateur
nouveauTour:-tour(utilisateur), retract(tour(utilisateur)), assert(tour(ordi)).
nouveauTour:-tour(ordi), retract(tour(ordi)), assert(tour(utilisateur)).

% Je suis pas tellement sure de ça j'ai testé un truc
% Déplacer un personnage : P pour le nom du perso, X et Y pour les coordonnees de la case où le perso va aller
deplacer(P,X,Y):- case(X,Y), retract(personnage(P,_,_)), assert(personnage(P,X,Y)).
