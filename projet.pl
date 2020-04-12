% Déclaration dynamique
:-dynamic(tuile/3).
:-retractall(tuile(_,_,_)).

:-dynamic(morts/1).
:-retractall(morts(_)).

:-dynamic(inutiles/1).
:-retractall(inutiles(_)).

:-dynamic(vivants/1).
:-retractall(vivants(_)).

:-dynamic(tour/1).
:-retractall(tour(_)).

:-dynamic(points/2).
:-retractall(points(_,_)).

:-dynamic(tueur/2).
:-retractall(tueur(_,_)).

:-dynamic(cibles/2).
:-retractall(cibles(_,_)).

% les joueurs
joueurs([utilisateur, ordi]).

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

% -------- DEBUT DE LA PARTIE ON INITIALISE TOUTES LES DONNEES ---------

% -- Prédicat tuile (ajout à la bdd de toutes les tuiles possibles initialisées avec des listes de perso vides)
% tous les personnages sont morts au début. On va les ressusiter (=déplacer dans la liste des vivant) en attribuant à chaque case un de ces personnages.
:-asserta(morts([girafe,lion, morse, crocodile, renard, lapin, loup, chat, chien, canard,oie, elephant, loutre, poisson, souris, serpent])).
:-asserta(inutiles([girafe,lion, morse, crocodile, renard, lapin, loup, chat, chien, canard,oie, elephant, loutre, poisson, souris, serpent])).
:-asserta(vivants([])).
:-asserta(cibles(utilisateur,[])).
:-asserta(cibles(ordi,[])).

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

debutPartie():- vivants(L),             % On récupère la liste des vivants
                longueur(N,L), N < 1,   % On démare la partie seulement si la liste des vivants est vide
                attribution(), 
                donneTueur(),
                donneCibles(), 
                joueurs(ListeJoueurs), random_member(J,ListeJoueurs), assert(tour(J)), %On choisi au hasard qui commence
                !.

attribution():- retract(tuile(Ligne, Colonne, LP)),         % On supprime la tuile et on récupère chaque Ligne et colonne (début de la boucle)
                retract(morts(ListeMorts)),                 % On récupère la liste des personnages à placer (en la supprimant)
                random_member(Perso, ListeMorts),           % on choisi au hasard un Personnage dans la liste des perso à placer                
                assert(tuile(Ligne,Colonne, [Perso|LP])),   % On ajoute le personnage à la tuile correspondante 
                retract(vivants(ListeVivants)),
                assert(vivants([Perso|ListeVivants])),      % On ajoute la liste des vivants avec le personnage ajouté
                supprimer(Perso,ListeMorts, NewList),       % On récupère la liste actualisée des personnages à placer
                assert(morts(NewList)),                     % On ajoute la nouvelle liste des perso à placer
                longueur(N, NewList), N < 1.                % On termine le programme si il n'y a plus de perso à placer. (Fin de la boucle)

donneTueur():-  retract(inutiles(L1)),              %recupère la liste des perso inutilisés
                random_member(Perso1, L1),          % Choisi au hasard un personnage dans cette liste
                assert(tueur(utilisateur,Perso1)),  % selectionne ce personnage comme tueur pour l'utilisateur
                supprimer(Perso1, L1, L2),          % supprime ce personnage de la liste des inutiles
                random_member(Perso2, L2),          % choisi un deuxième utilisateur
                assert(tueur(ordi,Perso2)),          % selectionne ce perso comme tueur de l'ordi
                supprimer(Perso2, L2, L3),          % supprime ce perso de la liste des inutiles
                assert(inutiles(L3)).                % actualise la liste des inutiles.

donneCibles():- donneCibles(utilisateur), donneCibles(ordi).
donneCibles(J):-
                retract(inutiles(L1)),              % On recupère la liste des inutilisés
                random_member(Perso1, L1),          % On choisi au hasard un perso parmi la liste
                supprimer(Perso1, L1, L2),          % on supprime ce personnage de la liste des inutiles
                random_member(Perso2, L2),          % On choisi au hasard un 2ème perso parmi la nouvelle liste
                supprimer(Perso2, L2, L3),          % supprime ce personnage de la liste des inutiles
                random_member(Perso3, L3),           % On choisi au hasard un 3ème perso parmi la nouvelle liste
                supprimer(Perso3, L3, L4),          % supprime ce personnage de la liste des inutiles 
                assert(inutiles(L4)),               % On actualise la liste des inutiles
                retract(cibles(J,L)), 
                assert(cibles(J,[Perso3|[Perso2|[Perso1|L]]])).

% Début de la partie = les scores sont initialisés à 2 car leur tueur à gage est toujours en vadrouille.
:- asserta(points(utilisateur,2)).
:- asserta(points(ordi,2)).

% Passer du tour du joueur au tour de l'ordinateur
nouveauTour():-tour(utilisateur), retract(tour(utilisateur)), assert(tour(ordi)).
nouveauTour():-tour(ordi), retract(tour(ordi)), assert(tour(utilisateur)).


% ----------------------- ACTIONS DES JOUEURS ---------------------------

% ------ DEPLACER ----- 
% Prédicat déplacer => ajouter un personnage au début de la listePerso d'une tuile et le supprimer de la listePerso de la tuile d'origine
deplacer(Perso, Ligne, Colonne) :- personnage(Perso), supprimer(Perso), ajouter(Perso, Ligne, Colonne), !.
% Ajoute le personnage à la tuile renseignée
ajouter(Perso, Ligne, Colonne) :-  retract(tuile(Ligne,Colonne, ListePerso)), asserta(tuile(Ligne,Colonne,[Perso|ListePerso])).
% Cherche parmis toutes les tuiles qu'il possède celle qui contient le personnage et supprime le personnage de cette dernière.
supprimer(Perso) :- tuile(L,C,ListePerso), dans(Perso,ListePerso), supprimer(Perso,ListePerso, NewList), retract(tuile(L,C,_)), asserta(tuile(L,C,NewList)).% On récupère la liste des perso à partir de ligne et ccolonne pour en supprimer le Perso

% ----- PEUT TUER ------ 

% -- Couteau
peutTuer(P1,P2,Xt,Yt):- personnage(P1), personnage(P2), P1\=P2, % on selectionne deux personnages différents
                        tuile(Xt,Yt,L), dans(P1,L), dans(P2,L). % on selectionne la tuile dans laquelle les deux se trouvent. Is il y en a une => P1 peut tuer P2
                    
                    
% -- Pistolet
peutTuer(P1,P2,Xt,Yt):- personnage(P1), tuile(X,Y,L), dans(P1,L), longueur(N,L), N==1,  % On vérifie que P1 est seul dans sa case
                        personnage(P2), P1\=P2, tuileAdj(X,Y,Xt,Yt,Li), dans(P2,Li).    % On prend un personnage P2 faisant partie d'une tuile adjacente. Si il y en a une ==> P1 peut tuer P2

% -- Fusil
peutTuer(P1,P2,Xt,Yt):- personnage(P1), tuile(X,Yt,L), 
                        dans(P1,L), longueur(N,L), N==1, viseur(X,Yt),           % On vérifie que P1 est seul dans sa case et possède un viseur
                        personnage(P2), P1\=P2, tuile(Xt,Yt,Li), dans(P2,Li).    % On cherche une tuile dans la même colonne où P2 se trouve. Si on trouve une tuile ==> P1 peut tuer P2
peutTuer(P1,P2,Xt,Yt):- personnage(P1), tuile(Xt,Y,L), 
                        dans(P1,L), longueur(N,L), N==1 , viseur(Xt,Y),          % On vérifie que P1 est seul dans sa case
                        personnage(P2), P1\=P2, tuile(Xt,Yt,Li), dans(P2,Li),!.    % On cherche une tuile dans la même ligne où P2 se trouve. Si on trouve une tuile ==> P1 peut tuer P2


% ----- TUE ------
tue(P1,P2):-    peutTuer(P1,P2,X,Y),
                retract(vivants(LV)), supprimer(P2,LV,NLV), assert(vivants(NLV)),       % On actualise la liste des vivants
                retract(morts(LM)), assert(morts([P2|LM])),                             % On actualise la liste des morts
                retract(tuile(X,Y,LP)), supprimer(P2,LP,NLP), assert(tuile(X,Y,NLP)),   % On actualise la tuile
                donnePoints(P2).                                                        % On actualise les points

donnePoints(P2):-   tour(J), points(J,N), 
                    cibles(J,LC), dans(P2,LC), NewN is N + 1,               % Si il a éliminé une de ses cibles
                    retract(points(J,N)), assert(points(J,NewN)),           % on actualise les points
                    retract(cibles(J,LC)), supprimer(P2,LC,NLC),             % on actualise sa liste de cibles.
                    assert(cibles(J,NLC)).

donnePoints(P2):-   tour(J), points(J,NJ),
                    tueur(J2,P2), J2\=J, points(J2,NJ2),                     % Si le joueur tué est le tueur à gage de l'autre.
                    NewNJ is NJ + 3,                                        % Le joueur actuel gagne 3 points
                    NewNJ2 is NJ2 - 2,                                      % Son adversaire en perd 2 car il n'a plus son tueur à gage.
                    retract(points(J,_)), assert(points(J,NewNJ)),
                    retract(points(J2,_)), assert(points(J2, NewNJ2)).

donnePoints(_):-   tour(J), points(J,N),                                  % Il n'a tué ni une de ses cibles ni le tueur à gage de l'autre, c'est donc un innocent
                    NewNJ is N,
                    retract(points(J,N)), assert(points(J,NewNJ)).

% -- CONTROLER --



% ------------------ AFFICHAGE --------------------

% Permet l'affichage du plateau en appuyant sur le ";" dans la console ProLog
plateau(X,Y,LP):- tuile(X,Y,LP).

% ------------------ Fonctions générales utiles ------------------

% Supprime un element d'une liste (l'element est ici toujours un personnage).
supprimer(Perso, [T|Q], [T|QT]) :- Perso\=T, supprimer(Perso,Q, QT).
supprimer(Perso, [Perso|Q], QT) :- supprimer(Perso, Q, QT).
supprimer(_,[],[]).

% Renseigne si un element X est dans une liste.
dans(X,[X|_]). 
dans(X,[_|Q]) :- dans(X,Q).

% Renseigne si un element n'est pas dans une liste
nonDans(_,[]).
nonDans(X,[T|Q]):- X\=T, nonDans(X,Q), !.

% Renseigne la longueur d'une liste
longueur(N,[_|Q]):- longueur(N1,Q), N is N1+1.
longueur(0,[]).

% Concatener deux listes
conc([X],L2,[X|L2]).
conc([T1|Q1],L2,[T1|NQ]) :- conc(Q1,L2,NQ).

% Donne la liste de personnage des tuiles adjacentes à la tuile(X,Y,_)
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X-1, Yi is Y-1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X-1, Yi is Y  , tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X-1, Yi is Y+1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X  , Yi is Y-1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X  , Yi is Y+1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X+1, Yi is Y-1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X+1, Yi is Y  , tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X+1, Yi is Y+1, tuile(Xi,Yi,Li).