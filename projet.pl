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

:-dynamic(actions/2).
:-retractall(actions(_,_)).

% les joueurs
joueurs([utilisateur, ordi]).

% liste des 16 personnages
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

% liste des 3 policiers
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

%  Liste des armes
arme(pistolet).
arme(couteau).
arme(fusil).

% -------- DEBUT DE LA PARTIE ON INITIALISE TOUTES LES DONNEES ---------

% -- Prédicat tuile (ajout à la bdd de toutes les tuiles possibles initialisées avec des listes de perso vides)
% tous les personnages sont morts au début. On va les ressusciter (=déplacer dans la liste des vivants) en attribuant à chaque case un de ces personnages.
:-asserta(morts([girafe,lion, morse, crocodile, renard, lapin, loup, chat, chien, canard,oie, elephant, loutre, poisson, souris, serpent])).
:-asserta(inutiles([girafe,lion, morse, crocodile, renard, lapin, loup, chat, chien, canard,oie, elephant, loutre, poisson, souris, serpent])).
:-asserta(vivants([])).
:-asserta(cibles(utilisateur,[])).
:-asserta(cibles(ordi,[])).
:-asserta(actions([deplacer, tue, controle], 2)).

:- asserta(tuile(1,1,[])).
:- asserta(tuile(1,2,[])).
:- asserta(tuile(1,3,[])).
:- asserta(tuile(1,4,[])).
:- asserta(tuile(2,1,[])).
:- asserta(tuile(2,2,[])).
:- asserta(tuile(2,3,[])).
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
                longueur(N,L), N < 1,   % On démarre la partie seulement si la liste des vivants est vide
                attribution(), 
                donneTueur(),
                donneCibles(), 
                joueurs(ListeJoueurs), random_member(J,ListeJoueurs), assert(tour(J)), %On choisi au hasard qui commence
                write(J), write(' qui commence.'),
                !.

attribution():- retract(tuile(Ligne, Colonne, LP)),         % On supprime la tuile et on récupère chaque ligne et colonne (début de la boucle)
                retract(morts(ListeMorts)),                 % On récupère la liste des personnages à placer (en la supprimant)
                random_member(Perso, ListeMorts),           % On choisi au hasard un personnage dans la liste des perso à placer                
                assert(tuile(Ligne,Colonne, [Perso|LP])),   % On ajoute le personnage à la tuile correspondante 
                retract(vivants(ListeVivants)),
                assert(vivants([Perso|ListeVivants])),      % On ajoute la liste des vivants avec le personnage ajouté
                supprimer(Perso,ListeMorts, NewList),       % On récupère la liste actualisée des personnages à placer
                assert(morts(NewList)),                     % On ajoute la nouvelle liste des perso à placer
                longueur(N, NewList), N < 1.                % On termine le programme si il n'y a plus de perso à placer. (Fin de la boucle)

donneTueur():-  retract(inutiles(L1)),              % Recupère la liste des perso inutilisés
                random_member(Perso1, L1),          % Choisi au hasard un personnage dans cette liste
                assert(tueur(utilisateur,Perso1)),  % Sélectionne ce personnage comme tueur pour l'utilisateur
                supprimer(Perso1, L1, L2),          % Supprime ce personnage de la liste des inutiles
                random_member(Perso2, L2),          % Choisi un deuxième personnage
                assert(tueur(ordi,Perso2)),         % Selectionne ce perso comme tueur de l'ordi
                supprimer(Perso2, L2, L3),          % Supprime ce perso de la liste des inutiles
                assert(inutiles(L3)).               % Actualise la liste des inutiles.

donneCibles():- donneCibles(utilisateur), donneCibles(ordi).
donneCibles(J):-
                retract(inutiles(L1)),              % On recupère la liste des inutilisés
                random_member(Perso1, L1),          % On choisi au hasard un perso parmi la liste
                supprimer(Perso1, L1, L2),          % On supprime ce personnage de la liste des inutiles
                random_member(Perso2, L2),          % On choisi au hasard un 2ème perso parmi la nouvelle liste
                supprimer(Perso2, L2, L3),          % Suppression de ce personnage de la liste des inutiles
                random_member(Perso3, L3),          % On choisi au hasard un 3ème perso parmi la nouvelle liste
                supprimer(Perso3, L3, L4),          % Suppression de ce personnage de la liste des inutiles 
                assert(inutiles(L4)),               % On actualise la liste des inutiles
                retract(cibles(J,L)), 
                assert(cibles(J,[Perso3|[Perso2|[Perso1|L]]])).

% Début de la partie = les scores sont initialisés à 2 car leur tueur à gage est toujours en vadrouille.
:- asserta(points(utilisateur,2)).
:- asserta(points(ordi,2)).

% Passer du tour du joueur au tour de l'ordinateur et inversement
nouveauTour():- retract(tour(P1)), points(P2,_), P2\=P1, assert(tour(P2)),             %On met à jour le tour en changeant de joueur
                retract(actions(_,_)), assert(actions([deplacer, tue, controle],2)),   %On réinitialise les actions possibles ainsi que le nombre d'actions restantes
                !.

% ----------------------- ACTIONS DES JOUEURS ---------------------------
verifieAction(A):- actions(ListeActions,_), dans(A,ListeActions).

actualiseAction(A):- A==tue, retract(actions(ListeActions,Nb)), supprimer(A,ListeActions,NL), NewNb is Nb - 1, assert(actions(NL, NewNb)),!. % on actualise la liste des actions possibles si c'est "tue" et on décrémente le nombre d'action restantes.
actualiseAction(_):- retract(actions(ListeActions,Nb)), NewNb is Nb - 1, assert(actions(ListeActions, NewNb)). % on décrémente le nombre d'action restantes

verifieTour():- actions(_,Nb), Nb==0, nouveauTour(), write('\nVotre tour est terminé.'). % On limite le nombre d'action à 2
verifieTour():- morts(LM), tueur(J1, P1), dans(P1,LM), tueur(J2,P2), J1\=J2, dans(P2,LM), finPartie(),!.
verifieTour():- cibles(_,LC), longueur(N,LC), N=<0, finPartie(), !.

% ------ DEPLACER ----- 
% Prédicat déplacer => ajouter un personnage au début de la listePerso d'une tuile et le supprimer de la listePerso de la tuile d'origine
deplacer(Perso, Ligne, Colonne) :- verifieAction(deplacer), personnage(Perso), supprimer(Perso), ajouter(Perso, Ligne, Colonne), write('Personnage déplacé.'), actualiseAction(deplacer), verifieTour(), !.
% Ajoute le personnage à la tuile renseignée
ajouter(Perso, Ligne, Colonne) :-  retract(tuile(Ligne,Colonne, ListePerso)), asserta(tuile(Ligne,Colonne,[Perso|ListePerso])).
% Cherche parmi toutes les tuiles qu'il possède celle qui contient le personnage et le supprime de cette dernière.
supprimer(Perso) :- tuile(L,C,ListePerso), dans(Perso,ListePerso), supprimer(Perso,ListePerso, NewList), retract(tuile(L,C,_)), asserta(tuile(L,C,NewList)).% On récupère la liste des perso à partir de ligne et ccolonne pour en supprimer le Perso

% ----- PEUT TUER ------ 

peutTuer(P2,Xt,Yt):-    verifieAction(tue),                                % Si l'action tuer est la liste des actions possible on dit qu'on peut tuer.
                        tour(J), tueur(J,Tueur), peutTuer(Tueur,P2, Xt,Yt). % Permet de savoir qui on tuer avec notre tueur à gage

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
                        personnage(P2), P1\=P2, tuile(Xt,Yt,Li), dans(P2,Li),!.  % On cherche une tuile dans la même ligne où P2 se trouve. Si on trouve une tuile ==> P1 peut tuer P2


% ----- TUE ------
tue(P2):-       tour(J), tueur(J,Tueur), tue(Tueur,P2).        % permet de tuer avec notre tueur à gage

tue(P1,P2):-    verifieAction(tue),                                                    % Si l'action tuer est dans la liste des action, on tue
                peutTuer(P1,P2,X,Y),
                retract(vivants(LV)), supprimer(P2,LV,NLV), assert(vivants(NLV)),       % On actualise la liste des vivants
                retract(morts(LM)), assert(morts([P2|LM])),                             % On actualise la liste des morts
                retract(tuile(X,Y,LP)), supprimer(P2,LP,NLP), assert(tuile(X,Y,NLP)),   % On actualise la tuile
                donnePoints(P2),                                                        % On actualise les points
                actualiseAction(tue),
                verifieTour(),
                !.                                                        

% -- CONTROLER --
controle(Perso):-   verifieAction(controle),
                    tour(J), tueur(J2,Perso),  J2\=J,               % Si le tueur de l'adversaire et le perso contrôlé
                    tuile(X,Y,L), dans(Perso,L),                    % on récupère la tuile où il y a le perso contrôlé
                    retract(tuile(X,Y,_)), supprimer(Perso,L, NL),   % On actualise la tuile du perso contrôlé
                    assert(tuile(X,Y,NL)),
                    retract(morts(LM)), assert(morts([Perso|LM])),    % on considère le personnage comme mort
                    retract(vivants(LV)), supprimer(Perso,LV,NLV), 
                    assert(vivants(NLV)),                           % On retire le perso de la liste des vivants
                    points(J,NJ), points(J2,NJ2),                   % On récupère les points des 2 joueurs
                    NewNJ is NJ + 1,                                % Le joueur actuel gagne 1 points
                    NewNJ2 is NJ2 - 2,                              % Son adversaire en perd 2 car il n'a plus son tueur à gage.
                    retract(points(J,_)), assert(points(J,NewNJ)),
                    retract(points(J2,_)), assert(points(J2, NewNJ2)),
                    write('Bravo ! Vous avez arrêté le tueur à gage adverse. Vous gagnez un point et votre adversaire perd ses deux points d\'avance.\n'), 
                    actualiseAction(controle),
                    verifieTour(),
                    !.

controle(_):-       verifieAction(controle),
                    write('Dommage ! Vous avez contrôlé un innocent. Vous ne gagnez ni ne perdez de points.\n'),
                    actualiseAction(controle),
                    verifieTour(),!.


% ------------------ AFFICHAGE --------------------

% Permet l'affichage du plateau en appuyant sur le ";" dans la console ProLog
plateau(X,Y,LP):- tuile(X,Y,LP).

% affiche qui est le joueur en cours
tour() :- tour(J), write(J).

% Donne les cible pour le joueur en cours
cibles(LC):-tour(J), cibles(J,LC).
cibles():- cibles(LC), write('Cibles : '), write(LC).

% Donne les points pour le joueur en cours
points(N):- tour(J), points(J,N).
points():- points(N), write('points : '), write(N).

% Donne le tueur pour le joueur en cours
tueur(P):-  tour(J), tueur(J,P).
tueur():- tueur(P), write('tueur : '), write(P).

% attibu les points à chaque action du joueur
donnePoints(P2):-   tour(J), points(J,N), 
                    cibles(J,LC), dans(P2,LC), NewN is N + 1,               % Si il a éliminé une de ses cibles
                    retract(points(J,N)), assert(points(J,NewN)),           % On actualise les points
                    retract(cibles(J,LC)), supprimer(P2,LC,NLC),            % On actualise sa liste de cibles.
                    assert(cibles(J,NLC)),
                    write('Bravo ! Vous avez tué une de vos cibles. Vous gagnez un point.\n'), !.

donnePoints(P2):-   tour(J), points(J,NJ),
                    tueur(J2,P2), J2\=J, points(J2,NJ2),                    % Si le joueur tué est le tueur à gage de l'autre.
                    NewNJ is NJ + 3,                                        % Le joueur actuel gagne 3 points
                    NewNJ2 is NJ2 - 2,                                      % Son adversaire en perd 2 car il n'a plus son tueur à gage.
                    retract(points(J,_)), assert(points(J,NewNJ)),
                    retract(points(J2,_)), assert(points(J2, NewNJ2)),
                    write('Bravo ! Vous avez tué le tueur à gage adverse. Vous gagnez trois point et votre adversaire perd ses deux points d\'avance.\n'), !.

donnePoints(_):-    tour(J), points(J,N),                                  % Il n'a tué ni une de ses cibles ni le tueur à gage de l'autre, c'est donc un innocent
                    NewNJ is N -1,
                    retract(points(J,N)), assert(points(J,NewNJ)),
                    write('Dommage ! Vous avez tué un innocent. Vous perdez un point.\n').

finPartie():-   points(J1,P1), points(J2,P2), J2\=J1, P1<P2, vainqueur(J2),!.
finPartie():-   points(J1,P1), points(J2,P2), J2\=J1, P1>P2, vainqueur(J1),!.
finPartie():-   points(J1,P), points(J2,P), J2\=J1, vainqueur(),!.

% Fin du jeu et attribution du titre vainqueur en fonction des points
vainqueur(P) :- write(P), write(' à gagné').
vainqueur() :- write('\n\n\nEgalité.\n\n\n').

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

% Donne la liste de personnage Li des tuile(Xi,Yi) adjacentes à la tuile(X,Y)
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X-1, Yi is Y-1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X-1, Yi is Y  , tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X-1, Yi is Y+1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X  , Yi is Y-1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X  , Yi is Y+1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X+1, Yi is Y-1, tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X+1, Yi is Y  , tuile(Xi,Yi,Li).
tuileAdj(X,Y,Xi,Yi,Li):- Xi is X+1, Yi is Y+1, tuile(Xi,Yi,Li).