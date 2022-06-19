-----------------
-- type de base :
type ID_Article = String
type ID_Journal = String
type ID_Auteur = String
type Titre = String
type Page = Int
type Annee = Int
type Numero = Int
-----------------------------------------------------
-- type représentant un article avec ses accesseurs :
type Article = ( ID_Article, Titre, ID_Journal, [ID_Auteur], Page )
idArticle :: Article -> ID_Article
idArticle (a, _, _, _, _) = a
titreArticle :: Article -> Titre
titreArticle (_, t, _, _, _ ) = t
idJournalArticle :: Article -> ID_Journal
idJournalArticle (_, _, j, _, _) = j
listeAuteurs :: Article -> [ID_Auteur]
listeAuteurs (_, _, _, aa, _) = aa
page :: Article -> Page
page (_, _, _, _, p) = p
-----------------------------------------------------
-- type représentant un journal avec ses accesseurs :
type Journal = ( ID_Journal, Titre, Annee, Numero )
idJournal :: Journal -> ID_Journal
idJournal ( j, _, _, _ ) = j
titreJournal :: Journal -> Titre
titreJournal (_, t, _, _) = t
annee :: Journal -> Annee
annee (_, _, a, _) = a
numero :: Journal -> Numero
numero (_, _, _, n) = n



-----------------------------------------------------------------------------------
-- type représentant des références (citation) entre articles avec ses accesseurs :
-- l’article identifie par ID_Article (referant) fait reference aux articles dans
-- la liste [ID_Article] (referees).
type Reference = ( ID_Article, [ID_Article] )
referant :: Reference -> ID_Article
referant (r, _) = r
referees :: Reference -> [ID_Article]
referees (_, rs) = rs
------------------------------
-- Une base de connaissances :
journaux :: [Journal]
journaux = [
( "j00_3", "Computational Linguistics", 2000, 3 ),
( "e89_1", "Conference of the European Association for Computational Linguistics",
1989, 1 ),
( "c90_2", "International Conference on Computational Linguistics", 1990, 2 ),
( "e99_1", "Conference of the European Association for Computational Linguistics",
1999, 1 ),
( "p89_1", "Annual Meeting of the Association for Computational Linguistics", 1989,
1 )
]
articles :: [Article]
articles = [
( "j00_3002", "Incremental Processing and Acceptability", "j00_3", [ "Glyn Morrill" ],
319 ),
( "e89_1002", "Parsing and Derivational Equivalence", "e89_1", [ "Mark Hepple",
"Glyn Morrill" ], 10 ),
( "j00_3004", "A Compression Based Algorithm for Chinese Word Segmentation", "j00_3",
[ "W J Teahan", "Rodger MCNab", "Yingying Wen", "Ian H Witten" ], 375 ),
( "c90_2030", "Normal Form Theorem Proving for the Lambek Calculus", "c90_2",
[ "Mark Hepple" ], 173 ),
( "e99_1009", "Geometry of Lexico Syntactic Interaction", "e99_1", [ "Glyn Morrill" ],
61 ),
( "p89_1033", "Parsing as Natural Deduction", "p89_1", [ "Esther Konig" ], 272 )
]
references = [
( "j00_3002", [ "c90_2030", "e89_1002", "e99_1009", "p89_1033" ] ),
( "e99_1009", [ "c90_2030", "p89_1033" ] )
]


-- a) (12 pts) Écrivez une fonction qui vérifie si un auteur (représenté par un ID_Auteur) est
-- l’auteur d’un article. Si l’auteur n’existe pas dans la base de connaissance, alors la fonction retourne
-- False.
estAuteur :: ID_Auteur -> Article -> Bool
estAuteur id art =  id `elem` (listeAuteurs art)


-- b) (12 pts) Écrivez une fonction qui trouve la liste des titres des articles écrits par un auteur.
-- Si l’auteur n’existe pas dans la base de connaissance, alors la fonction retourne une liste vide.
aEcrit :: ID_Auteur -> [Titre]
aEcrit id = [ titreArticle a | a <- a articles, estAuteur id a]