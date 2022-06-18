-- 3.1
-- Les questions de la section 3.1 utilisent la déclaration du type de données d’arbre binaire de recherche
-- suivant.

data Arbre a = Vide | Noeud
    { valeur :: a,
      gauche, droite :: Arbre a
    }

-- 3.1.1
-- Écrivez une fonction qui calcul le nombre de Nœud que contient un arbre. (Vide n’est pas un Nœud.)
nbrNoeud :: Arbre a -> Int
nbrNoeud Vide = 0
nbrNoeud (Noeud _ g d) = 1 + nbrNoeud g + nbrNoeud d

-- 3.1.2
-- Écrivez une fonction vérifie si un élément est dans un arbre
contient :: (Ord a) => a -> Arbre a -> Bool
contient _ Vide = False
contient x (Noeud a g d)  | x == a = True
                          | x < a = contient x g
                          | otherwise = contient x d


-- 3.1.3
-- Écrivez une fonction qui extrait toutes les valeurs présentes dans un Arbre en construisant une liste de
-- ces valeurs. Cette fonction doit utiliser un parcours infixe de l’Arbre.
arbreVersListe :: Arbre a -> [a]
arbreVersListe Vide = []
arbreVersListe (Noeud a g d) = arbreVersListe g ++ (a : arbreVersListe d)


inserer :: (Ord a) => a -> Arbre a -> Arbre a
inserer v Vide = Noeud v Vide Vide
inserer v r@(Noeud x g d)
        | v == x = r
        | v < x = Noeud x ( inserer v g ) d
        | otherwise = Noeud x g ( inserer v d )


insererTous :: (Ord a) => Arbre a -> [a] -> Arbre a
insererTous = foldr inserer

listeVersArbre :: (Ord a) => [a] -> Arbre a
listeVersArbre = insererTous Vide

-- 3.1.4
-- En utilisant les fonctions vues en classe pour les arbres binaires (inserer, insererTous,
-- listeVersArbre) et les fonctions des exercices précédents, il est possible d’écrire une fonction de
-- tri. Écrivez cette fonction.
trier :: (Ord a) => [a] -> [a]
trier [] = []
trier x = arbreVersListe(listeVersArbre x)

-- 3.2.1
-- Construisez un type de données algébrique pour représenter des formes en 2 dimensions :
-- 1. Un cercle a un rayon.
-- 2. Un carré a la longueur d’un côté.
-- 3. Un rectangle a la longueur de la base et de la hauteur.
data Dimens = Cercle Double
    | Rectangle Double Double
    | Carre Double


-- 3.2.2
-- Écrivez une fonction qui calcule l’aire d’une forme en 2 dimensions
aire2Dim :: Dimens -> Double
aire2Dim (Rectangle a b)=a*b
aire2Dim (Carre a)=a*a
aire2Dim (Cercle a)=a*pi

-- 3.2.3
-- Instanciez la classe Eq avec le type de forme 2 dimensions.
instance Eq Dimens where
    (Cercle r1) == (Cercle r2) = r1 == r2
    (Rectangle a b) == (Rectangle x y) = a==x && b==y
    (Carre c1) == (Carre c2) = c1 == c2
    _ == _ = False

    