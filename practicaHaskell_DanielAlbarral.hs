-- XMLTree
data Tree a = Node a [Tree a]

data XMLNode =
	Element String |
	Atribute String String |
	Text String   
	deriving (Show)

type XMLTree = Tree XMLNode

-- SHOW TREE
instance Show XMLTree where
    show (Node (Atribute att value) _) = " " ++ att ++ "=" ++ show value
    show (Node (Text txt) _) = txt
    show node = auxShow node 0

auxShow :: XMLTree -> Integer -> String
auxShow (Node (Element typ) sons) deep =
    tab ++ "<" ++ typ ++ concatMap show atributes ++ ">" ++ next
    where 
        tab = [' ' | x <- [1..deep*2]]
        (atributes, elements)= span isAtribute sons
        text = last sons
        next
            | isText text = show text ++ "</" ++ typ ++ ">" ++ "\n"
            | otherwise =   "\n" ++ concatMap (\x -> auxShow x (deep+1)) 
                            elements ++ tab ++ "</" ++ typ ++ ">\n" 

isAtribute :: XMLTree -> Bool
isAtribute (Node (Atribute _ _) _) = True
isAtribute a = False

isText :: XMLTree -> Bool
isText (Node (Text _) _) = True
isText a = False

-- READ TREE
readXMLTree :: String -> XMLTree
readXMLTree xml = fst (readXMLTree' xml)

readElementNode :: String -> XMLNode
readElementNode ele = Element ele

readAtributeNode :: String -> XMLNode
readAtributeNode atr = 
	Atribute (takeWhile (/='=') atr) (tail (dropWhile (/='=') atr))

readTextNode :: String -> XMLNode
readTextNode txt = Text txt

-- Donat un String del estil (tag=value tag=value ... >) retorna una llista 
-- amb tots els atributs que es poden llegir parametre d'entrada 
addAtributes :: String -> [XMLTree]
addAtributes "" = []
addAtributes str =
	(Node (atribute) []):(addAtributes next)
	where
		(current, next)= span (\x -> x/='>' && x/=' ') (dropWhile (==' ') str)
		atribute = Atribute tag (init value)
		(tag, (_:_:value)) = span (/='=') current 

readXMLTreeAux :: String -> ([XMLTree], String)
readXMLTreeAux ('<':'/':xml) = ([], next)
	where (_:next) = dropWhile (/='>') xml

readXMLTreeAux xml = (bro:xmlTree, next)
	where 
		(bro, others) = readXMLTree' xml
		(xmlTree, next) = readXMLTreeAux others

readXMLTree' :: String -> (XMLTree, String)
readXMLTree' (' ':xml) = readXMLTree' xml
readXMLTree' ('\n':xml) = readXMLTree' xml
readXMLTree' ('<':xml) = 
	(Node (Element tag) ((addAtributes att) ++ xmlTrees), rest')
	where
		(current,(_:next)) = span (/='>') xml
		(tag, att) = span (/=' ') current   
		(xmlTrees, rest) = readXMLTreeAux next
		rest' = dropWhile (/='<') rest

readXMLTree' xml = (Node (Text text) [], next)
	where (text, next) = span (/='<') xml

-- QNode
class QNode a where
    -- Ens indica si els dos elements son del mateix tipus
    isKind :: a -> a -> Bool
    -- Ens indica si el primer objecte es mes gran o igual que el segon.
    geq :: a -> a -> Bool
    -- Ens indica si el primer objecte es mes petit o igual que el segon.
    leq :: a -> a -> Bool

instance QNode XMLNode where
    isKind (Atribute fst snd) (Atribute fst' snd') =
           (fst == fst' && snd == snd')
        || (fst == fst' && snd' == "") 
        || (fst' == "" && snd' == "")
    isKind (Text x) (Text y) = x == y || y == ""
    isKind (Element x) (Element y) = x == y || y == ""
    isKind x y = False
    geq (Element x) (Element y) = x >= y
    geq (Text x) (Text y) = x >= y
    geq a1@(Atribute fst snd) a2@(Atribute fst' snd') =
        (fst == fst') && ((read $ snd::Integer) >= (read $ snd'::Integer))
    geq x y = False 
    leq (Element x) (Element y) = x <= y
    leq (Text x) (Text y) = x <= y
    leq (Atribute fst snd) (Atribute fst' snd') =
        (fst == fst') && ((read $ snd::Integer) <= (read $ snd'::Integer)) 
    leq x y = False

data Condition a =  IsKind a | Geq a | Leq a | CNot (Condition a) |
                    CAnd (Condition a) (Condition a) |
                    COr (Condition a)  (Condition a) | 
                    CTrue | CFalse 

-- Si el primer parametre satisfa la condicio del segon parametre
evaluate :: QNode a => a -> Condition a -> Bool
evaluate x (IsKind y) = isKind x y
evaluate x (Geq y) = geq x y
evaluate x (Leq y) = leq x y
evaluate x (CAnd y z) = (evaluate x y) && (evaluate x z)
evaluate x (COr y z) = (evaluate x y) || (evaluate x z) 
evaluate x CFalse = False 
evaluate x CTrue = True
evaluate x (CNot y) = not $ evaluate x y

data Range = 
    -- Indica totes les posicions mes grans o iguals que l'enter.
    PGe Int |
    -- Indica totes les posicions mes petites o iguals que l'enter.
    PLe Int |
    -- Indica la posicio de l'enter.
    PEq Int

data QTree a =
    -- Permet seleccionar l'arbre en curs
    ThisTree |
    -- Permet seleccionar l'arbre en curs i tots els seus subarbres.
    AnyTree |
    -- Representa un seleccio del resultat de la query del segon parametre
    Selection Range (QTree a) | 
    -- Si la llista de consultes als fill es buida, vol dir que el subarbre en
    -- curs satisfa la consulta. Si n'hi ha menys consultes que fills, es 
    -- considera per defecte que les que no hi son son ThisTree i si en 
    -- sobren, no s'apliquen.
    RNode (Condition a) [QTree a] |
    -- Representa que la condicio es pot aplicar zero o mes
    -- vegades. Per tant, la consulta (segon parametre) sempre s'aplica a
    -- l'arbre en curs i si la condicio es compleix per l'arrel, llavors 
    -- (tambe) es torna a aplicar la mateixa StarTree consulta als fills.
    StarTree (Condition a) (QTree a) |
    -- Es obligatori que la condicio es compleixi perl'arrel
    PlusTree (Condition a) (QTree a) |
    -- Representa la unio d'una llista de consultes.
    Union [QTree a]

-- Retorna la llista de tots els subarbres que satisfan la consulta
xQueryTree :: QNode a => QTree a -> Tree a -> [Tree a] 
-- ThisTree 
xQueryTree ThisTree xmlTree = [xmlTree] 

-- AnyTree 
xQueryTree AnyTree xmlTree@(Node _ []) = [xmlTree]
xQueryTree AnyTree xmlTree@(Node _ sons) = 
    xmlTree : (concatMap (\x -> xQueryTree AnyTree x) sons) 

-- Selection 
xQueryTree (Selection (PLe x) qTree) xmlTree = 
    take (x+1) (xQueryTree qTree xmlTree) 
xQueryTree (Selection (PGe x) qTree) xmlTree =
    drop (x) (xQueryTree qTree xmlTree)
xQueryTree (Selection (PEq x) qTree) xmlTree = 
    [(xQueryTree qTree xmlTree) !! x]

-- Rnode
xQueryTree qTree@(RNode cond []) xmlTree@(Node tag _)
    | evaluate tag cond = [xmlTree]
    | otherwise = []

xQueryTree qTree@(RNode cond qSons) (Node tag sons)
    | evaluate tag cond = xTreeNode qSons sons
    | otherwise = []
    where
        xTreeNode _ [] =  []
        xTreeNode [] (t:tSons) =
            (xQueryTree ThisTree t) ++ (xTreeNode [] tSons)
        xTreeNode (q:qqSons) (t:tSons) = 
             (xQueryTree q t) ++ (xTreeNode qqSons tSons)

-- StartTree
xQueryTree (StarTree cond qTree) xmlTree@(Node tag sons)
    | evaluate tag cond = 
        xQueryTree qTree xmlTree ++ 
        (concatMap applyXQuery sons)
    | otherwise = xQueryTree qTree xmlTree 
    where
        applyXQuery =  (\x -> xQueryTree (StarTree cond qTree) x)

-- PlusTree
xQueryTree (PlusTree cond qTree) xmlTree@(Node tag sons)
   | evaluate tag cond = 
        xQueryTree qTree xmlTree ++ 
        (concatMap applyXQuery sons)
    | otherwise = [] 
    where
        applyXQuery =  (\x -> xQueryTree (StarTree cond qTree) x)

-- Union
xQueryTree (Union qTree) xmlTree = 
    concatMap (\x -> xQueryTree x xmlTree) qTree 

xQueryNode :: QNode a => QTree a -> Tree a -> [a] 
-- ThisTree 
xQueryNode ThisTree (Node tag _) = [tag]

-- AnyTree 
xQueryNode AnyTree (Node _ []) = []
xQueryNode AnyTree (Node tag sons) = 
    tag : (concatMap (\x -> xQueryNode AnyTree x) sons) 

-- Selection 
xQueryNode (Selection (PLe x) qTree) xmlTree = 
    take x (xQueryNode qTree xmlTree) 
xQueryNode (Selection (PGe x) qTree) xmlTree =
    drop (x-1) (xQueryNode qTree xmlTree)
xQueryNode (Selection (PEq x) qTree) xmlTree = 
    [(xQueryNode qTree xmlTree) !! x]

-- Rnode
xQueryNode qTree@(RNode cond []) xmlTree@(Node tag _)
    | evaluate tag cond = [tag]
    | otherwise = []

xQueryNode (RNode cond qSons) (Node tag sons)
    | evaluate tag cond = xTreeNode qSons sons
    | otherwise = []
    where
        xTreeNode _ [] =  []
        xTreeNode [] (t:tSons) =
            (xQueryNode ThisTree t) ++ (xTreeNode [] tSons)
        xTreeNode (q:qqSons) (t:tSons) = 
            (xQueryNode q t) ++ (xTreeNode qqSons tSons)

-- StartTree
xQueryNode (StarTree cond qTree) xmlTree@(Node tag sons)
    | evaluate tag cond = 
        xQueryNode qTree xmlTree ++ 
        (concatMap applyXQuery sons)
    | otherwise = xQueryNode qTree xmlTree 
    where
        applyXQuery =  (\x -> xQueryNode (StarTree cond qTree) x)

-- PlusTree
xQueryNode (PlusTree cond qTree) xmlTree@(Node tag sons)
   | evaluate tag cond = 
        xQueryNode qTree xmlTree ++ 
        (concatMap applyXQuery sons)
    | otherwise = [] 
    where
        applyXQuery =  (\x -> xQueryNode (StarTree cond qTree) x)

-- Union
xQueryNode (Union qTree) xmlTree = 
    concatMap (\x -> xQueryNode x xmlTree) qTree 

isNotEmpty:: [a] -> Bool
isNotEmpty [] = False
isNotEmpty l = True

-- xRelative
xRelative :: QNode a => QTree a -> QTree a -> Tree a -> [Tree a]
xRelative query1 query2 xmlTree = 
    filter (\x -> isNotEmpty (concatMap res2 (anyTree x))) res
    where
        res = xQueryTree query1 xmlTree
        res2 x = xQueryNode query2 x
        anyTree x = xQueryTree AnyTree x

