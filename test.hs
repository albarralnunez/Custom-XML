--TEST  
testTree = readXMLTree testXML

testQuery1 = xQueryTree
    --Query
    (StarTree CTrue (RNode (IsKind (readElementNode "llibre")) [])) 
    --Tree
    testTree

testQuery2 = xQueryTree
    --Query
   (StarTree CTrue (RNode (CAnd (Geq (readAtributeNode "any=1999"))
       (Leq (readAtributeNode "any=2001"))) []))
   --Tree
   testTree

testQuery3 = xRelative
    --Query1
    (StarTree CTrue (RNode (IsKind (readElementNode "llibre")) []))
    --Query2
    (StarTree CTrue (RNode (CAnd (Geq (readAtributeNode "any=1999"))
        (Leq (readAtributeNode "any=2001"))) []))
    --Tree
    testTree

testQuery4 = xRelative
    --Query1
    (StarTree CTrue (RNode (IsKind (readElementNode "llibre")) []))
    --Query2
    ((RNode (CAnd (Geq (readAtributeNode "any=1999"))
        (Leq (readAtributeNode "any=2001"))) []))
    --Tree
    testTree

testQuery5 = xRelative
    --Query1
    (StarTree CTrue (RNode (IsKind (readElementNode "llibre")) []))
    --Query2
    (RNode (IsKind (readElementNode "editor")) [])
    --Tree
    testTree

tsAtt1 = "any=1"
tsAtt2 = "any=2"

testXML = 
       "<llibres>"
    ++ "   <llibre any=\"2004\" edicio=\"1\">"
    ++ "        <titol>Razonando con Haskell</titol>"
    ++ "        <autor>Blas C. Ruiz</autor>"
    ++ "        <autor>Francisco Gutierrez</autor>"
    ++ "        <autor>Pablo Guerrero</autor>"
    ++ "        <autor>Jose E. Gallardo</autor>"
    ++ "   </llibre>"
    ++ "   <llibre edicio=\"2\" any=\"1999\">"
    ++ "       <titol>HASKELL: The Craft of Functional Programming</titol>"
    ++ "       <editor>A. D. McGettrick</editor>"
    ++ "       <autor>Simon Thompson</autor>"
    ++ "   </llibre>"
    ++ "   <llibre edicio=\"1\" any=\"2000\">"
    ++ "       <titol>Programming language pragmatics</titol>"
    ++ "       <autor>Michael L. Scott</autor>"
    ++ "   </llibre>"
    ++ "</llibres>"

storageXmlString = "<storage>\n\
\  <disk>\n\
\    <size>500Gb</size>\n\
\    <name>main</name>\n\
\    <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\      <name>home</name>\n\
\      <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>albert</name>\n\
\       <file user=\"xwr\" group=\"r\" other=\"r\">\n\
\         <name>enunciat.pdf</name>\n\
\         <size>113605</size>\n\
\         <owner>albert</owner>\n\
\       </file>\n\
\       <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\         <name>Haskell</name>\n\
\         <file user=\"xwr\" group=\"r\" other=\"\">\n\
\           <name>practica.hs</name>\n\
\           <size>3330</size>\n\
\           <owner>albert</owner>\n\
\         </file>\n\
\          <file user=\"xwr\" group=\"r\" other=\"\">\n\
\           <name>jp.txt</name>\n\
\           <size>1580</size>\n\
\           <owner>albert</owner>\n\
\         </file>\n\
\       </folder>\n\
\       <file user=\"xwr\" group=\"r\" other=\"r\">\n\
\         <name>enunciat.tex</name>\n\
\         <size>12103</size>\n\
\         <owner>albert</owner>\n\
\       </file>\n\
\      </folder>\n\
\      <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>daniel</name>\n\
\      </folder>\n\
\      <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>jordi</name>\n\
\      </folder>\n\
\    </folder>\n\
\    <folder user=\"xwr\" group=\"r\" other=\"\">\n\
\      <name>bin</name>\n\
\      <file user=\"xwr\" group=\"r\" other=\"\">\n\
\        <name>ghc</name>\n\
\        <size>113605</size>\n\
\        <owner>root</owner>\n\
\      </file>\n\
\    </folder>\n\
\  </disk>\n\
\</storage>"

storageXmlTree = readXMLTree storageXmlString

rnFalse = RNode CFalse []

eName = [rnFalse,rnFalse,rnFalse,ThisTree,rnFalse,rnFalse]

isFile = RNode (IsKind $ readElementNode "file") eName

plusIsFolder = PlusTree (IsKind $ readElementNode "folder") isFile

starNoFolder x = StarTree (CNot $ IsKind $ readElementNode "folder") x

plusNoFolder x = PlusTree (CNot $ IsKind $ readElementNode "folder") x

selectFrom2 = Selection (PGe 1) $ starNoFolder plusIsFolder

starThisTree = starNoFolder ThisTree

isStorage = RNode (IsKind $ readElementNode "storage") eName

--xQueryTree selectFrom2 storageXmlTree

{- retorna
[<name>practica.hs</name>
,<name>jp.txt</name>
,<name>enunciat.tex</name>
,<name>ghc</name>
]
-}

--xQueryNode starThisTree storageXmlTree

{- retorna
[Element "storage",Element "disk",Element "size",Text "500Gb",Element "name",Text "main",Element "folder",Element "folder"]
-}

