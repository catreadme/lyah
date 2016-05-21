{- TODO add Maybe to failable functions -}
import Data.List

type Name = String
type Data = String

data FSItem = File Name Data | Folder Name [FSItem]
  deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem]
  deriving (Show)

type Zipper = (FSItem, [FSCrumb])

fs :: FSItem
fs =
    Folder "root"
        [ File "song.mp3" "baaaaaa"
        , File "test.hs" "putStrLn $ show 5"
        , Folder "photos"
            [ File "mrw.jpg" "bleargh"
            , File "tfw.gif" "smash!!"
            , File "me_irl.bmp" "Yikes!"
            ]
        , File "essay.doc" "whatever"
        , Folder "viruses"
            [ File "stux.exe" "stucs"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

up :: Zipper -> Zipper
up (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

to :: Name -> Zipper -> Zipper
to name (Folder folderName items, bs) =
  let (ls, item:rs) = break (nameIs name) items
  in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- to only "searches" in current folder
myFile = to "song.mp3" (fs,[]) -- (File "song.mp3" "baaaaaa",[FSCrumb "root" []...
myFolder = to "photos" (fs,[]) -- (Folder "photos" [File "mrw.jpg" "bleargh",File ...

myHsSource = to "random.hs" $ to "source code" $ to "viruses" (fs,[])
mySourceCode = up myHsSource

rename :: Name -> Zipper -> Zipper
rename newName (Folder name items, bs) = (Folder newName items, bs)
rename newName (File name dat, bs) = (File newName dat, bs)

root :: Zipper -> Zipper
root fs@(Folder "root" items,bs) = fs
root fs = root $ up fs

myRename = root $ rename "notsorandom.hs" myHsSource

newFile :: FSItem -> Zipper -> Zipper
newFile item (Folder folderName items, bs) =
  (Folder folderName (item:items), bs)

myNew = root $ newFile (File "test.hs" "testcode goes here") mySourceCode
