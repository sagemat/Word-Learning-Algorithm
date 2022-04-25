import System.Environment
import Data.Char
import System.Directory
import System.IO
import Data.List
import Control.Monad
import Data.List.Split

info string = string

 -- I   0   1   0   0   9   2   0   2   1   1   7   4   2   4   3
--(key:upt:dd1:dd2:md1:md2:yd1:yd2:yd3:yd4:hd1:hd2:nd1:nd2:sd1:sd2)
-- upt is update and its a Bool

-- date (_:_:dd1:dd2:md1:md2:yd1:yd2:yd3:yd4:_:_:_:_:_:_:_:_) = (dd1:dd2:md1:md2:yd1:yd2:yd3:yd4)

separateDate (dd1:dd2:md1:md2:yd1:yd2:yd3:yd4) = (dd1:dd2:'/':md1:md2:'/':yd1:yd2:yd3:yd4)

date info =
    let
        myDate = separateDate $ tail $ tail $ init $ init $ init $ init $ init $ init $ info
    in 
        if verifyFormat info then myDate else "Wrong Format"


separateTime (hd1:hd2:nd1:nd2:sd1:sd2) = (hd1:hd2:':':nd1:nd2:':':sd1:sd2)

time info = 
    let
        myTime = separateTime $ tail $ tail $ tail $ tail $ tail $ tail $ tail $ tail $ tail $ tail $ info
    in
        if verifyFormat info then myTime else "Wrong Format"

-- as putea merge un pas mai departe si sa vad pt luna a 13-a sau pt secunda 70 dar e o chestie logica. Nu da eroare
verifyFormat (x:xs) = x == 'I' && length xs == 15 && "" == filter (\y -> y < '0' || y > '9') xs

readFileM file = do
    --doesFileHavePath <- fileHasPath $ theCharsBefore '.' file
    fileExists <- doesFileExist file
    if fileExists then readFile file
                  else return (file ++ "-does-not-exist")

-- the Phrase Processor will grow bigger once synonyms are implemented
phraseCleaner string   = removeCommas $ words $ removeLastQuetionmark $ toLowerString string
--phraseProcessor :: String -> [String]
phraseProcessor inputLine = do
    let unprocessedListOfWords = phraseCleaner inputLine
        whatWord = head unprocessedListOfWords
    whatWordPath <- fileNamedPath whatWord
    let whatWordFilePath = whatWordPath ++ "\\" ++ whatWord ++ ".txt"
    whatWordContents <- readFileM whatWordFilePath
    if whatWord /= "what" then return $ (slashWords (theWordAfter "replace-with:" (words $ theWordsBetween  ("{", "}") (words whatWordContents) True)) '-') ++ (tail $ tail unprocessedListOfWords)
                          else return unprocessedListOfWords

-- it's doing a sloppy job but it's doing it
unknownWords inputLine = do 
    filePath <- getCurrentDirectory
    let listOfWords = phraseCleaner inputLine 
        listOfUniqueWords = removeEmptyStrings (removeDuplicates listOfWords)
    wordsPaths <- mapM (\x -> searchFolders filePath (x ++ ".txt")) listOfUniqueWords
    let currentUnknownWords = removeEmptyStrings (zipWith (\x y -> if y == [] then x else []) (listOfUniqueWords) (wordsPaths))
    return currentUnknownWords

slashWords' path words separator = let elemNr = justIntToInt (elemIndex separator path)
                                       condition = elemNr /= 0
                                       restOfPath = if words /= [""] then drop (elemNr + 1) path else path ++ [separator]
                                       word = take (justIntToInt (elemIndex separator restOfPath)) restOfPath
                                       newWords = words ++ [word]
                                   in  if condition then slashWords' restOfPath newWords separator else drop 1 (init (words))
                        
slashPath path = slashWords' path [[]] '\\'
slashWords string separator = slashWords' string [[]] separator

grabElemFromPath path 0 = take (justIntToInt (elemIndex '\\' path)) path
grabElemFromPath path n = grabElemFromPath (drop (1 + justIntToInt (elemIndex '\\' path)) path) (n - 1)

fileHasPath file = do 
    filePaths <- readFileM "directories.txt"
    let filePath = elem file (map (\x -> last (slashPath x)) (words filePaths))
    return filePath
    
fileNamedPath file = do 
    filePaths <- readFileM "directories.txt"
    let filePath = head (filter (\x -> last (slashPath x) == file) (words filePaths))
    doesFileHavePath <- fileHasPath file 
    if doesFileHavePath then return filePath else return "path-not-found"

addFile file file' log (fileContents, logContents) inputLine = do
    filePath <- getCurrentDirectory
    let listOfWords = phraseCleaner inputLine 
        fileIndexed = if elem "is" listOfWords then theWordBefore "is" listOfWords ++ " " else ""
        fileIndexed' = if elem "is" listOfWords then theWordBefore "is" listOfWords else ""
        newFileContents  = fileContents ++ fileIndexed
        newLogContents   = logContents ++ inputLine ++ "\n"
        partOfSpeech     = head listOfWords
        memoryFileFolderPartOfSpeech = "part-of-speech\\" ++ partOfSpeech ++ "\\"
        memoryFileFatherFolder = theWordAfters' ("is", "a", Just "an") listOfWords
  
        memoryFilePath   = filePath ++ "\\memory\\" ++ memoryFileFolderPartOfSpeech ++ memoryFileFatherFolder ++ "\\" ++ fileIndexed'
    
    --allMemoryContents <- directories filePath
    let memoryContents = newFileContents--if (listOfWords == ["refresh", "memory"]) then (unwords allMemoryContents) else newFileContents
    doesFileHavePath <- fileHasPath memoryFileFatherFolder
    fileNamedPath' <- fileNamedPath memoryFileFatherFolder
    if doesFileHavePath then putStrLn (fileNamedPath') else putStrLn "" -- aici e problema. Nu mai chema asta cand nu exista! Eroare
    let checkedMemoryFilePath = if doesFileHavePath then fileNamedPath' ++ "\\" ++ fileIndexed' else memoryFilePath
        memoryFile = checkedMemoryFilePath ++ "\\" ++ fileIndexed' ++ ".txt"
    putStrLn memoryFile
    createDirectoryIfMissing True checkedMemoryFilePath
    writeFile file' memoryContents
    writeFile memoryFile "" -- aici se adauga informatia
    renameFile file' file
    writeFile log newLogContents
    return (newFileContents, newLogContents)

loopUnknownWords :: [String] -> (String, String, String, (String, String)) -> IO (String, String)
loopUnknownWords [] (file, file', log, (fileContents, logContents)) = return (fileContents, logContents)
loopUnknownWords (unknownWord:otherUnknownWords) (file, file', log, (fileContents, logContents)) = do
    putStrLn ("What is " ++ unknownWord ++ "?")
    inputLine <- getLine
    let listOfWords = phraseCleaner inputLine 
        fileIndexed = if elem "is" listOfWords then theWordBefore "is" listOfWords else ""
    -- putStrLn (fileIndexed ++ " " ++ unknownWord)
    if (fileIndexed == unknownWord) then (do
        currentFilesContents <- addFile file file' log (fileContents, logContents) inputLine
        currentUnknownWords <- unknownWords inputLine
       -- return currentFilesContents
        loopUnknownWords (currentUnknownWords ++ otherUnknownWords) (file, file', log, currentFilesContents))
                                    else
        loopUnknownWords (unknownWord:otherUnknownWords) (file, file', log, (fileContents, logContents))
        

appLoop file file' log (fileContents, logContents) = do
    inputLine <- getLine
    filteredData <- filterInformation inputLine
    putStrLn (unwordsLn filteredData)
    --putStrLn (show (phrases '.' inputLine))
    filePath <- getCurrentDirectory
    --putStrLn (unwords wordsPaths) -- filePathurile fisierelor stite
    currentUnknownWords <- unknownWords inputLine
    currentFilesContents <- loopUnknownWords currentUnknownWords (file, file', log, (fileContents, logContents))
   -- addFile file file' log currentFilesContents inputLine
    appLoop file file' log currentFilesContents

 -- putStrLn ((theWordBefore "is" (words x)) ++ " = " ++ (theWordAfters ("is", "a") (words x))) -- Easter egg


extractNumber' :: [Char] -> Int -> Int
extractNumber' []     nr = nr
extractNumber' (x:xs) nr = if (isDigit x) then extractNumber' xs (nr*10 + (digitToInt x)) else extractNumber' xs nr

extractNumber string = extractNumber' string 0

ifStatement string = string == "when" || string == "if"

-- ce e in stringurile din copute e doar comentariu care trb implementat
compute string = if (ifStatement "elementul when sau if din acel string") then Just (if string == "caut ceea ce este in string si verific daca asa e" then "de exemplu retine sa faca asa alta data" else "ori nimic si ramane doar prima conditie, ori retine sa faca altceva") else Nothing  

-- "A dog is a mammal"
toLowerString string = map (toLower) string 

theCharsBefore' char (x:xs) newString = if elem char (x:xs) then (if x == char then reverse newString
                                                                               else theCharsBefore' char xs (x : newString))
                                                            else "char-not-found"
theCharsBefore  char string = theCharsBefore' char string []                                                     

theWordAfters''' (word1, word1', word2, word2', word2'') []         = "word-not-found"
theWordAfters''' (word1, word1', word2, word2', word2'') [_]        = "word-not-found"
theWordAfters''' (word1, word1', word2, word2', word2'') [x,y]      = if x == word1 || Just x == word1' then y else "word-not-found"
--               "is"    "are"   "the"   "a"    "an"
theWordAfters''' (word1, word1', word2, word2', word2'') (x:y:z:xs) = if x == word1 || Just x == word1' then (if (y == word2 || Just y == word2' || Just y == word2'') then z
                                                                                                                                                                       else y) 
                                                                      else theWordAfters''' (word1, word1', word2, word2', word2'') (y:z:xs)
--theWordAfters' can also have word1 and word2 be null 
theWordAfters' (word1, word2, word2') []         = "word-not-found"
theWordAfters' (word1, word2, word2') [_]        = "word-not-found"
theWordAfters' (word1, word2, word2') [x,y]      = if x == word1 then y else "word-not-found"
--               "is"   "a"     "an"
theWordAfters' (word1, word2, word2') (x:y:z:xs) = if x == word1 then (if (y == word2 || Just y == word2') then z
                                                                                                           else y) 
                                                                 else theWordAfters' (word1, word2, word2') (y:z:xs)

theWordAfters (word1, word2) (x:y:z:xs) = theWordAfters' (word1, word2, Nothing) (x:y:z:xs)

theWordsAfters' (word1, word2, word2') []         = "word-not-found"
theWordsAfters' (word1, word2, word2') [_]        = "word-not-found"
theWordsAfters' (word1, word2, word2') [_,_]      = "word-not-found"
--               "is"   "a"     "an"
theWordsAfters' (word1, word2, word2') (x:y:z:xs) = if (x == word1 && (y == word2 || Just y == word2') ) then xs else theWordsAfters' (word1, word2, word2') (y:z:xs)

theWordsAfters (word1, word2) (x:y:z:xs) = theWordsAfters' (word1, word2, Nothing) (x:y:z:xs)

theWordsAfter word [] = "word-not-found"
theWordsAfter word (x:xs) = if (x == word) then xs else theWordsAfter word xs

theWordBefore word []       = "word-not-found"
theWordBefore word [_]      = "word-not-found"
theWordBefore word (x:y:xs) = if (y == word) then x else theWordBefore word (y:xs)

theWordAfter word []       = "word-not-found"
theWordAfter word [_]      = "word-not-found"
theWordAfter word (x:y:xs) = if (x == word) then y else theWordAfter word (y:xs)


theWordsBetween' (word1, word2) []     True  string = string
theWordsBetween' (word1, word2) []     False string = string
theWordsBetween' (word1, word2) (x:xs) True  string = if x == word1 || x == word2 then theWordsBetween' (word1, word2) xs False string else theWordsBetween' (word1, word2) xs True   string
theWordsBetween' (word1, word2) (x:xs) False string = if x == word1 || x == word2 then theWordsBetween' (word1, word2) xs True  string else theWordsBetween' (word1, word2) xs False (string ++ x ++ " ")

theWordsBetween  (word1, word2) string bool = theWordsBetween' (word1, word2) string bool []

listOfWordAfters' [] wordsInputString wordList = wordList
listOfWordAfters' "word-not-found" wordsInputString wordList = wordList
listOfWordAfters' word wordsInputString wordList = 
    let
         wordsInputStringHasWord = elem word wordsInputString
         foundWord = theWordAfters' (word, "a", Just "an") wordsInputString
         updatedWordList = foundWord : wordList
         elemNumber = justIntToInt (elemIndex word wordsInputString)
         updatedInputString = drop (1 + elemNumber) wordsInputString -- this is the tail
    in
         if wordsInputStringHasWord then listOfWordAfters' word updatedInputString updatedWordList else wordList

listOfWordAfters word wordsInputString = listOfWordAfters' word wordsInputString []



showListOfStrings [] = putStr ""
showListOfStrings (x:xs) = do 
    putStrLn x
    showListOfStrings xs

lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl (const . drop 1) xs (drop n xs)

justIntToInt :: Maybe Int -> Int
justIntToInt (Just n) = n
justIntToInt Nothing  = 0

justStringToString :: Maybe String -> String
justStringToString (Just string) = string
justStringToString Nothing  = ""

removeLastQuetionmark string = if last string == '?' then init string
                                                     else string   

removeDuplicates [] = []
removeDuplicates (x:xs) = if elem x xs then removeDuplicates xs else x : removeDuplicates xs 

removeEmptyStrings string = filter (\x -> x /= []) string

removeNotFoundStrings string = filter (\x -> x /= "word-not-found") string

itsFile (name, extension) file =
    let 
        digitIndex = filter (\y -> y /= Nothing) (map (\x -> elemIndex x file) "0123456789")
        dotIndex  = elemIndex '.' file
        stopIndex = justIntToInt (if digitIndex == [] then dotIndex else minimum digitIndex)
        prefix = take stopIndex file
        sufix  = lastN' (length extension) file 
    in 
        prefix == name && sufix == extension -- sufix = ".txt", prefix = "log"
        
directoyFiles (name, extension) directoryContents = filter (\x -> itsFile (name, extension) x) directoryContents


updateDirectory folder filePath file = do 
    let updatedFilePath = filePath ++ "\\" ++ folder
    searchFolders updatedFilePath file
    

directoryFolders :: [String] -> [String]
directoryFolders directoryContents = filter (\x -> (elem '.' x) == False) directoryContents 

--searchHierarchy file listOfHierarchyWords

sentences' separator []     (y:ys) = (y:ys)
sentences' separator (x:xs) (y:ys) = if x == separator then sentences' separator xs ([] : (y:ys))
                                                       else let updatedHead = y ++ [x]
                                                                updatedListOfStrings = (updatedHead:ys)    
                                                            in  sentences' separator xs updatedListOfStrings

sentences separator string = sentences' separator string [""]

phrases' separator string listOfStrings = 
    let
        hasSeparator = elem separator string
        separatorPosition = justIntToInt (elemIndex separator string)
        nextPhrasePosition = separatorPosition + 2
        currentPhrase = take separatorPosition string
        updatedString = drop nextPhrasePosition string
        updatedListOfStrings = currentPhrase : listOfStrings
        updatePhrases = phrases' separator updatedString updatedListOfStrings        
    in
        if hasSeparator then updatePhrases else listOfStrings
        
phrases separator string = phrases' separator string [[]]

searchFolders filePath file = do 
    directoryContents <- getDirectoryContents filePath
    fileExists <- doesFileExist (filePath ++ "\\" ++ file)
    if fileExists then return (filePath ++ "\\" ++ file)
                  else (do mappedFolders <- (mapM (\x -> updateDirectory x filePath file) (directoryFolders directoryContents))
                           let fileOutputPath = head ((filter (\x -> x /= []) mappedFolders) ++ [""]) 
                           return fileOutputPath)

removeCharAfterChar'  _   []       newString = newString 
removeCharAfterChar'  _   [x]      newString = newString ++ [x]
removeCharAfterChar' char (x:y:xs) newString = if x == char then removeCharAfterChar' char    xs  (newString ++ [x])
                                                            else removeCharAfterChar' char (y:xs) (newString ++ [x])

removeLastChar char string = if elem char string then init string else string

removeCharAfterChar  char (x:xs) = removeCharAfterChar' char (x:xs) []

removeSpaceAfterDot listOfStrings = map (\string -> removeCharAfterChar '.' string) listOfStrings

removeCommas listOfStrings = map (\string -> removeLastChar ',' string) listOfStrings

searchFilesPaths listOfWords = do
    let file = theWordAfters''' ("is", Just "are", "the", Just "a", Just "an") listOfWords
        listOfHierarchyWords = (listOfWordAfters "of" listOfWords) ++ [file] -- hierarchy as in higher or lower in the context
    -- putStrLn (show listOfHierarchyWords)   
    hierarchyWordsPaths <- mapM (\file -> do doesFileHavePath <- fileHasPath file
                                             fileNamedPath' <- fileNamedPath file
                                             return (if doesFileHavePath then fileNamedPath'
                                                                         else "path-not-found")) listOfHierarchyWords -- la else era "information-not-found"
    return hierarchyWordsPaths                                -- aici la else trebuie cautat pe lista Hirearchy-ului pana ce se gaseste cv, daca nu s a gasit nimic, information-not-found

filterPhrases keyWord listOfStrings = filter (\string -> let listOfWords = removeCommas $ words string
                                                         in  elem keyWord listOfWords) listOfStrings 

filterPhrases' (keyWord, keyWord') listOfStrings = filter (\string -> let listOfWords = removeCommas $ words string
                                                                      in  (elem keyWord listOfWords) && (elem keyWord' listOfWords)) listOfStrings

-- TREBUIE TESTAT GRAV
filterInformation inputLine = do
    listOfWords <- phraseProcessor inputLine
   -- putStrLn (show listOfWords)
    filesPaths <- searchFilesPaths listOfWords
    --putStrLn (show ofDirectoriesInformation)
    let file = theWordAfters''' ("is", Just "are", "the", Just "a", Just "an") listOfWords
        listOfHierarchyWords = removeNotFoundStrings $ (listOfWordAfters "of" listOfWords) ++ [file]
        nrOfOfs = length listOfHierarchyWords - 1
        fatFile = head $ drop 1 listOfHierarchyWords
        grandFile = head filesPaths ++ "\\" ++ (head listOfHierarchyWords) ++ ".txt"
   -- putStrLn inputLine
    unprocessedFileContents <- if nrOfOfs >= 0 then readFileM grandFile else return "not-found"
    let uncutFileContents = removeCharAfterChar '.' unprocessedFileContents
        fileContents = splitOn "." uncutFileContents
    return $ case nrOfOfs of
                  0 -> fileContents
                  1 -> filterPhrases fatFile fileContents 
                  2 -> filterPhrases' (fatFile, file) fileContents
                  otherwise -> ["information existence: " ++ (unwordsLn filesPaths), "word existence: " ++ file]             

    
-- aici problema probabil provine de la fisierele cu structura de arbore 
directories filePath = do
    directoryContents <- getDirectoryContents filePath
    listOfFolders <- mapM (\x -> do let updatedFilePath = filePath ++ "\\" ++ x
                                    fileContents <- readFileM "directories.txt"
                                    writeFile "directories'.txt" (fileContents ++ "\n" ++ updatedFilePath)
                                    renameFile "directories'.txt" "directories.txt"
                                    directories updatedFilePath) (directoryFolders directoryContents)
    return directoryContents

newFile (name, extension) directoryContents = 
    let 
        filesDir = directoyFiles (name, extension) directoryContents
        maybeNr  = if filesDir == [] then [] else show (1 + maximum (map (\x -> extractNumber x) filesDir))
    in 
        name ++ maybeNr ++ extension  


inputToupled input fileContents =
    let 
        contentsNr = show (length (words fileContents))
    in
        show [input, contentsNr] ++ " "

wordsLn' []     string = reverse string
wordsLn' (x:xs) (y:ys) = if x == ' ' then wordsLn' xs ([""] ++ y:ys) else (if x == '\n' then wordsLn' xs ([""] ++ (y ++ [x]) : ys)
                                                                                        else wordsLn' xs ((y ++ [x]) : ys))

wordsLn string = wordsLn' string [""]

unwordsLn [] = []
unwordsLn (x:xs) = x ++ "\n" ++ unwordsLn xs

main :: IO ()
main = do
    args <- getArgs
    let file = args !! 0 -- infile
        log = args !! 1 -- outfile
    filePath <- getCurrentDirectory
    directoryContents <- getDirectoryContents filePath
    fileContents <- readFileM "memory.txt"
    -- showListOfStrings (directoyLogs directoryContents)
    let newLogFile  = newFile ("log", ".txt") directoryContents
        newFileContents = (unwordsLn (removeDuplicates (words fileContents))) ++ " "
    -- writeFile (newFile ("start", ".bat") directoryContents) "@echo off"
    -- putStrLn newFileContents
    writeFile "directories.txt" ""
    writeFile "directories'.txt" ""
    directories filePath
    directoriesContents <- readFile "directories.txt"
    writeFile "directories'.txt" (unwordsLn (removeDuplicates (words directoriesContents)))
    renameFile "directories'.txt" "directories.txt"
    putStrLn (date "I010092021174243")
    putStrLn (time "I010092021174243")
    putStrLn (time "I010a92021174243")
   -- putStrLn (show (slashWords "C:\\Users\\texur\\Desktop\\buddy\\memory\\part-of-speech\\refresh\\word-not-found"))
    appLoop "memory.txt" "memory'.txt" newLogFile (newFileContents, "")
    putStrLn fileContents