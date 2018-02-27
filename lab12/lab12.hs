module Lab12 where
import System.Environment

main :: IO ()
main = app

wc :: String -> (Int, Int, Int)
wc str = (length(lines(str)),length(words(str)),length(str))

paste :: [String] -> [String] -> [String]
paste [] x = x
paste x [] = x
paste (x:str1) (y:str2) = [x++"\t"++y] ++ paste str1 str2


--replace delimiter with space 
replaceDel :: String -> String -> String
replaceDel delim [] = [] 
replaceDel delim (x:str)
 |[x] == delim = [' '] ++ replaceDel delim str
 |otherwise = [x] ++ replaceDel delim str

cutStr :: String -> Int -> String -> String
cutStr delim ind str = head ([ i | (i,j)<-(zip (words(replaceDel delim str)) [1,2..]), j==ind ])

cut :: String -> Int -> [String] -> [String]
cut delim ind str = [cutStr delim ind x| x<-str ]

app :: IO()
app = 
    do
        params <- getArgs
        if null params
            then 
                print "Nu puteti rula programul fara argumente"
            else
                case params!!0 of
                    "wc" -> 
                        do
                        if length params == 2
                        then
                            do
                            file <- readFile $ params!!1
                            print $ wc file
                        else 
                            print $ "Se mai asteapta " ++ show (2 - length params ) ++ " argumente"
                    "paste" ->  
                        do
                        if length params == 3
                        then 
                            do
                            file1 <- readFile $ params!!1
                            file2 <- readFile $ params!!2
                            putStr $ unlines $ paste (lines file1) (lines file2)
                        else
                            print $ "Se mai asteapta " ++ show (2 - length params ) ++ " argumente"                            
                    "cut" -> 
                        do
                        if length params == 4
                        then
                            do
                            file <- readFile $ params!!3   
                            putStr $ unlines $ cut (params!!1) (read  ( params!!2) :: Int) (lines file)
                        else
                            print $ "Se mai asteapta " ++ show (2 - length params ) ++ " argumente"                            
                    other -> print "Optiune necunoscuta"


