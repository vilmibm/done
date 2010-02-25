-- what done.hs
-- who  nate smith
-- why  an elegant and basic approach to todo listing
-- when feb 2010

import System.IO
import System.Environment ( getArgs )
import Database.HDBC
import Database.HDBC.Sqlite3

insertSql :: String -> String
insertSql "nodue" = "INSERT INTO tasks VALUES (null, ?, null, (SELECT CURRENT_TIMESTAMP), 'f')"
insertSql "due"   = "INSERT INTO tasks VALUES (null, ?, ?, (SELECT CURRENT_TIMESTAMP), 'f')"

listSql :: String -> String
listSql "filter"   = "SELECT desc FROM tasks WHERE desc LIKE ? AND done = 'f' ORDER BY due_ts, created_ts"
listSql "nofilter" = "SELECT desc FROM tasks WHERE done = 'f' ORDER BY due_ts, created_ts"

indent :: String
indent = "  "

---- add task
add :: Connection -> [String] -> IO ()
add dbh argv = do
    case argv of
        (x:[])     -> insertTask dbh (head argv)
        (x:y:z:[]) -> insertTaskDueDate dbh (head argv) (last argv)
        []         -> help
        _          -> help

insertTask :: Connection -> String -> IO ()
insertTask dbh desc = do
    run dbh (insertSql "nodue") [toSql desc]
    commit dbh
    putStrLn $ (indent) ++ "added " ++ desc

insertTaskDueDate :: Connection -> String -> String -> IO ()
insertTaskDueDate dbh desc due = do
    run dbh (insertSql "due") [toSql desc, (toSql (parseDate due))]
    commit dbh
    putStrLn $ (indent) ++ "added " ++ desc ++ " (due: " ++ (parseDate due) ++ ")"

-- stubbed for now:
parseDate :: String -> String
parseDate due = "2011-02-21 19:55:17"

---- finish a task
done :: Connection -> [String] -> IO ()
done dbh argv =
    case argv of 
        []     -> do
            r <- quickQuery dbh (listSql "nofilter") []
            finishTasks dbh (map fromSql (map head r))
        (x:[]) -> do
            r <- quickQuery dbh (listSql "filter") [toSql $ "%"++x++"%"]
            finishTasks dbh (map fromSql (map head r))
        _      -> help    

finishTasks :: Connection -> [String] -> IO ()
finishTasks dbh [] = putStr ""
finishTasks dbh (x:xs)  = do
    putStr $ (indent) ++ "* " ++ x ++ "? [yN]: "
    answer <- getLine
    case answer of
        "y" -> finishOff dbh x
        "Y" -> finishOff dbh x
        _   -> putStr ""
    finishTasks dbh xs

finishOff :: Connection -> String -> IO ()
finishOff dbh desc = do
    run dbh "UPDATE tasks SET done='t' WHERE desc=?" [toSql desc]
    commit dbh
    putStrLn $ (indent) ++ (indent) ++ "X " ++ desc

---- list out tasks
list :: Connection -> [String] -> IO ()
list dbh [] = do
    r <- quickQuery dbh (listSql "nofilter") []
    listOut (map fromSql (map head r))

list dbh (x:[]) = do
    r <- quickQuery dbh (listSql "filter") [toSql $ "%"++x++"%"]
    listOut (map fromSql (map head r))

listOut :: [String] -> IO ()
listOut []     = putStr ""
listOut (x:xs) = do
    putStrLn $ (indent) ++ "* " ++ x
    listOut xs

---- go to sqlite3 backend
backend :: IO ()
backend = putStrLn "launch sqlite3"

help :: IO ()
help = putStrLn "available commands: aldh"

connectDB :: IO Connection
connectDB = do
    dbh <- connectSqlite3 "done.db"
    return dbh

runCommand :: Connection -> String -> [String] -> IO ()
runCommand dbh cmd argv =
    case cmd of
        "a"    -> add dbh argv
        "add"  -> add dbh argv
        "d"    -> done dbh argv
        "done" -> done dbh argv
        "l"    -> list dbh argv
        "list" -> list dbh argv
        "b" -> backend
        "h" -> help
        _   -> putStrLn $ "I don't understand: " ++ cmd

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    dbh  <- connectDB 
    argv <- getArgs
    case argv of
        [] -> help
        _  -> runCommand dbh (head argv) (tail argv) 
