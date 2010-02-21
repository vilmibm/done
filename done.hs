-- what done.hs
-- who  nate smith
-- why  an elegant and basic approach to todo listing
-- when feb 2010

import System.IO
import System.Environment ( getArgs )
import Database.HDBC
import Database.HDBC.Sqlite3

-- adlbh

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
    run dbh "INSERT INTO tasks VALUES (null, ?, null, (SELECT CURRENT_TIMESTAMP), 'f')" [toSql desc]
    commit dbh
    putStrLn $ "\tadded " ++ desc

insertTaskDueDate :: Connection -> String -> String -> IO ()
insertTaskDueDate dbh desc due = do
    run dbh "INSERT INTO tasks VALUES (null, ?, ?, (SELECT CURRENT_TIMESTAMP), 'f')" [toSql desc, (toSql (parseDate due))]
    commit dbh
    putStrLn $ "\tadded " ++ desc ++ " (due: " ++ (parseDate due) ++ ")"

-- stubbed for now:
parseDate :: String -> String
parseDate due = "2011-02-21 19:55:17"

---- finish a task
done :: Connection -> [String] -> IO ()
done dbh argv = putStrLn "finish a task"

---- list out tasks
list :: Connection -> [String] -> IO ()
list dbh [] = do
    r <- quickQuery dbh "SELECT desc FROM tasks WHERE done = 'f' ORDER BY due_date, created_ts" []
    listOut (map fromSql (map head r))

list dbh (x:[]) = do
    r <- quickQuery dbh "SELECT desc FROM tasks WHERE desc LIKE ? AND done = 'f' ORDER BY due_date, created_ts" [toSql $ "%"++x++"%"]
    listOut (map fromSql (map head r))

listOut :: [String] -> IO ()
listOut (x:xs) = do
    putStrLn x
    listOut xs

listOut [] = do
    putStrLn "\n"

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
        "a" -> add dbh argv
        "d" -> done dbh argv
        "l" -> list dbh argv
        "b" -> backend
        "h" -> help
        _   -> putStrLn $ "I don't understand: " ++ cmd

main :: IO ()
main = do
    dbh <- connectDB 
    argv <- getArgs
    case argv of
        [] -> help
        _  -> runCommand dbh (head argv) (tail argv) 
