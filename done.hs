-- what done.hs
-- who  nate smith
-- why  an elegant and basic approach to todo listing
-- when feb 2010

import System.IO
import System.Environment ( getArgs )
import Database.HDBC
import Database.HDBC.Sqlite3

-- adlbh


-- add into db, check for due date
--add :: Connection -> [String] -> IO ()
--add dbh argv = putStrLn "add a task"
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
    putStrLn $ "added " ++ desc

insertTaskDueDate :: Connection -> String -> String -> IO ()
insertTaskDueDate dbh desc due = do
    run dbh "INSERT INTO tasks VALUES (null, ?, ?, (SELECT CURRENT_TIMESTAMP), 'f')" [toSql desc, (toSql (parseDate due))]
    putStrLn $ "added " ++ desc ++ " (due: " ++ (parseDate due) ++ ")"

parseDate :: String -> String
parseDate due = "2011-02-21 19:55:17"

done :: Connection -> [String] -> IO ()
done dbh argv = putStrLn "finish a task"

list :: Connection -> [String] -> IO ()
list dbh argv = putStrLn "list tasks"

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
