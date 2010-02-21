-- what done.hs
-- who  nate smith
-- why  an elegant and basic approach to todo listing
-- when feb 2010

import System.IO
import System.Environment ( getArgs )
import Database.HDBC
import Database.HDBC.Sqlite3

-- adlbh

connectDB :: IO Connection
connectDB = do
    dbh <- connectSqlite3 "done.db"
    setBusyTimeout dbh 5000
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

add :: Connection -> [String] -> IO ()
add dbh argv = putStrLn "add a task"

done :: Connection -> [String] -> IO ()
done dbh argv = putStrLn "finish a task"

list :: Connection -> [String] -> IO ()
list dbh argv = putStrLn "list tasks"

backend :: IO ()
backend = putStrLn "launch sqlite3"

help :: IO ()
help = putStrLn "available commands: aldh"

main :: IO ()
main = do
    dbh <- connectDB 
    argv <- getArgs
    case argv of
        [] -> help
        _  -> runCommand dbh (head argv) (tail argv) 
