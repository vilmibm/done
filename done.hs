-- what done.hs
-- who  nate smith
-- why  an elegant and basic approach to todo listing
-- when feb 2010

import System.Environment ( getArgs )
import Database.HDBC
import Database.HDBC.Sqlite3

-- adlbh

connect :: IO Connection
connect = do
    dbh <- connectSqlite3 "do.db"
    setBusyTimeout dbh 5000
    -- prepDB dbh
    return dbh

run_command :: String -> [String] -> IO ()
run_command cmd argv = 
    case cmd of
        "a" -> add argv
        "d" -> done argv
        "l" -> list argv
        "b" -> backend
        "h" -> help
        _   -> putStrLn $ "I don't understand: " ++ cmd

add :: [String] -> IO ()
add argv = putStrLn "add a task"

done :: [String] -> IO ()
done argv = putStrLn "finish a task"

list :: [String] -> IO ()
list argv = putStrLn "list tasks"

backend :: IO ()
backend = putStrLn "launch sqlite3"

help :: IO ()
help = putStrLn "available commands: aldh"

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        [] -> help
        _  -> run_command (head argv) (tail argv)
