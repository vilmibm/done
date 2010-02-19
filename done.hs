-- what done.hs
-- who  nate smith
-- why  an elegant and basic approach to todo listing
-- when feb 2010

import System.IO
import System.Directory
import System.Environment ( getArgs )
import Database.HDBC
import Database.HDBC.Sqlite3

-- adlbh

initDB dbh = do
    run dbh "CREATE TABLE tasks (id integer primary key, desc text, due_date integer, created_ts integer, done boolean)" []
    commit dbh

connectDB :: IO Connection
connectDB = do
    dbh <- connectSqlite3 "done.db"
    setBusyTimeout dbh 5000
    return dbh

runCommand :: String -> [String] -> IO ()
runCommand cmd argv = 
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
    -- if not DB, init
    dbh <- connectDB
    foundDB <- doesFileExist "done.db"
    if foundDB then putStrLn "found db" else (initDB dbh)
    argv <- getArgs
    case argv of
        [] -> help
        _  -> runCommand (head argv) (tail argv) 
