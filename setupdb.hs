-- what setupdb.hs
-- who  nate smith
-- why  database install script for done.hs
-- when feb 2010

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment
import System.Directory

runCommand :: String -> IO ()
runCommand cmd =
    case cmd of
        "d" -> delete "done.db"
        "i" -> install "done.db"
        "r" -> do
            delete "done.db"
            install "done.db"
        _   -> help

help = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " [d|i|r]"

delete :: String -> IO ()
delete path = do
    removeFile path
    putStrLn $ path ++ " deleted"

install :: String -> IO ()
install path = do
    dbh <- connectSqlite3 path
    run dbh "CREATE TABLE tasks (id integer primary key, desc text, due_ts integer, created_ts integer, done boolean)" []
    commit dbh
    putStrLn $ path ++ " created and installialized"

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        [] -> help
        _  -> runCommand (head argv)
