module Main where

import Data.Time
    ( getCurrentTime, utcToLocalTime, getCurrentTimeZone, LocalTime )
import qualified Data.Text as T
import Control.Monad.Trans.State ()
import Control.Monad.IO.Class ()
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

main :: IO()
main = do
    removeIfExists "auth_user"
    isAuthenticate <- readAuthPhoneNumber
    if not (fst isAuthenticate) then do
        putStrLn "\nAccount is not found\n"
        putStrLn "[hint] Select phone number from db/accounts.csv\n"
        putStrLn "(Enter) Retry\n"
        _ <- getLine
        main
    else do
        let authUser = snd isAuthenticate
        case authUser of
            Just a ->  do
                menuSelector
            Nothing -> main


data AuthState = AuthState 
    {
        uid :: String,
        phoneNumber :: String,
        accountNo :: String,
        balance :: String,
        authCode :: String
    } deriving (Show) 


getAuthUser :: IO (AuthState)
getAuthUser = do
    contents <- readFile "auth_user"  
    let singleLine = (lines contents)
    let accounts = map (\a -> map (\b -> T.unpack b) (splitToText a "|")) singleLine
    let account = accounts!!0
    return AuthState {
        uid= account !! 0, 
        phoneNumber=account !! 1,
        accountNo=account !! 2,
        balance=account !! 3,
        pin=account !! 4}

findIdxFromPhone :: String -> [[String]] -> Maybe Int
findIdxFromPhone a xss = do 
    let exist = any (elem a) xss
    let foundIdx = fst ((filter (\x -> snd x == a) [(fst xs, x) | xs <- (zip [0..] xss), x <- snd xs]) !! 0)
    if exist then do 
        return foundIdx
    else Nothing

splitToText :: String -> String -> [T.Text]
splitToText a b = T.splitOn (T.pack b) (T.pack a)


readAuthPhoneNumber :: IO ((Bool, Maybe AuthState))
readAuthPhoneNumber = do
    putStrLn "\nPlease enter your phone number:"
    authPhone <- getLine
    if length authPhone >= 11
        then do 
            contents <- readFile "db/accounts.csv"  
            let singleLine = tail $ lines contents
            let accounts = map (\a -> map (\b -> T.unpack b) (splitToText a ";")) singleLine
            let maybeIdx = findIdxFromPhone authPhone accounts
            case maybeIdx of
                Just idx -> do
                        let account = accounts !! idx
                        let authUser = AuthState {
                            uid=account !! 0, 
                            phoneNumber=account !! 4,
                            accountNo=account !! 2,
                            balance=account !! 5,
                            authCode=account !! 6}
                        writeAuth authUser
                        return (True, Just authUser)
                Nothing -> do
                    return (False, Nothing)
    else return (False, Nothing)


transferConfirmation :: IO (Maybe String)
transferConfirmation = do
    putStrLn "Enter account number to transfer:"
    accountNumber <- getLine
    putStrLn "Enter amount of money to transfer:"
    amount <- getLine
    putStrLn "\n***************************************************"
    putStrLn "Please make sure the destination account is correct"
    putStrLn "***************************************************\n"
    putStrLn $  "Destination account: " ++ accountNumber
    putStrLn $  "Amount Transfer: " ++ amount ++ "\n"
    putStrLn "(c) Confirm"
    putStrLn "(b) Back"
    confirmation <- getLine
    if confirmation == "c" 
        then return $ Just confirmation
        else transferConfirmation

getActionName :: String -> String
getActionName a
    | a == "c" = "Check Balance"
    | a == "d" = "Deposit Balance"
    | a == "w" = "Withdraw Money"
    | a == "t" = "Transfer Balance"


writeAuth :: AuthState -> IO ()
writeAuth authState = do
    let logMsg = (uid authState) ++ "|" ++ (phoneNumber authState) ++ "|" ++ (accountNo authState) ++ "|" ++ (balance authState) ++ "|" ++ (authCode authState)
    appendFile "auth_user" logMsg

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

logAction :: String -> IO ()
logAction action = do
    authUser <- getAuthUser
    currentTime <- getLocalCurrentDateTime
    let logMsg = (show currentTime) ++ " | " ++ (accountNo authUser) ++ " " ++ (getActionName action) ++ "\n"
    appendFile "logs.txt" logMsg

getLocalCurrentDateTime :: IO LocalTime
getLocalCurrentDateTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    return zoneNow


menuSelector ::  IO()
menuSelector = do 
    putStrLn "\n========================"
    putStrLn "WELCOME TO ATM SIMULATOR"
    putStrLn "========================\n"
    putStrLn "(c) Check Balance"
    putStrLn "(d) Deposit Balance"
    putStrLn "(w) Withdrawal"
    putStrLn "(t) Transfer"
    putStrLn "\n"
    selectedMenu <- getLine
    logAction selectedMenu

    case selectedMenu of
        "c" -> do
            authUser <- getAuthUser
            let msg = "\nYour Balance is\nRp " ++ (balance authUser)
            putStrLn msg
            putStrLn "(Enter) Back to Menu\n"
            _ <- getLine
            menuSelector
        
        "d" -> do
            putStrLn "Enter amount of money to deposit:"
            amount <- getLine
            putStrLn "\nInput your PIN to process deposit"
            pin <- getLine
            putStrLn "\nYour money is successfully added to your balance"
            putStrLn "(Enter) Back to Menu\n"
            _ <- getLine
            menuSelector
        
        "w" -> do
            putStrLn "Enter amount of money to withdraw:"
            amount <- getLine
            putStrLn "\nInput your PIN to process deposit"
            pin <- getLine
            putStrLn "\nYour money is successfully withdraw"
            putStrLn "(Enter) Back to Menu\n"
            _ <- getLine
            menuSelector

        "t" -> do
            maybeConfirmation <- transferConfirmation
            case maybeConfirmation of
                Just (confirmation) -> do
                    putStrLn "\nYour money is successfully transfered"
                    putStrLn "(Enter) Back to Menu\n"
                    _ <- getLine
                    menuSelector

        _   -> menuSelector



