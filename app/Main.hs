import Data.Time
import qualified Data.Text as T
import Control.Monad.Trans.State (State, execState, modify, get)

main :: IO()
main = do
    isAuthenticate <- readAuthPhoneNumber
    if not (fst isAuthenticate) then do
        putStrLn "\nAccount is not found\n"
        putStrLn "(Enter) Retry\n"
        _ <- getLine
        main
    else do
        -- let authUser = snd isAuthenticate
        -- print (execState createSession authUser)
        menuSelector

-- createSession :: State AuthState ()
-- createSession = do
--    modify (authState)


phoneExist :: String -> [[String]] -> Bool
phoneExist a xss = any (elem a) xss

splitToText :: String -> [T.Text]
splitToText a = T.splitOn (T.pack ";") (T.pack a)

data AuthState = AuthState Int String Int deriving (Show) 

readAuthPhoneNumber :: IO ((Bool, Maybe AuthState))
readAuthPhoneNumber = do
    putStrLn "\nPlease enter your phone number:"
    authPhone <- getLine
    if length authPhone >= 11
        then do 
            contents <- readFile "db/accounts.csv"  
            let singleLine = tail $ lines contents
            let accounts = map (\a -> map (\b -> T.unpack b) (splitToText a)) singleLine
            let isAuthenticated = phoneExist authPhone accounts
            let authUser = AuthState 1 "2204050000001" 10000000
            return (isAuthenticated, Just authUser)
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
    

logAction :: String -> IO ()
logAction action = do
    currentTime <- getLocalCurrentDateTime
    let logMsg = (show currentTime) ++ " | " ++ (getActionName action) ++ "\n"
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
            putStrLn "\nYour Balance is"
            putStrLn "Rp 1000.000\n"
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



