{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
import Yesod
import Yesod.Form.Jquery
import Database.Persist.Sqlite
import Text.Hamlet
import Text.Blaze
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Data.Text (Text, pack)
import Data.Time (Day)

data Frequency = Weekly | BiWeekly | Monthly | SemiMonthly
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Frequency"

instance ToHtml Frequency where
    toHtml = string . show

instance ToHtml Day where
    toHtml = string . show

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Bill
    name Text
    amount Int
    freq Frequency
    saved Int
Goal
    name Text
    amount Int
    targetDate Day
    saved Int
|]

data DailySpend = DailySpend ConnectionPool

mkYesod "DailySpend" [parseRoutes|
/bills BillR GET
/bills/add AddBillR GET POST
/goals GoalR GET
/goals/add AddGoalR GET POST
|]

instance Yesod DailySpend where
    approot _ = ""

instance YesodJquery DailySpend

instance YesodPersist DailySpend where
    type YesodPersistBackend DailySpend = SqlPersist

    runDB action = liftIOHandler $ do
        DailySpend pool <- getYesod
	runSqlPool action pool

getBillR :: Handler RepHtml
getBillR = do
    allBills <- runDB $ selectList [] [Desc BillAmount]
    let bills = map snd allBills
    hamletToRepHtml $(hamletFile "bills.hamlet")

instance RenderMessage DailySpend FormMessage where
    renderMessage _ _ = defaultFormMessage

billForm  = renderDivs $ Bill
    <$> areq textField "Name" Nothing
    <*> areq intField "Amount" Nothing
    <*> areq (selectField freqs) "Frequency" Nothing
  where
    freqs = map (pack . show &&& id) $ [minBound..maxBound]

getAddBillR :: Handler RepHtml
getAddBillR = do
    ((_, widget), enctype) <- generateFormPost billForm
    defaultLayout $(whamletFile "addBill.hamlet")

postAddBillR :: Handler RepHtml
postAddBillR = do
    ((result, widget), enctype) <- runFormPost billForm
    case result of
        FormSuccess bill -> do
            runDB $ insert (bill 0)
            redirect RedirectTemporary BillR
	_ -> defaultLayout $(whamletFile "addBill.hamlet")

getGoalR :: Handler RepHtml
getGoalR = do
    allGoals <- runDB $ selectList [] [Desc GoalTargetDate]
    let goals = map snd allGoals
    hamletToRepHtml $(hamletFile "goals.hamlet")

goalForm = renderDivs $ Goal
    <$> areq textField "Name" Nothing
    <*> areq intField "Amount" Nothing
    <*> areq (jqueryDayField def
        { jdsChangeYear = True
	, jdsYearRange = "1900:+10"
	}) "Target Date" Nothing

getAddGoalR :: Handler RepHtml
getAddGoalR = do
    ((_, widget), enctype) <- generateFormPost goalForm
    defaultLayout $(whamletFile "addGoal.hamlet")

postAddGoalR :: Handler RepHtml
postAddGoalR = do
    ((result, widget), enctype) <- runFormPost goalForm
    case result of
        FormSuccess goal -> do
	    runDB $ insert (goal 0)
	    redirect RedirectTemporary GoalR
	_ -> defaultLayout $(whamletFile "addGoal.hamlet")

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = withSqlitePool "dailySpend.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    warpDebug 3000 $ DailySpend pool
