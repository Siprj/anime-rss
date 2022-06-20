module Main
    (main)
  where

import Relude hiding (on, id)
import Crypto.Error (throwCryptoErrorIO)
import Optics ((^.), toLensVL)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import Brick.Widgets.Core
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Effectful (runEff)
import qualified Database.PostgreSQL.Simple as SQL
import Brick
import Brick.Forms
import Brick.Focus
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import AnimeRss.Ids (UserId)
import AnimeRss.DataModel.Queries
import Crypto.PasswordStore (hashPassword, defaultOptions)
import AnimeRss.DataModel.Types
import DBE
import Options
import Data.Vector (Vector)
import Options.Applicative hiding (str)
import Data.ByteString.Char8 hiding (length, unlines)
import AnimeRss.DataModel.Migrations

data Name
    = NameField
    | EmailField
    | PasswordField
    | PasswordConfirmField
    | UserListWidget
    | ErrorWidget
  deriving stock (Eq, Ord, Show)

data UserInfo = UserInfo
    { id :: UserId
    , name :: Text
    , email :: Text
    }
  deriving stock (Show, Generic)

data NewUserInfo = NewUserInfo
    { name :: Text
    , email :: Text
    , password  :: Text
    , passwordConfirm :: Text
    }
  deriving stock (Show, Generic)

data AppStage e
    = UserList
    | AddingNewUser (Form NewUserInfo e Name)
    | Error Text (AppState e)

data AppState e = AppState
    { dbConnection :: SQL.Connection
    , stage :: AppStage e
    , userList :: L.GenericList Name Vector UserInfo
    }

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkCreateUserForm :: NewUserInfo -> Form NewUserInfo e Name
mkCreateUserForm = newForm
    [ label "Name" @@= editTextField (toLensVL #name) NameField (Just 1)
    , label "Email" @@= editTextField (toLensVL #email) EmailField (Just 1)
    , label "Password" @@= editPasswordField (toLensVL #password) PasswordField
    , label "Confirm password"
        @@= editPasswordField (toLensVL #passwordConfirm) PasswordConfirmField
    ]
  where
    label s w = padBottom (Pad 1)
        $ vLimit 1 (hLimit 17 $ str s <+> fill ' ') <+> w


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (userInfoAttr, fg V.cyan)
  ]

drawError :: Text -> AppState e -> [Widget Name]
drawError err _previousApp =
    [C.hCenter . C.vCenter . B.border . hLimit 80 . C.hCenter $ txt err]

userInfoAttr :: A.AttrName
userInfoAttr = L.listSelectedAttr <> "userInfo"

listDrawElement :: Bool -> UserInfo -> Widget Name
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr userInfoAttr (str $ "<" <> s <> ">")
                   else str s
    in selStr $ show a

draw :: AppState e -> [Widget Name]
draw AppState{..} = draw' stage
  where
    draw' :: AppStage e -> [Widget Name]
    draw' UserList = [ui]
      where
        label :: Widget Name
        label = str " Users "
        box :: Widget Name
        box = B.borderWithLabel label $
              L.renderList listDrawElement True userList
        ui :: Widget Name
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove user."
                              , C.hCenter $ str "Press Esc to exit."
                              ]
    draw' (AddingNewUser f) = [C.vCenter $ C.hCenter form]
        where
          form = B.border . padTop (Pad 1) . hLimit 50 $ renderForm f
    draw' (Error err previousApp) = drawError err previousApp

handleEvents :: AppState e -> BrickEvent Name e -> EventM Name (Next (AppState e))
handleEvents s@AppState{..} = handleEvents' stage
  where
    handleEvents' UserList = \case
        VtyEvent V.EvResize {} -> continue s
        VtyEvent (V.EvKey V.KEsc []) -> halt s
        VtyEvent (V.EvKey (V.KChar '+') []) -> do
            let initialUserInfo = NewUserInfo
                    { name = ""
                    , email = ""
                    , password = ""
                    , passwordConfirm = ""
                    }
            continue $ s { stage = AddingNewUser $ mkCreateUserForm initialUserInfo}
        VtyEvent (V.EvKey (V.KChar '-') []) ->
            case L.listSelectedElement userList of
                Nothing -> M.continue s
                Just (i, UserInfo{..}) -> do
                    void . liftIO . runEff . runDBESingle dbConnection $ deleteDbUser id
                    M.continue $ s {userList = L.listRemove i userList }
        VtyEvent e -> do
            userList' <- L.handleListEvent e userList
            M.continue $ s { userList = userList' }
        _ -> continue s
    handleEvents' (AddingNewUser form) = \case
        VtyEvent V.EvResize {} -> continue s
        VtyEvent (V.EvKey V.KEsc []) -> do
            continue $ s { stage = UserList }
        VtyEvent (V.EvKey V.KEnter []) -> do
            let newUser = formState form
            let nameCheck = newUser ^. #name /= ""
            let emailCheck = newUser ^. #email /= ""
            let passwordEmpty = newUser ^. #password /= ""
            let passwordCheck = newUser ^. #password == newUser ^. #passwordConfirm

            if nameCheck && emailCheck && passwordEmpty && passwordCheck
                then do
                    newUser' <- toCreateUser newUser
                    maybeUser <- liftIO . runEff . runDBESingle dbConnection $ insertDbUser newUser'
                    case maybeUser of
                        Nothing ->
                            continue s { stage = Error "User with given email already exists." s }
                        Just user -> do
                            let pos = length $ L.listElements userList
                            continue $ s
                                { stage = UserList
                                , userList = L.listInsert pos (toUserInfo user) userList
                                }
                else
                    continue s { stage =
                        Error (toError nameCheck emailCheck passwordEmpty passwordCheck) s }
        -- Enter quits only when we aren't in the multi-line editor.
        ev -> do
            form' <- handleFormEvent ev form

            -- Example of external validation:
            -- Require age field to contain a value that is at least 18.
            let fs = formState form'
            let passwordCheck = fs ^. #password == fs ^. #passwordConfirm
            continue $ s { stage = AddingNewUser $ setFieldValid passwordCheck PasswordConfirmField form' }
    handleEvents' (Error _ previousApp) = \case
        VtyEvent (V.EvKey _ _) -> continue previousApp
        _ -> continue s

when' :: Bool -> a -> [a]
when' v m = if v then pure m else mempty

toError :: Bool -> Bool -> Bool -> Bool -> Text
toError nameCheck emailCheck passwordEmpty passwordCheck = unlines $
    when' (not nameCheck) "Name can't be empty."
    <|> when' (not emailCheck) "Email can't be empty."
    <|> when' (not passwordEmpty) "Password can't be empty."
    <|> when' (not passwordCheck) "Passwords have to match."

chooseCursor :: AppState e -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor AppState{..} = chooseCursor' stage
  where
    chooseCursor' UserList = const Nothing
    chooseCursor' (AddingNewUser form) = focusRingCursor formFocus form
    chooseCursor' (Error _ _) = const Nothing

app :: App (AppState e) e Name
app = App
    { appDraw = draw
    , appHandleEvent = handleEvents
    , appChooseCursor = chooseCursor
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

toUserInfo :: GetUser -> UserInfo
toUserInfo GetUser{..} = UserInfo {..}

toCreateUser :: MonadIO m => NewUserInfo -> m CreateUser
toCreateUser NewUserInfo{..} = do
    (hashParameters, passwordByteString) <- liftIO $ hashPassword (encodeUtf8 password) defaultOptions 32
        >>= throwCryptoErrorIO
    pure CreateUser {password = DbPasswordHash passwordByteString hashParameters, ..}

main :: IO ()
main = do
    Configuration{..} <- execParser options
    dbConnection <- SQL.connectPostgreSQL (pack databaseConnectionString)
    migrateAll dbConnection
    userList <- runEff . runDBESingle dbConnection $ listDbUsers

    let f = AppState dbConnection UserList
            $ L.list UserListWidget (fromList $ fmap toUserInfo userList) 1

    void $ defaultMain app f
