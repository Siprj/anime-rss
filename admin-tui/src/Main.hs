{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}
module Main (main) where

import AnimeRss.DataModel.Migrations (migrateAll)
import AnimeRss.DataModel.Queries (
  deleteDbUser,
  insertDbUser,
  listDbUsers,
 )
import AnimeRss.DataModel.Types (
  CreateUser (..),
  DbPasswordHash (DbPasswordHash),
  User (..),
 )
import AnimeRss.Ids (UserId)
import Brick (
  App (..),
  AttrMap,
  BrickEvent (VtyEvent),
  CursorLocation,
  EventM,
  Widget,
  attrMap,
  attrName,
  defaultMain,
  fg,
  halt,
  nestEventM',
  on,
  zoom,
 )
import Brick.AttrMap qualified as A
import Brick.Focus (focusRingCursor)
import Brick.Forms (
  Form (..),
  editPasswordField,
  editTextField,
  focusedFormInputAttr,
  handleFormEvent,
  invalidFormInputAttr,
  newForm,
  renderForm,
  setFieldValid,
  (@@=),
 )
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (
  Padding (Pad),
  fill,
  hLimit,
  padBottom,
  padTop,
  str,
  txt,
  vBox,
  vLimit,
  withAttr,
  (<+>),
 )
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Crypto.Error (throwCryptoErrorIO)
import Crypto.PasswordStore (defaultOptions, hashPassword)
import DBE (runDBESingle)
import Data.ByteString.Char8 (pack)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple qualified as SQL
import Effectful (runEff)
import Graphics.Vty qualified as V
import Optics (preview, toLensVL, use, (%), (.~), (^.))
import Optics.State.Operators ((.=))
import Options (
  Configuration (Configuration, databaseConnectionString),
  options,
 )
import Options.Applicative (execParser)
import Relude (
  Alternative ((<|>)),
  Applicative (pure),
  Bool (True),
  ConvertUtf8 (encodeUtf8),
  Eq (..),
  Foldable (length),
  Functor (fmap),
  Generic,
  IO,
  IsList (fromList),
  Maybe (..),
  Monad ((>>=)),
  MonadIO (..),
  MonadState (get, put),
  Monoid (mempty),
  Ord,
  Semigroup ((<>)),
  Show,
  Text,
  const,
  gets,
  modify,
  not,
  show,
  unlines,
  void,
  ($),
  (&&),
  (.),
 )

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
  , password :: Text
  , passwordConfirm :: Text
  }
  deriving stock (Show, Generic)

data AppStage e
  = UserList
  | AddingNewUser (Form NewUserInfo e Name)
  | Error Text (AppState e)
  deriving stock (Generic)

data AppState e = AppState
  { dbConnection :: SQL.Connection
  , stage :: AppStage e
  , userList :: L.GenericList Name Vector UserInfo
  }
  deriving stock (Generic)

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkCreateUserForm :: NewUserInfo -> Form NewUserInfo e Name
mkCreateUserForm =
  newForm
    [ label "Name" @@= editTextField (toLensVL #name) NameField (Just 1)
    , label "Email" @@= editTextField (toLensVL #email) EmailField (Just 1)
    , label "Password" @@= editPasswordField (toLensVL #password) PasswordField
    , label "Confirm password"
        @@= editPasswordField (toLensVL #passwordConfirm) PasswordConfirmField
    ]
  where
    label s w =
      padBottom (Pad 1) $
        vLimit 1 (hLimit 17 $ str s <+> fill ' ') <+> w

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
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
userInfoAttr = L.listSelectedAttr <> attrName "userInfo"

listDrawElement :: Bool -> UserInfo -> Widget Name
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr userInfoAttr (str $ "<" <> s <> ">")
          else str s
   in selStr $ show a

draw :: AppState e -> [Widget Name]
draw AppState {..} = draw' stage
  where
    draw' :: AppStage e -> [Widget Name]
    draw' UserList = [ui]
      where
        label :: Widget Name
        label = str " Users "
        box :: Widget Name
        box =
          B.borderWithLabel label $
            L.renderList listDrawElement True userList
        ui :: Widget Name
        ui =
          C.vCenter $
            vBox
              [ C.hCenter box
              , str " "
              , C.hCenter $ str "Press +/- to add/remove user."
              , C.hCenter $ str "Press Esc to exit."
              ]
    draw' (AddingNewUser f) = [C.vCenter $ C.hCenter form]
      where
        form = B.border . padTop (Pad 1) . hLimit 50 $ renderForm f
    draw' (Error err previousApp) = drawError err previousApp

handleEvents :: BrickEvent Name e -> EventM Name (AppState e) ()
handleEvents event = do
  AppState {..} <- get
  handleEvents' stage event
  where
    handleEvents' :: AppStage e -> BrickEvent Name e -> EventM Name (AppState e) ()
    handleEvents' UserList = \case
      VtyEvent V.EvResize {} -> pure ()
      VtyEvent (V.EvKey V.KEsc []) -> halt
      VtyEvent (V.EvKey (V.KChar '+') []) -> do
        let initialUserInfo =
              NewUserInfo
                { name = ""
                , email = ""
                , password = ""
                , passwordConfirm = ""
                }
        #stage .= AddingNewUser (mkCreateUserForm initialUserInfo)
        pure ()
      VtyEvent (V.EvKey (V.KChar '-') []) -> do
        dbConnection <- use #dbConnection
        zoom (toLensVL #userList) $ do
          userList <- get
          case L.listSelectedElement userList of
            Nothing -> pure ()
            Just (i, UserInfo {..}) -> do
              void . liftIO . runEff . runDBESingle dbConnection $ deleteDbUser id
              modify (L.listRemove i)
      VtyEvent ev -> zoom (toLensVL #userList) $ L.handleListEvent ev
      _ -> pure ()
    handleEvents' (AddingNewUser form) = \case
      VtyEvent V.EvResize {} -> pure ()
      VtyEvent (V.EvKey V.KEsc []) -> do
        #stage .= UserList
      VtyEvent (V.EvKey V.KEnter []) -> do
        s <- get
        let newUser = formState form
        let nameCheck = newUser ^. #name /= ""
        let emailCheck = newUser ^. #email /= ""
        let passwordEmpty = newUser ^. #password /= ""
        let passwordCheck = newUser ^. #password == newUser ^. #passwordConfirm

        if nameCheck && emailCheck && passwordEmpty && passwordCheck
          then do
            newUser' <- toCreateUser newUser
            maybeUser <- liftIO . runEff . runDBESingle (s ^. #dbConnection) $ insertDbUser newUser'
            case maybeUser of
              Nothing ->
                #stage .= Error "User with given email already exists." s
              Just user -> do
                let pos = length $ L.listElements (s ^. #userList)
                #stage .= UserList
                #userList .= L.listInsert pos (toUserInfo user) (s ^. #userList)
          else
            #stage
              .= Error (toError nameCheck emailCheck passwordEmpty passwordCheck) s
      -- Enter quits only when we aren't in the multi-line editor.
      ev -> do
        maybeState <- gets (preview (#stage % #_AddingNewUser))
        case maybeState of
          Nothing -> pure ()
          Just state' -> do
            newState <- nestEventM' state' $ do
              handleFormEvent ev
              fs <- gets formState
              let passwordCheck = fs ^. #password == fs ^. #passwordConfirm
              modify (setFieldValid passwordCheck PasswordConfirmField)
            modify (#stage % #_AddingNewUser .~ newState)
    handleEvents' (Error _ previousApp) = \case
      VtyEvent (V.EvKey _ _) -> put previousApp
      _ -> pure ()

when' :: Bool -> a -> [a]
when' v m = if v then pure m else mempty

toError :: Bool -> Bool -> Bool -> Bool -> Text
toError nameCheck emailCheck passwordEmpty passwordCheck =
  unlines $
    when' (not nameCheck) "Name can't be empty."
      <|> when' (not emailCheck) "Email can't be empty."
      <|> when' (not passwordEmpty) "Password can't be empty."
      <|> when' (not passwordCheck) "Passwords have to match."

chooseCursor :: AppState e -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor AppState {..} = chooseCursor' stage
  where
    chooseCursor' UserList = const Nothing
    chooseCursor' (AddingNewUser form) = focusRingCursor formFocus form
    chooseCursor' (Error _ _) = const Nothing

app :: App (AppState e) e Name
app =
  App
    { appDraw = draw
    , appHandleEvent = handleEvents
    , appChooseCursor = chooseCursor
    , appStartEvent = pure ()
    , appAttrMap = const theMap
    }

toUserInfo :: User -> UserInfo
toUserInfo User {..} = UserInfo {..}

toCreateUser :: MonadIO m => NewUserInfo -> m CreateUser
toCreateUser NewUserInfo {..} = do
  (hashParameters, passwordByteString) <-
    liftIO $
      hashPassword (encodeUtf8 password) defaultOptions 32
        >>= throwCryptoErrorIO
  pure CreateUser {password = DbPasswordHash passwordByteString hashParameters, ..}

main :: IO ()
main = do
  Configuration {..} <- execParser options
  dbConnection <- SQL.connectPostgreSQL (pack databaseConnectionString)
  migrateAll dbConnection
  userList <- runEff . runDBESingle dbConnection $ listDbUsers

  let f =
        AppState dbConnection UserList $
          L.list UserListWidget (fromList $ fmap toUserInfo userList) 1

  void $ defaultMain app f
