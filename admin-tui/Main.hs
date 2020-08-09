{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
    (main)
  where

import Data.Functor (fmap)
import Data.String (String)
import Crypto.Error (throwCryptoErrorIO)
import Control.Applicative ((<|>), (<$>), pure)
import Control.Concurrent (forkIO)
import Control.Monad (Monad, void, (>>=), fail, when)
import Control.Monad.Freer.Internal (Eff(E, Val))
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer (runM, send, runNat)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative (pure)
import Data.Function (($), (.), const)
import Data.List (unlines)
import Data.Text.Encoding (encodeUtf8)
import Data.Semigroup ((<>))
import Data.Monoid (Monoid, mempty)
import Data.Bool (Bool(True, False), (&&), not)
import Text.Show (Show, show)
import Data.Text (Text)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Rest (RssApi, Context(Context, baseUri, title), rssApiHandler)
import Servant.Server (Handler, serve, hoistServer)
import System.IO (IO)
import Data.Eq (Eq, (==), (/=))
import Data.Ord (Ord)
import Optics (lensVL, (^.), makeFieldLabelsWith, noPrefixFieldLabels, toLensVL)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)
import qualified Graphics.Vty as V
import Brick
    ( App(App)
    , AttrMap
    , BrickEvent(VtyEvent)
    , CursorLocation
    , EventM
    , Next
    , Padding(Pad)
    , Widget
    , attrMap
    , continue
    , defaultMain
    , fg
    , fill
    , halt
    , hBox
    , padBottom
    , padTop
    , vBox
    , withAttr
    )
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L

import qualified Core.Type.User as Core
    ( NewUser(NewUser, name, email, password)
    , User(User, userId, name, email, password, newAnimeChannel, newEpisodeChannel), Email
    )
import Core.Type.Id (UserId, unsafeId)
import Control.Monad.Freer.Service (ServiceChannel)
import DataModel.Service
    ( DataModel
    , addUser
    , createDataModelChannel
    , deleteUser
    , listUsers
    , runDataModel
    , runDataModelEffect)
import Scraper.Service (runScraper)
import GHC.Num (Num((+)))
import Data.Foldable (Foldable(length))
import Data.Vector (Vector, fromList)
import Crypto.PasswordStore (PasswordHash, hashPassword, defaultOptions)


data Name
    = NameField
    | EmailField
    | PasswordField
    | PasswordConfirmField
    | UserListWidget
    | ErrorWidget
  deriving (Eq, Ord, Show)

data UserInfo = UserInfo
    { userId :: UserId
    , name :: Text
    , email :: Text
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''UserInfo

data NewUserInfo = NewUserInfo
    { name :: Text
    , email :: Text
    , password  :: Text
    , passwordConfirm :: Text
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''NewUserInfo

data AppStage e
    = UserList
    | AddingNewUser (Form NewUserInfo e Name)
    | Error String (AppState e)

data AppState e = AppState
    { dbChannel :: ServiceChannel DataModel
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
        $ (vLimit 1 $ hLimit 17 $ str s <+> fill ' ') <+> w


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (userInfoAttr, fg V.cyan)
  ]

drawError :: String -> AppState e -> [Widget Name]
drawError err previousApp =
    [C.hCenter . C.vCenter . B.border . hLimit 80 . C.hCenter $ str err]

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
        cur :: Widget Name
        cur = case userList ^. lensVL L.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total :: Widget Name
        total = str . show . length $ userList ^. lensVL L.listElementsL
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
          form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
    draw' (Error err previousApp) = drawError err previousApp

handleEvents :: AppState e -> BrickEvent Name e -> EventM Name (Next (AppState e))
handleEvents s@AppState{..} = handleEvents' stage
  where
    handleEvents' UserList = \case
        VtyEvent (V.EvResize {}) -> continue s
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
                    void . liftIO . runM . runDataModelEffect dbChannel $ deleteUser userId
                    M.continue $ s {userList = L.listRemove i userList }
        VtyEvent e -> do
            userList' <- L.handleListEvent e userList
            M.continue $ s { userList = userList' }
    handleEvents' (AddingNewUser form) = \case
        VtyEvent (V.EvResize {}) -> continue s
        VtyEvent (V.EvKey V.KEsc []) -> do
            continue $ s { stage = UserList }
        VtyEvent (V.EvKey V.KEnter []) -> do
            let newUser = formState form
            let nameCheck = (newUser ^. #name /= "")
            let emailCheck = (newUser ^. #email /= "")
            let passwordEmpty = (newUser ^. #password /= "")
            let passwordCheck = (newUser ^. #password == newUser ^. #passwordConfirm)

            if nameCheck && emailCheck && passwordEmpty && passwordCheck
                then do
                    newUser' <- toNewUserInfo newUser
                    maybeUser <- liftIO . runM . runDataModelEffect dbChannel $ addUser newUser'
                    case maybeUser of
                        Nothing ->
                            continue s { stage = Error "User with given email already exists." s }
                        Just user -> do
                            let pos = length $ L.listElements userList
                            continue $ s
                                { stage = UserList
                                , userList = L.listInsert pos (toUserInfo user) $ userList
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
            let passwordCheck = (fs ^. #password == fs ^. #passwordConfirm)
            continue $ s { stage = AddingNewUser $ setFieldValid passwordCheck PasswordConfirmField form' }
    handleEvents' (Error _ previousApp) = \case
        VtyEvent (V.EvKey _ _) -> continue previousApp
        _ -> continue s

when' :: Bool -> a -> [a]
when' v m = if v then pure m else mempty

toError :: Bool -> Bool -> Bool -> Bool -> String
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

toUserInfo :: Core.User -> UserInfo
toUserInfo Core.User{..} = UserInfo {..}

toNewUserInfo :: MonadIO m => NewUserInfo -> m Core.NewUser
toNewUserInfo NewUserInfo{..} = do
    password <- liftIO $ hashPassword (encodeUtf8 password) defaultOptions 32
        >>= throwCryptoErrorIO
    pure Core.NewUser {..}

main :: IO ()
main = do
    databaseChan <- createDataModelChannel
    forkIO $ runDataModel databaseChan
    userList <- runM . runDataModelEffect databaseChan $ listUsers

    let f = AppState databaseChan UserList
            $ L.list UserListWidget (fromList $ fmap toUserInfo userList) 1

    void $ defaultMain app f
