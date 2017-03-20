-- | Framework for Inv/Req/Dat message handling

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Communication.Relay.Logic
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , relayListeners
       , relayStubListeners
       , relayWorkers
       , RelayProxy (..)
       , InvOrData

       , invReqDataFlow
       , invReqDataFlowNeighbors
       ) where

import           Control.Concurrent.STM             (isFullTBQueue, readTBQueue,
                                                     writeTBQueue)
import           Formatting                         (build, sformat, shown, stext, (%))
import           Mockable                           (Mockable, MonadMockable, Throw,
                                                     handleAll, throw, throw)
import           Node.Message                       (Message)
import           Paths_cardano_sl_infra             (version)
import           Serokell.Util.Text                 (listJson)
import           Serokell.Util.Verify               (VerificationRes (..))
import           System.Wlog                        (WithLogger, logDebug, logError,
                                                     logInfo, logWarning)
import           Universum

import           Pos.Binary.Class                   (Bi (..))
import           Pos.Binary.Infra.Communication     ()
import           Pos.Communication.Limits.Types     (Limit, LimitedLength,
                                                     LimitedLengthExt (..),
                                                     MessageLimited (..), recvLimited,
                                                     reifyMsgLimit, withLimitedLength)
import           Pos.Communication.MessagePart      (MessagePart)
import           Pos.Communication.PeerState        (WithPeerState)
import           Pos.Communication.Protocol         (ConversationActions (..),
                                                     ListenerSpec, NodeId, OutSpecs,
                                                     SendActions (..), WorkerSpec,
                                                     listenerConv, mergeLs, worker)
import           Pos.Communication.Relay.Class      (MonadRelayMem (..), Relay (..))
import           Pos.Communication.Relay.Types      (RelayContext (..), RelayError (..),
                                                     RelayProxy (..), SomeInvMsg (..))
import           Pos.Communication.Types.Relay      (DataMsg (..), InvMsg (..), InvOrData,
                                                     ReqMsg (..))
import           Pos.Communication.Util             (stubListenerConv)
import           Pos.DB.Limits                      (MonadDBLimits)
import           Pos.DHT.Model                      (DHTNode, MonadDHT (..),
                                                     converseToNeighbors, converseToNode)
import           Pos.Reporting                      (MonadReportingMem, reportingFatal)

import           Pos.Communication.Limits.Instances ()

type MinRelayWorkMode m
    = ( WithLogger m
      , MonadMockable m
      , MonadDHT m
      , MonadIO m
      , WithPeerState m
      )

type RelayWorkMode m = ( MinRelayWorkMode m, MonadRelayMem m)

-- Returns useful keys.
handleInvL
    :: forall m key tag contents.
       ( Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Relay m tag key contents
       , MinRelayWorkMode m
       )
    => RelayProxy key tag contents
    -> InvMsg key tag
    -> m (Maybe key)
handleInvL proxy msg@(InvMsg{..}) =
    processMessage Nothing "Inventory" imTag verifyInvTag $ do
        let _ = invCatchType proxy msg
        invRes <- handleInv imTag imKey
        if invRes then
            Just imKey <$ logDebug (sformat
              ("We'll request data "%build%" for key "%build%", because it's useful")
              imTag imKey)
        else
            Nothing <$ logDebug (sformat
              ("Ignoring inv "%build%" for key "%build%", because it's useless")
              imTag imKey)

handleReqL
    :: forall key tag contents m .
       ( Bi (ReqMsg key tag)
       , Bi (InvOrData tag key contents)
       , Message (InvOrData tag key contents)
       , Relay m tag key contents
       , MinRelayWorkMode m
       , MonadDBLimits m
       )
    => RelayProxy key tag contents
    -> m (ListenerSpec m, OutSpecs)
handleReqL proxy = reifyMsgLimit (Proxy @(ReqMsg key tag)) $
  \(_ :: Proxy s) -> return $ listenerConv $
    \_ __peerId ConversationActions{..} ->
    whenJustM recv $ \(withLimitedLength @s -> msg@ReqMsg {..}) -> do
      let _ = reqCatchType proxy msg
      processMessage () "Request" rmTag verifyReqTag $ do
          dtMB <- handleReq rmTag rmKey
          case dtMB of
              Nothing ->
                  logDebug $ sformat ("We don't have data "%build%" for key "%build) rmTag rmKey
              Just dt -> do
                  logDebug $ sformat ("We have data "%build%" for key "%build) rmTag rmKey
                  send $ constructDataMsg dt
  where
    constructDataMsg :: contents -> InvOrData tag key contents
    constructDataMsg = Right . DataMsg

-- Returns True if we should propagate.
handleDataL
      :: forall tag key contents m .
      ( Bi (InvMsg key tag)
      , Bi (ReqMsg key tag)
      , Bi key
      , Bi tag
      , Bi (DataMsg contents)
      , MonadDHT m
      , MessagePart tag
      , MessagePart contents
      , Relay m tag key contents
      , RelayWorkMode m
      )
    => RelayProxy key tag contents
    -> DataMsg contents
    -> m ()
handleDataL proxy msg@(DataMsg {..}) =
    processMessage () "Data" dmContents verifyDataContents $ do
        let _ = dataCatchType proxy msg
        dmKey <- contentsToKey dmContents
        ifM (handleData dmContents)
            (handleDataLDo dmKey) $
                logDebug $ sformat
                    ("Ignoring data "%build%" for key "%build) dmContents dmKey
  where
    handleDataLDo dmKey = do
        shouldPropagate <- _rlyIsPropagation <$> askRelayMem
        if shouldPropagate then do
            tag <- contentsToTag dmContents
            let inv :: InvOrData tag key contents
                inv = Left $ InvMsg tag dmKey
            addToRelayQueue inv
            logInfo $ sformat
                ("Adopted data "%build%" "%
                  "for key "%build%", data has been pushed to propagation queue...")
                dmContents dmKey
        else
            logInfo $ sformat
                ("Adopted data "%build%" for "%
                  "key "%build%", no propagation")
                dmContents dmKey

processMessage
  :: (Buildable param, WithLogger m)
  => a -> Text -> param -> (param -> m VerificationRes) -> m a -> m a
processMessage defaultRes name param verifier action = do
    verRes <- verifier param
    case verRes of
      VerSuccess -> action
      VerFailure reasons ->
          defaultRes <$
              logWarning (sformat
                ("Wrong "%stext%": invalid "%build%": "%listJson)
                name param reasons)

-- | Type `InvOrData` with limited length.
type InvOrDataLimitedLength s key tag contents =
    LimitedLengthExt s
        (Limit (InvMsg key tag), LimitType (DataMsg contents))
        (InvOrData tag key contents)

relayListeners
  :: forall m key tag contents.
     ( MonadDHT m
     , Bi key
     , Bi tag
     , Bi (InvMsg key tag)
     , Bi (DataMsg contents)
     , Bi (ReqMsg key tag)
     , Bi (InvOrData tag key contents)
     , MessagePart contents
     , MessagePart tag
     , Relay m tag key contents
     , Mockable Throw m
     , WithLogger m
     , RelayWorkMode m
     , MonadDBLimits m
     )
  => RelayProxy key tag contents -> m ([ListenerSpec m], OutSpecs)
relayListeners proxy = mergeLs <$> sequence [handleReqL proxy, invDataListener]
  where
    invDataListener = reifyMsgLimit (Proxy @(InvOrData tag key contents)) $
      \(_ :: Proxy s) -> return $ listenerConv $ \_ __peerId
        (ConversationActions{..}::(ConversationActions
                                  (ReqMsg key tag)
                                  (InvOrDataLimitedLength s key tag contents)
                                  m)
        ) -> do
            inv' <- recv
            whenJust (withLimitedLength <$> inv') $ expectLeft $
                \inv@InvMsg{..} -> do
                    useful <- handleInvL proxy inv
                    whenJust useful $ \ne -> do
                        send $ ReqMsg imTag ne
                        dt' <- recv
                        whenJust (withLimitedLength <$> dt') $ expectRight $
                            \dt@DataMsg{..} -> handleDataL proxy dt

    expectLeft call (Left msg) = call msg
    expectLeft _ (Right _)     = throw UnexpectedData

    expectRight _ (Left _)       = throw UnexpectedInv
    expectRight call (Right msg) = call msg


relayStubListeners
    :: ( WithLogger m
       , Bi (InvMsg key tag)
       , Bi (ReqMsg key tag)
       , Bi (DataMsg contents)
       , Message (InvOrData tag key contents)
       , Message (ReqMsg key tag)
       )
    => RelayProxy key tag contents -> ([ListenerSpec m], OutSpecs)
relayStubListeners p = mergeLs
    [ stubListenerConv $ invDataMsgProxy p
    , stubListenerConv $ reqMsgProxy p
    ]

invCatchType :: RelayProxy key tag contents -> InvMsg key tag -> ()
invCatchType _ _ = ()

reqCatchType :: RelayProxy key tag contents -> ReqMsg key tag -> ()
reqCatchType _ _ = ()
reqMsgProxy :: RelayProxy key tag contents
            -> Proxy (InvOrData tag key contents, ReqMsg key tag)
reqMsgProxy _ = Proxy

dataCatchType :: RelayProxy key tag contents -> DataMsg ontents -> ()
dataCatchType _ _ = ()

invDataMsgProxy :: RelayProxy key tag contents
                -> Proxy (ReqMsg key tag, InvOrData tag key contents)
invDataMsgProxy _ = Proxy


addToRelayQueue :: forall tag key contents m .
                ( Bi (InvOrData tag key contents)
                , Bi (ReqMsg key tag)
                , Message (InvOrData tag key contents)
                , Message (ReqMsg key tag)
                , Buildable tag, Buildable key
                , RelayWorkMode m
                )
                => InvOrData tag key contents -> m ()
addToRelayQueue inv = do
    queue <- _rlyPropagationQueue <$> askRelayMem
    isFull <- atomically $ isFullTBQueue queue
    if isFull then
        logWarning $ "Propagation queue is full, no propagation"
    else
        atomically $ writeTBQueue queue (SomeInvMsg inv)

relayWorkers :: forall m .
             ( Mockable Throw m
             , RelayWorkMode m
             , MonadMask m
             , MonadReportingMem m
             )
             => OutSpecs -> ([WorkerSpec m], OutSpecs)
relayWorkers allOutSpecs =
    first (:[]) $ worker allOutSpecs $ \sendActions ->
        handleAll handleWE $ reportingFatal version $ action sendActions
  where
    action sendActions = do
        queue <- _rlyPropagationQueue <$> askRelayMem
        forever $ atomically (readTBQueue queue) >>= \case
            SomeInvMsg i@(Left (InvMsg{..})) -> do
                logDebug $ sformat
                    ("Propagation data with key: "%build%
                     " and tag: "%build) imKey imTag
                converseToNeighbors sendActions (convHandler i)
            SomeInvMsg (Right _) ->
                logWarning $ "DataMsg is contains in inv propagation queue"

    convHandler
        :: InvOrData tag1 key1 contents1
        -> NodeId
        -> ConversationActions
             (InvOrData tag1 key1 contents1) (ReqMsg key1 tag1) m
        -> m ()
    convHandler inv __peerId ConversationActions{..} = send inv

    handleWE e = do
        logError $ sformat ("relayWorker: error caught "%shown) e
        throw e

----------------------------------------------------------------------------
-- Helpers for Communication.Methods
----------------------------------------------------------------------------

invReqDataFlowNeighbors
    :: forall tag id contents m.
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinRelayWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> SendActions m -> tag -> id -> contents -> m ()
invReqDataFlowNeighbors what sendActions tag id dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg id tag)) $ \lim ->
        converseToNeighbors sendActions (invReqDataFlowDo what tag id dt lim)
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", id = "%build%" to neighbors: "%shown) what id e

invReqDataFlow
    :: forall tag id contents m.
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinRelayWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> SendActions m -> DHTNode -> tag -> id -> contents -> m ()
invReqDataFlow what sendActions addr tag id dt = handleAll handleE $
    reifyMsgLimit (Proxy @(ReqMsg id tag)) $ \lim ->
        converseToNode sendActions addr (invReqDataFlowDo what tag id dt lim)
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", id = "%build%" to "%shown%": "%shown) what id addr e

invReqDataFlowDo ::
    ( Message (InvOrData tag id contents)
    , Message (ReqMsg id tag)
    , Buildable id
    , MinRelayWorkMode m
    , MonadDBLimits m
    , Bi tag, Bi id
    , Bi (InvOrData tag id contents)
    , Bi (ReqMsg id tag))
    => Text -> tag -> id -> contents -> Proxy s -> NodeId
    -> ConversationActions (InvOrData tag id contents)
        (LimitedLength s (ReqMsg id tag)) m
    -> m ()
invReqDataFlowDo what tag id dt _ nodeId conv = do
    send conv $ Left $ InvMsg tag id
    recvLimited conv >>= maybe handleD replyWithData
  where
    replyWithData (ReqMsg _ _) = send conv $ Right $ DataMsg dt
    handleD = logDebug $
        sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                 \Inv id = "%build) what nodeId id
