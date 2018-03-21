{-# LANGUAGE OverloadedStrings #-}

module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)


import           FirstApp.Types           (ContentType (JSON), Error (Error),
                                           RqType (AddRq, ListRq, ViewRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

import           Data.Function            ((&))

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse st ct =
  responseLBS st [("Content-Type", renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t bs = do
  topic <- mkTopic t
  comment <- mkCommentText (lazyByteStringToStrictText bs)
  return $ AddRq topic comment
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t =
  fmap ViewRq (mkTopic t)

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse (Error e) =
  resp400 JSON $
       e
     & Data.Text.Encoding.encodeUtf8
     & LBS.fromStrict

-- :: Text
--   -> LBS.ByteString
--   -> Either Error RqType
--

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )

mkRequest request = case pathInfo request of
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  [t, "add"]  -> mkAddRequest t <$> strictRequestBody request
  [t, "view"] -> return $ mkViewRequest t
  ["list"]    -> return mkListRequest
  _           -> return $ Left (Error "Invalid path")

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response

handleRequest ListRq      = Right (resp200 JSON "Not implemented yet")
handleRequest (AddRq _ _) = Right (resp200 JSON "Not implemented yet")
handleRequest (ViewRq _)  = Right (resp200 JSON "Not implemented yet")

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app request response =
  responseIO >>= response where
    responseIO = do
      reqE <- mkRequest request
      return $ case reqE of
        Left e -> mkErrorResponse e
        Right req ->
          case handleRequest req of
            Left e    -> mkErrorResponse e
            Right rsp -> rsp

runApp :: IO ()
runApp = run 3000 app
