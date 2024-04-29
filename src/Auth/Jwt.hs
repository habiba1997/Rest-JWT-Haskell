module Auth.Jwt (createJwtToken, authenticateJwt, extractBearerToken) where

import Data.Aeson.Types (Value (String))
import qualified Data.Map as Map
import qualified Data.Map.Internal as M
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Helpers.Config (getConfigValueOrDefault)
import Helpers.Helper (getStringFromValue, toString, toText)
import Models.AuthLib
import Models.PayloadLib
import qualified Web.JWT as JWT
import qualified Data.Text.Lazy as L 

-- Convert UTCTime to IntDate
utctimeToIntDate :: UTCTime -> Maybe JWT.NumericDate
utctimeToIntDate utcTime = JWT.numericDate diffTime
  where
    secondsSinceEpoch :: Integer
    secondsSinceEpoch = round $ utcTimeToPOSIXSeconds utcTime
    diffTime = fromIntegral secondsSinceEpoch

convertPayloadRecordToMap :: Payload -> Map.Map T.Text Value
convertPayloadRecordToMap (Payload payloadUserId payloadUserEmail) =
  Map.fromList
    [ (toText "userId", String $ toText payloadUserId),
      (toText "email", String $ toText payloadUserEmail)
    ]

convertMapListToPayload :: Map.Map T.Text Value -> Payload
convertMapListToPayload mapList = Payload userIdString userEmailString
  where
    userIdString = getStringFromValue $ mapList M.! toText "userId" :: String
    userEmailString = getStringFromValue $ mapList M.! toText "email" :: String

-- | Create a JWT token for a user
-- createJwtToken :: User -> IO Text
createJwtToken :: Payload -> IO Token
createJwtToken payload = do
  expirationConfig <- getConfigValueOrDefault "JWT_EXPIRES_IN_MINUTES" "20"
  let expirationMinutes = read expirationConfig :: Integer
  let expirationDuration = (60 * fromIntegral expirationMinutes) :: NominalDiffTime
  currentTime <- getCurrentTime
  -- Calculate expiration time
  let expirationTime = addUTCTime expirationDuration currentTime

  let unregClaims = JWT.ClaimsMap $ convertPayloadRecordToMap payload
  -- Create claims set
  let claims =
        JWT.JWTClaimsSet
          { JWT.iss = Nothing,
            JWT.sub = Nothing,
            JWT.aud = Nothing,
            JWT.exp = utctimeToIntDate expirationTime,
            JWT.nbf = Nothing,
            JWT.iat = utctimeToIntDate currentTime,
            JWT.jti = Nothing,
            JWT.unregisteredClaims = unregClaims
          }
  -- Sign the token
  secret <- getConfigValueOrDefault "JWT_KEY" "mySecretKey" :: IO String
  let key = JWT.hmacSecret $ toText secret
  let tokenValue = toString $ JWT.encodeSigned key mempty claims
  return (TokenConstructor tokenValue)

-- | Authenticate a JWT token
authenticateJwt :: Token -> IO (Either String Payload)
authenticateJwt tokenObject = do
  secretKey <- getConfigValueOrDefault "JWT_KEY" "mySecretKey" :: IO String
  let key = JWT.toVerify $ JWT.hmacSecret $ toText secretKey
  -- Decode and verify the token
  let tokenValue = toText (token tokenObject)
  case JWT.decodeAndVerifySignature key tokenValue of -- Maybe (JWT VerifiedJWT)
    Just jwtValue -> do
      -- Check expiration
      currentTime <- getCurrentTime
      let claimsSet = JWT.claims jwtValue
      if JWT.exp claimsSet > utctimeToIntDate currentTime
        then do
          let claimsMap = JWT.unClaimsMap $ JWT.unregisteredClaims claimsSet :: M.Map T.Text Value
          --  let lookUpEmail = M.lookup (toText "userEmail") claimsMap
          return $ Right (convertMapListToPayload claimsMap)
        else return (Left "Token has expired")
    Nothing -> do
      return (Left "Failed to decode/verify token")


extractBearerToken :: [(L.Text,L.Text)] -> Maybe String
extractBearerToken headers = do
  authHeader <- lookup (L.pack "Authorization") headers
  let (prefix, tokenString) = break (== ' ') (L.unpack authHeader)
  if prefix == "Bearer"
    then Just $ drop 1 tokenString
    else Nothing
    
