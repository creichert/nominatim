
{-# LANGUAGE OverloadedStrings #-}

module Web.Nominatim
       (
         Place(..)
       , RequestFormat(..)
       , RequestParameter(..)

       , nominatim
       , nominatimLbs
       , nominatimSimple
       ) where

import           Control.Applicative
import           Control.Exception      as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson             as A (FromJSON (..), ToJSON (..),
                                              Value (..), decode, eitherDecode,
                                              parseJSON, (.:), (.:?))
import           Data.ByteString.Lazy   (ByteString)
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as F
import qualified Data.List              as L
import           Data.Maybe
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Client    (HttpException (..))
import           Network.Wreq


-- * Request

data RequestFormat = Json
                   | Xml
                   | Html
                   | JsonV2
                   deriving Show

data RequestParameter
  = Format RequestFormat
    -- ^ format=[html|xml|json|jsonv2]
    --
    --     Output format
  | JsonCallback Text
    -- ^ json_callback=<string>
    --   Wrap json output in a callback function (JSONP) i.e. <string>(<json>)

  | AcceptLanguage Text
    -- ^ accept-language=<browser language string>
    --     Preferred language order for showing search results, overrides the
    --     value specified in the "Accept-Language" HTTP header.
    --     Either uses standard rfc2616 accept-language string or
    --     a simple comma separated list of language codes.

  | Query Text
    -- ^ q=<query>
    --     Query string to search for.  Alternatively can be entered as:


    -- (experimental) Alternative query string format for structured requests.
    -- Structured requests are faster and require less server resources.
    -- DO NOT COMBINE WITH q=<query> PARAMETER.
  | Street Text
    -- ^ street=<housenumber> <streetname>
  | City Text
    -- ^ city=<city>
  | County Text
    -- ^ county=<county>
  | State Text
    -- ^ state=<state>
  | Country Text
    -- ^ country=<country>
  | PostalCode Text
    -- ^ postalcode=<postalcode>

  | CountryCodes [Text]
    -- ^ countrycodes=<countrycode>[,<countrycode>][,<countrycode>]...
    --     Limit search results to a specific country (or a list of countries).
    --     <countrycode> should be the ISO 3166-1alpha2 code,
    --     e.g. gb for the United Kingdom, de for Germany, etc.

  | ViewBox Text Text Text Text
    -- ^ viewbox=<left>,<top>,<right>,<bottom>
    --     or viewboxlbrt=<left>,<bottom>,<right>,<top>
    --     The preferred area to find search results

  | Bounded Bool
    -- ^ bounded=[0|1]
    --     Restrict the results to only items contained with the bounding box.
    --     Restricting the results to the bounding box also enables searching by amenity only.
    --     For example a search query of just "[pub]" would normally be rejected
    --     but with bounded=1 will result in a list of items matching within the bounding box.

  | Polygon Bool
    -- ^ polygon=[0|1]
    --     Output polygon outlines for items found
    --     (deprecated, use one of the polygon_* parameters instead)

  | AddressDetails Bool
    -- ^ addressdetails=[0|1]
    --     Include a breakdown of the address into elements

  | Email Text
    -- ^ email=<valid email address>
    --     If you are making large numbers of request please include a valid email address
    --     or alternatively include your email address as part of the User-Agent string.
    --     This information will be kept confidential and only used to contact you in the
    --     event of a problem, see Usage Policy for more details.

  | ExcludePlaceIds [Text]
    -- ^ exclude_place_ids=<place_id,[place_id],[place_id]>
    --     If you do not want certain openstreetmap objects to appear in the search result,
    --     give a comma separated list of the place_id's you want to skip. This can be used
    --     to broaden search results. For example, if a previous query only returned a few
    --     results, then including those here would cause the search to return other, less
    --     accurate, matches (if possible)

  | Limit Int
    -- ^ limit=<integer>
    --     Limit the number of returned results.

  | Dedupe Bool
    -- ^ dedupe=[0|1]
    --     No explanation yet.

  | Debug Bool
    -- ^ debug=[0|1]
    --     No explanation yet.

  | PolygonOutput OutputGeometry
    -- ^
    --   polygon_geojson=1
    --     Output geometry of results in geojson format.
    --   polygon_kml=1
    --     Output geometry of results in kml format.
    --   polygon_svg=1
    --     Output geometry of results in svg format.
    --   polygon_text=1
    --     Output geometry of results as a WKT.
  deriving Show


data OutputGeometry = PolygonGeoJson
                    | PolygonKml
                    | PolygonSvg
                    | PolygonText
                    deriving Show

type Header = (ByteString,ByteString)


-- * Results

type BoundingBox = [Text]

-- | A Nominatim 'Place'
data Place = Place
    {
      class_       :: Text
    , display_name :: Text
    , importance   :: Double
    , lat          :: Text
    , license      :: Maybe Text
    , lon          :: Text
    , osm_id       :: Text
    , osm_type     :: Text
    , place_id     :: Text
    , type_        :: Text
    , address      :: Maybe Address
      -- ^ Address data only present if the address query
      -- parameter is "1"
    , boundingbox  :: BoundingBox
    } deriving (Show)


instance FromJSON Place where
  parseJSON (Object o) =
    Place <$> o .: "class"
           <*> o .: "display_name"
           <*> o .: "importance"
           <*> o .: "lat"
           <*> o .:? "licence"
           <*> o .: "lon"
           <*> o .: "osm_id"
           <*> o .: "osm_type"
           <*> o .: "place_id"
           <*> o .: "type"
           <*> o .:? "address"
           <*> o .: "boundingbox"
  parseJSON _ = mzero


-- | Address data, generally embedded in a 'Place'
data Address = Address
    {
      city            :: Maybe Text
    , city_district   :: Maybe Text
    , construction    :: Maybe Text
    , continent       :: Maybe Text
    , country         :: Maybe Text
    , country_code    :: Maybe Text
    , house_number    :: Maybe Text
    , neighbourhood   :: Maybe Text
    , postcode        :: Maybe Text
    , public_building :: Maybe Text
    , state           :: Maybe Text
    , suburb          :: Maybe Text
    } deriving (Show)

instance FromJSON Address where
  parseJSON (Object o) =
    Address <$> o .:? "city"
            <*> o .:? "city_district"
            <*> o .:? "construction"
            <*> o .:? "continent"
            <*> o .:? "country"
            <*> o .:? "country_code"
            <*> o .:? "house_number"
            <*> o .:? "neighbourhood"
            <*> o .:? "postcode"
            <*> o .:? "public_building"
            <*> o .:? "state"
            <*> o .:? "suburb"
  parseJSON _ = mzero


createParam :: RequestParameter -> Options -> Options
createParam rp = case rp of
  JsonCallback t     -> param "json_callback"     .~ [t]
  AcceptLanguage t   -> param "accept-language"   .~ [t]
  Query t            -> param "q"                 .~ [t]
  Street t           -> param "street"            .~ [t]
  City t             -> param "city"              .~ [t]
  County t           -> param "county"            .~ [t]
  State t            -> param "state"             .~ [t]
  Country t          -> param "country"           .~ [t]
  PostalCode t       -> param "postalcode"        .~ [t]
  CountryCodes cs    -> param "countrycodes"      .~ (L.intersperse "," cs)
  ViewBox l t r b    -> param "viewbox"           .~ [l <> "," <> t <> "," <> r <> "," <> b ]
  Bounded b          -> param "bounded"           .~ [if b then "1" else "0"]
  AddressDetails b   -> param "addressdetails"    .~ [if b then "1" else "0"]
  Email t            -> param "email"             .~ [t]
  ExcludePlaceIds cs -> param "exclude_place_ids" .~ (L.intersperse "," cs)
  Limit i            -> param "limit"             .~ [T.pack (show i)]
  Dedupe b           -> param "dedupe"            .~ [if b then "1" else "0"]
  Debug b            -> param "debug"             .~ [if b then "1" else "0"]
  PolygonOutput g ->
    case g of
      PolygonGeoJson -> param "polygon_geojson" .~ ["1"]
      PolygonKml     -> param "polygon_kml"     .~ ["1"]
      PolygonSvg     -> param "polygon_svg"     .~ ["1"]
      PolygonText    -> param "polygon_text"    .~ ["1"]
  Format rf ->
    case rf of
      Html   -> param "format" .~ ["html"]
      Json   -> param "format" .~ ["json"]
      JsonV2 -> param "format" .~ ["jsonv2"]
      Xml    -> param "format" .~ ["xml"]


-- TODO use real url-encoding
urlEncodeSimple :: Text -> Text
urlEncodeSimple = T.replace " " "+"


nominatim
  :: (MonadIO m, Foldable t) => [Header] -> t RequestParameter -> m [Place]
nominatim hdrs params = do
  nominatimLbs hdrs params
    >>= return . fromMaybe [] . A.decode


-- | Call nominatim and retrieve the results as a
-- raw bytestring
nominatimLbs
  :: (MonadIO m, Foldable t) => [Header] -> t RequestParameter -> m ByteString
nominatimLbs hdrs params = do
  let opts = F.foldl (\acc p -> acc & createParam p) defaults params
  callNominatim opts


-- | Send a request to nominatim using a simple query string
--
-- No options are passed to the nominatim API and a query
-- is made using the address
nominatimSimple :: MonadIO m => Text -> m ByteString
nominatimSimple address = do
    let opts = defaults & param "format"         .~ ["json"]
                        & param "limit"          .~ ["1"]
                        & param "addressdetails" .~ ["1"]
                        & param "q"              .~ [urlEncodeSimple address]
    callNominatim opts


callNominatim :: MonadIO m => Options -> m ByteString
callNominatim opts = do
    r <- liftIO $ getNominatimWith opts `E.catch` handler
    return $ r ^. responseBody
  where
    getNominatimWith = flip getWith "http://nominatim.openstreetmap.org/search"
    -- TODO handle specific nominatim error cases
    handler e@(StatusCodeException s _ _)
      --- | s ^. statusCode == 400 = error "ill formatted request"
      | otherwise              = throwIO e
