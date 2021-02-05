
module Frontend.Component.Main (
  component
) where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Data.Array (any)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (wrap)
import Data.String (stripSuffix, toLower)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Simple.Ajax (AjaxError, getR)

import Common.Data.Base64Url as B64
import Common.Wire.GetTorrentResponse as W

--

type State = {
  magnetUri :: String,
  files :: Array W.TorrentFile,
  stage :: Stage
}

data Stage
  = Ready
  | Loading Progress
  | Loaded
  | Playing W.TorrentFile
  | Error String

data Progress
  = Downloading Number
  | Preparing

--

data Action
  = SetMagnetUri String
  | Load
  | Play W.TorrentFile
  | SetStage Stage
  | SetError String

--

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component = H.mkComponent {
  initialState: const { magnetUri: "", files: [], stage: Ready },
  render,
  eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
}

--

render :: forall m. State -> H.ComponentHTML Action () m
render { magnetUri, stage: Ready } =
  renderInput magnetUri
render { stage: Loading progress } =
  renderLoading progress
render { files, stage: Loaded } =
  renderLoaded files
render { stage: Playing file } =
  renderPlaying file
render { stage: Error err } =
  renderError err

renderInput :: forall m. String -> H.ComponentHTML Action () m
renderInput magnetUri = renderWrapper Nothing $
  HH.div [HP.class_ (wrap "input-group")] [
    HH.input [
      HP.id_ "magnet-uri",
      HP.class_ (wrap "form-control"),
      HP.type_ HP.InputText,
      HP.placeholder "Magnet URI",
      HP.value magnetUri,
      HE.onValueInput (pure <<< SetMagnetUri)
    ],
    HH.div [HP.class_ (wrap "input-group-append")] [
      HH.button
        [
          HP.id_ "download",
          HP.classes $ wrap <$> ["btn", "btn-primary"],
          HP.disabled (magnetUri == ""),
          HE.onClick (pure <<< const Load)
        ]
        [HH.text "Download"]
    ]
  ]

renderLoading :: forall m. Progress -> H.ComponentHTML Action () m
renderLoading (Downloading amt) = renderWrapper Nothing $
  HH.div [HP.class_ (wrap "progress")] [
    HH.div
      [
        HP.classes (wrap <$> ["progress-bar", "progress-bar-striped"]),
        HP.prop (wrap "style") ("width: " <> prog <> "%"),
        HPA.role "progressbar",
        HPA.valueNow prog,
        HPA.valueMin "0",
        HPA.valueMax "100"
      ]
      []
  ]
  where
  prog = (show <<< floor) (amt * 100.0)
renderLoading Preparing = renderWrapper Nothing $
  HH.div [HP.class_ (wrap "progress")] [
    HH.div
      [
        HP.classes (wrap <$> ["progress-bar", "progress-bar-striped", "progress-bar-animated"]),
        HP.prop (wrap "style") ("width: 100%"),
        HPA.role "progressbar",
        HPA.valueNow "100",
        HPA.valueMin "0",
        HPA.valueMax "100"
      ]
      []
  ]

renderLoaded :: forall m. Array W.TorrentFile -> H.ComponentHTML Action () m
renderLoaded files = renderWrapper (pure header) content
  where
  header =
    HH.button
      [
        HP.classes (wrap <$> ["btn", "btn-link", "mb-3"]),
        HE.onClick (pure <<< const (SetStage Ready))
      ]
      [HH.text "Back"]
  content =
    HH.ul [HP.class_ (wrap "list-group")] (renderFile <$> files)
  renderFile { fileName, url } =
    HH.li [HP.class_ (wrap "list-group-item")] $ [
      HH.div [HP.class_ (wrap "row")] [
        HH.div [HP.class_ (wrap "col-10")] [
          HH.b [] [HH.text fileName]
        ],
        HH.div [HP.class_ (wrap "col-2")] [
          HH.a
            [
              HP.class_ (wrap "mr-1"),
              HP.target "_blank",
              HP.href url
            ]
            [HH.span [HP.classes (wrap <$> ["lnr", "lnr-download", "mr-1"])] []],
          case isVideoFile fileName of
            false ->
              HH.text ""
            true ->
              HH.button
                [
                  HP.classes (wrap <$> ["btn", "btn-link"]),
                  HE.onClick (pure <<< const (Play { fileName, url }))
                ]
                [
                  HH.span [HP.classes (wrap <$> ["lnr", "lnr-film-play", "mr-1"])] []
                ]
        ]
      ]
    ]

isVideoFile :: String -> Boolean
isVideoFile fileName = any identity (map (flip hasSuffix fileName) videoSuffixes)

videoSuffixes :: Array String
videoSuffixes = [
  ".mp4",
  ".mpg",
  ".mov",
  ".flv",
  ".avi",
  ".mkv"
]

hasSuffix :: String -> String -> Boolean
hasSuffix suffix str = isJust $ stripSuffix (wrap $ toLower suffix) (toLower str)

renderPlaying :: forall m. W.TorrentFile -> H.ComponentHTML Action () m
renderPlaying { url } = renderWrapper (pure header) content
  where
  header =
    HH.button
      [
        HP.classes (wrap <$> ["btn", "btn-link", "mb-3"]),
        HE.onClick (pure <<< const (SetStage Loaded))
      ]
      [HH.text "Back"]
  content =
    HH.div [HP.class_ (wrap "video-container")] [
      HH.div [HP.class_ (wrap "video-content")] [
        HH.video
          [
            HP.id_ "video-player",
            HP.classes (wrap <$> ["video-js", "w-100", "h-100"]),
            HP.prop (wrap "controls") "true",
            HP.prop (wrap "preload") "auto",
            HP.prop (wrap "data-setup") "{}"
          ]
          [
            HH.source [HP.src url]
          ]
      ]
    ]

renderError :: forall m. String -> H.ComponentHTML Action () m
renderError err = renderWrapper Nothing $
  HH.h1 [HP.class_ (wrap "text-danger")] [HH.text err]

renderWrapper :: forall m. Maybe (H.ComponentHTML Action () m) -> (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
renderWrapper header child =
  HH.div [HP.classes $ wrap <$> ["row", "min-vh-100"]] [
    HH.div [HP.classes $ wrap <$> ["col-10", "col-md-6", "offset-1", "offset-md-3", "my-auto"]] [
      fromMaybe (HH.text "") header,
      HH.div [HP.class_ (wrap "card")] [
        HH.div [HP.class_ (wrap "card-body")] [
          child
        ]
      ]
    ]
  ]

--

handleAction ∷ forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction a = case a of
  SetMagnetUri uri ->
    H.modify_ (_ { magnetUri = uri })
  Load -> do
    { magnetUri } <- H.get
    H.modify_ (_ { stage = Loading (Downloading 0.0) })
    requestTorrent magnetUri
  Play file ->
    H.modify_ (_ { stage = Playing file })
  SetStage s ->
    H.modify_ (_ { stage = s })
  SetError err ->
    H.modify_ (_ { stage = Error err })

--

requestTorrent :: forall o m. MonadAff m => String -> H.HalogenM State Action () o m Unit
requestTorrent magnetUri = do
  let opts = { headers: [Accept applicationJSON, ContentType applicationJSON] }
  base64 <- B64.encode magnetUri
  let path = "/api/torrent/" <> base64
  res <- (liftAff (getR opts path) :: H.HalogenM State Action () o m (Either AjaxError W.GetTorrentResponse))
  case res of
    Left err ->
      H.modify_ (_ { stage = Error (show err) })
    Right (W.Downloading { progress }) -> do
      let p = if progress == 1.0 then Preparing else (Downloading progress)
      H.modify_ (_ { stage = Loading p })
      (liftAff <<< delay) (wrap 1000.0)
      requestTorrent magnetUri
    Right (W.Available { files: fx }) ->
      H.modify_ (_ { files = fx, stage = Loaded } )
