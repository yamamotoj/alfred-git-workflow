module Alfred
    where

import Text.XML.Light
import System.Environment(getEnv)

getHome::IO String
getHome = getEnv "HOME"

getCachePath:: IO String
getCachePath = do
  home <- getHome
  bundleId <- getBundleId
  return (home  ++ "/Library/Caches/com.runningwithcrayons.Alfred-2/Workflow Data/" ++ bundleId)

getDataPath::IO String
getDataPath = do
  home <- getHome
  bundleId <- getBundleId
  return (home  ++ "/Library/Application Support/Alfred 2/Workflow Data/" ++ bundleId)

getBundleId::IO String
getBundleId = do
  xml <- readInfo
  let dict:es = findChildren (QName "dict" Nothing Nothing) xml
  let (Just e) =  findDict "bundleid" (onlyElems $elContent dict)
  case elContent e of
    [] -> return ""
    [Text content] -> return (cdData content)
    where findDict str [] = Nothing
          findDict str (k:v:xs) = let [Text content] = elContent k in
                                  if str == (cdData content) then Just v
                                  else findDict str xs
          readInfo = do
                     xml <- readFile "info.plist"
                     case parseXMLDoc xml of
                       Nothing -> error "parse error"
                       Just root -> return root
            

data SubTitle = Normal String
              | Shift String
              | Fn String
              | Ctrl String
              | Alt String
              | Cmd String


data Icon = CommonIcon String
          | FileIcon String
              

data Feedback = Feedback { uid :: String -- the uid of the result, should be unique
                         , arg :: String -- the argument that will be passed on
                         , valid :: Bool --sets whether the result item can be actioned
                         , autocomplete :: String -- the autocomplete value for the result item
                         , title :: String -- The title of the result item
                         , subtitles :: [SubTitle] -- The subtitle text for the result item
                         , icon :: Maybe Icon -- the icon to use for the result item
                         }

simpleFeedback :: String -> Feedback
simpleFeedback str = Feedback str str True str str [] Nothing

simpleFeedback2 :: String -> String -> Feedback
simpleFeedback2 title sub = Feedback title title True title title [Normal sub] Nothing

messageFeedback :: String -> String -> Feedback
messageFeedback title sub = Feedback title title False [] title [Normal sub] Nothing
                   
feedbackElement :: Feedback -> Element
feedbackElement (Feedback uid arg valid ac title subs icon) = node (unqual "item") (attrs, elems)
    where attrs = [ Attr (unqual "uid") uid
                  , Attr (unqual "arg") arg
                  , Attr (unqual "valid") $if valid then "YES" else "NO"
                  , Attr (unqual "autocomplete") ac]
          elems = case icon of
                    Nothing -> t:ss
                    Just icon -> (iconElement icon):t:ss
              where ss = map subTitleElement subs
                    t = node (unqual "title") $stringContent title

          iconElement (CommonIcon str)  = node (unqual "icon") (stringContent str)
          iconElement (FileIcon str) = node (unqual "icon") (Attr (unqual "type") "fileicon",  str)

          subTitleElement (Normal str) = node (unqual "subtitle") (stringContent str)
          subTitleElement st = node (unqual "subtitle") $nodeArg st
              where nodeArg (Shift str) = (Attr (unqual "mod") "shift" ,str)
                    nodeArg (Fn str) = (Attr (unqual "mod") "fn" ,str)
                    nodeArg (Ctrl str) = (Attr (unqual "mod") "ctrl" ,str)
                    nodeArg (Alt str) = (Attr (unqual "mod") "alt" ,str)
                    nodeArg (Cmd str) = (Attr (unqual "mod") "cmd" ,str)

          stringContent ::String -> Content
          stringContent str = Text CData{ cdVerbatim = CDataText
                                        , cdData = str
                                        , cdLine = Nothing
                                        }

putFeedbacks::[Feedback] -> IO()
putFeedbacks fs = putStrLn $toXML fs
    where toXML fs = showTopElement $node (unqual "items") $map feedbackElement fs
                                   
