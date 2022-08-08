{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.ByteString qualified as B
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List ( foldl1' )
import Data.Maybe
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml as Yaml
import Data.Yaml.Pretty as Yaml
import Witherable
import GHC.Stack
import Options.Applicative as O
import System.Environment
import System.FilePath

mergeValues :: Value -> Value -> Value
mergeValues (Array  xs) (Array  ys) = Array $ ordNub $ xs <> ys
mergeValues (Object xs) (Object ys) = Object $ KM.unionWith mergeValues xs ys
mergeValues (Array  xs) y           = Array xs `mergeValues` Array (pure y)
mergeValues x           (Array ys)  = Array (pure x) `mergeValues` Array ys
mergeValues x           y           = Array $ ordNub $ pure x <> pure y

data Path = All | Keys (Set.Set T.Text) deriving Show
instance IsString Path where
    fromString = Keys . Set.singleton . fromString

components :: Set.Set T.Text
components = ["tests", "executables", "benchmarks"]

tidyUp :: T.Text
    -> [T.Text]
    -> Value -> Value
tidyUp newName oldNames
    = editMaybe ["default-extensions"] sortExts
    . edit ["library"] perComponent
    . edit [Keys components, All] (addLibDep . perComponent)
    where

    addLibDep = edit ["dependencies"] $ \case
        Array xs -> Array $ V.cons (String newName) xs
        x -> error $ show x

    perComponent = edit ["dependencies"] sortDeps
        . editMaybe ["default-extensions"] sortExts

    sortDeps (Array xs) = Array $ V.filter (\(String x) -> x `notElem` oldNames)
        $ nubVec xs
    sortDeps x = error $ show x

sortExts :: Value -> Maybe Value
sortExts (Array xs) = prune
    $ V.filter
        (\case
            String name -> not $ T.isSuffixOf ".yaml" name
            _ -> False
        )
    $ nubVec xs
    where
        prune v
            | V.null v = Nothing
            | otherwise = Just $ Array v
sortExts (String str) | T.isSuffixOf ".yaml" str = Nothing -- delete import
sortExts x = error $ show x

nubVec :: Ord a => V.Vector a -> V.Vector a
nubVec = V.fromList . Set.toList . Set.fromList . V.toList

edit :: HasCallStack => [Path] -> (Value -> Value) -> Value -> Value
edit path f = editMaybe path (Just . f)

editMaybe :: HasCallStack => [Path] -> (Value -> Maybe Value) -> Value -> Value
editMaybe path0 upd = fromMaybe Null . go path0 where
    go [] v = upd v
    go (All : xs) (Object objs) = Just $ Object $ KM.mapMaybe (go xs) objs
    go (All : xs) (Array objs) = Just $ Array $ V.mapMaybe (go xs) objs
    go (All : _) v = upd v
    go (Keys keys : xs) (Object objs) = Just $ Object $ KM.mapMaybeWithKey
        (\k v -> if Key.toText k `Set.member` keys then go xs v else Just v) objs
    go path val = error $ "unhandled: " <> show path <> ": " <> show val

getPackgageName :: Value -> T.Text
getPackgageName (Object m)
    | Just (String v) <- KM.lookup "name" m = v
getPackgageName _ = error "missing package name"

main = join $ O.execParser $ flip O.info mempty $ do
    thisPackageName <- O.strOption $ O.long "name" <> O.help "name of the merged package"
    version <- O.strOption $ O.long "version" <> O.value "0"
    ghcOptions <- O.strOption $ O.long "ghc-options" <> O.value "-j -Wall -Wcompat"
    let defaultExtensions = [] :: [FilePath]
    files <- O.some $ O.strArgument $ O.metavar "package.yaml"
    pure $ do
        yamls <- traverse Yaml.decodeFileThrow files
        let oldNames = map getPackgageName yamls

        exts <- foldr mergeValues (Array []) <$> traverse Yaml.decodeFileThrow defaultExtensions
        B.putStr
            $ Yaml.encodePretty Yaml.defConfig
            $ addGhcOptions ghcOptions
            $ edit ["name"] (const $ String thisPackageName)
            $ edit ["version"] (const $ String version)
            $ tidyUp thisPackageName oldNames
            $ edit ["default-extensions"] (mergeValues exts)
            $ foldl1' mergeValues
            $ zipWith3 preprocess oldNames files yamls

addGhcOptions :: T.Text -> Value -> Value
addGhcOptions opts (Object obj) = Object $ KM.insert "ghc-options" (String opts) obj

preprocess :: T.Text -> FilePath -> Value -> Value
preprocess oldName path
    = edit ["library", "source-dirs", All] prefixPath
    . edit [Keys components, All, "source-dirs", All] prefixPath
    . edit ["extra-source-files", All] prefixPath
    . edit ["data-files", All] prefixPath
    . edit ["tests"] renameTests
    where
        renameTests = \case
            Object obj -> Object $ KM.fromList $ map (\(k, v) -> (Key.fromText $ oldName <> "-" <> Key.toText k, v)) $ KM.toList obj
            x -> error $ show x

        prefixPath = \case
            String p -> String $ T.pack (takeDirectory path </> T.unpack p)
            v -> error $ show v
