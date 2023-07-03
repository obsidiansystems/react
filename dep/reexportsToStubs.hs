{-# LANGUAGE RecordWildCards #-}

import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription
import Distribution.Types.Library
import Distribution.Types.ModuleReexport
import Distribution.Verbosity
import Distribution.ModuleName (components)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), joinPath, takeDirectory)
import Data.List.Split (splitOn)
import Data.List (intercalate)

-- Helper function to convert a module name to a file path
convertModuleToPath :: String -> FilePath
convertModuleToPath moduleName = "src" </> joinPath (splitOn "." moduleName) <.> "hs"

-- Function to write a file with the given module structure
writeModuleFile :: (String, String) -> IO ()
writeModuleFile (originalModule, newModule) = do
    let content = unlines
            [ "module " ++ newModule ++ " (module X) where"
            , ""
            , "import " ++ originalModule ++ " as X"
            ]
        filePath = convertModuleToPath newModule
    createDirectoryIfMissing True (takeDirectory filePath)
    writeFile filePath content

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            result <- parseCabalFile fileName
            case result of
                Left err -> putStrLn $ "Error: " ++ err
                Right reexports -> mapM_ writeModuleFile reexports
        _ -> putStrLn "Usage: cabal-file-parser <filename.cabal>"

parseCabalFile :: FilePath -> IO (Either String [(String, String)])
parseCabalFile filePath = do
    result <- readGenericPackageDescription silent filePath
    let maybeMainLib = case condLibrary result of
            Just (CondNode lib _ _) -> Just lib
            Nothing -> Nothing
        subLibs = [lib | (_, CondNode lib _ _) <- condSubLibraries result]
        libs = maybe subLibs (:subLibs) maybeMainLib
    return $ case libs of
        (lib:_) -> Right (extractReexportedModules lib)
        [] -> Left "No library found in .cabal file."

extractReexportedModules :: Library -> [(String, String)]
extractReexportedModules lib =
    map (\ModuleReexport{..} -> (moduleNameString moduleReexportOriginalName, moduleNameString moduleReexportName)) (reexportedModules lib)

moduleNameString = intercalate "." . components
