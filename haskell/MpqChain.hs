import Data.List (sort)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Text.Regex.Posix ((=~))
import Text.Groom (groom)

-- TODO: caseless
patchPattern = "/base/d3-update-base-(.*).[mM][pP][qQ]"

getRecursiveContents :: FilePath -> IO [ FilePath ]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)

findPatches :: FilePath -> IO [ (Int, FilePath) ]
findPatches path = do
    names <- getRecursiveContents path
    return . sort $ foldl findPatch [] names
        where findPatch patches name = case name =~ patchPattern of
                  [ [ wholeName, patchName ] ] -> (read patchName :: Int, wholeName) : patches
                  _ -> patches

main :: IO ()
main = do
    mpqs <- findPatches "../mpq"
    putStrLn . groom $ mpqs
