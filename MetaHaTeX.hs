
import Prelude hiding (lookup)
import Language.Haskell.Exts hiding (ModuleName)
import qualified Language.Haskell.Exts as LH
import Documentation.Haddock hiding (Decl)
import Control.Monad.State
import Data.Maybe
import Data.List (find,intercalate)
import qualified Name as GHC
import qualified Outputable as GHC
import qualified Module as GHC
import Text.Parsec
import Text.Parsec.String
import Data.Map hiding (filter)
import System.FilePath (dropFileName)
import System.Directory (createDirectoryIfMissing)
import Data.Char (isAlpha)
import System.IO (hFlush,stdout)
import Control.Monad (filterM)
-- Cabal
import Distribution.PackageDescription
        (exposedModules,condTreeData,condLibrary)
import Distribution.PackageDescription.Parse
        (readPackageDescription)
import Distribution.Verbosity (silent)
import Distribution.ModuleName (components)

-- | Module names are written with 'String's.
type ModuleName = String

-- | The name of a monad version of one module is the
--   the name of that module followed by @.Monad@.
monadMName :: ModuleName -- Original module name
           -> ModuleName -- New module name
monadMName mn = mn ++ ".Monad"

-- | All exposed modules of HaTeX.
allExposed :: IO [ModuleName]
allExposed =
 fmap
  (fmap (intercalate "." . components)
   . exposedModules . condTreeData . fromJust . condLibrary)
   $ readPackageDescription silent "HaTeX.cabal"

-- | The list of modules to be transformed by HaTeX-meta.
moduleList :: IO [ModuleName]
moduleList =
 allExposed >>=
  filterM
   (fmap (hatexPragma . fromParseResult) . parseFileS . mNameToFilePath)

parseFileS :: FilePath -> IO (ParseResult Module)
parseFileS fp = parseFile fp `catch` \e -> do
 let name = filePathToMName fp
 putStrLn $ "HaTeX-meta: The module " ++ name ++ " does not exist."
 return $ ParseOk $ Module loc0 (LH.ModuleName name) [] Nothing Nothing [] []

-- | Check if a module has the HaTeX /MakeMonadic/ option:
--
-- > {-# OPTIONS_HATEX MakeMonadic #-}
hatexPragma :: Module -> Bool
hatexPragma (Module _ _ pragmas _ _ _ _) = any isMonadicPragma pragmas

isMonadicPragma :: ModulePragma -> Bool
isMonadicPragma (OptionsPragma _ (Just (UnknownTool "HATEX")) str) =
  takeWhile isAlpha str == "MakeMonadic"
isMonadicPragma _ = False

-- | Get the 'FilePath' of a 'ModuleName'.
mNameToFilePath :: ModuleName -> FilePath
mNameToFilePath = (++ ".hs") . fmap (\c -> if c == '.' then '/' else c)

filePathToMName :: FilePath -> ModuleName
filePathToMName = fmap (\c -> if c == '/' then '.' else c) . reverse . drop 3 . reverse

nameString :: Name -> String
nameString = prettyPrint

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

unlines2 :: [String] -> String
unlines2 = intercalate "\n\n"

ghcName :: GHC.Name -> String
ghcName = GHC.occNameString . GHC.nameOccName

loc0 :: SrcLoc
loc0 = SrcLoc [] 0 0

symbolParens :: String -> String
symbolParens [] = []
symbolParens str@(x:_) =
 if isAlpha x then str
              else "(" ++ str ++ ")"

--------------

data FunDoc = FD
 { funName    :: String
 , funDoc     :: Maybe (Doc GHC.Name)
 , funArgsDoc :: FnArgsDoc GHC.Name
   }

data HSInfo = HSI
 { hsModuleName :: ModuleName
 , hsPragma     :: String
 , hsModuleInfo :: Maybe (Doc GHC.Name)
 , hsExportList :: String
 , hsTypeNames  :: [String]
 , hsFunInfo    :: [(FunDoc,Type)]
   }

getHSInfo :: ModuleName -> Interface -> IO HSInfo
getHSInfo mn i = do
 let fp = mNameToFilePath mn
 -- Parsing module
 pr <- parseFile fp
 (Module _ _ pragmas _ _ imports decls)
   <- case pr of
       ParseOk x -> return x
       ParseFailed loc err -> fail $ show loc ++ ":" ++ err
 -- Getting export list
 let exportListParser :: Parser String
     exportListParser = do
       manyTill anyChar (try $ string $ "module " ++ mn)
       str <- manyTill anyChar (try $ string $ "where")
       return $ "module " ++ monadMName mn ++ str ++ "where"
     getExportList :: IO String
     getExportList = do
       pr <- parseFromFile exportListParser fp
       case pr of
         Left err -> fail $ "getExportList: parse error: " ++ show err
         Right str -> return str
 expList <- getExportList
 -- Exported names
 let exportedNames = fmap (symbolParens . ghcName) $ ifaceExports i
 -- Getting type names
 let getTypeName :: Decl -> Maybe String
     getTypeName (TypeDecl _ n _ _) = Just $ nameString n
     getTypeName (DataDecl _ _ _ n _ _ _) = Just $ nameString n
     getTypeName _ = Nothing
 -- Getting functions info
 let getFunInfo :: Decl -> [(FunDoc,Type)]
     getFunInfo (TypeSig _ ns_ t) =
      let ns = filter (`elem` exportedNames) $ fmap nameString ns_
          funmap = ifaceDeclMap i
          strfunmap = mapKeys ghcName funmap
      in  fmap (\n -> let dinfo = lookup n strfunmap
                          (fd,fad) = maybe (Nothing,Data.Map.empty) snd3 dinfo
                      in ( FD n fd fad , t )) ns
     getFunInfo _ = []
 -- Final result
 return $
  HSI { hsModuleName = mn
      , hsPragma     = unlines $ fmap prettyPrint
                               $ filter (not . isMonadicPragma) $ pragmas
      , hsModuleInfo = ifaceDoc i
      , hsExportList = expList
      , hsTypeNames  = catMaybes $ fmap getTypeName decls
      , hsFunInfo    = concat $ fmap getFunInfo decls
       }

-- Rendering Doc

renderDoc :: Doc GHC.Name -> String
renderDoc DocEmpty = []
renderDoc (DocAppend d1 d2) = renderDoc d1 ++ renderDoc d2
renderDoc (DocString str) = str
renderDoc (DocParagraph d) = renderDoc d
renderDoc (DocIdentifier xs) = "'" ++ ghcName (last xs) ++ "'"
renderDoc (DocModule str) = "\"" ++ str ++ "\""
renderDoc (DocEmphasis d) = "/" ++ renderDoc d ++ "/"
renderDoc (DocMonospaced d) = "@" ++ renderDoc d ++ "@"
renderDoc (DocUnorderedList xs) = unlines2 $ fmap renderDoc xs
renderDoc (DocOrderedList xs) = unlines2
                                  $ zipWith (++) (fmap (\n -> show n ++ ". ") [1 .. ])
                                  $ fmap renderDoc xs
renderDoc (DocCodeBlock d) = "\n> " ++ renderDoc d ++ "\n"
renderDoc (DocURL str) = "<" ++ str ++ ">"

docCommenting :: String -> String
docCommenting str =
 let lns = length $ lines str
 in  if lns > 1 then "{-|\n" ++ str ++ "-}"
                else "-- | " ++ str

docString :: Doc GHC.Name -> String
docString = docCommenting . renderDoc

-- Type synonyms

typeSynonym :: String -> String
typeSynonym t = unwords [ "type" , t , "=" , "App." ++ t ]

-- Monadic functions

namesInType :: Type -> [String]
namesInType (TyForall _ _ t) = namesInType t
namesInType (TyFun t1 t2) = namesInType t1 ++ namesInType t2
namesInType (TyTuple _ ts) = concat $ fmap namesInType ts
namesInType (TyList t) = namesInType t
namesInType (TyApp t1 t2) = namesInType t1 ++ namesInType t2
namesInType (TyVar _) = []
namesInType (TyCon qn) = [prettyPrint qn]
namesInType (TyParen t) = namesInType t

latexType :: Type -> Type
latexType (TyForall m c t) = TyForall m c $ latexType t
latexType (TyFun t1 t2) = latexType t1 `TyFun` latexType t2
latexType (TyTuple b ts) = TyTuple b $ fmap latexType ts
latexType (TyList t) = TyList $ latexType t
latexType (TyApp t1 t2) = TyApp (latexType t1) (latexType t2)
latexType (TyVar v) = TyVar v
latexType (TyCon (UnQual (Ident "LaTeX"))) = TyApp (TyCon $ UnQual $ Ident "LaTeXT_")
                                                   (TyVar $ Ident "m")
latexType (TyCon qn) = TyCon qn
latexType (TyParen t) = TyParen $ latexType t

argNumber :: Type -> Int
argNumber (TyForall _ _ t) = argNumber t
argNumber (TyFun t1 t2) = argNumber t1 + argNumber t2
argNumber _ = 1

typeList :: Type -> [Type]
typeList (TyForall _ _ t) = typeList t
typeList (TyFun t1 t2) = typeList t1 ++ typeList t2
typeList t = [t]

doStatement :: Pat -> Type -> Maybe Stmt
doStatement v@(PVar n) (TyCon (UnQual (Ident "LaTeX"))) = Just $
  Generator loc0 v $ App (Var $ UnQual $ Ident "extractLaTeX_")
                   $ Var $ UnQual n
doStatement v@(PVar n) (TyList (TyCon (UnQual (Ident "LaTeX")))) = Just $
 Generator loc0 v $ foldl1 App
       [ Var $ UnQual $ Ident "mapM"
       , Var $ UnQual $ Ident "extractLaTeX_"
       , Var $ UnQual n ]
doStatement v@(PVar n) (TyApp (TyCon (UnQual (Ident "Maybe")))
                              (TyCon (UnQual (Ident "LaTeX")))) = Just $
 Generator loc0 v $ foldl1 App
   [ Var $ UnQual $ Ident "maybe"
   , App (Var $ UnQual $ Ident "return") (Con $ UnQual $ Ident "Nothing")
   , InfixApp (RightSection (QVarOp $ UnQual $ Symbol ">>=")
                            (InfixApp (Var $ UnQual $ Ident "return")
                                      (QVarOp $ UnQual $ Symbol ".")
                                      (Con $ UnQual $ Ident "Just") ) )
              (QVarOp $ UnQual $ Symbol ".")
              (Var $ UnQual $ Ident "extractLaTeX_")
   , Var $ UnQual n
     ]
doStatement _ _ = Nothing

qualify :: String -> String
qualify [] = []
qualify str@(x:_) = if isAlpha x then "App." ++ str
                                 else "(App." ++ (tail $ init $ str) ++ ")"

monadicFunction :: (FunDoc,Type) -> String
monadicFunction (fd,t) =
 let n :: String
     n = funName fd
     varsn :: [Name]
     varsn = fmap (Ident . ('a':) . show) [1 .. argNumber t - 1]
     vars :: [Pat]
     vars  = fmap PVar varsn
     varse :: [Exp]
     varse = fmap (Var . UnQual) varsn
     lastStmt = Qualifier $ App (Var $ UnQual $ Ident "textell")
                          $ foldl App (Var $ UnQual $ Ident [])
                          $ (Var $ UnQual $ Ident $ qualify n) : varse
 in
 if elem "LaTeX" (namesInType t)
    then unlines [ maybe [] docString $ funDoc fd
                 , unwords [n , "::" , prettyPrint
                                        $ TyForall Nothing
                                                   [ClassA (UnQual $ Ident "Monad")
                                                     [TyVar $ Ident "m"]
                                                       ]
                                        $ latexType t]
                 , prettyPrint
                   $ FunBind [Match loc0
                                  (Ident n)
                                  vars
                                  Nothing
                                  (UnGuardedRhs $ Do $ (++[lastStmt]) $ catMaybes
                                    $ zipWith doStatement vars $ init $ typeList t )
                                  (BDecls []) ]
                   ]
    else unlines [ maybe [] docString $ funDoc fd
                 , unwords [n , "::" , prettyPrint t]
                 , unwords [n , "=" , qualify n] ]

-- Import list string

importString :: HSInfo -> String
importString hsi =
 let mn = hsModuleName hsi
 in
     unlines [
   "import Text.LaTeX.Base.Writer"
 , "import Text.LaTeX.Base.Render"
 , "import Text.LaTeX.Base.Types"
 , "import qualified " ++ mn ++ " as App"
 , "import " ++ mn ++ "(" ++ intercalate "," (hsTypeNames hsi) ++ ")"
   ]

-- HaTeX-meta warning

hatexmetawarn :: ModuleName -> String
hatexmetawarn mn = unlines $ fmap ("-- "++) $ [
   ""
 , "/For contributors: This module was automatically generated by HaTeX-meta./"
 , "/So, please, don't make any change here directly, because/"
 , "/this is intended to be generated from/"
 , "\"" ++ mn ++ "\" /module via HaTeX-meta,/"
 , "/and therefore, changes must to be done in these places./"
   ]

-- Building the monad module

buildMonadModule :: HSInfo -> String
buildMonadModule hsi = unlines [
   hsPragma hsi
 , concat [ maybe "-- |\n" docString $ hsModuleInfo hsi
          , hatexmetawarn $ hsModuleName hsi ]
 , hsExportList hsi ++ "\n"
 , importString hsi
 , unlines $ fmap monadicFunction $ hsFunInfo hsi
   ]

-- Main function

main :: IO ()
main = do
 mlist <- moduleList
 is <- createInterfaces [] mlist
 mapM_ (\m -> do putStr $ "HaTeX-meta: Processing " ++ m ++ "... "
                 hFlush stdout
                 let fp  = mNameToFilePath m
                     fp' = mNameToFilePath $ monadMName m
                     i = fromJust $ find (\x -> (GHC.moduleNameString
                                                   $ GHC.moduleName
                                                   $ ifaceMod x) == m) is
                 createDirectoryIfMissing True $ dropFileName fp'
                 getHSInfo m i >>= writeFile fp' . buildMonadModule
                 putStrLn "Done."
                  ) mlist
