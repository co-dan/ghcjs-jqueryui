{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module JavaScript.JQuery.UI.TH where

import Control.Applicative        hiding (many, optional)
import Control.Monad
import Data.Char
import qualified Data.Text as T
import Data.Monoid                (mconcat)
import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Instances

import Text.Parsec                hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.String

import JavaScript.JQuery.UI.Class

-- | Terrible helper operators
(<::>) :: String -> Q Type -> Q Exp -> (String, Q Type, Q Exp)
a <::> b = \c -> (a, b, c)

(<==>) :: (Q Exp -> (String, Q Type, Q Exp)) -> Q Exp -> (String, Q Type, Q Exp)
f <==> c = f c

instance Lift T.Text where
    lift t = litE (stringL (T.unpack t))


mkFieldName :: Name    -- ^ Widget name
            -> String  -- ^ Field
            -> Name
mkFieldName widgetName s =
    mkName $ map toLower widgetNameBase ++ capitalize s
  where widgetNameBase = nameBase widgetName

-- | Main entry point
mkWidget :: Name -> [(String, Q Type, Q Exp)] -> Q [Dec]
mkWidget widgetName opts = do
    let widgetNameBase = nameBase widgetName
    let wDecl = DataD [] widgetName []
                    [ NormalC widgetName [] ]
                    []
    let showInst = InstanceD [] (AppT (ConT ''Show) (ConT widgetName))
                       [ FunD 'show
                             [ Clause [ WildP ]
                                   (NormalB
                                    (LitE
                                     (StringL (map toLower widgetNameBase))))
                                 [] ]]

    opts' <- forM opts $ \(s,ty,e) -> do
        let s' = mkFieldName widgetName s
        ty' <- ty
        e'  <- e
        return (s', ty', e')
    let optsDecl = map (\(nm, ty, _) -> (nm, IsStrict, ty)) opts'
    let widgetOptsName = mkName $ widgetNameBase ++ "Opts"
    let wOpts = DataInstD [] ''WidgetOpts [ ConT widgetName ]
                      [ RecC widgetOptsName optsDecl ]
                      []

    let defWopts = FunD 'defOpts [ Clause []
                      (NormalB (RecConE widgetOptsName
                                 (map (\(nm,_,e) -> (nm, e)) opts')))
                      [] ]

    optsVar <- newName "opts" 
    objList <- mapM (\(nm, ty, _) -> do
                 let nm' = T.pack nm
                 let op = mkFieldName widgetName nm
                 let e = appE (varE op) (varE optsVar)
                 [|nm' ^= ($e :: $ty)|]) opts
    let optsObjInst = FunD 'optsObj [ Clause [ VarP optsVar ] 
                          (NormalB
                            (AppE (VarE 'obj)
                                  (ListE objList)))
                          [] ]

    let widgetInst = InstanceD [] (AppT (ConT ''Widget) --(mkName "Widget"))
                                   (ConT widgetName))
                         [ wOpts, defWopts, optsObjInst ]

    return [ showInst , widgetInst ]

-- widgetParser = do
--     widgetStr <- capitalize <$> many1 letter
--     let widgetName = mkName widgetStr
--         widgetOptsName = mkName (widgetStr ++ "Opts")
--     opts <- many $ do
--         (,,) <$> (spaces *> many1 letter <?> "Option name")
--              <*> (spaces *> many1 (satisfy (not . isSpace)) <?> "Option type")
--              <*> (spaces *> many1 (satisfy (not . isSpace)) <?>
--                   "Default value") <* spaces
--     let optsDecl = map (\(nm, ty, _) ->
--                          (mkName nm, NotStrict, ConT (mkName ty)))
--                        opts
--     traceM $ show opts
--     let wDecl = DataD [] widgetName []
--                     [ NormalC widgetName [] ]
--                     []
--     let wOpts = DataD [] widgetOptsName [  ]
--                     [ RecC widgetOptsName optsDecl ]
--                     []
--     return $ return [ wDecl , wOpts]


-- pp :: Parser a -> String -> a
-- pp p s = either (error.show) id (parse p "" s)

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x:xs

