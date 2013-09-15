{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module JavaScript.JQuery.UI.TH where

import Control.Applicative        hiding (many, optional)
import Control.Monad
import Data.Char
import qualified Data.Text as T
import Data.Monoid                (mconcat)
import Data.Default
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


-- | Create a field name based on the type.
-- > mkFieldName ''Expr "id" = "exprId"
mkFieldName :: Name    -- ^ Widget name
            -> String  -- ^ Field
            -> Name
mkFieldName widgetName s =
    mkName $ map toLower widgetNameBase ++ capitalize s
  where widgetNameBase = nameBase widgetName

-- | Create a singleton instance based on the name.
--        
-- > mkSingletonDec (mkName Button)
--        
-- @
-- data Button = Button
-- @
mkSingletonDec :: Name -> Dec
mkSingletonDec name = DataD [] name []
                        [ NormalC name [] ]
                        []

-- | Make a simple show instance
--
-- > mkSimplShowInst ''Thunk "<thunk>"
--
-- @
-- instance Show Thunk where
--     show _ = "<thunk>"
-- @
-- 
mkSimplShowInst :: Name -> String -> Dec
mkSimplShowInst nm str = 
    InstanceD [] (AppT (ConT ''Show) (ConT nm))
        [ FunD 'show
          [ Clause [ WildP ]
            (NormalB
              (LitE
                 (StringL str)))
             [] ]]

-- | Make a default instance
--
-- > mkDefaultInst ''Expr [|Nil|]
--
-- @
-- instance Default Expr where
--     def = Nil
-- @          
mkDefaultInst :: Type -> Exp -> Dec
mkDefaultInst ty defF =           
    InstanceD [] (AppT (ConT ''Default) ty)
        [ FunD 'def
          [ Clause [ ]
            (NormalB defF)
            [] ]]

-- | Convert String to a Name, extract type and expression
extractOpts :: Name -> [(String, Q Type, Q Exp)] -> Q [(Name, Type, Exp)]
extractOpts nm = mapM $ \(s,ty,e) -> do
    let s' = mkFieldName nm s
    ty' <- ty
    e'  <- e
    return (s', ty', e')

-- | Construct a list of defaults from a fresh variable, widget name,
-- and the options list    
defOptsList :: Name -> Name -> [(String, Q Type, a)] -> Q [Exp]
defOptsList var widgetName opts = do
    mapM (\(nm, ty, _) -> do
        let nm' = T.pack nm
        let op = mkFieldName widgetName nm
        let e = appE (varE op) (varE var)
        [|nm' ^= ($e :: $ty)|]) opts
    
mkWidget :: Name -> [(String, Q Type, Q Exp)] -> Q [Dec]
mkWidget widgetName opts = do
    -- names
    let widgetNameBase = nameBase widgetName
    let widgetOptsName = mkName $ widgetNameBase ++ "Opts"
    -- declaration
    let wDecl = mkSingletonDec widgetName
    -- show & default instances
    let showInst = mkSimplShowInst widgetName (map toLower widgetNameBase)
    opts' <- extractOpts widgetName opts
    let defOpts = RecConE widgetOptsName
                    (map (\(nm,_,e) -> (nm, e)) opts')
    let widgetOptsTy = AppT (ConT ''WidgetOpts) (ConT widgetName)
    let defInst = mkDefaultInst widgetOptsTy defOpts
    -- widget instances
    let optsDecl = map (\(nm, ty, _) -> (nm, IsStrict, ty)) opts'
    let wOpts = DataInstD [] ''WidgetOpts [ ConT widgetName ]
                      [ RecC widgetOptsName optsDecl ]
                      []

    optsVar <- newName "opts" 
    objList <- defOptsList optsVar widgetName opts
    let optsObjInst = FunD 'widgetOptsObj [ Clause [ VarP optsVar ] 
                          (NormalB
                            (AppE (VarE 'obj)
                                  (ListE objList)))
                          [] ]

    let widgetInst = InstanceD [] (AppT (ConT ''Widget) 
                                   (ConT widgetName))
                         [ wOpts, optsObjInst ]

    return [ showInst , defInst, widgetInst ]


capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x:xs

