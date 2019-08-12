{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DerivingVia,
  DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module GL.Data.SyntaxTree where

import           Data.Functor
import           Data.List
import           Data.Tree
import           GL.Data.TH
import           GL.Type
import           GL.Utils
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read
import           Lens.Family2
import           GL.Data.Token

$(genEnum
    "SetOp"
    "Op"
    (mkEnumList
       [ "AddSet +="
       , "SubSet -="
       , "MulSet *="
       , "DivSet /="
       , "ModSet %="
       , "AndSet &&="
       , "OrSet ||="
       , "XorSet ^^="
       , "BAndSet &="
       , "BOrSet |="
       , "BXorSet ^="
       , "Set ="
       ]))

setOps :: [SetOp]
setOps = [minBound .. maxBound]

instance Read SetOp where
  readPrec = foldl1 (<++) $ map (\x -> lift (RP.string (show x)) $> x) setOps

$(genEnum
    "ExprOp"
    "EOp"
    (mkEnumList
       [ "Equal =="
       , "LessEq <="
       , "GreaterEq >="
       , "NotEq !="
       , "Less <"
       , "Greater >"
       , "Add +"
       , "Sub -"
       , "Mul *"
       , "Div /"
       , "Mod %"
       , "And &&"
       , "Or ||"
       , "Xor ^^"
       , "BAnd &"
       , "BOr |"
       , "BXor ^"
       ]))

exprOps :: [ExprOp]
exprOps = [minBound .. maxBound]

instance Read ExprOp where
  readPrec = foldl1 (<++) $ map (\x -> lift (RP.string (show x)) $> x) exprOps

$(genEnum "ExprPrefixOp" "EPfxOp" (mkEnumList ["Not !", "Sub -"]))

exprPrefixOps :: [ExprPrefixOp]
exprPrefixOps = [minBound .. maxBound]

instance Read ExprPrefixOp where
  readPrec =
    foldl1 (<++) $ map (\x -> lift (RP.string (show x)) $> x) exprPrefixOps

data AST t =
  AST [GLImport] (GLClass t)
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (AST t))

newtype GLImport =
  GLImport [String]

data GLClass t =
  GLClass ClassName [GLFun t]
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (GLClass t))

data GLFun t =
  GLFun t Ident [(t, Ident)] [GLStat t]
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (GLFun t))

data GLStat t
  = SIf (GLExpr t) (GLStat t) (Maybe (GLStat t))
  | SFor (GLStat t) (GLExpr t) (GLStat t) (GLStat t)
  | SWhile (GLExpr t) (GLStat t)
  | SDoWhile (GLExpr t) (GLStat t)
  | SLet t Ident (GLExpr t)
  | SSet Ident SetOp (GLExpr t)
  | SReturn (GLExpr t)
  | SBreak
  | SContinue
  | SNoOp
  | SBraces [GLStat t]
  | SExpr (GLExpr t)
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (GLStat t))

data GLExpr t =
    EIntLit t Integer
  | EFloatLit t Double
  | ECharLit t Char
  | EStringLit t String
  | EOp t (GLExpr t) ExprOp (GLExpr t)
  | EPrefix t ExprPrefixOp (GLExpr t)
  | EVar t (Maybe (GLExpr t)) Ident [GLExpr t]
  | EParen t (GLExpr t)
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (GLExpr t))

instance IsType t => Treeable (AST t) where
  toTree (AST i c) = Node "AST" [listToTree "imports" i, toTree c]

instance Treeable GLImport where
  toTree = toTree . show

instance IsType t => Treeable (GLClass t) where
  toTree (GLClass n f) = listToTree ("class " ++ show n) f

instance IsType t => Treeable (GLFun t) where
  toTree (GLFun t n as s) = listToTree
    ("fun " ++ showTypeShow t n)
    (listToTree "args" (uncurry showTypeShow <$> as) : map toTree s)

instance IsType t => Treeable (GLStat t) where
  toTree (SIf e s Nothing) = Node "if" [toTree e, Node "then" [toTree s]]
  toTree (SIf e s1 (Just s2)) =
    Node "if" [toTree e, Node "then" [toTree s1], Node "else" [toTree s2]]
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, Node "do" [toTree s3]]
  toTree (SWhile   e s) = Node "while" [toTree e, Node "do" [toTree s]]
  toTree (SDoWhile e s) = Node "do" [toTree s, Node "while" [toTree e]]
  toTree (SLet t n  e ) = Node ("let " ++ showTypeShow t n ++ " =") [toTree e]
  toTree (SSet n op e ) = Node (show n ++ " " ++ show op) [toTree e]
  toTree (SReturn e   ) = Node "return" [toTree e]
  toTree SBreak         = toTree "break"
  toTree SContinue      = toTree "continue"
  toTree SNoOp          = toTree "no op"
  toTree (SBraces s)    = listToTree "braces" s
  toTree (SExpr   e)    = toTree e

instance IsType t => Treeable (GLExpr t) where
  toTree (EIntLit    t i) = showTypeTree t $ toTree $ show i
  toTree (EFloatLit  t f) = showTypeTree t $ toTree $ show f
  toTree (EStringLit t s) = showTypeTree t $ toTree $ show s
  toTree (ECharLit   t c) = showTypeTree t $ toTree $ show c
  toTree (EOp t e1 op e2) =
    showTypeTree t $ Node ("operator " ++ show op) [toTree e1, toTree e2]
  toTree (EPrefix t op e) =
    showTypeTree t $ Node ("operator " ++ show op) [toTree e]
  toTree (EVar t Nothing n []) = showTypeTree t $ toTree n
  toTree (EVar t (Just d) n []) =
    showTypeTree t $ Node (show n) [Node "of" [toTree d]]
  toTree (EVar t Nothing n xs) =
    showTypeTree t $ Node (show n) [listToTree "args" xs]
  toTree (EVar t (Just d) n xs) =
    showTypeTree t $ Node (show n) [Node "of" [toTree d], listToTree "args" xs]
  toTree (EParen t e) = showTypeTree t $ Node "parens" [toTree e]

instance Show GLImport where
  show (GLImport s) = intercalate "." s

astClass :: Lens (AST t) (AST t') (GLClass t) (GLClass t')
astClass f (AST i c) = AST i <$> f c

classFuns :: Traversal (GLClass t) (GLClass t') (GLFun t) (GLFun t')
classFuns f (GLClass n fs) = GLClass n <$> traverse f fs

funStats :: Traversal' (GLFun t) (GLStat t)
funStats f (GLFun t n a s) = GLFun t n a <$> traverse f s

statExprs :: Traversal' (GLStat t) (GLExpr t)
statExprs f (SIf e s1 s2) =
  SIf <$> f e <*> statExprs f s1 <*> traverse (statExprs f) s2
statExprs f (SFor s1 e s2 s3) =
  SFor <$> statExprs f s1 <*> f e <*> statExprs f s2 <*> statExprs f s3
statExprs f (SWhile   e s) = SWhile <$> f e <*> statExprs f s
statExprs f (SDoWhile e s) = SDoWhile <$> f e <*> statExprs f s
statExprs f (SLet t n  e ) = SLet t n <$> f e
statExprs f (SSet n op e ) = SSet n op <$> f e
statExprs f (SReturn e   ) = SReturn <$> f e
statExprs _ SBreak         = pure SBreak
statExprs _ SContinue      = pure SContinue
statExprs _ SNoOp          = pure SNoOp
statExprs f (SBraces xs)   = SBraces <$> traverse (statExprs f) xs
statExprs f (SExpr   e )   = SExpr <$> f e

exprType1 :: Lens' (GLExpr t) t
exprType1 f (EIntLit    t i) = flip EIntLit i <$> f t
exprType1 f (EFloatLit  t n) = flip EFloatLit n <$> f t
exprType1 f (EStringLit t s) = flip EStringLit s <$> f t
exprType1 f (ECharLit   t c) = flip ECharLit c <$> f t
exprType1 f (EOp t e1 op e2) = (\t' -> EOp t' e1 op e2) <$> f t
exprType1 f (EPrefix t op e) = (\t' -> EPrefix t' op e) <$> f t
exprType1 f (EVar t d n a  ) = (\t' -> EVar t' d n a) <$> f t
exprType1 f (EParen t e    ) = flip EParen e <$> f t

changeExprType :: t -> GLExpr t -> GLExpr t
changeExprType = set exprType1

getExprType :: GLExpr t -> t
getExprType = view exprType1
