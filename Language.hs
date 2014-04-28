module Language where

import Prelude hiding (pi)
import Data.Aeson hiding (Success)
import Data.ByteString.Lazy.UTF8

import Parser

class Defaultable a where
  defaultValue :: a

instance Defaultable () where
  defaultValue = ()

instance Defaultable (Maybe a) where
  defaultValue = Nothing

instance FromJSON a => FromJSON (Term a)

instance ToJSON a => ToJSON (Term a)

lam :: Defaultable a => [Name] -> Term a -> Term a
lam []       t = t
lam (n : ns) t = Lam defaultValue (Just n) (lam ns t)

pi :: Defaultable a => [(Name, Type a)] -> Term a -> Term a
pi []             t = t
pi ((n, τ) : nτs) t = Pi defaultValue (Just n) τ (pi nτs t)

var :: Defaultable a => Name -> Term a
var = Var defaultValue

hole :: Defaultable a => Term a
hole = Hole defaultValue

set :: Defaultable a => Type a
set = Type defaultValue

annot :: Defaultable a => Term a -> Type () -> Type a
annot = Annot defaultValue

infixr 1 -->
(-->) :: Defaultable a => Type a -> Term a -> Term a
τ --> t = Pi defaultValue Nothing τ t

infixl 9 $$
($$) :: Defaultable a => Term a -> Term a -> Term a
t1 $$ t2 = App defaultValue t1 t2

-- Contexts

type TypCtxt a = [(Name, Type a)]

(+:) :: (Maybe Name, Type a) -> TypCtxt a -> TypCtxt a
(Nothing, _) +: γ = γ
(Just n,  τ) +: γ = (n, τ) : γ

data Work t tr r
  = Todo t (tr -> Work t tr r)
  | Done r

instance (Show t, Show r) => Show (Work t tr r) where
  show (Todo t _) = "Todo (" ++ show t ++ ")"
  show (Done r) = "Done (" ++ show r ++ ")"

stepWork :: (t -> (tr -> Work t tr r) -> Work t tr r) -> Work t tr r -> Work t tr r
stepWork _ (Done r) = Done r
stepWork f (Todo t k) = f t k

data Task
  = Synth (TypCtxt ()) (Term ())
  | Check (TypCtxt ()) (Term ()) (Type ())

instance Show Task where
  show (Synth γ t) =
    "Synth\n  " ++
    --show γ ++ "\n  " ++
    show t
  show (Check γ t τ) =
    "Check\n  " ++
    --show γ ++ "\n  " ++
    show t ++ "\n  " ++ show τ

-- within a better type system, I'd have Success be Synthesized or Checked
data TaskResult
  = Success (Type ()) TCResult
  | Failure TCResult

type TCResult = Term (Either String (Type ()))

type TCWork = Work Task TaskResult TCResult

annotateHeadWith :: a -> Term a -> Term a
annotateHeadWith a t = case t of
  Type _ -> Type a
  Pi _ n τ1 τ2 -> Pi a n τ1 τ2
  Var _ x -> Var a x
  Lam _ n t -> Lam a n t
  App _ t1 t2 -> App a t1 t2
  Let _ n t1 t2 -> Let a n t1 t2
  Annot _ t τ -> Annot a t τ
  Hole _ -> Hole a

annotateWith :: b -> Term a -> Term b
annotateWith a t = case t of
  Type _ -> Type a
  Pi _ n τ1 τ2 -> Pi a n (annotateWith a τ1) (annotateWith a τ2)
  Var _ x -> Var a x
  Lam _ n t -> Lam a n (annotateWith a t)
  App _ t1 t2 -> App a (annotateWith a t1) (annotateWith a t2)
  Let _ n t1 t2 -> Let a n (annotateWith a t1) (annotateWith a t2)
  Annot _ t τ -> Annot a (annotateWith a t) τ
  Hole _ -> Hole a

unchecked = annotateWith (Left "Not type-checked")

substitute :: Name -> Term a -> Term a -> Term a
substitute n v t = case t of
  Type a -> Type a
  Pi a n' τ1 τ2 ->
    Pi a n' (substitute n v τ1) (if Just n == n' then τ2 else substitute n v τ2)
  Var a n' ->
    if n == n' then v else Var a n'
  Lam a n' t -> Lam a n' (if Just n == n' then t else substitute n v t)
  App a t1 t2 -> App a (substitute n v t1) (substitute n v t2)
  Let a n' t1 t2 ->
    Let a n' (substitute n v t1) (if Just n == n' then t2 else substitute n v t2)
  Annot a t τ -> Annot a (substitute n v t) (substitute n (annotateWith () v) τ)
  Hole a -> Hole a

redβ :: Term a -> Term a
redβ t = case t of
  Type a -> Type a
  Pi a n τ1 τ2 -> Pi a n (redβ τ1) (redβ τ2)
  Var a x -> Var a x
  Lam a n t -> Lam a n (redβ t) -- reduce under lambda?
  App a t1 t2 ->
    let t1' = redβ t1 in
    case t1' of
      Lam _ (Just n) t -> redβ (substitute n t2 t)
      Lam _ Nothing t -> redβ t
      _ -> App a t1' (redβ t2)
  Let a n t1 t2 -> Let a n t1 t2 -- TODO: reduce lets?
  Annot a t τ -> Annot a (redβ t) τ
  Hole a -> Hole a

matchNames :: Maybe Name -> Maybe Name -> Maybe (Maybe Name)
matchNames Nothing Nothing = Just Nothing
matchNames a Nothing = Just $ a
matchNames Nothing b = Just $ b
matchNames (Just a) (Just b) =
  if a == b then Just $ Just a else Nothing

notFree :: Name -> Term a -> Bool
notFree n t = case t of
  Var _ n' -> n /= n'
  Pi _ n' τ t ->
    notFree n τ && case n' of
      Just n' -> n == n'
      Nothing -> False
      || notFree n t
  Lam _ Nothing t -> notFree n t
  Lam _ (Just n') t -> n == n' || notFree n t
  App _ t1 t2 -> notFree n t1 && notFree n t2
  Let _ Nothing t1 t2 -> notFree n t1 && notFree n t2
  Let _ (Just n') t1 t2 ->
    notFree n' t1 && (n == n' || notFree n t2)
  Annot _ t τ -> notFree n t && notFree n τ
  Hole _ -> True

-- TODO: add equality test for (x : T) → Q and T → Q
eqα :: Defaultable a => Term a -> Term a -> Bool
Hole _ `eqα` _ = True
_ `eqα` Hole _ = True
Type _ `eqα` Type _ = True
Var _ x `eqα` Var _ y | x == y = True
Pi _ Nothing τ1 τ2 `eqα` Pi _ Nothing τ3 τ4 =
  τ1 `eqα` τ3 && τ2 `eqα` τ4
Pi _ (Just n) τ1 τ2 `eqα` Pi _ Nothing τ3 τ4 =
  notFree n τ2 && notFree n τ4 && τ1 `eqα` τ3 && τ2 `eqα` τ4
Pi _ Nothing τ3 τ4 `eqα` Pi _ (Just n) τ1 τ2 =
  notFree n τ2 && notFree n τ4 && τ1 `eqα` τ3 && τ2 `eqα` τ4
Pi _ (Just n1) τ1 τ2 `eqα` Pi _ (Just n2) τ3 τ4 =
  if n1 == n2
  then τ1 `eqα` τ3 && τ2 `eqα` τ4
  else notFree n1 τ4 && notFree n2 τ2 &&
       τ1 `eqα` τ3 && τ2 `eqα` (substitute n2 (var n1) τ4)
_ `eqα` _ = False

eqβ :: Defaultable a => Term a -> Term a -> Bool
a `eqβ` b = (redβ a) `eqα` (redβ b)

typeCheckStep :: Task -> (TaskResult -> TCWork) -> TCWork
typeCheckStep t k = case t of

  -- (n : τ1) → τ2
  Synth γ (Pi _ n τ1 τ2) ->
    Todo (Check γ τ1 set) $ \res ->
    case res of
      Success _ τ1' ->
        Todo (Check ((n, τ1) +: γ) τ2 set) $ \res ->
        case res of
          Success _ τ2' -> k $ Success set (Pi (Right set) n τ1' τ2')
          Failure τ2' ->
            k $ Failure $ Pi (Left "Failed because of subterm") n τ1' τ2'
      Failure τ1' ->
        k $ Failure $ Pi (Left "Failed because of subterm") n τ1' (unchecked τ2)

  Synth γ (App _ f x) ->
    Todo (Synth γ f) $ \res ->
    case res of
      Success τf f' ->
        case τf of
          Pi _ n τx τr ->
            Todo (Check γ x τx) $ \res ->
            case res of
              Success _ x' ->
                case n of
                  Just n' ->
                    let τr' = redβ (substitute n' x τr) in
                    k $ Success τr' (App (Right τr') f' x')
                  Nothing ->
                    k $ Success τr (App (Right τr) f' x')
              Failure x' ->
                k $ Failure $
                App
                (Left "The argument to this application did not type-check correctly")
                f' x'
          _ ->
            k $ Failure $
            App (Left $
                 "The type synthesized for the function being applied" ++
                 " is not a Π-type:\n" ++
                 pprint τf)
            f' (unchecked x)
      Failure f' ->
        k $ Failure $
        App (Left "The function being applied did not type-check")
        f' (unchecked x)

  -- t : τ
  Synth γ (Annot _ t τ) ->
    Todo (Check γ t τ) $ \res ->
    case res of
      Success τ' t' ->
        k $ Success τ (Annot (Right τ) t' τ)
      Failure t' ->
        k $ Failure $
        Annot
        (Left "The term did not type-check at the annotated type")
        t' τ

  -- λ n → t
  Check γ (Lam _ n t) τ ->
    Todo (Check γ τ set) $ \res ->
    case res of
      Success _ _ ->
        case redβ τ of
          Pi _ x σ τ' ->
            case matchNames n x of
              Nothing ->
                error "TODO4"
              Just mn ->
                let γ' = (n, σ) +: γ in
                Todo (Check γ' t τ') $ \res ->
                case res of
                  Success _ t' -> k $ Success τ (Lam (Right τ) n t')
                  Failure t' ->
                    k $ Failure $
                    Lam
                    (Left "The body of the λ-term did not type-check")
                    n t'
          _ ->
            k $ Failure $ Lam
            (Left $
             "The type expected for the abstraction:\n" ++
             pprint τ ++
             "\ndid not reduce to a π-type.")
            n (unchecked t)

  Check γ (Hole _) τ ->
    Todo (Check γ τ set) $ \res ->
    case res of
      Success _ _ ->
        k $ Success τ $ Hole (Right τ)
      Failure τ' ->
        k $ Failure $ Hole $ Left $
        "The type of this hole:\n" ++
        pprint τ' ++
        "\nis not a type"

  -- conversion rule
  Check γ t τ ->
    Todo (Synth γ t) $ \res ->
    case res of
      Success τ' t' ->
        if τ `eqβ` τ'
        then k $ Success τ t'
        else k $ Failure $ annotateHeadWith
             (Left $ "This term has type:\n" ++
              pprint τ' ++
              "\nbut a term of type:\n" ++
              pprint (redβ τ) ++
              "\nis expected")
             t'
      Failure t' ->
        k $ Failure t'

  Synth γ (Var _ n) ->
    case lookup n γ of
      Just τ -> k $ Success τ (Var (Right τ) n)
      Nothing ->
        k $ Failure $ Var (Left "Undefined reference") n

  Synth _ (Type _) -> k $ Success set (Type (Right set))

  Synth _ (Lam _ _ _) ->
    error "Cannot synthesize the type of an abstraction"

stepTCWork :: TCWork -> TCWork
stepTCWork = stepWork typeCheckStep

tcTrace :: TCWork -> [TCWork]
tcTrace (Done r) = [Done r]
tcTrace w = w : (tcTrace (stepTCWork w))

mkWorkWithContext :: TypCtxt () -> Term () -> TCWork
mkWorkWithContext γ t =
  Todo
  (Synth γ t)
  (\res ->
    case res of
      Success _ r -> Done r
      Failure r -> Done r
  )

mkWork :: Term () -> TCWork
mkWork = mkWorkWithContext []

tcTraceResult :: TCWork -> TCResult
tcTraceResult = go . tcTrace
  where
    go :: [TCWork] -> TCResult
    go (Done r : rest) = r
    go (_ : rest) = go rest

infer :: Term a -> (Term a, Type ())
infer (Annot a t τ) = (t, τ)
infer t = (t, hole)

inferWork :: Term () -> TCWork
inferWork t0 =
  let (t, τ) = infer t0 in
  mkWork $ annot t τ

process :: String -> String
process s =
  let term = parseTerm s in
  let work = inferWork term in
  let res = tcTraceResult work in
  let resByt = encode res in
  let resStr = toString resByt in
  resStr

{-
process :: String -> String
process = BLC.unpack . encode . tcTraceResult . inferWork . parseTerm
-}
