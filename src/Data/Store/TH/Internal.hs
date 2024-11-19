{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Store.TH.Internal
    (
    -- * TH functions for generating Store instances
      deriveManyStoreFromStorable
    , deriveTupleStoreInstance
    , deriveGenericInstance
    , deriveGenericInstanceFromName
    , deriveManyStorePrimVector
    , deriveManyStoreUnboxVector
    , deriveStore
    , makeStore
    -- * Misc utilties used in Store test
    , getAllInstanceTypes1
    , isMonoType
    ) where

import           Control.Applicative
import           Data.Complex ()
import           Data.Generics.Aliases (extT, mkQ, extQ)
import           Data.Generics.Schemes (listify, everywhere, something)
import           Data.List (find)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Primitive.ByteArray
import           Data.Primitive.Types
import           Data.Store.Core
import           Data.Store.Impl
import qualified Data.Text as T
import           Data.Traversable (forM)
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Unboxed as UV
import           Data.Word
import           Foreign.Storable (Storable)
import           GHC.Types (Int(..))
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany.Internal (TypeclassInstance(..), getInstances, unAppsT)
import           Language.Haskell.TH.Syntax (lift)
import           Prelude
import           Safe (headMay)
import           TH.Derive (Deriver(..))
import           TH.ReifySimple
import           TH.Utilities (expectTyCon1, dequalify, plainInstanceD, appsT)

instance Deriver (Store a) where
    runDeriver _ preds ty = do
        argTy <- expectTyCon1 ''Store ty
        dt <- reifyDataTypeSubstituted argTy
        (:[]) <$> deriveStore preds argTy (dtCons dt)

-- | Given the name of a type, generate a Store instance for it,
-- assuming that all type variables also need to be Store instances.
--
-- Note that when used with datatypes that require type variables, the
-- ScopedTypeVariables extension is required.
makeStore :: Name -> Q [Dec]
makeStore name = do
    dt <- reifyDataType name
    let preds = map (storePred . VarT) (dtTvs dt)
        argTy = appsT (ConT name) (map VarT (dtTvs dt))
    (:[]) <$> deriveStore preds argTy (dtCons dt)

deriveStore :: Cxt -> Type -> [DataCon] -> Q Dec
deriveStore preds headTy cons0 =
    makeStoreInstance preds headTy
        <$> sizeExpr
        <*> peekExpr
        <*> pokeExpr
  where
    cons :: [(Name, [(Name, Type)])]
    cons =
      [ ( dcName dc
        , [ (mkName ("c" ++ show ixc ++ "f" ++ show ixf), ty)
          | ixf <- ints
          | (_, ty) <- dcFields dc
          ]
        )
      | ixc <- ints
      | dc <- cons0
      ]
    -- NOTE: tag code duplicated in th-storable.
    (tagType, _, tagSize) =
        fromMaybe (error "Too many constructors") $
        find (\(_, maxN, _) -> maxN >= length cons) tagTypes
    tagTypes :: [(Name, Int, Int)]
    tagTypes =
        [ ('(), 1, 0)
        , (''Word8, fromIntegral (maxBound :: Word8), 1)
        , (''Word16, fromIntegral (maxBound :: Word16), 2)
        , (''Word32, fromIntegral (maxBound :: Word32), 4)
        , (''Word64, fromIntegral (maxBound :: Word64), 8)
        ]
    fName ix = mkName ("f" ++ show ix)
    ints = [0..] :: [Int]
    fNames = map fName ints
    sizeNames = zipWith (\_ -> mkName . ("sz" ++) . show) cons ints
    tagName = mkName "tag"
    valName = mkName "val"
    sizeExpr
        -- Maximum size of GHC tuples
        | length cons <= 62 =
            caseE (tupE (concatMap (map sizeAtType . snd) cons))
                  (case cons of
                     -- Avoid overlapping matches when the case expression is ()
                     [] -> [matchConstSize]
                     [c] | null (snd c) -> [matchConstSize]
                     _ -> [matchConstSize, matchVarSize])
        | otherwise = varSizeExpr
      where
        sizeAtType :: (Name, Type) -> ExpQ
        sizeAtType (_, ty) = [| size :: Size $(return ty) |]
        matchConstSize :: MatchQ
        matchConstSize = do
            let sz0 = VarE (mkName "sz0")
                sizeDecls =
                    if null sizeNames
                        then [valD (varP (mkName "sz0")) (normalB [| 0 |]) []]
                        else zipWith constSizeDec sizeNames cons
            sameSizeExpr <-
                case sizeNames of
                    (_ : tailSizeNames) ->
                        foldl (\l r -> [| $(l) && $(r) |]) [| True |] $
                        map (\szn -> [| $(return sz0) == $(varE szn) |]) tailSizeNames
                    [] -> [| True |]
            result <- [| ConstSize (tagSize + $(return sz0)) |]
            match (tupP (map (\(n, _) -> conP 'ConstSize [varP n])
                             (concatMap snd cons)))
                  (guardedB [return (NormalG sameSizeExpr, result)])
                  sizeDecls
        constSizeDec :: Name -> (Name, [(Name, Type)]) -> DecQ
        constSizeDec szn (_, []) =
            valD (varP szn) (normalB [| 0 |]) []
        constSizeDec szn (_, fields) =
            valD (varP szn) body []
          where
            body = normalB $
                foldl1 (\l r -> [| $(l) + $(r) |]) $
                map (\(sizeName, _) -> varE sizeName) fields
        matchVarSize :: MatchQ
        matchVarSize = do
            match (tupP (map (\(n, _) -> varP n) (concatMap snd cons)))
                  (normalB varSizeExpr)
                  []
        varSizeExpr :: ExpQ
        varSizeExpr =
            [| VarSize $ \x -> tagSize + $(caseE [| x |] (map matchVar cons)) |]
        matchVar :: (Name, [(Name, Type)]) -> MatchQ
        matchVar (cname, []) =
            match (conP cname []) (normalB [| 0 |]) []
        matchVar (cname, fields) =
            match (conP cname (zipWith (\_ fn -> varP fn) fields fNames))
                  body
                  []
          where
            body = normalB $
                foldl1 (\l r -> [| $(l) + $(r) |])
                (zipWith (\(sizeName, _) fn -> [| getSizeWith $(varE sizeName) $(varE fn) |])
                         fields
                         fNames)
    -- Choose a tag size large enough for this constructor count.
    -- Expression used for the definition of peek.
    peekExpr = case cons of
        [] -> [| error ("Attempting to peek type with no constructors (" ++ $(lift (show headTy)) ++ ")") |]
        [con] -> peekCon con
        _ -> doE
            [ bindS (varP tagName) [| peek |]
            , noBindS (caseE (sigE (varE tagName) (conT tagType))
                      (map peekMatch (zip [0..] cons) ++ [peekErr]))
            ]
    peekMatch (ix, con) = match (litP (IntegerL ix)) (normalB (peekCon con)) []
    peekErr = match wildP (normalB
        [| peekException $ T.pack $ "Found invalid tag while peeking (" ++ $(lift (show headTy)) ++ ")" |]) []
    peekCon (cname, fields) =
        case fields of
            [] -> [| pure $(conE cname) |]
            _ -> doE $
                map (\(fn, _) -> bindS (varP fn) [| peek |]) fields ++
               [noBindS $ appE (varE 'return) $ appsE $ conE cname : map (\(fn, _) -> varE fn) fields]
    pokeExpr = lamE [varP valName] $ caseE (varE valName) $ zipWith pokeCon [0..] cons
    pokeCon :: Int -> (Name, [(Name, Type)]) -> MatchQ
    pokeCon ix (cname, fields) =
        match (conP cname (map (\(fn, _) -> varP fn) fields)) body []
      where
        body = normalB $
            case cons of
                (_:_:_) -> doE (pokeTag ix : map pokeField fields)
                _ -> doE (map pokeField fields)
    pokeTag ix = noBindS [| poke (ix :: $(conT tagType)) |]
    pokeField (fn, _) = noBindS [| poke $(varE fn) |]

{- What the generated code looks like

data Foo = Foo Int Double Float

instance Store Foo where
    size =
        case (size :: Size Int, size :: Size Double, size :: Size Float) of
            (ConstSize c0f0, ConstSize c0f1, ConstSize c0f2) -> ConstSize (0 + sz0)
              where
                sz0 = c0f0 + c0f1 + c0f2
            (c0f0, c0f1, c0f2)
                VarSize $ \(Foo f0 f1 f2) -> 0 +
                    getSizeWith c0f0 f0 + getSizeWith c0f1 f1 + getSizeWith c0f2 f2
    peek = do
        f0 <- peek
        f1 <- peek
        f2 <- peek
        return (Foo f0 f1 f2)
    poke (Foo f0 f1 f2) = do
        poke f0
        poke f1
        poke f2

data Bar = Bar Int | Baz Double

instance Store Bar where
    size =
        case (size :: Size Int, size :: Size Double) of
            (ConstSize c0f0, ConstSize c1f0) | sz0 == sz1 -> ConstSize (1 + sz0)
              where
                sz0 = c0f0
                sz1 = c1f0
            (c0f0, c1f0) -> VarSize $ \x -> 1 +
                case x of
                    Bar f0 -> getSizeWith c0f0 f0
                    Baz f0 -> getSizeWith c1f0 f0
    peek = do
        tag <- peek
        case (tag :: Word8) of
            0 -> do
                f0 <- peek
                return (Bar f0)
            1 -> do
                f0 <- peek
                return (Baz f0)
            _ -> peekException "Found invalid tag while peeking (Bar)"
    poke (Bar f0) = do
        poke 0
        poke f0
    poke (Bar f0) = do
        poke 1
        poke f0
-}

------------------------------------------------------------------------
-- Generic

deriveTupleStoreInstance :: Int -> Dec
deriveTupleStoreInstance n =
    deriveGenericInstance (map storePred tvs)
                          (foldl1 AppT (TupleT n : tvs))
  where
    tvs = take n (map (VarT . mkName . (:[])) ['a'..'z'])

deriveGenericInstance :: Cxt -> Type -> Dec
deriveGenericInstance cs ty = plainInstanceD cs (AppT (ConT ''Store) ty) []

deriveGenericInstanceFromName :: Name -> Q Dec
deriveGenericInstanceFromName n = do
    tvs <- map VarT . dtTvs <$> reifyDataType n
    return $ deriveGenericInstance (map storePred tvs) (appsT (ConT n) tvs)

------------------------------------------------------------------------
-- Storable

-- TODO: Generate inline pragmas? Probably not necessary

deriveManyStoreFromStorable :: (Type -> Bool) -> Q [Dec]
deriveManyStoreFromStorable p = do
    storables <- postprocess . instancesMap <$> getInstances ''Storable
    stores <- postprocess . instancesMap <$> getInstances ''Store
    return $ M.elems $ flip M.mapMaybe (storables `M.difference` stores) $
        \(TypeclassInstance cs ty _) ->
        let argTy = head (tail (unAppsT ty))
            tyNameLit = LitE (StringL (pprint ty)) in
        if p argTy && not (superclassHasStorable cs)
            then Just $ makeStoreInstance cs argTy
                (AppE (VarE 'sizeStorableTy) tyNameLit)
                (AppE (VarE 'peekStorableTy) tyNameLit)
                (VarE 'pokeStorable)
            else Nothing

-- See #143. Often Storable superclass constraints should instead be
-- Store constraints, so instead it just warns for these.
superclassHasStorable :: Cxt -> Bool
superclassHasStorable = isJust . something (mkQ Nothing justStorable `extQ` ignoreStrings)
  where
    justStorable :: Type -> Maybe ()
    justStorable (ConT n) | n == ''Storable = Just ()
    justStorable _ = Nothing
    ignoreStrings :: String -> Maybe ()
    ignoreStrings _ = Nothing

------------------------------------------------------------------------
-- Vector

deriveManyStorePrimVector :: Q [Dec]
deriveManyStorePrimVector = do
    prims <- postprocess . instancesMap <$> getInstances ''PV.Prim
    stores <- postprocess . instancesMap <$> getInstances ''Store
    let primInsts =
            M.mapKeys (map (AppT (ConT ''PV.Vector))) prims
            `M.difference`
            stores
    forM (M.toList primInsts) $ \primInst -> case primInst of
        ([_], TypeclassInstance cs ty _) -> do
            let argTy = head (tail (unAppsT ty))
            sizeExpr <- [|
                VarSize $ \x ->
                    I# $(primSizeOfExpr (ConT ''Int)) +
                    I# $(primSizeOfExpr argTy) * PV.length x
                |]
            peekExpr <- [| do
                len <- peek
                let sz = I# $(primSizeOfExpr argTy)
                array <- peekToByteArray $(lift ("Primitive Vector (" ++ pprint argTy ++ ")"))
                                         (len * sz)
                return (PV.Vector 0 len array)
                |]
            pokeExpr <- [| \(PV.Vector offset len (ByteArray array)) -> do
                let sz = I# $(primSizeOfExpr argTy)
                poke len
                pokeFromByteArray array (offset * sz) (len * sz)
                |]
            return $ makeStoreInstance cs (AppT (ConT ''PV.Vector) argTy) sizeExpr peekExpr pokeExpr
        _ -> fail "Invariant violated in derivemanyStorePrimVector"


primSizeOfExpr :: Type -> ExpQ
primSizeOfExpr ty = [| $(varE 'sizeOf#) (error "sizeOf# evaluated its argument" :: $(return ty)) |]

deriveManyStoreUnboxVector :: Q [Dec]
deriveManyStoreUnboxVector = do
    unboxes <- getUnboxInfo
    stores <- postprocess . instancesMap <$> getInstances ''Store
    unboxInstances <- postprocess . instancesMap <$> getInstances ''UV.Unbox
    let dataFamilyDecls =
            M.fromList (map (\(preds, ty, cons) -> ([AppT (ConT ''UV.Vector) ty], (preds, cons))) unboxes)
            `M.difference`
            stores
#if MIN_VERSION_template_haskell(2,10,0)
        substituteConstraint (AppT (ConT n) arg)
          | n == ''UV.Unbox = AppT (ConT ''Store) (AppT (ConT ''UV.Vector) arg)
#else
        substituteConstraint (ClassP n [arg])
          | n == ''UV.Unbox = ClassP ''Store [AppT (ConT ''UV.Vector) arg]
#endif
        substituteConstraint x = x
    -- TODO: ideally this would use a variant of 'deriveStore' which
    -- assumes VarSize.
    forM (M.toList dataFamilyDecls) $ \case
        ([ty], (_, cons)) -> do
            let headTy = getTyHead (unAppsT ty !! 1)
            (preds, ty') <- case M.lookup [headTy] unboxInstances of
              Nothing -> do
                reportWarning $ "No Unbox instance found for " ++ pprint headTy
                return ([], ty)
              Just (TypeclassInstance cs (AppT _ ty') _) -> case ty' of
                AppT (ConT conName) arg ->
                  if nameBase conName `elem` doNotUnboxConstructors
                    then return ([AppT (ConT ''Store) arg], AppT (ConT ''UV.Vector) ty')
                    else return (map substituteConstraint cs, AppT (ConT ''UV.Vector) ty')
                _ -> return (map substituteConstraint cs, AppT (ConT ''UV.Vector) ty')
              Just _ -> fail "Impossible case"
            deriveStore preds ty' cons
        _ -> fail "impossible case in deriveManyStoreUnboxVector"

-- TODO: Add something for this purpose to TH.ReifyDataType

getUnboxInfo :: Q [(Cxt, Type, [DataCon])]
getUnboxInfo = do
    FamilyI _ insts <- reify ''UV.Vector
    return (map (everywhere (id `extT` dequalVarT)) $ mapMaybe go insts)
  where
#if MIN_VERSION_template_haskell(2,15,0)
    go (NewtypeInstD preds _ lhs _ con _)
      | [_, ty] <- unAppsT lhs
      = toResult preds ty [con]
    go (DataInstD preds _ lhs _ cons _)
      | [_, ty] <- unAppsT lhs
      = toResult preds ty cons
#elif MIN_VERSION_template_haskell(2,11,0)
    go (NewtypeInstD preds _ [ty] _ con _) = toResult preds ty [con]
    go (DataInstD preds _ [ty] _ cons _) = toResult preds ty cons
#else
    go (NewtypeInstD preds _ [ty] con _) = toResult preds ty [con]
    go (DataInstD preds _ [ty] cons _) = toResult preds ty cons
#endif
    go x = error ("Unexpected result from reifying Unboxed Vector instances: " ++ pprint x)
    toResult :: Cxt -> Type -> [Con] -> Maybe (Cxt, Type, [DataCon])
    toResult _ _ [NormalC conName _]
      | nameBase conName `elem` skippedUnboxConstructors = Nothing
    toResult preds ty cons
      = Just (preds, ty, concatMap conToDataCons cons)
    dequalVarT :: Type -> Type
    dequalVarT (VarT n) = VarT (dequalify n)
    dequalVarT ty = ty

-- See issue #174
skippedUnboxConstructors :: [String]
skippedUnboxConstructors = ["MV_UnboxAs", "V_UnboxAs", "MV_UnboxViaPrim", "V_UnboxViaPrim"]

-- See issue #179
doNotUnboxConstructors :: [String]
doNotUnboxConstructors = ["DoNotUnboxLazy","DoNotUnboxStrict","DoNotUnboxNormalForm"]

------------------------------------------------------------------------
-- Utilities

-- Filters out overlapping instances and instances with more than one
-- type arg (should be impossible).
postprocess :: M.Map [Type] [a] -> M.Map [Type] a
postprocess =
    M.mapMaybeWithKey $ \tys xs ->
        case (tys, xs) of
            ([_ty], [x]) -> Just x
            _ -> Nothing

makeStoreInstance :: Cxt -> Type -> Exp -> Exp -> Exp -> Dec
makeStoreInstance cs ty sizeExpr peekExpr pokeExpr =
    plainInstanceD
        cs
        (AppT (ConT ''Store) ty)
        [ ValD (VarP 'size) (NormalB sizeExpr) []
        , ValD (VarP 'peek) (NormalB peekExpr) []
        , ValD (VarP 'poke) (NormalB pokeExpr) []
        ]

-- TODO: either generate random types that satisfy instances with
-- variables in them, or have a check that there's at least a manual
-- check for polymorphic instances.

getAllInstanceTypes :: Name -> Q [[Type]]
getAllInstanceTypes n =
    map (\(TypeclassInstance _ ty _) -> drop 1 (unAppsT ty)) <$>
    getInstances n

getAllInstanceTypes1 :: Name -> Q [Type]
getAllInstanceTypes1 n =
    fmap (fmap (fromMaybe (error "getAllMonoInstances1 expected only one type argument") . headMay))
         (getAllInstanceTypes n)

isMonoType :: Type -> Bool
isMonoType = null . listify isVarT

isVarT :: Type -> Bool
isVarT VarT{} = True
isVarT _ = False

-- TOOD: move these to th-reify-many

-- | Get a map from the 'getTyHead' type of instances to
-- 'TypeclassInstance'.
instancesMap :: [TypeclassInstance] -> M.Map [Type] [TypeclassInstance]
instancesMap =
    M.fromListWith (++) .
    map (\ti -> (map getTyHead (instanceArgTypes ti), [ti]))

instanceArgTypes :: TypeclassInstance -> [Type]
instanceArgTypes (TypeclassInstance _ ty _) = drop 1 (unAppsT ty)

getTyHead :: Type -> Type
getTyHead (SigT x _) = getTyHead x
getTyHead (ForallT _ _ x) = getTyHead x
getTyHead (AppT l _) = getTyHead l
getTyHead x = x

storePred :: Type -> Pred
storePred ty =
#if MIN_VERSION_template_haskell(2,10,0)
        AppT (ConT ''Store) ty
#else
        ClassP ''Store [ty]
#endif
