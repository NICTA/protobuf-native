{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, UnicodeSyntax, KindSignatures, OverlappingInstances, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}
module Data.Protobuf (
    protobuf
  , protobuf'
  , readProtobuf
  , writeProtobuf
  , Protobuf
  , ProtobufValue(..)
  , module Foreign.C
  , module Control.Applicative
  
  , Std__basic_string
  , newPb
  , derefPb
  ) where

import Data.Int
import Data.Word
import Data.Char
import Data.ByteString (ByteString)
import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.ForeignPtr.Unsafe
import Foreign.Concurrent
import Foreign.CPlusPlus
import Foreign.CPlusPlusStdLib
import Language.Haskell.TH

data TypeKind = Assign | Pointer | Vector | MaybeP TypeKind
  deriving (Show, Eq)

newPb :: Protobuf a => IO (ForeignPtr a)
newPb = new >>= \p -> addForeignPtrFinalizer p (delete (unsafeForeignPtrToPtr p)) >> return p

derefPb :: ProtobufValue a b => ForeignPtr a -> IO b
derefPb p = withForeignPtr p load

class Protobuf a where
  new :: IO (ForeignPtr a)
  delete :: Ptr a -> IO ()

class Protobuf a => ProtobufValue a b | a -> b, b -> a where
  load :: Ptr a -> IO b
  assign :: ForeignPtr a -> b -> IO ()

-- "not really" an orphan
instance (ProtobufValue b a) => CPlusPlusLand (Maybe a) (Ptr b) where
  to Nothing = return nullPtr
  to (Just x) = to x
  from x = if x == nullPtr then return Nothing else (Just `fmap` from x)

-- "not really" an orphan
instance (ProtobufValue b a) => CPlusPlusLand a (Ptr b) where
  to x = new >>= \(p :: ForeignPtr b) -> assign p x >> withForeignPtr p (\p' -> return (p' :: Ptr b))
  from = load

cplusplus "haskell::readProtobuf(char const*, google::protobuf::Message&)"  "cbits/hsprotobuf.o" [t|CString -> Ptr () -> IO CInt|]
cplusplus "haskell::writeProtobuf(char const*, google::protobuf::Message&)" "cbits/hsprotobuf.o" [t|CString -> Ptr () -> IO CInt|]

readProtobuf :: Protobuf a => FilePath -> ForeignPtr a -> IO ()
readProtobuf f m = do
  s <- newCString f
  r <- withForeignPtr m $ haskell__readProtobuf s . castPtr
  if r /= 0
    then error "readProtobuf exception" -- TODO: error
    else return ()

writeProtobuf :: Protobuf a => FilePath -> ForeignPtr a -> IO ()
writeProtobuf f m = do
  s <- newCString f
  r <- withForeignPtr m $ haskell__writeProtobuf s . castPtr
  if r /= 0
    then error "writeProtobuf exception" -- TODO: error
    else return ()

-- use ("name", [t|String|])
protobuf' :: FilePath -> String -> [(String, Type)] -> Q [Dec]
protobuf' objfile tname fields = do
  let dataDefn = DataD [] (mkName tname) [] [] []
  let typeDefn = TySynD (mkName (tname ++ "Ptr")) [] (AppT (ConT ''ForeignPtr) (ConT (mkName tname)))
  let lowCaseTname = toLower (head tname):(tail tname)
  con <- cplusplus (tname ++ "::New() const") objfile
          (appT (conT ''IO) (appT (conT ''Ptr) (conT (mkName tname))))
  del <- cplusplus (tname ++ "::~" ++ tname ++ "()") objfile
          (appT (appT arrowT (appT (conT ''Ptr) (conT (mkName tname)))) (appT (conT ''IO) (conT ''())))
  instanceDef <- instanceD (cxt []) (appT (conT ''Protobuf) (conT (mkName tname)))
                   [valD (varP 'new) (normalB
                     (infixE
                       (Just (varE (mkName (lowCaseTname ++ "__New"))))
                       (varE '(>>=))
                       (Just (varE 'newForeignPtr_)))) []
                   ,funD 'delete [clause [varP (mkName "p")]
                     (normalB (appE
                       (varE (mkName (lowCaseTname ++ "__~" ++ tname)))
                       (varE (mkName "p")))) []]]
  xs <- fmap concat $ mapM (deHaskell objfile tname) fields
  return $ dataDefn:typeDefn:instanceDef:[] ++ con ++ del ++ xs

protobuf :: FilePath -> Name -> Q [Dec]
protobuf objfile name = reify name >>= \val -> case val of
  (TyConI (DataD [] _tyName [] [cx@(RecC _conName fields)] [])) -> do
    pb <- protobuf' objfile (init (nameBase name)) $ map (\(n, _, t) -> (nameBase n, t)) fields
    xs <- instanceD (cxt [])
            (appT (appT (conT ''ProtobufValue) (conT (mkName (init (nameBase name))))) (conT name))
            [
              loadDef (nameBase name) cx
            , assignDef (nameBase name) cx
            ]
    return $ pb ++ [xs]
  x -> error $ "can not handle decl of shape: " ++ show x

loadDef :: String -> Con -> DecQ
loadDef name (RecC conName fields) =
  let
    n' = let (x:xs) = init name in toLower x:xs
    ((fstField, _, fstType):_) = fields
    apply y (Vector, _, _) =
      -- optional_features_size >>= mapM optional_features . \x -> [0..(x-1)]
      (InfixE
        (Just (AppE (VarE (mkName (n' ++ "__" ++ nameBase y ++ "_size"))) (VarE (mkName "x"))))
        (VarE '(>>=))
        (Just
          (InfixE
            (Just
              (AppE
                (VarE 'mapM)
                (LamE [VarP (mkName "y")]
                  (InfixE
                    (Just
                      (AppE
                        (AppE
                          (VarE (mkName (n' ++ "__" ++ nameBase y)))
                          (VarE (mkName "x")))
                        (VarE (mkName "y"))))
                    (VarE '(>>=))
                    (Just (VarE 'from))))))
            (VarE '(.))
            (Just
              (LamE [VarP (mkName "y")]
                (ArithSeqE
                  (FromToR (LitE (IntegerL 0))
                    (InfixE
                      (Just (VarE (mkName "y")))
                      (VarE '(-))
                      (Just (LitE (IntegerL 1)))))))))))

    apply y (MaybeP typ', _, _) =
      (InfixE
        (Just
          (AppE
            (AppE
              (case typ' of
                Pointer -> (VarE 'getMaybePtr)
                Assign -> (VarE 'getMaybeVal)
                _ -> error "impossible data defn")
              (AppE
                (VarE
                  (mkName (n' ++ "__" ++ nameBase y)))
                (VarE xv)))
            (AppE
              (VarE
                (mkName (n' ++ "__has_" ++ nameBase y)))
              (VarE xv))))
        (VarE '(>>=))
        (Just (VarE 'from)))

    -- Assign and Pointer
    apply y (_, _, _) =
      (InfixE
        (Just (AppE (VarE (mkName (n' ++ "__" ++ nameBase y))) (VarE xv)))
        (VarE '(>>=))
        (Just (VarE 'from)))
    xv = mkName "x"
  in funD 'load
      -- TODO: todo for loading an array NEXT
      [clause [varP xv]
        (normalB
          (return (foldl
            (\x (y, _, t) -> InfixE (Just x) (VarE '(<*>))
              (Just (apply y (typeinfo t)))
            )
            (InfixE (Just (ConE conName)) (VarE '(<$>))
              (Just (apply fstField (typeinfo fstType))))
            (tail fields)
        )))
      [] ]
loadDef _ _ = error "invalid constructor definition"

assignDef :: String -> Con -> DecQ
assignDef name (RecC _conName fields) =
  let
    n' = let (x:xs) = init name in toLower x:xs
    ((fstField, _, fstType):_) = fields
    sety y (Vector, ftyp, _) =
      (InfixE
        (Just
          (AppE (VarE 'to)
            (AppE (VarE (mkName (show y))) (VarE xv))))
        -- TODO: should clear the array first?
        (VarE '(>>=))
        (Just
          (AppE
            (VarE 'mapM_)
            (setApply n' "__add_" ftyp pp pv y Nothing))))

    sety y (Pointer, _, _) =
      (InfixE
        (Just
          (AppE (VarE 'to)
            (AppE (VarE (mkName (show y))) (VarE xv))))
        (VarE '(>>=))
        (Just
          (AppE
            (VarE (mkName (n' ++ "__set_allocated_" ++ (nameBase y))))
            (VarE pv))))

    sety y (Assign, ftyp, _) =
      (InfixE
        (Just
          (AppE (VarE 'to)
            (AppE (VarE (mkName (show y))) (VarE xv))))
        (VarE '(>>=))
        (Just
          (setApply n' "__set_" ftyp pp pv y Nothing)))

    sety y (MaybeP typ', ftyp, _) =
      (InfixE
        (Just
          (AppE (VarE 'to)
            (AppE (VarE (mkName (show y))) (VarE xv))))
        (VarE '(>>=))
        (Just
          (AppE
            (AppE
              (case typ' of
                Pointer -> (VarE 'setMaybePtr)
                Assign -> (VarE 'setMaybeVal)
                _ -> error "invalid data defn")
              (setApply n' "__set_" ftyp pp pv y (Just typ')))
            (AppE
              (VarE (mkName (n' ++ "__clear_" ++ (nameBase y))))
              (VarE pv)))))

    pv = mkName "pv"
    pp = mkName "pp"
    xv = mkName "x"
  in funD 'assign
      [clause [varP pp, varP xv]
        (normalB
          (appE
            (appE (varE 'withForeignPtr) (varE pp))
            (lamE [varP pv]
              (return
                (foldl
                  (\x (y, _, t) ->
                    InfixE
                      (Just x)
                      (VarE '(>>))
                      (Just (sety y (typeinfo t))))
                  (sety fstField (typeinfo fstType))
                  (tail fields))))))
      []]
assignDef _ _ = error "invalid constructor definition"

setApply :: String -> String -> Type -> Name -> Name -> Name -> Maybe TypeKind -> Exp
setApply n' meth ftyp pp pv y typ' =
  (AppE
    (VarE (mkName (n' ++ meth
      ++ (if typ' == Just Pointer then "allocated_" else "")
      ++ (if ftyp == ConT ''Foreign.CPlusPlusStdLib.Std__basic_string
            then "ret_"
            else "")
      ++ (nameBase y))))
      (if ftyp == ConT ''Foreign.CPlusPlusStdLib.Std__basic_string
                            then (VarE pp)
                            else (VarE pv))
      )

deHaskell :: String -> String -> (String, Type) -> Q [Dec]
deHaskell objfile tname (name, typ) = do
  let ptrTypName = appT (conT ''Ptr) (conT (mkName tname))
  gets <- genGetter tname name ptrTypName objfile typ
  sets <- genSetter tname name ptrTypName objfile typ
  return $ gets ++ sets

genGetter :: String -> String -> TypeQ -> String -> Type -> Q [Dec]
genGetter tname name ptrTypName objfile typ = do
  let (typeKind, ftype, _ctype) = typeinfo typ
  case typeKind of
    Vector -> do
      x <- cplusplus
            (tname ++ "::" ++ name ++ "(int) const") objfile
            (appT (appT arrowT ptrTypName) (appT (appT arrowT (conT ''Int)) (appT (conT ''IO) (return ftype))))
      y <- cplusplus
            (tname ++ "::" ++ name ++ "_size() const") objfile
            (appT (appT arrowT ptrTypName) (appT (conT ''IO) (conT ''Int)))
      return (x ++ y)
    MaybeP _typ -> do
      g <- cplusplus
            (tname ++ "::" ++ name ++ "() const")    objfile
            (appT (appT arrowT ptrTypName) (appT (conT ''IO) (return ftype)))
      s <- cplusplus
            (tname ++ "::has_" ++ name ++ "() const")    objfile
            (appT (appT arrowT ptrTypName) (appT (conT ''IO) (conT ''Bool)))
      return (g ++ s)
    -- Assign and Pointer
    _ -> cplusplus
      (tname ++ "::" ++ name ++ "() const")    objfile
      (appT (appT arrowT ptrTypName) (appT (conT ''IO) (return ftype)))

genSetter :: String -> String -> TypeQ -> String -> Type -> Q [Dec]
genSetter tname name ptrTypName objfile typ = do
  let (typeKind, ftype, ctype) = typeinfo typ
      isAllocated Pointer n = "allocated_" ++ n
      isAllocated _ n = n
  case typeKind of
    Assign -> do
      s <- cplusplus
            (tname ++ "::set_" ++ name ++ "(" ++ ctype ++ ")") objfile
            (appT (appT arrowT ptrTypName)
              (appT (appT arrowT (return ftype)) (appT (conT ''IO) (conT ''()))))
      al <- if ftype == ConT ''Foreign.CPlusPlusStdLib.Std__basic_string
              then sequence [allocated_set tname name "set"]
              else return []
      return (s ++ al)
    Vector -> do
      s <- cplusplus
          (tname ++ "::add_" ++ name ++ "(" ++ ctype ++ ")") objfile
            (appT
              (appT arrowT ptrTypName)
              (appT (appT arrowT (return ftype)) (appT (conT ''IO) (conT ''()))))
      al <- if ftype == ConT ''Foreign.CPlusPlusStdLib.Std__basic_string
              then sequence [allocated_set tname name "add"]
              else return []
      return (s ++ al)
    Pointer -> cplusplus
        (tname ++ "::set_allocated_" ++ name ++ "(" ++ ctype ++ ")") objfile
        (appT (appT arrowT ptrTypName) (appT (appT arrowT (return ftype)) (appT (conT ''IO) (conT ''()))))
    MaybeP typ' -> do
      set <- cplusplus
        (tname ++ "::set_" ++ isAllocated typ' name ++ "(" ++ ctype ++ ")") objfile
        (appT (appT arrowT ptrTypName) (appT (appT arrowT (return ftype)) (appT (conT ''IO) (conT ''()))))
      al <- if ftype == ConT ''Foreign.CPlusPlusStdLib.Std__basic_string
              then sequence [allocated_set tname name "set"]
              else return []
      clear <- cplusplus
        (tname ++ "::clear_" ++ name ++ "()") objfile
        (appT (appT arrowT ptrTypName) (appT (conT ''IO) (conT ''())))
      return (set ++ al ++ clear)

allocated_set :: String -> String -> String -> DecQ
allocated_set tname name meth =
  (funD (mkName (lower tname ++ "__" ++ meth ++  "_ret_" ++ name))
    [clause [varP (mkName "p"), varP (mkName "v")]
      (normalB
        (appE
          (appE (varE 'withForeignPtr) (varE (mkName "p")))
          (lamE [varP (mkName "pv")]
            (infixE
              (Just
                (appE
                  (appE
                    (varE (mkName (lower tname ++ "__" ++ meth ++ "_" ++ name)))
                    (varE (mkName "pv")))
                  (varE (mkName "v"))))
              (varE '(>>))
              (Just
                (appE
                  (appE
                    (varE 'retainForeign)
                    (varE (mkName "p")))
                  (varE (mkName "v"))))
        )))) []])

typeinfo :: Type -> (TypeKind, Type, String)
typeinfo t
  | t == ConT ''String = (Assign, ConT ''Std__basic_string, "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > const&")
  | t == ConT ''Bool   = (Assign, ConT ''CChar, "bool")
  | t == ConT ''Int    = (Assign, ConT ''CInt, "int")
  | t == ConT ''Int32  = (Assign, ConT ''CInt, "int")
  | t == ConT ''Int64  = (Assign, ConT ''CLLong, "long long")
  | t == ConT ''Word32  = (Assign, ConT ''CUInt, "unsigned int")
  | t == ConT ''ByteString  = (Assign, ConT ''Std__basic_string, "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > const&")
  | otherwise =
    case t of
      ConT x ->
        if last (nameBase x) == 'T'
          then
            -- TODO: reify x to check it is in Protobuf, that way it can be extended by client code, like enum
            (Pointer, AppT (ConT ''Ptr) (ConT (mkName (init (nameBase x)))), init (nameBase x) ++ "*")
          else
            (Assign, t, nameBase x)
            -- error $ "unknown type to typeinfo: " ++ nameBase x
      AppT (ConT innerT) x -> typeinfo' innerT x
      AppT ListT x -> typeinfoList x
      _ -> error $ "type info not implemented for: " ++ show t
  where
    typeinfo' innerT x
      | innerT == ''Maybe = let (i, y, z) = typeinfo x in (MaybeP i, y, z)
        -- possible: | innerT == ''V.Vector = let (_, y, z) = typeinfo x in (Vector, y, z)
      | otherwise = error $ "type info not implemented for: " ++ show innerT
    typeinfoList x =
      let (_, y, z) = typeinfo x in (Vector, y, z)

lower :: String -> String
lower (x:xs) = toLower x:xs
lower _ = error "NEL"