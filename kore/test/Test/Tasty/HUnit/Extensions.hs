module Test.Tasty.HUnit.Extensions where

import Test.Tasty.HUnit
       ( assertBool, assertFailure )

import           Control.DeepSeq
                 ( NFData, force )
import           Control.Exception
                 ( SomeException, catch, evaluate )
import           Control.Monad
import           Data.CallStack
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import           Data.Functor.Const
                 ( Const (..) )
import           Data.Functor.Foldable
import           Data.List
                 ( intercalate, isInfixOf )
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import           Data.Sequence
                 ( Seq )
import           Data.Set
                 ( Set )
import qualified Data.Set as Set
import           Data.Text
                 ( Text )
import           Data.Void
                 ( Void )

import Data.Sup

assertEqualWithPrinter
    :: (Eq a, HasCallStack)
    => (a -> String)
    -> String -- ^ The message prefix
    -> a      -- ^ The expected value
    -> a      -- ^ The actual value
    -> IO ()
assertEqualWithPrinter printer preface expected actual =
    unless (actual == expected) (assertFailure msg)
  where
    msg =
        (if null preface then "" else preface ++ "\n")
        ++ "expected: " ++ printer expected ++ "\n but got: " ++ printer actual

assertInListP
    :: (Eq a, HasCallStack)
    => (a -> String)
    -> String -- ^ The message prefix
    -> [a]    -- ^ The expected value list
    -> a      -- ^ The actual value
    -> IO ()
assertInListP printer message expectedList actual =
    foldr
        (\ expected -> unless (expected == actual))
        (assertFailure ("No match for: " ++ printer actual ++ "\n" ++ message))
        expectedList

assertInList
    :: (Eq a, Show a, HasCallStack)
    => String -- ^ The message prefix
    -> [a]    -- ^ The expected value list
    -> a      -- ^ The actual value
    -> IO ()
assertInList = assertInListP show

assertError :: (HasCallStack, NFData a) => (String -> IO()) -> a -> IO()
assertError errorTest action = do
    maybeErr <-
        catch
            (do
                _ <- evaluate $ force action
                return Nothing
            )
            (\err -> return (Just (show (err :: SomeException))))
    case maybeErr of
        Nothing  -> assertFailure "No error during action."
        Just err -> errorTest err

assertErrorIO :: (HasCallStack, NFData a) => (String -> IO()) -> IO a -> IO()
assertErrorIO errorTest action = do
    maybeErr <-
        catch
            (do
                value <- action
                _ <- evaluate $ force value
                return Nothing
            )
            (\err -> return (Just (show (err :: SomeException))))
    case maybeErr of
        Nothing  -> assertFailure "No error during action."
        Just err -> errorTest err

assertSubstring :: HasCallStack => String -> String -> String -> IO()
assertSubstring message first second =
    assertBool
        (  message
        ++ ": '"
        ++ first
        ++ "' is not a substring of '"
        ++ second
        ++ "'"
        )
        (first `isInfixOf` second)

{-| 'EqualWithExplanation' is a class for objects that can be compared for
equality, and for which an explanation of an equality failure is desired.

This can be used with, e.g., assertEqualWithExplanation.
-}
class EqualWithExplanation a where
    {-| 'compareWithExplanation' compares two values, returning Nothing
    if they are equal or (Just explanation) if they are different.

    This explanation is assumed to be a human readable representation of the
    two input values that highlights why they are not equal. As an example,
    whn comparing (a, b) with (a, c), this function may return
    (..., b <vs> c).
    -}
    compareWithExplanation :: a -> a -> Maybe String
    {-| 'printWithExplanation' should display the data passed to it.
    TODO: Consider removing it and using 'show'.
    -}
    printWithExplanation :: a -> String

assertEqualWithExplanation
    :: (EqualWithExplanation a, HasCallStack)
    => String -- ^ The message prefix
    -> a      -- ^ The expected value
    -> a      -- ^ The actual value
    -> IO ()
assertEqualWithExplanation prefix expected actual =
    case compareWithExplanation expected actual of
        Just explanation ->
            assertFailure
                ((if null prefix then "" else prefix ++ "\n") ++ explanation)
        Nothing -> pure ()

data EqWrap = forall a . EqualWithExplanation a => EqWrap String a a

formatDiffForExplanation :: String -> String -> String
formatDiffForExplanation expected actual =
    "\n    " ++ expected ++"\n***vs***\n    " ++ actual ++ "\n"

compareListWithExplanation :: [EqWrap] -> Maybe String
compareListWithExplanation l =
    case foldl compareListElement (Right []) l of
        Right _   -> Nothing
        Left diff -> Just (intercalate ", " (reverse diff))
  where
    compareListElement
        :: Either [String] [String]
        -> EqWrap
        -> Either [String] [String]
    compareListElement (Left diff) _ =
        Left ("..." : diff)
    compareListElement (Right same) (EqWrap prefix x y) =
        case compareWithExplanation x y of
            Just diff -> Left ((prefix ++ " " ++ diff) : same)
            Nothing   -> Right ("..." : same)

rawCompareWithExplanation :: (Eq a, Show a) => a -> a -> Maybe String
rawCompareWithExplanation expected actual =
    if expected /= actual
        then Just $ formatDiffForExplanation (show expected) (show actual)
        else Nothing

instance (EqualWithExplanation a1, EqualWithExplanation a2)
    => EqualWithExplanation (a1, a2)
  where
    compareWithExplanation (a, b) (c, d) =
        case compareListWithExplanation [EqWrap "" a c, EqWrap "" b d] of
            Just err -> Just ("(" ++ err ++ ")")
            Nothing  -> Nothing
    printWithExplanation (a, b) =
        "(" ++ printWithExplanation a ++ ", " ++ printWithExplanation b ++ ")"

instance
    ( EqualWithExplanation a1
    , EqualWithExplanation a2
    , EqualWithExplanation a3
    )
    => EqualWithExplanation (a1, a2, a3)
  where
    compareWithExplanation (a, b, c) (d, e, f) =
        case
            compareListWithExplanation
                [EqWrap "" a d, EqWrap "" b e, EqWrap "" c f]
        of
            Just err -> Just ("(" ++ err ++ ")")
            Nothing  -> Nothing
    printWithExplanation (a, b, c) =
        "(" ++ printWithExplanation a
        ++ ", " ++ printWithExplanation b
        ++ ", " ++ printWithExplanation c
        ++ ")"

instance (EqualWithExplanation a1, EqualWithExplanation a2)
    => SumEqualWithExplanation (Either a1 a2)
  where
    sumConstructorPair (Right a1) (Right a2) =
        SumConstructorSameWithArguments (EqWrap "Right" a1 a2)
    sumConstructorPair a1@(Right _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair (Left a1) (Left a2) =
        SumConstructorSameWithArguments (EqWrap "Left" a1 a2)
    sumConstructorPair a1@(Left _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance (EqualWithExplanation a1, EqualWithExplanation a2)
    => EqualWithExplanation (Either a1 a2)
  where
    compareWithExplanation = sumCompareWithExplanation

    printWithExplanation (Right a) = "Right (" ++ printWithExplanation a ++ ")"
    printWithExplanation (Left a)  = "Left (" ++ printWithExplanation a ++ ")"

instance (EqualWithExplanation a)
    => SumEqualWithExplanation (Maybe a)
  where
    sumConstructorPair (Just a1) (Just a2) =
        SumConstructorSameWithArguments (EqWrap "Just" a1 a2)
    sumConstructorPair a1@(Just _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair Nothing Nothing =
        SumConstructorSameNoArguments
    sumConstructorPair a1@Nothing a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance (EqualWithExplanation a)
    => EqualWithExplanation (Maybe a)
  where
    compareWithExplanation = sumCompareWithExplanation

    printWithExplanation (Just a) = "Just (" ++ printWithExplanation a ++ ")"
    printWithExplanation Nothing  = "Nothing"

instance
    EqualWithExplanation a =>
    SumEqualWithExplanation (Sup a)
  where
    sumConstructorPair (Element a1) (Element a2) =
        SumConstructorSameWithArguments (EqWrap "Element" a1 a2)
    sumConstructorPair a1@(Element _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair Sup Sup =
        SumConstructorSameNoArguments
    sumConstructorPair a1@Sup a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance
    EqualWithExplanation a =>
    EqualWithExplanation (Sup a)
  where
    compareWithExplanation = sumCompareWithExplanation

    printWithExplanation (Element a) =
        "Element (" ++ printWithExplanation a ++ ")"
    printWithExplanation Sup  = "Sup"

instance EqualWithExplanation Integer where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation Bool where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation Text where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance {-# OVERLAPPING #-} EqualWithExplanation String where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation a => EqualWithExplanation [a]
  where
    compareWithExplanation expected actual =
        case compareUnequalListWithExplanation expected actual of
            Left _     -> Nothing
            Right errs -> Just ("[" ++ intercalate ", " errs ++ "]")
      where
        compareUnequalListWithExplanation
            :: EqualWithExplanation a => [a] -> [a] -> Either () [String]
        compareUnequalListWithExplanation [] []                = Left ()
        compareUnequalListWithExplanation (expect : es) [] =
            Right
                [ formatDiffForExplanation
                    (printWithExplanation expect
                        ++ intercalate ", " (map (const "...") es))
                    "<nothing>"
                ]
        compareUnequalListWithExplanation [] (act : as) =
            Right
                [ formatDiffForExplanation
                    "<nothing>"
                    (printWithExplanation act
                        ++ intercalate ", " (map (const "...") as))
                ]
        compareUnequalListWithExplanation (expect : es) (act : as) =
            case compareWithExplanation expect act of
                Just diff ->
                    Right [diff, "..."]
                Nothing -> do
                    diff <- compareUnequalListWithExplanation es as
                    return ("..." : diff)

    printWithExplanation a =
        "[" ++ intercalate ", " (map printWithExplanation a) ++ "]"

instance EqualWithExplanation a => EqualWithExplanation (Seq a) where
    compareWithExplanation expected actual =
        compareWithExplanation (Foldable.toList expected) (Foldable.toList actual)

    printWithExplanation = printWithExplanation . Foldable.toList

instance EqualWithExplanation a => EqualWithExplanation (Set a) where
    compareWithExplanation expected actual =
        compareWithExplanation (Set.toList expected) (Set.toList actual)

    printWithExplanation = printWithExplanation . Set.toList

instance
    (EqualWithExplanation a, EqualWithExplanation k) =>
    EqualWithExplanation (Map k a)
  where
    compareWithExplanation expected actual =
        compareWithExplanation (Map.toAscList expected) (Map.toAscList actual)

    printWithExplanation = printWithExplanation . Map.toList

instance (Show (thing (Fix thing)), Show1 thing, EqualWithExplanation (thing (Fix thing)))
    => WrapperEqualWithExplanation (Fix thing)
  where
    wrapperField (Fix a) (Fix b) = EqWrap "" a b
    wrapperConstructorName _ = "Fix"

instance (Show (thing (Fix thing)), Show1 thing, EqualWithExplanation (thing (Fix thing)))
    => EqualWithExplanation (Fix thing)
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation Void where
    compareWithExplanation = \case {}
    printWithExplanation = \case {}

instance
    (EqualWithExplanation a, Show a) =>
    WrapperEqualWithExplanation (Const a b)
  where
    wrapperField (Const a) (Const b) = EqWrap "" a b
    wrapperConstructorName _ = "Const"

instance
    (EqualWithExplanation a, Show a) =>
    EqualWithExplanation (Const a b)
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

data StructEWEField struct = StructEWEField
    { structEWEFieldName :: String
    , structEWEFieldGetter
        :: struct
        -> struct
        -> (forall field . EqualWithExplanation field => (field, field))
    }
{-| 'StructEqualWithExplanation' is a helper class for declaring structs
as instances of 'EqualWithExplanation'
-}
class EqualWithExplanation struct => StructEqualWithExplanation struct where
    structFieldsWithNames :: struct -> struct -> [EqWrap]
    structConstructorName :: struct -> String
    structCompareWithExplanation :: struct -> struct -> Maybe String
    structCompareWithExplanation expected actual
        | expectedConstructor /= actualConstructor
      = error
            (  "Different constructor names! '"
            ++ expectedConstructor
            ++ "' vs '" ++ actualConstructor ++ "'"
            )
      where
        expectedConstructor = structConstructorName expected
        actualConstructor = structConstructorName actual
    structCompareWithExplanation expected actual =
        case
            compareListWithExplanation
                (structFieldsWithNames expected actual)
        of
            Just err ->
                Just (structConstructorName expected ++ " {" ++ err ++ "}")
            Nothing  -> Nothing

class EqualWithExplanation struct => WrapperEqualWithExplanation struct where
    wrapperField :: struct -> struct -> EqWrap
    wrapperConstructorName :: struct -> String
    wrapperCompareWithExplanation :: struct -> struct -> Maybe String
    wrapperCompareWithExplanation expected actual
        | expectedConstructor /= actualConstructor
      = error
            (  "Different constructor names! '"
            ++ expectedConstructor
            ++ "' vs '" ++ actualConstructor ++ "'"
            )
      where
        expectedConstructor = wrapperConstructorName expected
        actualConstructor = wrapperConstructorName actual
    wrapperCompareWithExplanation expected actual =
        case
            compareListWithExplanation [wrapperField expected actual]
        of
            Just err ->
                Just (wrapperConstructorName expected ++ " (" ++ err ++ ")")
            Nothing  -> Nothing

data SumConstructor
    = SumConstructorDifferent String String
    | SumConstructorSameNoArguments
    | SumConstructorSameWithArguments EqWrap
{-| 'SumEqualWithExplanation' is a helper class for declaring sum types
as instances of 'EqualWithExplanation'
-}
class EqualWithExplanation sum => SumEqualWithExplanation sum where
    sumConstructorPair :: sum -> sum -> SumConstructor
    sumCompareWithExplanation :: sum -> sum -> Maybe String
    sumCompareWithExplanation expected actual =
        case sumConstructorPair expected actual of
            SumConstructorDifferent expectedConstructor actualConstructor ->
                Just
                    (formatDiffForExplanation
                        expectedConstructor
                        actualConstructor
                    )
            SumConstructorSameNoArguments -> Nothing
            SumConstructorSameWithArguments eqWrap ->
                case compareListWithExplanation [eqWrap] of
                    Just err -> Just ("(" ++ err ++ ")")
                    Nothing  -> Nothing
