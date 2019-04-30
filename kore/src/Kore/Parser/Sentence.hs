{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

-}

module Kore.Parser.Sentence
    ( ParsedSentenceSort
    , ParsedSentenceSymbol
    , ParsedSentenceAlias
    , ParsedSentenceImport
    , ParsedSentenceAxiom
    , ParsedSentenceHook
    , ParsedSentence
    , ParsedModule
    , ParsedDefinition
    -- * Re-exports
    , module Kore.AST.Sentence
    ) where

import Kore.AST.MetaOrObject
import Kore.AST.Sentence
import Kore.Parser.Pattern

type ParsedSentenceSort = SentenceSort Object (Pattern Variable)

type ParsedSentenceSymbol = SentenceSymbol Object (Pattern Variable)

type ParsedSentenceAlias = SentenceAlias Object (Pattern Variable)

type ParsedSentenceImport = SentenceImport (Pattern Variable)

type ParsedSentenceAxiom = SentenceAxiom SortVariable (Pattern Variable)

type ParsedSentenceHook = SentenceHook (Pattern Variable)

type ParsedSentence = Sentence Object SortVariable (Pattern Variable)

type ParsedModule = Module ParsedSentence

type ParsedDefinition = Definition ParsedSentence
