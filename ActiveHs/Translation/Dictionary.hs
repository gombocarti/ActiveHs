module ActiveHs.Translation.Dictionary (
      Dictionary
    , hunDict
    , lookupDict
    ) where

import           ActiveHs.Translation.Base (Translation, (<|), translationText, translationId)
import qualified ActiveHs.Translation.Entries as E

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

------------

newtype Dictionary = Dictionary { unDictionary :: Map.HashMap Int Translation }

lookupDict :: Translation -> Dictionary -> Maybe T.Text
lookupDict trans dict =
  case Map.lookup (translationId trans) (unDictionary dict) of
    Nothing -> Nothing
    Just translation -> Just (translationText translation)

entriesToMap :: [Translation] -> Map.HashMap Int Translation
entriesToMap entries = Map.fromList [(translationId trans, trans) | trans <- entries]

hunDict :: Dictionary
hunDict = Dictionary $ entriesToMap
  [ E.msg_Converter_ErroneousEval <| "Hibás kiértékelés"
  , E.msg_Eval_CantCompareDiagrams <| "Lehet, hogy jó, sajnos nem tudom az ábrák egyezőségét ellenőrizni."
  , E.msg_Eval_CantDecide <| "Nem tudom eldönteni, hogy jó megoldás-e:"
  , E.msg_Eval_DontKnowHowToEvaluate <| "Sajnos nem tudom, miként kell kiértékelni ezt a kifejezést, de a típusát meg tudom mondani: %s"
  , E.msg_Eval_ErroneousEval <| "Hibás kiértékelés"
  , E.msg_Eval_Error <| "Hibába botlottam kiértékelés közben."
  , E.msg_Eval_GhcException <| "Kivétel keletkezett a GHC-ban"
  , E.msg_Eval_GoodSolution <| "Jó megoldás! Szintén helyes megoldás:"
  , E.msg_Eval_NotAllowed <| "Nem megengedett"
  , E.msg_Eval_NoHoogleInfo <| "Nincs erről információ: %s"
  , E.msg_Eval_NotSupported <| "A %s parancs itt nem támogatott"
  , E.msg_Eval_SpecializeError <| "Belső hiba típusellenőrzés közben. Sajnálom."
  , E.msg_Eval_UnknownError <| "Ismeretlen hiba"
  , E.msg_Eval_WontCompile <| "Nem fordítható"
  , E.msg_Eval_WrongSolution <| "Nem jó megoldás:"
  , E.msg_Hoogle_NoInfo <| "Nincs erről információ: %s"
  , E.msg_Test_AllCompleted <| "Minden általam ismert tesztesetnek megfelelt!"
  , E.msg_WebServer_Inconsistency <| "Inkonzisztencia a kliens és a szerver között. Próbáld meg újratölteni az oldalt!"
  ]


{-

langTable :: [[String]]
langTable =
    [ ["en", "hu"]
    , ["Erroneous evaluation", "Hibás kiértékelés"]
    , ["Test", "Teszt"]
    , ["Solution", "Megoldás"]
    , ["Check", "Ellenőrzés"]
    , ["The", "A"]
    , ["command is not supported", "parancs itt nem támogatott"]
    , ["Erroneous solution", "Hibás megoldás"]
    , ["Interrupted evaluation.", "Félbehagyott kiértékelés."]
    , ["Inconsistency between server and client. Try to reload the page.", "Inkonzisztencia a kliens és a szerver között. Próbáld meg újratölteni az oldalt!"]
    , ["The site is overloaded.", "Az oldal leterhelt."]
    , ["Too long request.", "Túl hosszú kérés."]
    , ["Inconsistency between server and client.", "Inkonzisztencia a kliens és a szerver között."]
    , ["All test cases are completed.", "Minden általam ismert tesztesetnek megfelelt!"]
    , ["Good solution! Another good solution:", "Jó megoldás! Szintén helyes megoldás:"]
    , ["I cannot decide whether this is a good solution:", "Nem tudom eldönteni, hogy jó megoldás-e:"]
    , ["Wrong solution:", "Nem jó megoldás:"]
    , ["Unknown error: ", "Ismeretlen hiba: "]
    , ["Not allowed: ", "Nem engedélyezett: "]
    , ["GHC exception: ", "GHC kivétel: "]
    , ["Can't decide the equality of diagrams (yet).", "Lehet hogy jó, sajnos nem tudom az ábrák egyezőségét ellenőrizni (most még)."]
    , ["No info for ", "Nincs erről információ: "]
    ]

-}
