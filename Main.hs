{-# LANGUAGE UnicodeSyntax, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           CalculiTester.FileReader
import           CalculiTester.Parser
import           CalculiTester.Printer
import           CalculiTester.Prover
import           CalculiTester.TypeDef
import           CalculiTester.Unifier
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Either (isRight, partitionEithers)
import           Data.Foldable
import           Data.List (partition, intercalate)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Monoid (First(..))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Tree
import           Data.Typeable
import           System.Console.Haskeline
import           System.Exit
import           System.IO
import           System.IO.Error
import           Text.Read (readMaybe)

--obvious MonadState instance for InputT
instance MonadState s m => MonadState s (InputT m) where
  get = lift get 
  put = lift . put

--the main monad
type CTIO a = InputT (StateT CTState IO) a

type Proof = Tree RuleName

--internal state of the program
data CTState = CTState
  {
    --used to provide fresh names during proof process
    _namer :: Namer,
    --the deductive theory
    _theoryV :: Theory Text,
    --the fixity information
    _fixityV :: Fixity,
    --path to the file to save proofs
    _proofsFile :: FilePath,
    --current list of goals during proof process
    _goals :: [Rec Text],
    --function to transform future proofs of the current goals
    --into the proof of the initial goal
    _proofMaker :: [Proof] → Proof,
    --name of theory used currently
    --used in the prompt
    _theoryName :: String,
    --name of theorem currently being proved
    --used in the prompt
    _theoremName :: String,
    --theorems database
    _theorems :: Map String Proof
  }
  --deriving(Show)

--variable used in the prove process are undefined outside of it
initCTState :: CTState
initCTState = CTState {
  _namer = undefined,
  --no rules initially
  _theoryV = mempty,
  --no operator initially
  _fixityV = mempty,
  --file to save proofs is unknown initially
  _proofsFile = "",
  _goals = undefined,
  _proofMaker = undefined,
  --theory is initially nameless
  _theoryName = "",
  _theoremName = undefined,
  --initially no theorems
  _theorems = mempty
                      }

makeLenses ''CTState

--function to run the program
runCT :: CTIO a → IO (a, CTState)
runCT m = runStateT (runInputT defaultSettings m) initCTState

--costruct prompt for the main mode
prompt :: CTIO String
prompt =
  use theoryName <&> \case
  "" → "CT> "
  s → s ++ "> "

--read the command, execute it print the response
readAndRespond :: CTIO ()
readAndRespond =
  prompt >>= getInputLine >>= \case
  Nothing → exitCT
  Just l → do
    let command = readCommand l
    response ← execute command `catch` pure
    outputStrLn $ showResponse response ""

--command parser
readCommand :: String → Command
readCommand s = case words s of
  "Load":ws → case ws of
                "theory":ws → LoadTheory $ unwords ws
                "proofs":ws → LoadProofs $ unwords ws
                _ → UndefCommand $ unwords ws
  "Theorem":ws → 
      case ws of
        w:":":ws → Prove w $ T.unwords (fmap T.pack ws)
        w:ws → if last w == ':'
               then Prove (init w) $ T.unwords (fmap T.pack ws)
               else UndefCommand $ unwords (w:ws)
        _ → UndefCommand $ unwords ws
  "Show":"proof":theorem:_ → ShowProof theorem
  "Draw":"proof":theorem:ws → case ws of
                                [] → DrawProof theorem Nothing
                                ws → DrawProof theorem (Just $ unwords ws)
  "Save":ws → case ws of
                "proofs":ws → SaveProofs ws
                _ → UndefCommand $ unwords ws
  "Exit":_ → CTExit
  ws → UndefCommand $ unwords ws

--contains data about the command to be executed
data Command =
  --LoadTheory file : load theory from the file "file"
  LoadTheory FilePath |
  --Prove name expr : prove the theorem named "name",
  --expressed with expression "expr"
  Prove String Text |
  --ShowProof name : show proof of the theorem "name"
  ShowProof String |
  --DrawProof name : draw proof of the theorem "name"
  --as a proof tree
  --DrawProof name file : save the drawing to the file "file"
  DrawProof String (Maybe FilePath) |
  --LoadProofs file : Load the proofs from the file "file"
  LoadProofs FilePath |
  --SaveProofs names : Save the proofs with names from "names"
  SaveProofs [String] |
  --UndefCommand s : parsing of command has failed,
  --s in the unparsed part
  UndefCommand String |
  --CTExit : exit the program
  CTExit
  deriving (Show)

--executes the command and returns a response,
--may also throw a response
execute :: Command → CTIO Response
--response : TheoryLoaded,
--may throw : NotFound and FileReadingErr
execute (LoadTheory file) = let
  --executes m if file doesn't exists
  handler m e = if isDoesNotExistError e
                then m
                else throwIO e
  in do
  --tries to load file and file.th, if both are unsuccessful
  --throws NotFound file
  text ← liftIO (T.readFile file) `catch`
         handler (liftIO (T.readFile (file ++ ".th")) `catch`
                  handler (throwIO (NotFound file)))
  --tries to read from the file
  ((name, fixity), theory) ← either (throwIO . FileReadingErr) pure $
                     loadTheory $ T.lines text
  --saved the read information
  fixityV .= fixity
  theoryV .= theory
  theoryName .= name
  --clear the theory dependent data
  theorems .= mempty
  proofsFile .= ""
  --reports that everything was loaded
  pure $ TheoryLoaded (name, file)
--responce: ProofSuccess
--may throw: Malformed and ProofCancelled
execute (Prove theorem text) = do
  fixity ← use fixityV
  --parse the theorem expression
  case recParser fixity text of
    Left err → pure $ Malformed text err
    Right expr → do
      --prepare the state for the prover
      theoremName .= theorem
      --the only goal is the theorem's conclusion
      goals .= [expr]
      --variables in the conclusion are not fresh
      namer .= M.fromList (zip (listVars expr) (repeat 0))
      --proofMaker just returns the proof
      proofMaker .= \[x] → x
      --call the prover
      proverLoop
      --regenerate the proof
      proof ← use proofMaker <&> ($ [])
      --undefine internal prover variables
      theoremName .= undefined
      goals .= undefined
      namer .= undefined
      proofMaker .= undefined
      --save the proof
      theorems . at theorem .= Just proof
      --report the successful proof
      pure $ ProofSuccess theorem
--response : ToPrint or TheoremNotFound
execute (ShowProof theorem) =
  use (theorems . at theorem) <&> \case
  Nothing → TheoremNotFound theorem
  Just proof → ToPrint $ drawTree (T.unpack <$> proof)
--response : ToPrint or PrintedProof
--may throw : TheoremNotFound
execute (DrawProof theorem maybeFile) =
  use (theorems . at theorem) >>= \case
  --check if theorem exists
  Nothing → throwIO $ TheoremNotFound theorem
  Just proof → do
    fixity ← use fixityV
    theory ← use theoryV
    --prepare tree for the drawing
    case decipherTree theory proof of
      Nothing→ error $ "DrawProof: incorrect proof for " ++ show theorem
      Just tree →
        --draw the proof tree
        let image = printTree fixity (zipTree proof tree) "" in
          --either return the image or print it to the file
          case maybeFile of
            Nothing → pure $ ToPrint image
            Just file → do
              traverse (liftIO . appendFile file)
                [theorem,"\n",image,"\n"]
              pure $ PrintedProof theorem file
--response : ProofsSorted
--may throw : NotFound, NotProofFile
execute (LoadProofs file) = do
  readProofsFromFile file >>= \case
    Nothing → throwIO $ NotProofFile file
    Just assocs → do
      theory ← use theoryV
      --separate the theorems with correct proofs
      let (proven, notProven) =
            flip partition assocs $ \(_,proof) →
            isJust (decipherTree theory proof)
      --save the proven theorem
      theorems %= (flip mappend $ M.fromList proven)
      --report which theorems are loaded
      pure $ ProofsSorted (fst <$> notProven) (fst <$> proven)
--response : SavedExcept
--may throw : Cancelled
execute (SaveProofs names) = do
  --check if the name of the file is saved,
  --if not aks the user
  file ← use proofsFile >>= \case
    "" → getInputLine "Enter file name: " >>= \case
      Nothing → throwIO $ Cancelled
      Just file → pure file
    file → pure file
  --save the name of the file
  proofsFile .= file
  --find which theorems exists
  (notProven, proven) ← use theorems <&> \theorems →
    case names of
      [] → ([], M.toList theorems)
      names → 
        partitionEithers $ names <&> \name →
        case M.lookup name theorems of
          Nothing → Left name
          Just proof → Right (name, proof)
  --try to read the file
  readProofsFromFile file `catch` (\(NotFound _) → pure Nothing) >>= \case
    --if failed overwrite
    Nothing → liftIO $ writeFile file $ show proven
    --if succeeded unite and overwrite
    Just alreadySaved →
      liftIO $ writeFile file $ show
      (M.toList $ mappend (M.fromList alreadySaved) (M.fromList proven))
  --report which theorems are saved
  pure $ (SavedExcept (fst <$> proven) notProven file)
--report non-recognised substring
execute (UndefCommand s) = pure $ NotRecognised s
--exit
execute CTExit = exitCT

--tries to read a file, may throw NotFound
readProofsFromFile :: FilePath → CTIO (Maybe [(String, Proof)])
readProofsFromFile file =
  liftIO (readFile file <&> readMaybe) `catch` \err →
  if isDoesNotExistError err
  then throwIO $ NotFound file
  else throwIO err

--contains data about the result of the command
data Response =
  --Errors
  --NotFound file : "file" doesn't exist
  NotFound FilePath |
  --FileReadingErr err : error "err" arose while reading a file
  FileReadingErr FileReaderError |
  --Malformed s err : parsing s gave error "err"
  Malformed Text CTParserError |
  --UnifierErr err : unification failed with "err"
  UnifierErr (UnifierError Text) |
  --MissingRule name : rule "name" doesn't exists in the theory
  MissingRule RuleName |
  --TheoremNotFound name : theorem "name" doesn't exists in the database
  TheoremNotFound String |
  --NotProofFile file : reading proofs from the text of "file" failed
  NotProofFile FilePath |
  --NotRecognised s : substring s in the command couldn't be parsed
  NotRecognised String |
  --FailT tactic was invoked
  TacticFailed |
  --Successes
  --TheoryLoaded (name, file) : theory "name" was loaded from "file"
  TheoryLoaded (String, FilePath) |
  --ProofCancelled name : proof of the theorem "name" was cancelled
  ProofCanceled String |
  --ProofSuccess name : theorem "name" was successfully proven
  ProofSuccess String |
  --ToPrint s : s should be shown
  ToPrint String |
  --PrintedProof name file : proof tree of "name" was drawn into "file"
  PrintedProof String FilePath |
  --ProofsSorted proven notProven :
  --theorems with names from "proven" are correct and were loaded
  --theorems with names from "notProven" are incorrect and were not loaded
  ProofsSorted [String] [String] |
  --Cancelled : the command was cancelled
  Cancelled |
  --SavedExcept found notFound file:
  --theorems with names from "found" were saved
  --theorems with names from "not found" don't exists
  SavedExcept [String] [String] FilePath
  deriving (Show, Typeable)

instance Exception Response

--converts response into a string
showResponse :: Response → ShowS
showResponse (NotFound file) =
  showString "Error: file " . showString file .
  showString " does not exists"
showResponse (FileReadingErr (RuleFieldError l parseErr)) =
  case parseErr of
    TokenizerError c →
      showString "Error: unexpected token" .
      showString " at " . shows l . showChar ':' . shows c
    CTParserError err →
      showString "Error: parser error" .
      showString " at line " . shows l . showString ":\n" .
      shows err
showResponse (TheoryLoaded (name, file)) =
  case name of
    "" → showString "Theory loaded from " . showString file
    _ → showString "Loaded theory " . showString name
showResponse (Malformed s err) =
  case err of
    TokenizerError c →
      showString "Error: unexpected token at position " .
      shows c . showString " in " . shows s
    CTParserError err →
      showString "Error: parser error in " . shows s . showString ":\n" .
      shows err
showResponse (ProofCanceled theorem) =
  showString "Theorem " . showString theorem . showString " discarded"
showResponse (ProofSuccess theorem) =
  showString "Theorem " . showString theorem . showString " saved"
showResponse (UnifierErr err) = showUnifierErr err
showResponse (MissingRule rulename) =
  showString "Rule " . shows rulename . showString " not found"
showResponse (TheoremNotFound theorem) =
  showString "Error: theorem " . showString theorem . showString " not found"
showResponse (ToPrint s) = showString s
showResponse (PrintedProof theorem file) =
  showString "The proof of " . showString theorem .
  showString " was printed to the file " . showString file
showResponse (NotProofFile file) =
  showString "Error: cannot parse contents of the file" . showString file
showResponse (ProofsSorted notProven proven) =
  (if null notProven
   then id
   else showString "Incorrect proofs of: " .
        showString (intercalate ", " notProven) . showString "\n") .
  showString "Loaded proofs of: " .
  showString (intercalate ", " proven)
showResponse Cancelled =
  showString "Command canceled"
showResponse (SavedExcept proven notProven file) =
  showString "Theorems: " .
  showString (intercalate ", " proven) .
  showString " saved to file " . showString file .
  if null notProven
  then id
  else showString "\nTheorems: " .
       showString (intercalate ", " notProven) .
       showString " not Found"
showResponse TacticFailed = showString "Tactic failed"
showResponse (NotRecognised s) =
  showString "Error: " . shows s . showString "cannot be recognised"

--helper function for unifier errors
showUnifierErr :: UnifierError Text → ShowS
showUnifierErr (NotInitialized x) =
  error $ "variable " ++ T.unpack x ++ " not initialized"
showUnifierErr (IncompabitibleAtoms a a') =
  showString "Error: Can't unify atoms " .
  showString (T.unpack a) . showString " and " . showString (T.unpack a')
showUnifierErr (ConflictingArity a) =
  showString "Error: Unexpected number of arguments for atom" .
  showString (T.unpack a)
showUnifierErr (CyclicVar x) =
  showString "Error: Unification requires " . showString (T.unpack x) .
  showString " to depend on itself"

--exit function
exitCT :: CTIO a
exitCT = liftIO exitSuccess

--construct prompt for the prover mood
proverPrompt :: CTIO String
proverPrompt = do
  theoryName ← use theoryName
  theoremName ← use theoremName
  pure $ (case theoryName of
            "" → "CT"
            name → name)
    ++
    (case theoremName of
       "" → " prover> "
       name → ": " ++ name ++ "> ")

--call prover until list of goals is empty
proverLoop :: CTIO ()
proverLoop =
  use goals >>= \case
  [] → pure ()
  _ → do
    prover
    proverLoop

--show Goals, read a tactic, execute it and, possibly show response
--all responses here are errors, normal operation shouldn't
--produce responses
prover :: CTIO ()
prover = do
  showGoals
  proverPrompt >>= getInputLine >>= \case
    Nothing → use theoremName >>= throwIO . ProofCanceled
    Just l → do
      let tactic = readTactic l
      executeTactic tactic `catch` \response →
        outputStrLn $ showResponse response ""

--tactic parser
readTactic :: String → Tactic
readTactic s = case words s of
  "idtac":_ → IdTac
  "apply":rulename:_ → Apply $ T.pack rulename
  "hyp":ws → case ws of
    [] → hypTactic "hyp"
    w:_ → hypTactic $ T.pack w
  "repeat":w:ws → case readMaybe w :: Maybe Int of
                    Nothing → repeatTacticInf $ readTactic (unwords (w:ws))
                    Just n → repeatTactic n $ readTactic (unwords ws)
  ws → UndefTactic $ unwords ws

--construct a tactic that repeats t n times and reverts state on failure
repeatTactic :: Int → Tactic → Tactic
repeatTactic n t = SeqT $ replicate n t

--construct a tactic that repeats t until failure
--limited to 255 repeats to avoid an infinite loop
repeatTacticInf :: Tactic → Tactic
repeatTacticInf t = repeat 255 t where
  repeat 0 t = IdTac
  repeat n t = IfT t (repeat (n - 1) t) IdTac

--constructs interpetation of tactic hyp
hypTactic :: Text → Tactic
hypTactic name = hypTactic' 255 name where
  hypTactic' 0 _ = FailT
  hypTactic' n name =
    IfT (Apply $ mappend name "_head")
    IdTac
    (SeqT [Apply $ mappend name "_tail", hypTactic' (n - 1) name])

data Tactic =
  --do nothing
  IdTac |
  --match conclusion with current goal, replace it with premises
  Apply RuleName |
  --IfT t1 t2 t3, try t1, if is succeeds do t2, otherwise revert and
  --do t3
  IfT Tactic Tactic Tactic |
  --execute a sequence of tactics, revert on failure
  SeqT [Tactic] |
  --parsing of a tactic failed
  FailT |
  UndefTactic String

--executes the tactic by modifying goals and proofMaker,
--in case of an error, state should stay as it was before the execution,
--unless stated otherwise
executeTactic :: Tactic → CTIO ()
--do nothing
executeTactic IdTac = pure ()
--new goals : conclusion of the rule is replaced premises of the rule
--new proofMaker : collects the proofs of the new goals in the new argument
--may throw : MissingRule, UnifierErr
executeTactic (Apply rulename) = do
  use goals >>= \case
    --if no goals do nothing
    [] → pure ()
    goal:rest → do
      --try to lookup the rule
      Rule concl premises ← use theoryV <&> M.lookup rulename >>= \case
        Nothing → throwIO $ MissingRule rulename
        --return the rule with fresh variables
        Just rule → freshVars namer rule
      --try to unify the conclusion with the goal
      store ← case unify goal concl of
        Left err → throwIO $ UnifierErr err
        Right store → pure store
      --replace the goal with the premises
      addGoals premises
      --apply results of the unification to all goals
      goals %= fmap (eval store)
      --new proofMaker collect the proofs of the premises,
      --collects them with the head "rulename" and
      --pass it as the first argument to the old proofMaker
      proofMaker %= \f ps → let
        l = length premises
        (new, old) = splitAt l ps
        in f (Node rulename new : old)
--new goals and proofMaker : the same effect as manual execution
--of the sequence
--may throw : the same as tactics in the sequence
executeTactic (SeqT ts) = do
  traverse_ executeTactic ts 
--new goals and proofMaker : if t succeds, the same as SeqT [t, tT],
--otherwise, the same as tF
--may throw : the same as tT and tF
executeTactic (IfT t tT tF) =
  (executeTactic t >> pure True) `catch`
  (\(err :: Response) → executeTactic tF >> pure False) >>= \b →
  when b (executeTactic tT)
--always error
executeTactic FailT = throwIO TacticFailed
executeTactic (UndefTactic s) = throwIO (NotRecognised s)

--replaces the first goal with new goals
addGoals :: [Rec Text] → CTIO ()
addGoals newGoals = goals %= \case
  [] → error "addGoals: List of goals is empty"
  (x:xs) → newGoals ++ xs

--shows the list of goals, the first goal is displayed more prominently
showGoals :: CTIO ()
showGoals = do
  fixity ← use fixityV
  goals ← use goals
  --calculate goals' string
  let goalsS = printRec fixity <$> goals
  case goalsS of
    [] → outputStrLn "All goals eliminated"
    (x:xs) → do
      --print first goal and underline it
      outputStrLn ""
      outputStrLn (x "")
      outputStrLn (replicate (max 40 (length (x ""))) '─')
      --print other goals with indentation
      traverse (\x → outputStrLn (showChar '\t' . x $ "")) xs >> pure ()

main =
  let loop = do
        readAndRespond
        loop
  in runCT loop
