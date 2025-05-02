--getSolfunmeme.lean
import Lean.Data.Json
import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
import Std.Time.Internal
import Std.Time.Time
import Std.Time
import Std.Time.Date
import Lean.Data.Json
import Std.Time.Time.Unit.Second
import Std.Time.DateTime.Timestamp
open Lean Json ToJson FromJson

namespace SolfunmemeLean.Common
-- structure Pubkey where
--   pubkey : String
-- deriving ToJson, FromJson, Inhabited, Repr

-- instance : ToString Pubkey where
--   toString pk := pk.pubkey

-- structure Entry where
--   entry_date : String -- ISO date or timestamp
--   description : String -- Transaction type or details
-- deriving ToJson, FromJson, Inhabited, Repr

-- instance : ToString Entry where
--   toString e := s!"Entry(date: {e.entry_date}, description: {e.description})"

-- structure Ledger where
--   account_name : String -- Token metadata (e.g., mint details)
--   account_number : String -- Token address
--   entries : Array Entry
-- deriving ToJson, FromJson, Inhabited, Repr

-- instance : ToString Ledger where
--   toString l := s!"Ledger(account_name: {l.account_name}, account_number: {l.account_number}, entries: {l.entries})"

def Signature := String
deriving ToJson, FromJson, Inhabited, Repr, ToString

def Slot := Nat
deriving ToJson, FromJson, Inhabited, Repr, ToString, BEq

instance : OfNat Slot n where
  ofNat := n

--{"jsonrpc":"2.0","result":[
--{"blockTime":1745839669,
--"confirmationStatus":"finalized",
--"err":null,
--"memo":null,
--"signature":"2pozgVUc8vbGLeDWAhriwuv7zitKx3GFDmc7TrQEg3saqET18tuxo3ut7HVNQxoCzpXWnjzYN3u9CF5dGSXZG2Tc",

--  [{"slot":
--    314278210,
--    "signature":
--    "4R7qTtTUArspBJ7oE1kjGExKTB8LSSJY5GqBeCMBxr7hMRjejKF2ULCbUgA6r7WYa7sE5ije15evfnRrHRQsQG4a",
--    "memo":
--    null,
--    "err":
--    {"InstructionError":
--     [2,
--      {"Custom":
--       6001}]},
--    "confirmationStatus":
--    "finalized",
--    "blockTime":
--    1736996132},

structure CustomError where
  Custom : Nat
deriving ToJson, FromJson, Inhabited, Repr

--structure InstructionError where
--  index : Nat
--  customCode : Nat
--deriving ToJson, FromJson, Inhabited, Repr


inductive TransactionError where
  | InstructionError : Json -> TransactionError
  --| AccountInUse
  --| InvalidAccountData
deriving ToJson, FromJson, Inhabited

structure TransactionDetails where
  signature : String
  blockTime : Nat
  slot : Slot
  --memo: Option String
  memo : Option String

  --TransactionDetails.err: String expected
  err : Option Json
  confirmationStatus : String -- "finalized",

deriving ToJson, FromJson, Inhabited --, Repr

-- Structure for inner instructions
structure InnerInstruction where
  accounts : List Nat
  data : String
  programIdIndex : Nat
  stackHeight : Option Nat
  deriving ToJson, FromJson, Inhabited, Repr

-- Structure for an inner instruction block
structure InnerInstructionBlock where
  index : Nat
  instructions : List InnerInstruction
  deriving ToJson, FromJson, Inhabited, Repr

-- Structure for loaded addresses
structure LoadedAddresses where
  readonly : List String
  writable : List String
  deriving ToJson, FromJson, Inhabited, Repr

-- Structure for token balance
structure TokenBalance where
  accountIndex : Nat
  mint : String
  owner : String
  programId : String
  uiTokenAmount : Json -- Simplified; could be a detailed structure if needed
  deriving ToJson, FromJson, Inhabited

-- Structure for meta information
structure Meta2 where
  computeUnitsConsumed : Nat
  err : Option String
  fee : Nat
  innerInstructions : List InnerInstructionBlock
  loadedAddresses : LoadedAddresses
  logMessages : List String
  postBalances : List Nat
  postTokenBalances : List TokenBalance
  preBalances : List Nat
  preTokenBalances : List TokenBalance
  rewards : List Json -- Empty in the example, so using Json for flexibility
  status : Json -- Could be a sum type if status has fixed variants
  deriving ToJson, FromJson, Inhabited

-- Structure for instruction
structure Instruction where
  accounts : List Nat
  data : String
  programIdIndex : Nat
  stackHeight : Option Nat
  deriving ToJson, FromJson, Inhabited

-- Structure for message header
structure MessageHeader where
  numReadonlySignedAccounts : Int
  numReadonlyUnsignedAccounts : Int
  numRequiredSignatures : Int
  deriving ToJson, FromJson, Inhabited

-- Structure for message
structure Message2 where
  accountKeys : List String
  addressTableLookups : List Json -- Empty in the example
  header  : MessageHeader
  instructions : List Instruction
  recentBlockhash : String
  deriving ToJson, FromJson, Inhabited

-- Structure for transaction
structure Transaction where
  message : Message2
  signatures : List String
  deriving ToJson, FromJson, Inhabited

-- Main TransactionDetails structure
-- structure TransactionDetails2 where
--    signatures : List String -- Extracted from transaction.signatures[0]
--    blockTime : Nat
--    slot : Slot
--    memo : Option String -- Not present in JSON, so Option String
--    err : Option String -- From meta.err
--    confirmationStatus : String -- Inferred as "finalized"
--    deriving ToJson, FromJson, Inhabited

structure TransactionDetailsResult where
  version : Int -- 0.0.0
  transaction : Transaction
  slot : Int
  meta : Meta2
  blockTime :Int --// : UnixTimestamp
  deriving ToJson, FromJson, Inhabited

structure Error where
    code : Int
    message : String
    deriving ToJson, FromJson, Inhabited

structure TransactionDetailsResult2 where
  result : Option TransactionDetailsResult
  error : Option Error
  deriving ToJson, FromJson, Inhabited
  --toString (resp :TransactionDetailsResp):String := s!"TransactionDetailsResp(response: {resp.response.map toString})"

-- Function to convert JSON result to TransactionDetails
-- def TransactionDetails.fromResult (result : Json) : Option TransactionDetails :=
--   do
--     let blockTime ← result.getObjVal? "blockTime" >>= Json.getNat?
--     let slot ← result.getObjVal? "slot" >>= Json.getNat?
--     let meta :Meta2 ← result.getObjVal? "meta" >>= fromJson?
--     let transaction ← result.getObjVal? "transaction" >>= Transaction.fromJson?
--     let signature ← transaction.signatures.get? 0 -- First signature
--     let err ← meta.err -- Already an Option String
--     let confirmationStatus := "finalized" -- Hardcoded as per example
--     pure {
--       signature := signature,
--       blockTime := blockTime,
--       slot := slot,
--       memo := none, -- Not present in JSON
--       err := err,
--       confirmationStatus := confirmationStatus
--     }

-- Example usage (pseudo-code for parsing the JSON)
-- def parseTransactionDetails (json : Json) : Option TransactionDetails :=
--   do
--     let result ← json.getObjVal? "result"
    --TransactionDetails.fromResult result

--.result.map (fun td => s!"{td.signature} {td.blockTime} {td.slot} {td.confirmationStatus}"
--instance : ToString TransactionDetailsResp where
-- toString resp := resp.result.map (fun td => s!"{td.signature} {td.blockTime} {td.slot} {td.confirmationStatus}")
 --s!"TransactionDetailsResp(response: {res})"  resp.result
-- toString _ := s!"TransactionDetailsResp(response:TODO)"

structure TransactionDetailsResp where
  result : List TransactionDetails
  deriving ToJson, FromJson, Inhabited
  --, Repr
  --toString (resp :TransactionDetailsResp):String := s!"TransactionDetailsResp(response: {resp.response.map toString})"

instance : ToString TransactionDetailsResp where
toString _ := s!"TransactionDetailsResp"

instance : ToString TransactionDetails where
toString _ := s!"TransactionDetails"
  --toString td := s!"TransactionDetails(signature: {td.signature}, blockTime: {td.blockTime}, slot: {td.slot}, err: {td.err}, programId: {td.programId}, accounts: {td.accounts})"

def Pubkey := String
deriving ToJson, FromJson, Inhabited, Repr, ToString

structure TokenInfo where
  mint  : Pubkey
  supply : Nat
  decimals : Nat
  mintAuthority : Option Pubkey
  freezeAuthority : Option Pubkey
deriving ToJson, FromJson, Inhabited, Repr

--instance : ToString TokenInfo where
--toString _ := s!"TokenInfo..."
structure Foo where
  name : String
  age : Nat
  deriving ToJson, FromJson, Inhabited, Repr

-- Define the struct
structure SidechainConfig where
  chunkSize : Nat
  sidechainDir : String
  tokenAddress : String  -- Using String for Pubkey as Lean doesn't have a native Pubkey type
  cacheDir : String
  deriving ToJson, FromJson, Inhabited, Repr

-- JSON serialization instance
-- instance : ToJson SidechainConfig where
--   toJson cfg :=
--     Json.obj [
--       ("chunkSize", Json.num cfg.chunkSize),
--       ("sidechainDir", Json.str cfg.sidechainDir),
--       ("tokenAddress", Json.str cfg.tokenAddress),
--       ("cacheDir", Json.str cfg.cacheDir)
--     ]

-- JSON deserialization instance
-- instance : FromJson SidechainConfig where
--   fromJson? j := do
--     let chunkSize ← j.getObjValAs? Nat "chunkSize"
--     let sidechainDir ← j.getObjValAs? String "sidechainDir"
--     let tokenAddress ← j.getObjValAs? String "tokenAddress"
--     let cacheDir ← j.getObjValAs? String "cacheDir"
--     pure ⟨chunkSize, sidechainDir, tokenAddress, cacheDir⟩

-- Example data
-- def defaultConfig : SidechainConfig := {
--   chunkSize := 100,
--   sidechainDir := "ai_sidechain",
--   tokenAddress := "BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump",
--   cacheDir := "rpc_cache"
-- }

-- -- Example usage
-- def main : IO Unit := do
--   -- Create and serialize example config
--   let config := defaultConfig
--   let json := toJson config
--   IO.println s!"Serialized JSON:\n{json.pretty}"

--   -- Demonstrate deserialization
--   match fromJson? json with
--   | .ok deserialized =>
--     IO.println s!"Deserialized config:\n{repr deserialized}"
--   | .error err =>
--     IO.println s!"Deserialization error: {err}"

-- -- Run example
-- #eval main
def getSig (td:TransactionDetails) := td.signature

def good (td:TransactionDetails) : Bool :=
  match td.err with
  | none => true
  | some _ => false
