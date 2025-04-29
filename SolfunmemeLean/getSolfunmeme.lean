--getSolfunmeme.lean
import Lean.Data.Json
import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
open Lean Json ToJson FromJson

-- structure Pubkey where
--   pubkey : String
-- deriving ToJson, FromJson, Inhabited, Repr

-- instance : ToString Pubkey where
--   toString pk := pk.pubkey

structure Entry where
  entry_date : String -- ISO date or timestamp
  description : String -- Transaction type or details
deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Entry where
  toString e := s!"Entry(date: {e.entry_date}, description: {e.description})"

structure Ledger where
  account_name : String -- Token metadata (e.g., mint details)
  account_number : String -- Token address
  entries : Array Entry
deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Ledger where
  toString l := s!"Ledger(account_name: {l.account_name}, account_number: {l.account_number}, entries: {l.entries})"

def Signature := String
deriving ToJson, FromJson, Inhabited, Repr, ToString

def Slot := Nat
deriving ToJson, FromJson, Inhabited, Repr, ToString, ToString

instance : OfNat Slot n where
  ofNat := n

--{"jsonrpc":"2.0","result":[
--{"blockTime":1745839669,
--"confirmationStatus":"finalized",
--"err":null,
--"memo":null,
--"signature":"2pozgVUc8vbGLeDWAhriwuv7zitKx3GFDmc7TrQEg3saqET18tuxo3ut7HVNQxoCzpXWnjzYN3u9CF5dGSXZG2Tc",

structure TransactionDetails where
  signature : String
  blockTime : Nat
  slot : Slot
  --memo: Option String
  --err : Option String   TransactionDetails.err: String expected
  confirmationStatus : String -- "finalized",
deriving ToJson, FromJson, Inhabited, Repr

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
  deriving ToJson, FromJson, Inhabited, Repr
  --toString (resp :TransactionDetailsResp):String := s!"TransactionDetailsResp(response: {resp.response.map toString})"

def getSig (td:TransactionDetails) := td.signature

def getTransactionSignaturesDetails (json2: Json) : IO (Except String (List String)) := do
  --IO.println s!"Ledger details: {(json2.pretty).toSubstring.take 512 }"
  match fromJson?  json2 with
  | Except.ok (details:TransactionDetailsResp) => do
      let resp  :List String := details.result.map getSig
    --  IO.println s!"TransactionDetails details: {resp}"
      return (Except.ok (resp))
  | Except.error err => do
    IO.println s!"Error parsing JSON: {err}"
      return (Except.error s!"Error parsing JSON: {err.toSubstring.take 1000 }")

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

instance : ToString TokenInfo where
toString _ := s!"TokenInfo..."

-- Constants
def CHUNK_SIZE : Nat := 100 -- Entries per chunk
def SIDECHAIN_DIR : String := "ai_sidechain" -- Simulated sidechain
def TOKEN_ADDRESS : Pubkey := "BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump"
def CACHE_DIR : String := "rpc_cache" -- Directory for cached results

-- Generate a cache key from method and params
def generateCacheKey (method : String) (params : Json) : String :=
  let paramsHash := toString (hash params.compress)
  s!"{method}_{paramsHash}"

-- Check if cache exists for a given key
def checkCache (cacheKey : String) : IO (Option String) := do
  let cacheFile := s!"{CACHE_DIR}/{cacheKey}.json"
  --System.FilePath.pathExists
  --(p : System.FilePath) : BaseIO Bool

  if (← System.FilePath.pathExists cacheFile) then
    try
      let content ← IO.FS.readFile cacheFile
      pure (some content)
    catch _ =>
      pure none
  else
    pure none

-- Save response to cache
def saveToCache (cacheKey : String) (content : String) : IO Unit := do
  try
    IO.FS.createDirAll CACHE_DIR
    let cacheFile := s!"{CACHE_DIR}/{cacheKey}.json"
    IO.FS.writeFile cacheFile content
  catch e =>
    IO.println s!"Failed to save cache: {e.toString}"

-- JSON parsing for ledger
def get_ledger_from_json_string (s : String) : Except String Ledger := do
  let j ← Json.parse s
  let ledger ← fromJson? j
  pure ledger

def extractKey (method : String) (params : Json) : String :=
  match method with
  | "getSignaturesForAddress" =>
    match params with
    --| Json.arr #[Json.str address, Json.mkObj obj] =>
--      match obj.find? "limit" with
--      | some (Json.num limit) => s!"method:{method}_address:{address}_limit:{limit}"
      --| _ => s!"method:{method}_address:{address}"
    | Json.arr #[Json.str address, _] =>
      s!"method_{method}_address_{address}"
    | _ =>
      s!"method_{method}_unknown"
  | "getTransaction" =>
    match params with
    --| Json.arr #[Json.str signature, Json.mkObj _] =>
--      s!"method:{method}_signature:{signature}"
    | Json.arr #[Json.str signature, _] =>
      s!"method_{method}_signature_{signature}"
    | _ =>
      s!"method_{method}_unknown"
  | "getAccountInfo" =>
    match params with
    --| Json.arr #[Json.str address, Json.mkObj _] =>
--      s!"method:{method}_address:{address}"
    | Json.arr #[Json.str address, _] =>
      s!"method_{method}_address_{address}"
    | _ =>
      s!"method_{method}_unknown"
  | _ =>
    s!"method_{method}_unknown"

-- Execute curl for Solana RPC with caching
def callSolanaRpc (method : String) (params : Json) : IO (Except String Json) := do
  let keyName := ( extractKey method params)
  IO.println s!"keyName: {keyName}"
  let cacheKey := generateCacheKey  keyName params
  match (← checkCache cacheKey) with
  | some cachedContent =>
    IO.println s!"Using cached result for {method} {cacheKey}"
    match Json.parse cachedContent with
    | Except.ok json => pure (Except.ok json)
    | Except.error err => pure (Except.error s!"JSON parsing failed for cached content: {err}")
  | none =>
    IO.println s!"No cache found for {method}, making RPC call"
    IO.sleep 400
    let payload := Json.mkObj [
      ("jsonrpc", Json.str "2.0"),
      ("id", Json.num 1),
      ("method", Json.str method),
      ("params", params)
    ]

    let tempFileName := s!"{CACHE_DIR}/temp_{cacheKey}_request.json"
    IO.FS.createDirAll CACHE_DIR
    IO.FS.writeFile tempFileName (payload.pretty 2)

    let url := (← IO.getEnv "SOLANA_URL").getD "https://api.mainnet-beta.solana.com"

    let result ← IO.Process.output {
      cmd := "curl",

      args := #[
        "-X", "POST",
        "-H", "Content-Type: application/json",
        "--data", s!"@{tempFileName}",
        url
      ]
    }

    -- Save response details for debugging
    let respFileName := s!"{CACHE_DIR}/temp_{cacheKey}_response.json"
    IO.FS.writeFile respFileName (url ++ result.stdout )

    if !result.stderr.isEmpty then
      let errFileName := s!"{CACHE_DIR}/temp_{cacheKey}_error.txt"
      IO.FS.writeFile errFileName (url ++ result.stderr)

    match Json.parse result.stdout with
    | Except.ok json =>
      IO.println s!"Received response for {method}"
      -- Cache the successful response
      saveToCache cacheKey result.stdout
      pure (Except.ok json)
    | Except.error err =>
      pure (Except.error s!"JSON parsing failed: {err}")

-- Query token mint info
def getTokenInfo (address : Pubkey) : IO (Except String TokenInfo) := do
  let params := Json.arr #[Json.str address, Json.mkObj [("encoding", Json.str "jsonParsed")]]
  let response ← callSolanaRpc "getAccountInfo" params
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok json =>
    -- For simplicity, returning a default TokenInfo
    -- In a real implementation, you would parse the JSON response
    let prettyJson := json.pretty 2
    IO.println s!"Got token info response, processing... "
    IO.println s!"Got token info response, processing. {prettyJson.length}"
    let supply := 0
    let decimals := 0
    let mintAuthority := none
    let freezeAuthority := none
    pure (Except.ok { mint := address, supply, decimals, mintAuthority, freezeAuthority })


def processElement (sig : Json) : String :=
  match sig.getObjVal? "key" with
  | .ok  value =>
    let f := value.getStr?
    match f with
    | .ok str1 => str1
    | .error _ => "default1"
  | .error _ => "default2"

def processElement2 ( _ :Array Json) : (List String) :=
  [""]

def processTransactionSignatures (sig : Json) : IO (Except String (Array String)) := do
 let arr := sig.getArr?
 match arr with
  --| none => pure (Except.error "No result array in response")
  | .error err =>
    IO.println s!"Error parsing response: {err}"
    --IO.println s!"Error parsing response: {sig.pretty 2}"
    pure (Except.error s!"Error parsing response: {err}")
  | .ok arr2 =>
      let vals := arr2.map processElement
      pure (Except.ok vals)

instance : ToString TransactionDetails2 where
toString _ := s!"TransactionDetails2..."


def getTransactionDetails (signature : Signature) : IO (Except String TransactionDetailsResult2) := do
  let params := Json.arr #[Json.str signature,  Json.mkObj [ ( "maxSupportedTransactionVersion", Json.num 0 ) ]]
    --("encoding", Json.str "jsonParsed")


  let response ← callSolanaRpc "getTransaction" params
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok json =>

    IO.println s!"Got transaction details response {(json.pretty 2).length}"
    IO.println s!"Got transaction details response "
    -- For simplicity, returning a default TransactionDetails
    --let details := TransactionDetails.mk signature, 0, 0, none, none, "finalized"
    IO.println s!"debug1 ok, {(json.pretty).toSubstring.take 256 }"
    --let txd : TransactionDetails2  := fromJson? json

    match fromJson?  json with
    | Except.ok (details:TransactionDetailsResult2) => do
      --  IO.println s!"TransactionDetails details: {resp}"
      IO.println s!"debug2 txd, {(details)}"

      if details.error.isSome then
        let err := details.error.get!
        IO.println s!"Error in transaction details: {err.message}"
        --sorry
        return (Except.error s!"Error in transaction details: {err.message}")

      return (Except.ok (details))
    | Except.error err => do
      IO.println s!"Error parsing JSON2: {err}"
      --sorry
      --IO.sleep 12000
      return (Except.error s!"Error parsing JSON: {err.toSubstring.take 1000 }")



def getTransactionSignatures (address : Pubkey) (limit : Nat) : IO (Except String (List String)) := do
  let params := Json.arr #[Json.str address, Json.mkObj [("limit", Json.num limit)]]
  let response ← callSolanaRpc "getSignaturesForAddress" params
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok json2 =>
    IO.println s!"debug2, {(json2.pretty).length}"
    let res <- getTransactionSignaturesDetails json2
    match res with
    | Except.ok details =>
      IO.println s!"Transaction details: {details.length}"
      pure (Except.ok details) -- Placeholder for actual return value
    | Except.error err =>
      IO.println s!"ERROR, {res}"
      --IO.println s!"Error retrieving transaction details: {err}"
      pure (Except.error err)

-- Chunk ledger entries
def chunkEntries (ledger : Ledger) : List (Nat × Array Entry) := Id.run do
  let mut chunks : List (Nat × Array Entry) := []
  let mut id : Nat := 0
  for i in [:ledger.entries.size:CHUNK_SIZE] do
    let chunk := ledger.entries.extract i (i + CHUNK_SIZE)
    chunks := (id, chunk) :: chunks
    id := id + 1
  chunks.reverse

def processWithLLM (tx : Json) : IO (Except String String) := do
  let cachePath := s!"{CACHE_DIR}/llm_process_{hash tx.compress}.txt"

  -- Check if we have a cached LLM result
  if (← System.FilePath.pathExists cachePath) then
    try
      let cachedOutput ← IO.FS.readFile cachePath
      IO.println "Using cached LLM processing result"
      return Except.ok cachedOutput
    catch _ =>
      IO.println "Failed to read cached LLM result, processing again"

  let tempFile := s!"{CACHE_DIR}/llm_input_{hash tx.compress}.json"
  IO.FS.createDirAll CACHE_DIR
  IO.FS.writeFile tempFile (tx.pretty 2)

  let outputFile := s!"{CACHE_DIR}/llm_output_{hash tx.compress}.txt"
  let leanCode := "todo"

  let leanFile := s!"{CACHE_DIR}/temp_processor_{hash tx.compress}.lean"
  IO.FS.writeFile leanFile leanCode
  let result ← IO.Process.output {
    cmd := "lean",
    args := #["--run", leanFile]
  }

  if !result.stderr.isEmpty then
    IO.println s!"LLM Processing Error:\n{result.stderr}"

  try
    let output ← IO.FS.readFile outputFile
    -- Cache the LLM processing result
    IO.FS.writeFile cachePath output
    pure (Except.ok output)
  catch e =>
    pure (Except.error s!"Failed to read LLM output: {e.toString}")

def ProcessSignfun (sig:String) : IO Unit := do
      IO.println s!"Processing transaction signature: {sig}"
      let txDetails ← getTransactionDetails sig
      match txDetails with
      | Except.error err =>
        IO.println s!"Failed to fetch transaction details: {err}"
        pure ()
      | Except.ok details =>
        IO.println s!"Transaction Details fetched successfully \n{details}"

-- Main function
def SolfunmemeLean : IO Unit := do
  IO.println s!"Introspecting token: {TOKEN_ADDRESS}"

  -- Create cache directory
  IO.FS.createDirAll CACHE_DIR
  IO.println s!"Using cache directory: {CACHE_DIR}"

  -- Get token info
  IO.println "Fetching token info..."
  let tokenInfo ← getTokenInfo TOKEN_ADDRESS
  match tokenInfo with
  | Except.error err =>
    IO.println s!"Failed to fetch token info: {err}"
    pure ()
  | Except.ok info =>
    IO.println s!"Token Info fetched successfully \n{info}"

    -- Get transactions
    IO.println "Fetching transaction signatures..."
    let txSignatures ← getTransactionSignatures TOKEN_ADDRESS 1000  -- Limited to 10 for testing
    match txSignatures with
    | Except.error err =>
      IO.println s!"Failed to fetch transactions: {err}"
      pure ()
    | Except.ok txs =>
      IO.println s!"Successfully fetched {txs} transaction signatures"

      for i in txs do
        IO.println s!"Done processing token {i}"
        ProcessSignfun i

      --let txSignatures ← getTransactionDetails sig
      IO.println "Done processing token data"

def main : IO Unit := do
  SolfunmemeLean
