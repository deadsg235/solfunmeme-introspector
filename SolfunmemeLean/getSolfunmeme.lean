--getSolfunmeme.lean

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

structure TransactionDetails where
  signature : Signature
  blockTime : Nat
  slot : Slot
  memo: Option String
  err : Option String
  confirmationStatus : String -- "finalized",


deriving ToJson, FromJson, Inhabited, Repr


--instance : ToString TransactionDetailsResp where
--  toString resp := s!"TransactionDetailsResp(response: {resp.response})"
--  toString resp := s!"TransactionDetailsResp(response:)"

structure TransactionDetailsResp where
  result : List TransactionDetails
  deriving ToJson, FromJson, Inhabited, Repr
  --toString (resp :TransactionDetailsResp):String := s!"TransactionDetailsResp(response: {resp.response.map toString})"
def getTransactionSignaturesDetails (json2: Json) : IO (Except String (TransactionDetailsResp)) := do
  --IO.println s!"Ledger details: {json2.pretty}"
  match fromJson?  json2 with
  | Except.ok details => do
      --IO.println s!"Ledger details: {details}"
      return (Except.ok (details))
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

-- Execute curl for Solana RPC with caching
def callSolanaRpc (method : String) (params : Json) : IO (Except String Json) := do
  let cacheKey := generateCacheKey method params
  match (← checkCache cacheKey) with
  | some cachedContent =>
    IO.println s!"Using cached result for {method}"
    match Json.parse cachedContent with
    | Except.ok json => pure (Except.ok json)
    | Except.error err => pure (Except.error s!"JSON parsing failed for cached content: {err}")
  | none =>
    IO.println s!"No cache found for {method}, making RPC call"
    let payload := Json.mkObj [
      ("jsonrpc", Json.str "2.0"),
      ("id", Json.num 1),
      ("method", Json.str method),
      ("params", params)
    ]

    let tempFileName := s!"{CACHE_DIR}/temp_{cacheKey}_request.json"
    IO.FS.createDirAll CACHE_DIR
    IO.FS.writeFile tempFileName (payload.pretty 2)

    let result ← IO.Process.output {
      cmd := "curl",
      args := #[
        "-X", "POST",
        "-H", "Content-Type: application/json",
        "--data", s!"@{tempFileName}",
        "https://api.mainnet-beta.solana.com"
      ]
    }

    -- Save response details for debugging
    let respFileName := s!"{CACHE_DIR}/temp_{cacheKey}_response.json"
    IO.FS.writeFile respFileName result.stdout

    if !result.stderr.isEmpty then
      let errFileName := s!"{CACHE_DIR}/temp_{cacheKey}_error.txt"
      IO.FS.writeFile errFileName result.stderr

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

--def processTransactionSignatures (sig :Json) : IO (Except String (List String)) := do
        --let blockTime := sig.getObjValD "blockTime" |>.      getNat .getD 0
        --let confirmationStatus := sig.getObjValD "confirmationStatus" |>.getStr?.getD ""
        --let err := sig.getObjValD "err" |>.getStr?.getD ""
        --let memo := sig.getObjValD "memo" |>.getStr?.getD ""
  --      let signature := sig.getObjValD "signature"

       -- let sig := signature |>.getStr?.getD ""

        -- |>.getStr?.getD ""
        --let slot := sig.getObjValD "slot" |>.getNat?.getD 0
        -- let ret := {
        --   signature, blockTime, slot, err, programId := "", accounts := []
        --   }
      --pure (Except.ok sig)
--n  Except String (Array Json) : Type\nbut is expected to have type\n  Except ?m.21088 (Type ?u.21091) : Type (max ?u.21082 (?u.21091 + 1))",

def processElement (sig : Json) : String :=
  match sig.getObjVal? "key" with
  | .ok  value =>
    let f := value.getStr?
    match f with
    | .ok str1 => str1
    | .error _ => "default1"
  | .error _ => "default2"

def processElement2 ( _ :Array Json) : (List String) :=
  --  let blockTime := sig.getObjValD "blockTime" |>.getNat?.getD 0
  --  let confirmationStatus := sig.getObjValD "confirmationStatus" |>.getStr?.getD ""
  --  let err := sig.getObjValD "err" |>.getStr?.getD ""
  --  let memo := sig.getObjValD "memo" |>.getStr?.getD ""
    -- sig.getObjVal "signature" |>.getStr? |>.bind (fun signature =>
    -- match signature with
    -- | none => pure (Except.error "No signature found")
    -- | some sig =>
    --   -- IO.println s!"Got signature {sig}"
    --   pure (Except.ok sig)
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

  --let signature := sig.getObjValD "signature" |>.getStr?.getD ""
  --  let slot := sig.getObjValD "slot" |>.getNat?.getD 0
  --  let programId := sig.getObjValD "programId" |>.getStr?.getD ""
  --  let accounts := [] -- Placeholder for accounts, would need to be parsed from the response
  --pure (Except.ok signature)
 --)


  -- sig.getArr? |>.bind (fun arr =>
  --   let signatures := arr.filterMap fun sig =>
  --     match sig.getObjVal? "signature" |>.bind (·.getStr?) with
  --     | none => none
  --     | some signature => some signature
  --   pure (Except.ok signatures))
  --let signature := sig.getObjValD "signature" |>.getStr?.getD ""
  --let blockTime := sig.getObjValD "blockTime" |>.getNat?.getD 0
  --let confirmationStatus := sig.getObjValD "confirmationStatus" |>.getStr?.getD ""
  --let err := sig.getObjValD "err" |>.getStr?.getD ""
  --let memo := sig.getObjValD "memo" |>.getStr?.getD ""
  --let slot := sig.getObjValD "slot" |>.getNat?.getD 0
  --let programId := sig.getObjValD "programId" |>.getStr?.getD ""
  --let accounts := [] -- Placeholder for accounts, would need to be parsed from the response
 -- pure (Except.ok signature)


-- def getTransactionSignatures3 (json2: Json) : IO (Except String (List String)):= do
--   let details:TransactionDetailsResp ← fromJson? json2
--   IO.println s!"Ledger details: {details}"

-- def proc2 (json:Json) := do
--     --let ledger : TransactionDetailsResp <- fromJson? json
--     --IO.println s!"Ledger details: {ledger}"
--     pure ledger

-- def getTransactionSignatures2 {Json} (json2: Json) := do
--   match json2 with
--   | Except.ok details => fromJson? details
--   | Except.error err => Except.error s!"Error parsing JSON: {err}"

def getTransactionSignatures (address : Pubkey) (limit : Nat) : IO (Except String (List String)) := do
  let params := Json.arr #[Json.str address, Json.mkObj [("limit", Json.num limit)]]
  let response ← callSolanaRpc "getSignaturesForAddress" params
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok json2 =>
    IO.println s!"debug, {(json2.pretty).length}"
    let res <- getTransactionSignaturesDetails json2

    --IO.println s!"debug, {res}"
    match res with
    | Except.ok details =>
      IO.println s!"Transaction details: {details}"
      pure (Except.ok [""]) -- Placeholder for actual return value
    | Except.error err =>
      IO.println s!"Error retrieving transaction details: {err}"
      pure (Except.error err)
    --let food := proc2 json2
    --pure  (Except.ok [""])
    --let res := json.getObjValD "result"
    --let maybeAr := getArr? res
    --IO.println s!"Got signatures response, {maybeAr}"
    --let txs <- fromJson? json2
    -- match fromJson? json2 with
    -- | .error err =>
    --   IO.println s!"Error parsing response: {err}"
    --   pure (Except.error s!"Error parsing response: {err}")
    -- | .ok arr =>
    --   IO.println s!"Got signatures response, {arr.pretty 2}"
    --   let l := arr.map processElement2
      --let result <- fromJson? json2
      --IO.println s!"Error parsing response: {result}"
      --IO.println s!"Got signatures response, {json.pretty 2}"
      --IO.println s!"Got signatures response"
      --let l := processTransactionSignatures json
      --IO.println s!"Got signatures {l}}"
      --let result <- l
      --#check result
      --match result with
      --| Except.ok arr =>
        --IO.println s!"Got signatures {arr}}"
        --pure (Except.ok [""])
      --| Except.error err =>
        --IO.println s!"Error processing transaction signatures: {err}"
        --pure (Except.error err)
    --let result <-  fromJson? json2
    --IO.println s!"Error parsing response: {result}"
    --TransactionDetailsResp
    -- match maybeAr with
    -- | .error err =>
    --   IO.println s!"Error parsing response: {err}"
    --   pure (Except.error s!"Error parsing response: {err}")
    -- | .ok arr =>
    --   IO.println s!"Got signatures response: {arr}"

    pure (Except.ok [""])
      -- | .ok arr =>
      --   let signatures := arr.filterMap fun sig =>
      --     match sig.getObjVal? "signature" |>.bind (·.getStr?) with
      --     | none => none
      --     | some signature => some signature
      --IO.println s!"Got signatures response, {json.pretty 2}"
      -- IO.println s!"Got signatures response"
      -- let l := processTransactionSignatures json
      -- IO.println s!"Got signatures {l}}"
      -- let result <- l
      -- #check result
      -- match result with
      -- | Except.ok arr =>
      --     IO.println s!"Got signatures {arr}}"
      --     pure (Except.ok arr)
      -- | Except.error err =>
      --     IO.println s!"Error processing transaction signatures: {err}"
      --     pure (Except.error err)
    -- |>.getArr? |>.bind (fun arr =>
    --   let signatures := arr.filterMap fun sig =>
    --     match sig.getObjVal? "signature" |>.bind (·.getStr?) with
    --     | none => none
    --     | some signature => some signature
    --   pure (Except.ok signatures))
    --IO.println s!"Got signatures response, {json.pretty 2}"
    -- IO.println s!"Got signatures response"
    -- let l := processTransactionSignatures json
    -- --IO.println s!"Got signatures {l}}"
    -- let result <- l
    -- --#check result
    -- --match result with
    -- --| Except.ok arr =>
    -- IO.println s!"Got signatures {result}}"
    -- pure (Except.ok ["fff"])
    --| Except.error err =>
--      IO.println s!"Error processing transaction signatures: {err}"
      --pure (Except.error err)

   -- #check l
    -- match l with
    -- | Except.ok arr =>
    --     IO.println s!"Got signatures {arr}}"
    --     pure (Except.ok arr)
    -- | Except.error err =>
    --     IO.println s!"Error processing transaction signatures: {err}"
    --     pure (Except.error err)
    --//let res :Json := json.getObjValD "result"
    --let res2  := res.getArr?
    -- match getArr? res with
    -- | Except.ok arr => IO.println s!"Extracted array: {arr}"
    -- | Except.error err => IO.println s!"Error: {err}"
    --  match res.getArr? with
    --  | .ok arr =>
    --    let signatures := arr.filterMap fun sig =>
    --      match sig.getObjVal? "signature" |>.bind (·.getStr?) with
    --      | none => none
    --      | some signature => some signature
    --     pure (Except.ok signatures)
    -- | none => pure (Except.error "No result array in response")
    -- | arr  =>
    --   let signatures := arr.filterMap fun sig =>
    --     match sig.getObjVal? "signature" |>.bind (·.getStr?) with
    --     | none => none
    --     | some signature => some signature
    --   pure (Except.ok signatures)

-- Query transaction signatures
-- def getTransactionSignatures2 (address : Pubkey) (limit : Nat) : IO (Except String (List TransactionDetails)) := do
--   let params := Json.arr #[Json.str address.pubkey, Json.mkObj [("limit", Json.num limit)]]
--   let response ← callSolanaRpc "getSignaturesForAddress" params
--   match response with
--   | Except.error err => pure (Except.error err)
--   | Except.ok json =>
--     IO.println s!"Got signatures response, would process in full implementation"
--     -- In a real implementation, you would parse the JSON response
--     let res := json.getObjValD "result"
--     let arr1 := res.getArr?
--     match arr1 with
--     |.error err =>
--       IO.println s!"Error parsing response: {err}"
--       pure (Except.error s!"Error parsing response: {err}")
--     |.ok g =>
--       let mut txs : List TransactionDetails := []
--       --for sig in g do
--       let list := g.map fun sig =>
--         --let blockTime := sig.getObjValD "blockTime" |>.getNat?.getD 0
--         --let confirmationStatus := sig.getObjValD "confirmationStatus" |>.getStr?.getD ""
--         --let err := sig.getObjValD "err" |>.getStr?.getD ""
--         --let memo := sig.getObjValD "memo" |>.getStr?.getD ""
--         let signature1 := sig.getObjValD "signature"
--         let signature := (signature1).getStr?
--         match signature with
--         |.error err =>
--           IO.println s!"Error parsing signature: {err}"
--         |.ok sig =>
--          -- IO.println s!"Got signature {sig}"
--           pure (sig)

--         -- let sig := signature.bind (fun sig =>
--         --   IO.println s!"Processing signature: {sig}"
--         --   pure sig)
--         --let sig := signature |>.getStr?.getD ""

--         -- |>.getStr
--         --let slot := sig.getObjValD "slot" |>.getNat?.getD 0
--         --let programId := sig.getObjValD "programId" |>.getStr?.getD ""
--         --let accounts := [] -- Placeholder for accounts, would need to be parsed from the response
--         --txs := { signature, blockTime, slot, err, programId := "", accounts := [] } :: txs
--       pure (Except.ok txs)
--   --  |.error err =>
--       --IO.println s!"Error parsing response: {err}"


--     -- match arr1 with
--     -- | some arr =>
--     --   arr.map (processTransactionSignature)
--     -- | none => pure (Except.error "No result array")
--     --  "result": [
--     --     {
--     --         "blockTime": 1745776798,
--     --         "confirmationStatus": "finalized",
--     --         "err": null,
--     --         "memo": null,
--     --         "signature": "3pwbDDzkfP6uhJqSLg1UHnsZ4vavFSoCTB3tJKx5aCzx1Dy78wXxxmaJe1wfDEj32izLjnUMbkWa9FSbMnUKwixV",
--     --         "slot": 336292412
--     --     },
--     pure (Except.ok [])

-- Get detailed transaction data
def getTransactionDetails (signature : Signature) : IO (Except String TransactionDetails) := do
  let params := Json.arr #[Json.str signature, Json.mkObj [("encoding", Json.str "jsonParsed")]]
  let response ← callSolanaRpc "getTransaction" params
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok json =>

    IO.println s!"Got transaction details response {(json.pretty 2).length}"
    IO.println s!"Got transaction details response "
    -- For simplicity, returning a default TransactionDetails
    let details := TransactionDetails.mk
      signature, 0, 0, none, none, "finalized"

    -- "signature", 0, 0, none, none, "finalized"
    pure (Except.ok details)

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
  let leanCode := s!"
import Lean.Data.Json

def main : IO Unit := do
  let jsonStr ← IO.FS.readFile \"{tempFile}\"
  match Json.parse jsonStr with
  | Except.ok json =>
    let summary := \"Processed chunk \" ++ json.getObjValD \\\"chunkId\\\".pretty 2 ++ \": \" ++ json.getObjValD \\\"data\\\".pretty 2
    IO.FS.writeFile \"{outputFile}\" summary
  | Except.error err =>
    IO.FS.writeFile \"{outputFile}\" (\"Error: \" ++ err)
"
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
      IO.println s!"Successfully fetched {txs.length} transaction signatures"
      IO.println "Done processing token data"

def main : IO Unit := do
  SolfunmemeLean
