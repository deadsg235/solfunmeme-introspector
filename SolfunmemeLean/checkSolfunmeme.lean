import SolfunmemeLean.Common
-- This file contains environment variables for the SolfunmemeLean project.
import Lean.Data.Json
import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
import Std.Time.Internal
import Std.Time.Time
import Std.Time
import Std.Time.Date
import Std.Time.Time.Unit.Second
import Std.Time.DateTime.Timestamp
open SolfunmemeLean.Common
open Lean Json ToJson FromJson



-- recursive function to get the signature from the transaction details
def getTransactionSignaturesDetails (json2: Json) : IO (Except String (SolfunmemeLean.Common.TransactionDetailsResp)) := do

  --IO.println s!"Ledger details: {(json2.pretty).toSubstring.take 512 }"
  match fromJson?  json2 with
  | Except.ok (details:SolfunmemeLean.Common.TransactionDetailsResp) => do
      --let firstT := details.result[0]?
      --let goodList  := details.result.filter good
      --IO.println s!"found {goodList.length} good transactions"
    --let resp  :List String :=  details.result.map getSig
      -- match firstT with
      -- | none => do
      --   IO.println s!"Error: No transaction details found"
      --   return (Except.error s!"Error: No transaction details found")
      -- | some firstT =>
      --   --IO.println s!"TransactionDetails details: {resp}"
      --   --IO.println s!"TransactionDetails details: {firstT}"
      --   let secs :Std.Time.Second.Offset := Std.Time.Second.Offset.ofNat firstT.blockTime
      --   let timestampVal  := Std.Time.Timestamp.ofSecondsSinceUnixEpoch secs
      --   let dt := Std.Time.DateTime.ofTimestamp timestampVal Std.Time.TimeZone.GMT
      --   let year := dt.year
      --   let mon := dt.month
      --   let day := dt.day
      --   let hour := dt.hour.toInt
      --   let minute := dt.minute.toInt
      --   IO.println s!"TransactionDetails details: {year} {mon.toInt} {day.toInt} {hour} {minute}"

    --  IO.println s!"TransactionDetails details: {resp}"
      return (Except.ok (details))
  | Except.error err => do
    IO.println s!"Error parsing JSON: {err}"
      return (Except.error s!"Error parsing JSON: {err.toSubstring.take 1000 }")

-- Constants
-- def CHUNK_SIZE : Nat := 100 -- Entries per chunk
-- def SIDECHAIN_DIR : String := "ai_sidechain" -- Simulated sidechain
-- def TOKEN_ADDRESS : Pubkey := "BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump"
-- def CACHE_DIR : String := "rpc_cache" -- Directory for cached results

def generateContentCacheKey(method : String) (params : Json) (content : Json) : String :=
  let paramsHash := toString (hash params.compress)
  let contentHash := toString (hash content.compress)
  s!"{method}_{paramsHash}_{contentHash}"
-- Generate a cache key from method and params
def generateCacheKey (method : String) (params : Json) : String :=
  let paramsHash := toString (hash params.compress)
  s!"{method}_{paramsHash}"

-- Check if cache exists for a given key
def checkCache (cacheKey : String) : IO (Option String) := do

  let config: SolfunmemeLean.Common.SidechainConfig ← pure {
    cacheDir := "rpc_cache",
    chunkSize := 100,
    sidechainDir := "ai_sidechain",
    tokenAddress := "BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump"
  } -- Replace with actual implementation of getConfig
  let cacheDir := config.cacheDir
  let cacheFile := s!"{cacheDir}/{cacheKey}.json"
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

def prepareCallSolanaRpc (method : String) (params : Json) :  String :=
  let keyName := ( extractKey method params)
  generateCacheKey  keyName params

-- Execute curl for Solana RPC with caching
def callSolanaRpc (_config : SidechainConfig)(_method : String) (_params : Json ) (cacheKey:String) : IO (Except String String ) := do

  match (← checkCache cacheKey) with
  | some cachedContent =>
    --IO.println s!"Using cached result for {method} {cacheKey}"
    pure (Except.ok cachedContent)
    --match Json.parse cachedContent with
    --| Except.ok json => pure (Except.ok json)
    --| Except.error err => pure (Except.error s!"JSON parsing failed for cached content: {err}")
  | none =>
    pure (Except.error ("Missing:" ++ cacheKey))

def prepareSaveRPC (_method : String) (result: String) : IO (Except String Json) := do
    match Json.parse result with
    | Except.ok json =>
      --IO.println s!"Received response for {method}"
      pure (Except.ok json)
    | Except.error err =>
      pure (Except.error s!"JSON parsing failed: {err} {result}")

--def saveRPC (method : String) (params : Json) (result: String) : IO (Except String Json) := do
       --let cacheKey := generateContentCacheKey method params json
       --saveToCache cacheKey (json.pretty 2)
       --pure (Except.ok json)

-- Query token mint info
def getTokenInfo (config: SolfunmemeLean.Common.SidechainConfig) (address : Pubkey) : IO (Except String SolfunmemeLean.Common.TokenInfo) := do
  let name := "getAccountInfo"
  let params := Json.arr #[Json.str address, Json.mkObj [("encoding", Json.str "jsonParsed")]]
  let cacheKey := prepareCallSolanaRpc name params
  let response ← callSolanaRpc config name params cacheKey
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok astr =>
    let ajson ← prepareSaveRPC "getAccountInfo" astr
    match ajson with
    | Except.error err => pure (Except.error err)
    | Except.ok ajson =>
      IO.println s!"Got token info response {(ajson.pretty 2).length}"
        --let prettyJson := ajson.pretty 2
        --IO.println s!"Got token info response, processing... "
      --IO.println s!"Got token info response, processing. {prettyJson.length}"
      --saveToCache cacheKey (ajson.pretty 2)
      let supply := 0
      let decimals := 0
      let mintAuthority := none
      let freezeAuthority := none
      pure (Except.ok { mint := address, supply, decimals, mintAuthority, freezeAuthority })
    -- For simplicity, returning a default TokenInfo
    -- In a real implementation, you would parse the JSON response



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


def getTransactionDetails (config: SidechainConfig) (signature : Signature) : IO (Except String TransactionDetailsResult2) := do
  let params := Json.arr #[Json.str signature,  Json.mkObj [ ( "maxSupportedTransactionVersion", Json.num 0 ) ]]
  let name := "getTransaction"
  let cacheKey := prepareCallSolanaRpc name params
  let response ← callSolanaRpc config name params  cacheKey
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok astr =>
    let ajson ← prepareSaveRPC name  astr
    match ajson with
      | Except.error err => pure (Except.error err)
      | Except.ok ajson =>
      --IO.println s!"Got transaction details response {(json.pretty 2).length}"
      --IO.println s!"Got transaction details response "
      -- For simplicity, returning a default TransactionDetails
      --let details := TransactionDetails.mk signature, 0, 0, none, none, "finalized"
      IO.println s!"debug1 ok, {(ajson.pretty).toSubstring.take 256 }"
      --let txd : TransactionDetails2  := fromJson? json

      match fromJson?  ajson with
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

def getTransactionSignaturesBefore (limit : Nat)(before: Option String) : Lean.Json :=

  match before with
    | none => Json.mkObj [ ("limit", Json.num limit)]
    | some beforeValue =>
        Json.mkObj [
        ("limit", Json.num limit),
        ("before", Json.str beforeValue)
      ]

def firstTransactionSignature (details : TransactionDetailsResp ) : String :=
  details.result.getLast? |>.get!.signature -- Get the last element of the list


def getTransactionSignatures (config: SidechainConfig) (address : Pubkey) (limit : Nat) (before : Option String): IO (Except String (TransactionDetailsResp )) := do
  let name := "getSignaturesForAddress"

  --let before := Except.error "No previous signature"
  let args  := getTransactionSignaturesBefore limit before
  let params := Json.arr #[Json.str address, args]
  let cacheKey := prepareCallSolanaRpc name params
  let response ← callSolanaRpc config name params cacheKey
  match response with
  | Except.error err => pure (Except.error err)
  | Except.ok astr =>
    let ajson ← prepareSaveRPC name  astr
    match ajson with
    | Except.error err => pure (Except.error err)
    | Except.ok ajson =>
      --IO.println s!"debug2, {(ajson.pretty).length}"

      -- recurseTransactions(ajson)
      let res ← getTransactionSignaturesDetails ajson
      match res with
        | Except.ok details =>
        -- now we use the earlist signature to get the transaction details
          --let firstSig := firstTransactionSignature details
          --let ajson ← saveRPC name params astr
          --IO.println s!"Transaction details: {details.length} {firstSig}"
          --saveToCache (cacheKey) (ajson.pretty 2)
          --firstSig.toSubstring.take 10
          --saveToCache ("txn_"++firstSig) (ajson.pretty 2)
          --saveToCache ("txn_"++firstSig) (ajson.pretty 2)
          pure (Except.ok details) -- Placeholder for actual return value
        | Except.error err =>
          IO.println s!"ERROR, {res}"
          --IO.println s!"Error retrieving transaction details: {err}"
          pure (Except.error err)
      --00pure (Except.error "TODO")



def ProcessSignfun (config: SidechainConfig) (sig:String) : IO Unit := do
      IO.println s!"Processing transaction signature: {sig}"
      let txDetails ← getTransactionDetails config sig
      match txDetails with
      | Except.error err =>
        IO.println s!"Failed to fetch transaction details: {err}"
        pure ()
      | Except.ok details =>
        IO.println s!"Transaction Details fetched successfully \n{details}"

-- filepath: c:\Users\gentd\OneDrive\Documents\GitHub\SOLFUNMEME\SolfunmemeLean\getSolfunmeme.lean

def findBlocksInTransaction (details : TransactionDetailsResp) : List Nat :=
  details.result.map (fun td => td.blockTime)

def foldBlocksIntoRanges (blocks : List Nat) : List (Nat × Nat) :=
  let sortedBlocks := blocks.mergeSort (· < ·) -- Sort the blocks in ascending order
  sortedBlocks.foldl (fun acc block =>
    match acc with
    | [] => [(block, block)] -- Start with the first block as a range
    | (start, end2) :: rest =>
      if end2 - block < 2 then
        -- Extend the current range if the block is sequential
        (start, block) :: rest
      else
        -- Start a new range
        (block, block) :: acc
  ) []
  |>.reverse -- Reverse the result to maintain the original order
  --termination_by fetchSignaturesLoop cursor



def mergeRanges (ranges1 ranges2 : List (Nat × Nat)) : List (Nat × Nat) :=
  let sortedRanges := (ranges1 ++ ranges2).mergeSort (fun (a b : Nat × Nat) => a.fst < b.fst)
  sortedRanges.foldl (fun acc range =>
    match acc with
    | [] => [range] -- Start with the first range
    | (start, end2) :: rest =>
      if range.fst <= end2 + 1 then
        -- Merge overlapping or adjacent ranges
        (start, max end2 range.snd) :: rest
      else
        -- Add a new range
        range :: acc
  ) []
  |>.reverse -- Reverse the result to maintain the original order

def fetchSignaturesLoop (config: SidechainConfig) (tokenAddress : String) (limit : Nat) (cursor : Option String) (maxBatches : Nat) (start_blocks:List (Nat × Nat)): IO Unit := do
  if maxBatches = 0 then
    IO.println "Reached maximum batch limit."
    pure ()
  else
    let txSignatures ← getTransactionSignatures config tokenAddress limit cursor
    match txSignatures with
    | Except.error err =>
      IO.println s!"Failed to fetch transactions: {err}"
      pure ()
    | Except.ok txs =>
      --IO.println s!"Fetched {txs.length} transaction signatures"
      --if txs.isEmpty then
--        IO.println "No more transactions found."
        --pure ()
      --else
        let firstSig := firstTransactionSignature txs
        let blocks := findBlocksInTransaction txs
        let ranges := foldBlocksIntoRanges blocks

        let mranges := mergeRanges ranges start_blocks

        IO.println s!"Loop First transaction signature: {firstSig}"
        IO.println s!"Blocks found in transaction: {ranges}"
        IO.println s!"Merged ranges: {mranges}"  -- Added line to print merged ranges
        let txSignatures ← fetchSignaturesLoop config tokenAddress limit (some firstSig) (maxBatches - 1) mranges
        IO.println s!"Fetched transaction signatures: {txSignatures}"


  --termination_by maxBatches cursor


-- Main function
def SolfunmemeLean : IO Unit := do

  let config: SidechainConfig := {
    cacheDir := "default_cache_dir",
    chunkSize := 100,
    sidechainDir := "ai_sidechain",
    tokenAddress := "BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump"}


  IO.println s!"Introspecting token: {config.tokenAddress}"

  -- Create cache directory
  IO.FS.createDirAll config.cacheDir
  IO.println s!"Using cache directory: {config.cacheDir}"

  -- Get token info
  IO.println "Fetching token info..."
  let tokenInfo ← getTokenInfo config config.tokenAddress
  match tokenInfo with
  | Except.error err =>
    IO.println s!"Failed to fetch token info: {err}"
    pure ()
  | Except.ok info =>
    IO.println s!"Token Info fetched successfully \n{info}"

    -- Get transactions
    IO.println "Fetching transaction signatures..."



--    fetchSignaturesLoop TOKEN_ADDRESS 1000 none
      -- Entry point
      let maxBatches := 20000 -- Arbitrary limit, adjust as needed
      let txSignatures ← fetchSignaturesLoop config config.tokenAddress 1000 none maxBatches []
      IO.println s!"Done processing token {txSignatures}"

     -- let report := generateReport txSignatures
     -- IO.println s!"Report generated: {report}"

      --pure ()


      -- for i in txs do
      --   IO.println s!"Done processing token {i}"
      --   ProcessSignfun i

      --let txSignatures ← getTransactionDetails sig
     -- IO.println "Done processing token data"

def main : IO Unit := do
  IO.println "Starting SolfunmemeLean..."
  SolfunmemeLean
