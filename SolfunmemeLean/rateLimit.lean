import Lean.Data.Json
import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
open Lean Json ToJson FromJson


-- Updated RpcNode structure with additional metadata
structure RpcNode where
  address : String -- IP:port (e.g., "103.50.32.83:8899")
  version : Option String -- Node version (e.g., "1.10.40")
  asn_id : Option String -- ASN ID (e.g., "396356")
  network : Option String -- Network (e.g., "103.50.32.0/24")
  country : Option String -- Country (e.g., "Brazil")
  isp : Option String -- ISP (e.g., "LATITUDE-SH, BR")
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString RpcNode where
  toString n := s!"RpcNode(address: {n.address}, version: {n.version}, asn_id: {n.asn_id}, network: {n.network}, country: {n.country}, isp: {n.isp})"

-- TCP-like request state
inductive RequestState
  | Idle -- No request
  | Connecting -- Request initiated
  | Active -- Request in progress
  | RateLimited -- Tokens exhausted
  | Closed -- Request completed/failed
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString RequestState where
  toString
    | RequestState.Idle => "Idle"
    | RequestState.Connecting => "Connecting"
    | RequestState.Active => "Active"
    | RequestState.RateLimited => "RateLimited"
    | RequestState.Closed => "Closed"

-- Cache state
inductive CacheState
  | Cached (timestamp : Nat) -- Cached response with timestamp (ms)
  | Stale (timestamp : Nat) -- Expired cache
  | Uncached -- No cache
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString CacheState where
  toString
    | CacheState.Cached t => s!"Cached({t})"
    | CacheState.Stale t => s!"Stale({t})"
    | CacheState.Uncached => "Uncached"

-- Updated RateLimitState with token bucket
structure RateLimitState where
  state : RequestState := RequestState.Idle
  requests : Nat := 0 -- Requests in current 10-second window
  tokens : Nat := 40 -- Available request tokens (max 40 per 10 seconds)
  connections : Nat := 0 -- Active connections
  connectionRate : Nat := 0 -- Connections in current 10-second window
  dataBytes : Nat := 0 -- Data downloaded in current 30-second window
  lastResetRequests : Nat := 0 -- Timestamp (ms) of last request window reset
  lastResetData : Nat := 0 -- Timestamp (ms) of last data window reset
  deriving ToJson, FromJson, Inhabited, Repr

-- Updated GlobalRateLimitState
structure GlobalRateLimitState where
  totalRequests : Nat := 0 -- Total requests across all RPCs in 10-second window
  totalTokens : Nat := 100 -- Total available tokens (max 100 per 10 seconds)
  lastReset : Nat := 0 -- Timestamp (ms) of last reset
  rpcStates : List (String × RateLimitState) := [] -- Map RPC/node address to its state
  deriving ToJson, FromJson, Inhabited, Repr

-- Scheduler structure
structure Scheduler where
  publicRpcs : List String -- Public RPC URLs
  knownNodes : List RpcNode -- Known RPC nodes for snapshot checks
  globalState : GlobalRateLimitState -- Rate-limiting state
  deriving ToJson, FromJson, Inhabited, Repr


-- Existing structures from Solfunmeme.lean (unchanged, included for context)
-- [Previous structures like Pubkey, Entry, Ledger, TransactionDetails, etc., remain as provided]

instance : ToString RpcNode where
  toString n := s!"RpcNode(address: {n.address}, version: {n.version})"

structure Snapshot where
  slot : Int -- Snapshot slot number
  isIncremental : Bool -- True for incremental, False for full snapshot
  fileName : String -- Snapshot file name (e.g., "snapshot-123456789.tar.bz2")
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Snapshot where
  toString s := s!"Snapshot(slot: {s.slot}, isIncremental: {s.isIncremental}, fileName: {s.fileName})"

structure SnapshotInfo where
  node : RpcNode
  snapshot : Snapshot
  latency : Nat -- Latency in milliseconds
  slotsDiff : Nat -- Difference from current slot
  downloadSpeed : Option Nat -- Download speed in bytes/sec (None if not measured)
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString SnapshotInfo where
  toString si := s!"SnapshotInfo(node: {si.node}, snapshot: {si.snapshot}, latency: {si.latency}, slotsDiff: {si.slotsDiff}, downloadSpeed: {si.downloadSpeed})"

-- Configuration structure for snapshot finder
structure SnapshotConfig where
  rpcAddresses : List String := ["https://api.mainnet-beta.solana.com"] -- List of RPC endpoints
  specificSlot : Option Int := none -- Specific slot to search for
  specificVersion : Option String := none -- Specific node version
  wildcardVersion : Option String := none -- Major/minor version (e.g., "1.18")
  maxSnapshotAge : Nat := 1300 -- Max slot age
  minDownloadSpeed : Nat := 60 * 1000000 -- Min speed in bytes/sec (60 MB/s)
  maxDownloadSpeed : Option Nat := none -- Max speed in bytes/sec
  maxLatency : Nat := 100 -- Max latency in ms
  withPrivateRpc : Bool := false -- Include private RPCs
  measurementTime : Nat := 7 -- Time to measure speed (seconds)
  snapshotPath : String := "." -- Download path
  numRetries : Nat := 5 -- Max retry attempts
  sleepBeforeRetry : Nat := 7 -- Sleep between retries (seconds)
  sortOrder : String := "latency" -- "latency" or "slots_diff"
  ipBlacklist : List String := [] -- Blacklisted IP:port
  snapshotBlacklist : List String := [] -- Blacklisted slots or hashes
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString SnapshotConfig where
  toString c := s!"SnapshotConfig(rpcAddress: {c.rpcAddresses}, maxSnapshotAge: {c.maxSnapshotAge}, ...)" -- Abbreviated for brevity


-- Initialize scheduler
--def initScheduler (config : SnapshotConfig) : Scheduler := ()
  -- let initialState : GlobalRateLimitState :=
  --   {
  --     totalRequests := 0,
  --     totalTokens := 100,
  --     lastReset := 0,
  --     --rpcStates := () --config.rpcAddresses.map (fun addr => (addr, ⟨⟩)) }
  --     publicRpcs := config.rpcAddresses,
  --     knownNodes := knownRpcNodes,
  --     globalState := initialState
  --     }

-- Refresh tokens for an RPC/node
def refreshTokens (addr : String) (state : RateLimitState) (currentTime : Nat) : RateLimitState :=
  let windowSeconds := 10000 -- 10 seconds in ms
  if currentTime >= state.lastResetRequests + windowSeconds
  then { state with requests := 0, tokens := 40, connectionRate := 0, lastResetRequests := currentTime }
  else if state.tokens < 40 then { state with tokens := Nat.min 40 (state.tokens + 1) } -- Gradual refill
  else state

-- Refresh global tokens
--def refreshGlobalTokens (globalState : GlobalRateLimitState) (currentTime : Nat) : GlobalRateLimitState :=
--  let windowSeconds := 10000 -- 10 seconds in ms
  -- if currentTime >= globalState.lastReset + windowSeconds
  -- then {
  --   -- globalState with totalRequests := 0,
  --   --       totalTokens := 100,
  --   --       lastReset := currentTime
  --         --,
  --         --rpcStates := globalState.rpcStates.map (fun (addr, state) => (addr, refreshTokens addr state currentTime))
  --        }
  -- else { globalState with totalTokens := Nat.min 100 (globalState.totalTokens + 1), -- Gradual refill
  --        rpcStates := globalState.rpcStates.map (fun (addr, state) => (addr, refreshTokens addr state currentTime)) }

-- Check if a request is allowed
def canMakeRequest (addr : String) (globalState : GlobalRateLimitState) (currentTime : Nat) : Bool × GlobalRateLimitState :=
  let maxRequestsPerRpc := 40
  let maxTotalRequests := 100
  let maxConnections := 40
  let maxConnectionRate := 40
  --let globalState := refreshGlobalTokens globalState currentTime
  let rpcStateOpt := globalState.rpcStates.find? (fun (a, _) => a = addr)
  --let rpcState := rpcStateOpt.getD (addr, ⟨⟩) |>.snd
  --let updatedRpcState := refreshTokens addr rpcState currentTime
  -- let withinRpcLimits := updatedRpcState.requests < maxRequestsPerRpc &&
  --                       updatedRpcState.tokens > 0 &&
  --                       updatedRpcState.connections < maxConnections &&
  --                       updatedRpcState.connectionRate < maxConnectionRate
  let withinGlobalLimits := globalState.totalRequests < maxTotalRequests && globalState.totalTokens > 0
  --if withinRpcLimits && withinGlobalLimits
  -- then
  --   let newRpcState := { updatedRpcState with
  --     requests := updatedRpcState.requests + 1,
  --     tokens := updatedRpcState.tokens - 1,
  --     connections := updatedRpcState.connections + 1,
  --     connectionRate := updatedRpcState.connectionRate + 1 }
  --   let newRpcStates := globalState.rpcStates.filter (fun (a, _) => a ≠ addr) ++ [(addr, newRpcState)]
  --   let newGlobalState := { globalState with
  --     totalRequests := globalState.totalRequests + 1,
  --     totalTokens := globalState.totalTokens - 1,
  --     rpcStates := newRpcStates }
  --   (true, newGlobalState)
  -- else
  --   (false, globalState)
  sorry

-- Check data limit (unchanged from previous)
def canDownloadData (rpcAddr : String) (bytes : Nat) (globalState : GlobalRateLimitState) (currentTime : Nat) : Bool × GlobalRateLimitState :=
  let maxDataBytes := 100 * 1000000 -- 100 MB
  let windowSeconds := 30000 -- 30 seconds in ms
  let resetState (state : RateLimitState) : RateLimitState :=
    if currentTime >= state.lastResetData + windowSeconds
    then { state with dataBytes := 0, lastResetData := currentTime }
    else state
  let rpcStateOpt := globalState.rpcStates.find? (fun (addr, _) => addr = rpcAddr)
  --let rpcState := rpcStateOpt.getD (rpcAddr, ⟨⟩) |>.snd
  --let updatedRpcState := resetState rpcState
  -- if updatedRpcState.dataBytes + bytes <= maxDataBytes
  -- then
  --   let newRpcState := { updatedRpcState with dataBytes := updatedRpcState.dataBytes + bytes }
  --   let newRpcStates := globalState.rpcStates.filter (fun (addr, _) => addr ≠ rpcAddr) ++ [(rpcAddr, newRpcState)]
  --   let newGlobalState := { globalState with rpcStates := newRpcStates }
  --   (true, newGlobalState)
  -- else
  --   (false, globalState)
  sorry

-- Schedule a request to a public RPC
def schedulePublicRpcRequest (scheduler : Scheduler) (method : String) (payload : Json) : IO (Except String Json × Scheduler) := do
  -- let currentTime ← IO.monoMsNow
  -- let eligibleRpcs := scheduler.publicRpcs.filter (fun rpc =>
  --   let (canRequest, _) := canMakeRequest rpc scheduler.globalState currentTime
  --   canRequest)
  -- match eligibleRpcs with
  -- | [] =>
  --   IO.println "No public RPCs available due to rate limits"
  --   pure (Except.error "No available RPCs", scheduler)
  -- | rpc :: _ =>
  --   let (canRequest, newGlobalState) := canMakeRequest rpc scheduler.globalState currentTime
  --   if !canRequest then
  --     pure (Except.error "Rate limit exceeded for selected RPC", scheduler)
  --   else
  --     --let response ← callSolanaRpc method payload
  --     let response <- ""
  --     pure (response, { scheduler with globalState := newGlobalState })
  sorry

-- Schedule a snapshot check for a known node
def scheduleSnapshotCheck (scheduler : Scheduler) (node : RpcNode) (currentSlot : Slot) (config : SnapshotConfig) : IO (Except String (Option SnapshotInfo) × Scheduler) := do
  let currentTime ← IO.monoMsNow
  let (canRequest, newGlobalState) := canMakeRequest node.address scheduler.globalState currentTime
  if !canRequest then
    pure (Except.ok none, { scheduler with globalState := newGlobalState })
  else
    --let (result, newState) ← getSnapshotInfo node currentSlot config newGlobalState
    --pure (result, { scheduler with globalState := newState })
    sorry

-- Updated getCurrentSlot using scheduler
def getCurrentSlot (scheduler : Scheduler) : IO (Except String (Slot × Scheduler)) := do
  -- let payload := Json.mkObj [("jsonrpc", "2.0"), ("id", 1), ("method", "getSlot")]
  -- let (response, newScheduler) ← schedulePublicRpcRequest scheduler "getSlot" payload
  -- match response with
  -- | Except.error err => pure (Except.error s!"Failed to get current slot: {err}")
  -- | Except.ok json =>
  --   match json.getObjVal? "result" >>= Json.getNat? with
  --   | Except.ok slot => pure (Except.ok (slot, newScheduler))
  --   | Except.error err => pure (Except.error s!"Failed to parse slot: {err}")
  sorry

-- Updated getRpcNodes using scheduler
def getRpcNodes (scheduler : Scheduler) (config : SnapshotConfig) : IO (Except String (List RpcNode × Scheduler)) := do
  -- let payload := Json.mkObj [("jsonrpc", "2.0"), ("id", 1), ("method", "getClusterNodes")]
  -- let (response, newScheduler) ← schedulePublicRpcRequest newScheduler "getClusterNodes" payload
  -- match response with
  -- | Except.error err =>
  --   IO.println s!"Failed to get RPC nodes: {err}, using known nodes"
  --   pure (Except.ok (scheduler.knownNodes.filter (fun n =>
  --     !config.ipBlacklist.contains n.address &&
  --     (config.specificVersion.isNone || n.version == config.specificVersion) &&
  --     (config.wildcardVersion.isNone || n.version.getD "".startsWith (config.wildcardVersion.getD ""))), newScheduler))
  -- | Except.ok json =>
  --   match json.getObjVal? "result" >>= Json.getArr? with
  --   | Except.error err =>
  --     IO.println s!"Failed to parse nodes: {err}, using known nodes"
  --     pure (Except.ok (scheduler.knownNodes.filter (fun n =>
  --       !config.ipBlacklist.contains n.address &&
  --       (config.specificVersion.isNone || n.version == config.specificVersion) &&
  --       (config.wildcardVersion.isNone || n.version.getD "".startsWith (config.wildcardVersion.getD ""))), newScheduler))
  --   | Except.ok nodes =>
  --     let rpcNodes := nodes.filterMap (fun node =>
  --       let addr := node.getObjVal? "rpc" >>= Json.getStr?
  --       let version := node.getObjVal? "version" >>= Json.getStr?
  --       let gossip := if config.withPrivateRpc then node.getObjVal? "gossip" >>= Json.getStr? else Except.error ""
  --       match addr, version, gossip with
  --       | Except.ok a, Except.ok v, _ =>
  --         if config.ipBlacklist.contains a then none
  --         else if config.specificVersion.isSome && config.specificVersion ≠ some v then none
  --         else if config.wildcardVersion.isSome && !v.startsWith (config.wildcardVersion.getD "") then none
  --         else
  --           let knownNode := scheduler.knownNodes.find? (fun n => n.address = a)
  --           some { address := a, version := some v,
  --                  asn_id := knownNode.bind (fun n => n.asn_id),
  --                  network := knownNode.bind (fun n => n.network),
  --                  country := knownNode.bind (fun n => n.country),
  --                  isp := knownNode.bind (fun n => n.isp) }
  --       | Except.error _, Except.ok v, Except.ok g =>
  --         if config.withPrivateRpc && !config.ipBlacklist.contains (g.split ":").head! then
  --           let addr := s!"{(g.split ":").head!}:8899"
  --           let knownNode := scheduler.knownNodes.find? (fun n => n.address = addr)
  --           some { address := addr, version := some v,
  --                  asn_id := knownNode.bind (fun n => n.asn_id),
  --                  network := knownNode.bind (fun n => n.network),
  --                  country := knownNode.bind (fun n => n.country),
  --                  isp := knownNode.bind (fun n => n.isp) }
  --         else none
  --       | _, _, _ => none)
  --     let allNodes := (rpcNodes ++ scheduler.knownNodes).filter (fun n =>
  --       !config.ipBlacklist.contains n.address &&
  --       (config.specificVersion.isNone || n.version == config.specificVersion) &&
  --       (config.wildcardVersion.isNone || n.version.getD "".startsWith (config.wildcardVersion.getD "")))
  --     pure (Except.ok (allNodes.eraseDups.sort (fun a b => a.country.getD "" = "United States of America" && b.country.getD "" ≠ "United States of America"), newScheduler))
  sorry
-- Updated findSnapshot with scheduler
def findSnapshot (config : SnapshotConfig) : IO (Except String Unit) := do
  IO.println s!"Starting snapshot finder with config: {config}"
  sorry
  --IO.FS.createDirAll config.snapshotPath
--   let mut scheduler := initScheduler config
--   let mut attempt := 1
--   let localSnapshots ← IO.FS.readDir config.snapshotPath
--   let fullLocalSnapSlot := localSnapshots.files.filterMap (fun f =>
--     if f.fileName.startsWith "snapshot-" && (f.fileName.endsWith ".tar.bz2" || f.fileName.endsWith ".tar")
--     then (f.fileName.split (· = '-')).get? 1 >>= Json.getNat?
--     else none) |>.maximum?.getD 0
--   if fullLocalSnapSlot > 0 then
--     IO.println s!"Found local full snapshot with slot: {fullLocalSnapSlot}"
--   else
--     IO.println s!"No local full snapshots found in {config.snapshotPath}"
--   while attempt <= config.numRetries do
--     IO.println s!"Attempt {attempt} of {config.numRetries}"
--     let currentSlot ←
--       if config.specificSlot.isSome
--       then pure config.specificSlot.get!
--       else
--         let (res, newScheduler) ← getCurrentSlot scheduler
--         scheduler := newScheduler
--         match res with
--         | Except.ok slot => pure slot
--         | Except.error err =>
--           IO.println s!"Failed to get current slot: {err}"
--           IO.sleep (config.sleepBeforeRetry * 1000).toUInt32
--           attempt := attempt + 1
--           continue
--     IO.println s!"Current slot: {currentSlot}"
--     let (rpcNodes, newScheduler) ← getRpcNodes scheduler config
--     scheduler := newScheduler
--     IO.println s!"Found {rpcNodes.length} RPC nodes"
--     let batchSize := 10 -- Process 10 nodes at a time to maximize throughput
--     let mut snapshotInfos : List SnapshotInfo := []
--     for i in [:rpcNodes.length:batchSize] do
--       let batch := rpcNodes.slice i (i + batchSize)
--       let batchResults ← batch.mapM (fun node => scheduleSnapshotCheck scheduler node currentSlot config)
--       scheduler := batchResults.head!.2 -- Update scheduler state
--       snapshotInfos := snapshotInfos ++ batchResults.filterMap (fun (res, _) =>
--         match res with
--         | Except.ok (some info) => some info
--         | _ => none)
--     IO.println s!"Found {snapshotInfos.length} suitable snapshots"
--     let sortedInfos := if config.sortOrder = "slots_diff"
--       then snapshotInfos.sort (fun a b => a.slotsDiff < b.slotsDiff)
--       else snapshotInfos.sort (fun a b => a.latency < b.latency)
--     let suitableInfos ← sortedInfos.foldM (fun acc info =>
--       if acc.length >= 15 then pure acc
--       else
--         let (speedRes, newScheduler) ← measureDownloadSpeed info.node config scheduler.globalState
--         scheduler := { scheduler with globalState := newScheduler }
--         match speedRes with
--         | Except.ok speed =>
--           if speed >= config.minDownloadSpeed
--           then
--             IO.println s!"Suitable speed at {info.node.address} ({info.node.country.getD "Unknown"}, {info.node.isp.getD "Unknown"}): {speed / 1000000} MB/s"
--             pure ({ info with downloadSpeed := some speed } :: acc)
--           else
--             IO.println s!"Speed too slow at {info.node.address}: {speed / 1000000} MB/s"
--             pure acc
--         | Except.error err =>
--           IO.println s!"Failed to measure speed at {info.node.address}: {err}"
--           pure acc) []
--     scheduler := { scheduler with globalState := suitableInfos.2 }
--     for info in suitableInfos do
--       if info.snapshot.isIncremental && info.snapshot.slot ≤ fullLocalSnapSlot then
--         continue
--       IO.println s!"Attempting to download snapshot from {info.node.address}: {info.snapshot.fileName}"
--       let (res, newGlobalState) ← downloadSnapshot info config scheduler.globalState
--       scheduler := { scheduler with globalState := newGlobalState }
--       match res with
--       | Except.ok _ =>
--         IO.println s!"Successfully downloaded snapshot: {info.snapshot.fileName}"
--         return Except.ok ()
--       | Except.error err =>
--         IO.println s!"Failed to download from {info.node.address}: {err}"
--     if suitableInfos.isEmpty then
--       IO.println s!"No suitable snapshots found, enabling private RPCs"
--       config := { config with withPrivateRpc := true }
--     IO.println s!"Sleeping for {config.sleepBeforeRetry} seconds before retry"
--     IO.sleep (config.sleepBeforeRetry * 1000).toUInt32
--     attempt := attempt + 1
--   pure (Except.error "Failed to find suitable snapshot after all retries")

-- -- Updated SolfunmemeLean (unchanged public RPCs and config)
def SolfunmemeLean : IO Unit := do
--   IO.println s!"Introspecting token: {TOKEN_ADDRESS}"
--   IO.FS.createDirAll CACHE_DIR
--   IO.println s!"Using cache directory: {CACHE_DIR}"
--   IO.println "Fetching token info..."
--   let tokenInfo ← getTokenInfo TOKEN_ADDRESS
--   match tokenInfo with
--   | Except.error err =>
--     IO.println s!"Failed to fetch token info: {err}"
--   | Except.ok info =>
--     IO.println s!"Token Info: {info}"
--     IO.println "Fetching transaction signatures..."
--     let txSignatures ← getTransactionSignatures TOKEN_ADDRESS 1000
--     match txSignatures with
--     | Except.error err =>
--       IO.println s!"Failed to fetch transactions: {err}"
--     | Except.ok txs =>
--       IO.println s!"Fetched {txs.length} transaction signatures"
--       for sig in txs do
--         IO.println s!"Processing signature: {sig}"
--         let txDetails ← getTransactionDetails sig
--         match txDetails with
--         | Except.error err =>
--           IO.println s!"Failed to fetch transaction details: {err}"
--         | Except.ok details =>
--           IO.println s!"Transaction Details: {details}"
--   IO.println "Running snapshot finder..."
  let config : SnapshotConfig := {
    rpcAddresses := [
      "https://api.mainnet-beta.solana.com",
      "https://solana.drpc.org",
      "https://rpc.ankr.com/solana",
      "https://go.getblock.io/4136d34f90a6488b84214ae26f0ed5f4",
      "https://solana-rpc.publicnode.com",
      "https://api.blockeden.xyz/solana/67nCBdZQSH9z3YqDDjdm",
      "https://solana.leorpc.com/?api_key=FREE",
      "https://endpoints.omniatech.io/v1/sol/mainnet/public",
      "https://solana.api.onfinality.io/public"
    ],
     snapshotPath := "./snapshots",
     maxSnapshotAge := 1300,
     minDownloadSpeed := 50 * 1000000, -- 50 MB/s
     maxLatency := 100,
     numRetries := 3,
     sleepBeforeRetry := 5,
     wildcardVersion := some "1.10" -- Match known nodes' versions
   }
   let snapshotResult ← findSnapshot config
--   match snapshotResult with
--   | Except.ok _ => IO.println "Snapshot finder completed successfully"
--   | Except.error err => IO.println s!"Snapshot finder failed: {err}"
--   IO.println "Done processing token data and snapshot finding"

def main : IO Unit := do
  SolfunmemeLean
  sorry
