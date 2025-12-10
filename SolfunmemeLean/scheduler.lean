import Std.Sync
import Lean.Data.Json
import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
open Lean Json ToJson FromJson


structure Request where
  id : Nat
  type : String  -- "R1" (100 ms) or "R2" (150 ms)
  duration : Nat -- ms
    deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Request where
    toString r := s!"Request(id: {r.id}, type: {r.type}, duration: {r.duration})"

structure Server where
  id : Nat
  url : String   -- e.g., Solana beta URL
  version : Option String -- Node version (e.g., "1.10.40")
  asn_id : Option String -- ASN ID (e.g., "396356")
  network : Option String -- Network (e.g., "103.50.32.0/24")
  country : Option String -- Country (e.g., "Brazil")
  isp : Option String -- ISP (e.g., "LATITUDE-SH, BR")

  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Server where
    toString s := s!"Server(id: {s.id}, url: {s.url}, version: {s.version}, asn_id: {s.asn_id}, network: {s.network}, country: {s.country}, isp: {s.isp})"

structure Slot where
  request : Request
  server : Server
  deriving ToJson, FromJson, Inhabited, Repr

structure MCount where
  threadId : Nat
  serverId : Nat
  reqType : String
  count : Nat
  deriving ToJson, FromJson, Inhabited, Repr

-- -- Data types
-- structure Request where
--   id : Nat
--   type : String
--   duration : Nat
-- deriving Repr

-- structure Server where
--   id : Nat
--   url : String
-- deriving Repr

-- structure Slot where
--   request : Request
--   server : Server
-- deriving Repr

-- structure MCount where
--   threadId : Nat
--   serverId : Nat
--   reqType : String
--   count : Nat
-- deriving Repr

-- Mock Solana API call
def discoverServers : IO (List Server) := do
  -- Simulate API call returning new servers
  pure [
      (Server.mk 2 "https://api2.solana.com" none none none none none),
      (Server.mk 3 "https://api3.solana.com" none none none none none),

      ]

-- Check server performance (mock)
def checkServerPerformance (server : Server) : IO Bool := do
  -- Simulate checking response time < 500 ms
  pure (server.id % 2 == 0) -- Mock: even IDs are responsive

-- Execute a request (mock)
def executeRequest (req : Request) (server : Server) : IO Unit := do
  IO.println s!"Executing {req} on {server}"
  IO.sleep 300 -- req.duration.toUInt64 -- Simulate duration
  pure ()

-- Server Pool Manager
def serverPoolManager (channel : Std.Channel (List Server)) : IO Unit := do
  let initialServer := [Server.mk 1 "https://api.solana.com" none none none none none]
  let mutex ← Std.Mutex.new initialServer
  let condvar ← Std.Condvar.new
  let rec loop := do
    -- Discover new servers
    let newServers ← discoverServers
    mutex.atomically do
      let current ← get
      set (current ++ newServers.filter (fun s => s ∉ current))
    condvar.notifyAll
    -- Check and remove slow servers
    mutex.atomically do
      let current ← get
      let filtered ← current.filterM checkServerPerformance
      set filtered
    condvar.notifyAll
    -- Send updated server list
    let servers ← mutex.atomically get
    channel.send servers
    IO.sleep 10000 -- Run every 10 seconds
    loop
  loop

-- Scheduler
def scheduler (channel : Std.Channel (List Server)) : IO Unit := do
  let rec processBlock (blockId : Nat) (startReqId : Nat) := do
    -- Receive server list
    let serversTask ← channel.recv?
    let servers ← IO.wait serversTask
    match servers with
    | some servers =>
        if servers.isEmpty then
          IO.println "No servers available, retrying..."
          IO.sleep 1000
          processBlock blockId startReqId
        else
          let S := servers.length
          -- Generate requests
          let requests : List Request := List.range 100 |>.map fun i =>
            let id := startReqId + i
            if id % 2 == 0 then
              { id, type := "R1", duration := 100 }
            else
              { id, type := "R2", duration := 150 }
          -- Compute slot array (pure task)
          let slotArrayTask := Task.spawn (fun _ => List.range 100 |>.map fun i =>
            let server := servers[i % S]
            let req := requests[i]
            { request := req, server })
          let slots ← slotArrayTask.get
          -- Group into batches
          let batches := [(0, 80), (80, 96), (96, 100)] -- 80, 16, 4
          let tasks ← batches.mapM fun (start, end) => do
            let batchSlots := slots.slice start (end - start)
            let batchTasks ← batchSlots.mapM fun slot => do
              let task ← IO.asTask (executeRequest slot.request slot.server)
              pure task
            pure batchTasks
          -- Execute batches sequentially (respect dependencies)
          for batchTasks in tasks do
            let results ← batchTasks.mapM IO.wait
            pure () -- Cache results in IO.Ref if needed
          -- Calculate M
          let mCounts ← slots.groupBy (fun slot => (slot.request.type, slot.server.id)) |>.toList.mapM fun ((reqType, serverId), group) => do
            let count := group.length
            let threadId ← IO.getTID
            pure { threadId, serverId, reqType, count }
          IO.println s!"Block {blockId} M Counts: {mCounts}"
          -- Next block
          processBlock (blockId + 1) (startReqId + 100)
    | none =>
        IO.println "Channel closed, stopping scheduler"
        pure ()
  processBlock 1 1

-- Main
def main : IO Unit := do
  IO.println "Starting Solana Scheduler"
  let channel ← Std.Channel.new
  let poolTask ← IO.asTask (serverPoolManager channel) Task.Priority.dedicated
  let schedulerTask ← IO.asTask (scheduler channel)
  IO.wait poolTask
  IO.wait schedulerTask
  pure ()
