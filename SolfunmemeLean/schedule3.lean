import Std.Sync
import Lean.Data.Json
import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
open Lean Json ToJson FromJson

-- System constants
def MAX_CONCURRENT_REQUESTS : Nat := 40  -- Maximum concurrent requests across all servers
def SERVER_DISCOVERY_INTERVAL : Nat := 5000  -- Server discovery interval in ms
def SERVER_HEALTH_CHECK_INTERVAL : Nat := 3000  -- Server health check interval in ms
def DEFAULT_REQUEST_TIMEOUT : Nat := 2000  -- Default request timeout in ms

-- Core DFA structures
structure State where
  id : Nat
  name : String
  isFinal : Bool
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString State where
  toString s := s!"State(id: {s.id}, name: {s.name}, isFinal: {s.isFinal})"

structure Transition where
  fromState : Nat
  toState : Nat
  action : String
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Transition where
  toString t := s!"Transition(from: {t.fromState}, to: {t.toState}, action: {t.action})"

structure DFA where
  states : List State
  transitions : List Transition
  initialState : Nat
  deriving ToJson, FromJson, Inhabited, Repr

-- API-related structures
structure Endpoint where
  path : String
  method : String
  expectedStatus : Nat
  responseTimeout : Nat -- in ms
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Endpoint where
  toString e := s!"Endpoint(path: {e.path}, method: {e.method}, status: {e.expectedStatus})"

structure APIAction where
  id : String
  endpoint : Endpoint
  deriving ToJson, FromJson, Inhabited, Repr

structure Server where
  id : Nat
  baseUrl : String
  version : Option String
  status : String := "active"  -- active, degraded, offline
  lastHealthCheck : Option Nat := none  -- timestamp of last health check
  lastResponseTime : Option Nat := none  -- last measured response time in ms
  errorCount : Nat := 0  -- consecutive error count
  successRate : Float := 1.0  -- success rate (0.0-1.0)
  deriving ToJson, FromJson, Inhabited, Repr

instance : ToString Server where
  toString s := s!"Server(id: {s.id}, url: {s.baseUrl}, version: {s.version}, status: {s.status}, successRate: {s.successRate})"

-- Execution structures
structure ExecutionStep where
  state : State
  action : Option APIAction
  nextState : Option State
  server : Server
  timestamp : Nat
  success : Bool
  responseTime : Option Nat
  deriving ToJson, FromJson, Inhabited, Repr

structure ExecutionTrace where
  steps : List ExecutionStep
  completed : Bool
  totalSteps : Nat
  deriving ToJson, FromJson, Inhabited, Repr

-- Execution proof certificate
structure ExecutionProof where
  trace : ExecutionTrace
  statesVisited : List Nat
  finalStateReached : Bool
  executionTime : Nat
  deriving ToJson, FromJson, Inhabited, Repr

-- In-memory traversal state
structure TraversalState where
  currentState : Nat
  visitedStates : List Nat
  steps : Nat
  deriving Inhabited, Repr

-- Mock API call with proof of execution
def executeAPICall (action : APIAction) (server : Server) : IO (Bool × Option Nat) := do
  let startTime ← IO.monoMsNow
  IO.println s!"Executing {action.endpoint.method} {server.baseUrl}{action.endpoint.path}"
  -- Simulate network call
  IO.sleep 300--(action.endpoint.responseTimeout / 2).toUInt64
  let endTime ← IO.monoMsNow
  let responseTime := endTime - startTime
  -- For demo, we'll say it succeeds if responseTime is less than timeout
  let success := responseTime < action.endpoint.responseTimeout
  pure (success, some responseTime)

-- Create a step in the execution trace
def createExecutionStep (currentState : State) (action : Option APIAction) (nextState : Option State)
    (server : Server) (success : Bool) (responseTime : Option Nat) : IO ExecutionStep := do
  let timestamp ← IO.monoMsNow
  pure {
    state := currentState,
    action := action,
    nextState := nextState,
    server := server,
    timestamp := timestamp,
    success := success,
    responseTime := responseTime
  }

-- Find state by ID
def findState (dfa : DFA) (stateId : Nat) : Option State :=
  dfa.states.find? (fun s => s.id == stateId)

-- Find transitions from a state
def findTransitions (dfa : DFA) (stateId : Nat) : List Transition :=
  dfa.transitions.filter (fun t => t.fromState == stateId)

-- Find action mapping for a transition
def findAction (actions : List APIAction) (actionName : String) : Option APIAction :=
  actions.find? (fun a => a.id == actionName)

-- Core traversal function with step limit and proof generation
def traverseDFA (dfa : DFA) (actions : List APIAction) (server : Server) (maxSteps : Nat) : IO ExecutionProof := do
  let startTime ← IO.monoMsNow

  -- Initialize traversal state
  let initialState := (findState dfa dfa.initialState).get!
  let initialTS : TraversalState := {
    currentState := initialState.id,
    visitedStates := [initialState.id],
    steps := 0
  }

  let stateRef ← IO.mkRef initialTS
  let traceRef ← IO.mkRef ([] : List ExecutionStep)
  -- let trc : ExecutionTrace := {
  --   steps := [],
  --   completed := false,
  --   totalSteps := 0
  -- }
  let trc : ExecutionTrace := ExecutionTrace.mk [] false 0
   let initialProof : ExecutionProof := {
      trace := trc,
      statesVisited := [],
      finalStateReached := false,
      executionTime := 0 }

  pure initialProof
  -- [] false 0
  -- let initialProof : ExecutionProof := {
  --   trace := trc,
  --   statesVisited := [],
  --   finalStateReached := false,
  --   executionTime := 0
  -- }

  -- -- DFA traversal loop with explicit termination measure
  -- let rec traverse (stepsRemaining : Nat) : IO (Bool × List ExecutionStep) := do
  --   let ts ← stateRef.get

  --   -- Check termination conditions (explicit base case)
  --   if stepsRemaining = 0 then
  --     let trace ← traceRef.get
  --     return (false, trace) -- Terminated due to step limit

  --   let currentState := (findState dfa ts.currentState).get!
  --   if currentState.isFinal then
  --     let trace ← traceRef.get
  --     return (true, trace) -- Reached final state

  --   -- Find possible transitions
  --   let transitions := findTransitions dfa ts.currentState
  --   if transitions.isEmpty then
  --     let trace ← traceRef.get
  --     return (false, trace) -- Dead end

  --   -- Choose next transition (deterministically take first available)
  --   let transition := transitions.head!
  --   let nextStateOpt := findState dfa transition.toState

  --   match nextStateOpt with
  --   | none =>
  --       -- Invalid transition target
  --       let step ← createExecutionStep currentState none none server false none
  --       traceRef.modify (fun trace => trace ++ [step])
  --       let trace ← traceRef.get
  --       return (false, trace)

  --   | some nextState =>
  --       -- Get corresponding API action
  --       let actionOpt := findAction actions transition.action

  --       match actionOpt with
  --       | none =>
  --           -- Missing action mapping
  --           let step ← createExecutionStep currentState none (some nextState) server false none
  --           traceRef.modify (fun trace => trace ++ [step])
  --           let trace ← traceRef.get
  --           return (false, trace)

  --       | some action =>
  --           -- Execute API call
  --           let (success, responseTime) ← executeAPICall action server

  --           -- Record step
  --           let step ← createExecutionStep currentState (some action) (some nextState) server success responseTime
  --           traceRef.modify (fun trace => trace ++ [step])

  --           if !success then
  --             -- API call failed
  --             let trace ← traceRef.get
  --             return (false, trace)

  --           -- Update traversal state and continue
  --           stateRef.modify (fun s => {
  --             currentState := nextState.id,
  --             visitedStates := s.visitedStates ++ [nextState.id],
  --             steps := s.steps + 1
  --           })
  --           -- Recursive call with decreased termination measure
  --           traverse (stepsRemaining - 1)
def traverseDFA.traverse (dfa : DFA) (actions : List Action) (server : Server) (stateRef : IO.Ref TraversalState) (traceRef : IO.Ref (List ExecutionStep)) (stepsRemaining : Nat) : IO (Bool × List ExecutionStep) := do
  let rec traverse (stepsRemaining : Nat) : IO (Bool × List ExecutionStep) := do
    let ts ← stateRef.get

    -- Check termination conditions (explicit base case)
    if stepsRemaining = 0 then
      let trace ← traceRef.get
      return (false, trace) -- Terminated due to step limit

    let currentState := (findState dfa ts.currentState).get!
    if currentState.isFinal then
      let trace ← traceRef.get
      return (true, trace) -- Reached final state

    -- Find possible transitions
    let transitions := findTransitions dfa ts.currentState
    if transitions.isEmpty then
      let trace ← traceRef.get
      return (false, trace) -- Dead end

    -- Choose next transition (deterministically take first available)
    let transition := transitions.head!
    let nextStateOpt := findState dfa transition.toState

    match nextStateOpt with
    | none =>
        -- Invalid transition target
        let step ← createExecutionStep currentState none none server false none
        traceRef.modify (fun trace => trace ++ [step])
        let trace ← traceRef.get
        return (false, trace)

    | some nextState =>
        -- Get corresponding API action
        let actionOpt := findAction actions transition.action

        match actionOpt with
        | none =>
            -- Missing action mapping
            let step ← createExecutionStep currentState none (some nextState) server false none
            traceRef.modify (fun trace => trace ++ [step])
            let trace ← traceRef.get
            return (false, trace)

        | some action =>
            -- Execute API call
            let (success, responseTime) ← executeAPICall action server

            -- Record step
            let step ← createExecutionStep currentState (some action) (some nextState) server success responseTime
            traceRef.modify (fun trace => trace ++ [step])

            if !success then
              -- API call failed
              let trace ← traceRef.get
              return (false, trace)

            -- Update traversal state and continue
            stateRef.modify (fun s => {
              currentState := nextState.id,
              visitedStates := s.visitedStates ++ [nextState.id],
              steps := s.steps + 1
            })
            -- Recursive call with decreased termination measure
            traverse (stepsRemaining - 1)
  termination_by stepsRemaining
  decreasing_by
    simp_wf
    apply Nat.sub_one_lt_of_ne_zero
    intro h
    contradiction
  -- Execute traversal with explicit max steps
  let (finalStateReached, trace) ← traverse maxSteps
  let ts ← stateRef.get
  let endTime ← IO.monoMsNow

  -- Construct execution proof
  pure {
    trace := {
      steps := trace,
      completed := finalStateReached,
      totalSteps := ts.steps
    },
    statesVisited := ts.visitedStates,
    finalStateReached := finalStateReached,
    executionTime := (endTime - startTime).toNat
  }

-- Server pool management
structure ServerPool where
  servers : List Server
  mutex : Std.Mutex
  condvar : Std.Condvar

def createServerPool (initialServers : List Server) : IO ServerPool := do
  let mutex ← Std.Mutex.new initialServers
  let condvar ← Std.Condvar.new
  pure { servers := initialServers, mutex, condvar }

-- Discover new servers (mock implementation)
def discoverServers : IO (List Server) := do
  -- In a real implementation, this would make API calls to discover servers
  -- For this example, we'll create random servers
  let now ← IO.monoMsNow
  let r1 ← IO.rand 100 999
  let r2 ← IO.rand 100 999

  pure [
    { id := r1, baseUrl := s!"https://api{r1}.example.com", version := some "1.0", lastHealthCheck := some now.toNat },
    { id := r2, baseUrl := s!"https://api{r2}.example.com", version := some "1.1", lastHealthCheck := some now.toNat }
  ]

-- Check server health
def checkServerHealth (server : Server) : IO Server := do
  let startTime ← IO.monoMsNow

  -- Simulate health check request
  IO.sleep (DEFAULT_REQUEST_TIMEOUT / 10).toUInt64
  let success ← IO.rand 0 10 -- Simulate 90% success rate for health checks

  let endTime ← IO.monoMsNow
  let responseTime := endTime - startTime

  if success < 9 then
    -- Health check succeeded
    let newSuccessRate := 0.9 * server.successRate + 0.1 * 1.0 -- EMA of success rate
    let newStatus := if newSuccessRate > 0.8 then "active" else "degraded"

    pure {
      server with
      status := newStatus,
      lastHealthCheck := some endTime.toNat,
      lastResponseTime := some responseTime.toNat,
      errorCount := 0,
      successRate := newSuccessRate
    }
  else
    -- Health check failed
    let newErrorCount := server.errorCount + 1
    let newSuccessRate := 0.9 * server.successRate + 0.1 * 0.0 -- EMA of success rate
    let newStatus := if newErrorCount > 3 then "offline" else "degraded"

    pure {
      server with
      status := newStatus,
      lastHealthCheck := some endTime.toNat,
      lastResponseTime := some responseTime.toNat,
      errorCount := newErrorCount,
      successRate := newSuccessRate
    }

-- Server discovery thread
def serverDiscoveryThread (pool : ServerPool) : IO Unit := do
  -- Termination handled by explicit cancellation from main thread
  let rec loop (iterCount : Nat) : IO Unit := do
    if iterCount = 0 then
      -- This branch is unreachable but needed for termination proof
      pure ()
    else
      -- Discover new servers
      let newServers ← discoverServers
      IO.println s!"Discovered {newServers.length} new server(s)"

      -- Update pool with new servers
      pool.mutex.atomically do
        let currentServers ← get
        let newServerIds := newServers.map (·.id)
        let filteredCurrent := currentServers.filter (fun s => s.id ∉ newServerIds)
        set (filteredCurrent ++ newServers)

      pool.condvar.notifyAll

      -- Wait for next discovery interval
      IO.sleep SERVER_DISCOVERY_INTERVAL.toUInt64
      -- Continue with decremented counter for termination proof
      loop (iterCount - 1)

  -- Use UInt64.max as practical infinity for iterCount
  loop UInt64.toNat UInt64.max

-- Server health monitor thread
def serverHealthMonitorThread (pool : ServerPool) : IO Unit := do
  -- Termination handled by explicit cancellation from main thread
  let rec loop (iterCount : Nat) : IO Unit := do
    if iterCount = 0 then
      -- This branch is unreachable but needed for termination proof
      pure ()
    else
      -- Get current servers
      let servers ← pool.mutex.atomically get

      -- Check health of each server
      let updatedServers ← servers.mapM checkServerHealth

      -- Update pool with health status
      pool.mutex.atomically (fun _ => set updatedServers)
      pool.condvar.notifyAll

      -- Print server status summary
      let activeCount := updatedServers.filter (·.status == "active").length
      let degradedCount := updatedServers.filter (·.status == "degraded").length
      let offlineCount := updatedServers.filter (·.status == "offline").length

      IO.println s!"Server pool status: {activeCount} active, {degradedCount} degraded, {offlineCount} offline (total: {updatedServers.length})"

      -- Wait for next health check interval
      IO.sleep SERVER_HEALTH_CHECK_INTERVAL.toUInt64
      -- Continue with decremented counter for termination proof
      loop (iterCount - 1)

  -- Use UInt64.max as practical infinity for iterCount
  loop UInt64.toNat UInt64.max

-- Task throttling semaphore
def createTaskThrottler : IO (Nat × Std.Mutex × Std.Condvar) := do
  let mutex ← Std.Mutex.new MAX_CONCURRENT_REQUESTS
  let condvar ← Std.Condvar.new
  pure (MAX_CONCURRENT_REQUESTS, mutex, condvar)

-- Acquire a task slot
def acquireTaskSlot (throttler : Nat × Std.Mutex × Std.Condvar) : IO Unit := do
  let (_, mutex, condvar) := throttler

  let rec tryAcquire := do
    let available ← mutex.atomically do
      let count ← get
      if count > 0 then
        set (count - 1)
        pure true
      else
        pure false

    if available then
      pure ()
    else
      condvar.wait mutex
      tryAcquire

  tryAcquire

-- Release a task slot
def releaseTaskSlot (throttler : Nat × Std.Mutex × Std.Condvar) : IO Unit := do
  let (max, mutex, condvar) := throttler

  mutex.atomically do
    let count ← get
    if count < max then
      set (count + 1)

  condvar.notifyAll

-- Run a throttled task
def runThrottledTask (throttler : Nat × Std.Mutex × Std.Condvar) (task : IO α) : IO α := do
  acquireTaskSlot throttler
  try
    let result ← task
    pure result
  finally
    releaseTaskSlot throttler

-- Choose a server from the pool based on status and load
def chooseServer (pool : ServerPool) : IO (Option Server) := do
  let servers ← pool.mutex.atomically get

  -- Filter for active servers
  let activeServers := servers.filter (·.status == "active")
  if activeServers.isEmpty then
    -- Try degraded servers if no active ones
    let degradedServers := servers.filter (·.status == "degraded")
    if degradedServers.isEmpty then
      pure none
    else
      -- Choose server with best success rate among degraded
      let best := degradedServers.foldl
        (fun best srv => if srv.successRate > best.successRate then srv else best)
        degradedServers.head!
      pure (some best)
  else
    -- Choose server with best success rate among active
    let best := activeServers.foldl
      (fun best srv => if srv.successRate > best.successRate then srv else best)
      activeServers.head!
    pure (some best)

-- Update server statistics after an API call
def updateServerStats (pool : ServerPool) (server : Server) (success : Bool) : IO Unit := do
  pool.mutex.atomically do
    let servers ← get
    let idx := servers.findIdx? (·.id == server.id)
    match idx with
    | some i =>
        let oldServer := servers[i]
        let newSuccessRate := 0.9 * oldServer.successRate + 0.1 * (if success then 1.0 else 0.0)
        let newErrorCount := if success then 0 else oldServer.errorCount + 1
        let newStatus :=
          if success then
            if newSuccessRate > 0.8 then "active" else "degraded"
          else
            if newErrorCount > 3 then "offline" else "degraded"

        let now ← IO.monoMsNow
        let newServer := {
          oldServer with
          status := newStatus,
          errorCount := newErrorCount,
          successRate := newSuccessRate,
          lastHealthCheck := some now.toNat
        }

        let newServers := servers.set i newServer
        set newServers
    | none =>
        pure ()

-- Execute API call with server tracking
def executeAPICallWithTracking (action : APIAction) (server : Server) (pool : ServerPool) : IO (Bool × Option Nat) := do
  let startTime ← IO.monoMsNow
  IO.println s!"Executing {action.endpoint.method} {server.baseUrl}{action.endpoint.path}"

  -- Simulate network call
  IO.sleep (action.endpoint.responseTimeout / 2).toUInt64

  -- Random success with bias based on server status
  let baseSuccessProb :=
    if server.status == "active" then 0.95
    else if server.status == "degraded" then 0.7
    else 0.3

  let r ← IO.rand 0 100
  let success := r < (baseSuccessProb * 100).toUInt32

  let endTime ← IO.monoMsNow
  let responseTime := endTime - startTime

  -- Update server stats based on call result
  updateServerStats pool server success

  pure (success, some responseTime.toNat)

-- Enhanced DFA traversal with server tracking and failover
def traverseDFAWithFailover (dfa : DFA) (actions : List APIAction) (pool : ServerPool) (maxSteps : Nat)
    (throttler : Nat × Std.Mutex × Std.Condvar) : IO ExecutionProof := do
  let startTime ← IO.monoMsNow

  -- Initialize traversal state
  let initialState := (findState dfa dfa.initialState).get!
  let initialTS : TraversalState := {
    currentState := initialState.id,
    visitedStates := [initialState.id],
    steps := 0
  }

  let stateRef ← IO.mkRef initialTS
  let traceRef ← IO.mkRef ([] : List ExecutionStep)

  -- Get initial server
  let initialServerOpt ← chooseServer pool
  match initialServerOpt with
  | none =>
      -- No servers available
      IO.println "Error: No servers available for traversal"
      let now ← IO.monoMsNow
      pure {
        trace := {
          steps := [],
          completed := false,
          totalSteps := 0
        },
        statesVisited := [],
        finalStateReached := false,
        executionTime := (now - startTime).toNat
      }

  | some initialServer =>
      let serverRef ← IO.mkRef initialServer

      -- DFA traversal loop with step limit and server failover
      -- Use explicit decreasing measure for termination proof
      let rec traverse (stepsRemaining : Nat) (retriesLeft : Nat) : IO (Bool × List ExecutionStep) := do
        if stepsRemaining = 0 then
          -- Reached max steps - explicit base case for termination proof
          let trace ← traceRef.get
          return (false, trace)

        if retriesLeft = 0 then
          -- Reached max retries - explicit base case for termination proof
          let trace ← traceRef.get
          return (false, trace)

        let ts ← stateRef.get

        -- Check termination condition for final state
        let currentState := (findState dfa ts.currentState).get!
        if currentState.isFinal then
          let trace ← traceRef.get
          return (true, trace) -- Reached final state

        -- Find possible transitions
        let transitions := findTransitions dfa ts.currentState
        if transitions.isEmpty then
          let trace ← traceRef.get
          return (false, trace) -- Dead end

        -- Choose next transition (deterministically take first available)
        let transition := transitions.head!
        let nextStateOpt := findState dfa transition.toState

        match nextStateOpt with
        | none =>
            -- Invalid transition target
            let currentServer ← serverRef.get
            let step ← createExecutionStep currentState none none currentServer false none
            traceRef.modify (fun trace => trace ++ [step])
            let trace ← traceRef.get
            return (false, trace)

        | some nextState =>
            -- Get corresponding API action
            let actionOpt := findAction actions transition.action

            match actionOpt with
            | none =>
                -- Missing action mapping
                let currentServer ← serverRef.get
                let step ← createExecutionStep currentState none (some nextState) currentServer false none
                traceRef.modify (fun trace => trace ++ [step])
                let trace ← traceRef.get
                return (false, trace)

            | some action =>
                -- Get current server
                let currentServer ← serverRef.get

                -- Execute API call with throttling
                let resultTask := runThrottledTask throttler do
                  executeAPICallWithTracking action currentServer pool

                let (success, responseTime) ← resultTask

                -- Record step
                let step ← createExecutionStep currentState (some action) (some nextState) currentServer success responseTime
                traceRef.modify (fun trace => trace ++ [step])

                if !success then
                  -- API call failed - try with a different server
                  let newServerOpt ← chooseServer pool
                  match newServerOpt with
                  | none =>
                      -- No more servers available
                      let trace ← traceRef.get
                      return (false, trace)

                  | some newServer =>
                      -- Update current server and retry same state (don't decrement stepsRemaining)
                      -- But decrement retriesLeft for termination guarantee
                      serverRef.set newServer
                      IO.println s!"Server failover: switching to {newServer.baseUrl}"
                      traverse stepsRemaining (retriesLeft - 1)

                else
                  -- API call succeeded, update traversal state and continue
                  stateRef.modify (fun s => {
                    currentState := nextState.id,
                    visitedStates := s.visitedStates ++ [nextState.id],
                    steps := s.steps + 1
                  })
                  -- Decrement stepsRemaining for termination proof
                  traverse (stepsRemaining - 1) maxSteps -- Reset retry counter

      -- Execute traversal with explicit termination measures
      let (finalStateReached, trace) ← traverse maxSteps maxSteps -- Max steps and max retries
      let ts ← stateRef.get
      let endTime ← IO.monoMsNow

      -- Construct execution proof
      pure {
        trace := {
          steps := trace,
          completed := finalStateReached,
          totalSteps := ts.steps
        },
        statesVisited := ts.visitedStates,
        finalStateReached := finalStateReached,
        executionTime := (endTime - startTime).toNat
      }

-- Run multiple traversals with concurrent request limit
def batchTraverseWithConcurrency (dfa : DFA) (actions : List APIAction) (pool : ServerPool) (maxSteps : Nat)
    (numTraversals : Nat) : IO (List ExecutionProof) := do
  -- Create request throttler
  let throttler ← createTaskThrottler

  -- Create tasks for each traversal
  let tasks ← List.range numTraversals |>.mapM (fun _ => do
    let task ← IO.asTask (traverseDFAWithFailover dfa actions pool maxSteps throttler)
    pure task)

  -- Wait for all tasks to complete
  tasks.mapM IO.wait

-- Define a sample DFA for REST API traversal
def createSampleDFA : DFA := {
  states := [
    { id := 1, name := "Start", isFinal := false },
    { id := 2, name := "Auth", isFinal := false },
    { id := 3, name := "Fetch", isFinal := false },
    { id := 4, name := "Process", isFinal := false },
    { id := 5, name := "Complete", isFinal := true }
  ],
  transitions := [
    { fromState := 1, toState := 2, action := "login" },
    { fromState := 2, toState := 3, action := "getItems" },
    { fromState := 3, toState := 4, action := "processItem" },
    { fromState := 4, toState := 5, action := "commit" }
  ],
  initialState := 1
}

-- Define sample API actions
def createSampleActions : List APIAction := [
  {
    id := "login",
    endpoint := {
      path := "/api/auth",
      method := "POST",
      expectedStatus := 200,
      responseTimeout := 500
    }
  },
  {
    id := "getItems",
    endpoint := {
      path := "/api/items",
      method := "GET",
      expectedStatus := 200,
      responseTimeout := 300
    }
  },
  {
    id := "processItem",
    endpoint := {
      path := "/api/process",
      method := "POST",
      expectedStatus := 200,
      responseTimeout := 700
    }
  },
  {
    id := "commit",
    endpoint := {
      path := "/api/commit",
      method := "PUT",
      expectedStatus := 200,
      responseTimeout := 400
    }
  }
]

-- Define sample servers
def createSampleServers : List Server := [
  { id := 1, baseUrl := "https://api1.example.com", version := some "1.0" },
  { id := 2, baseUrl := "https://api2.example.com", version := some "1.1" }
]

-- Print execution statistics
def printProofStats (proofs : List ExecutionProof) : IO Unit := do
  -- Overall statistics
  let totalProofs := proofs.length
  let completedProofs := proofs.filter (·.finalStateReached).length
  let avgSteps := proofs.foldl (fun sum p => sum + p.trace.totalSteps) 0 / totalProofs
  let avgTime := proofs.foldl (fun sum p => sum + p.executionTime) 0 / totalProofs

  IO.println "=================================="
  IO.println "EXECUTION SUMMARY"
  IO.println "=================================="
  IO.println s!"Total traversals: {totalProofs}"
  IO.println s!"Completed traversals: {completedProofs} ({(completedProofs * 100) / totalProofs}%)"
  IO.println s!"Average steps per traversal: {avgSteps}"
  IO.println s!"Average execution time: {avgTime}ms"
  IO.println "=================================="
  IO.println ""

  -- Print individual proof details
  for i in List.range (proofs.length) do
    let proof := proofs[i]
    if proof.trace.steps.isEmpty then
      IO.println s!"Traversal #{i+1}: No steps recorded (likely no servers available)"
      IO.println ""
    else
      let server := proof.trace.steps.head!.server
      IO.println s!"Traversal #{i+1} - Server {server.baseUrl} (v{server.version.getD \"unknown\"}):"
      IO.println s!"  Status: {if proof.finalStateReached then "COMPLETED" else "INCOMPLETE"}"
      IO.println s!"  States visited: {proof.statesVisited}"
      IO.println s!"  Steps taken: {proof.trace.totalSteps}/{proof.trace.steps.length}"
      IO.println s!"  Execution time: {proof.executionTime}ms"

      -- Show trace details
      IO.println "  Trace summary:"
      for j in List.range (proof.trace.steps.length) do
        let step := proof.trace.steps[j]
        let actionStr := match step.action with
          | none => "NO ACTION"
          | some action => s!"{action.endpoint.method} {action.endpoint.path}"

        let resultStr := if step.success then "✓" else "✗"
        let timeStr := match step.responseTime with
          | none => "N/A"
          | some time => s!"{time}ms"

        IO.println s!"    {j+1}. {step.state.name} → {actionStr} → {resultStr} ({timeStr})"

      IO.println ""

-- Main
def main : IO Unit := do
  IO.println "Starting Provable REST API Traversal Scheduler with Dynamic Server Pool"

  -- Set up sample data
  let dfa := createSampleDFA
  let actions := createSampleActions
  let initialServers := createSampleServers
  let maxSteps := 10
  let numTraversals := 30  -- Number of traversals to run

  -- Create server pool
  IO.println "Initializing server pool..."
  let pool ← createServerPool initialServers

  -- Start server discovery thread
  IO.println "Starting server discovery thread..."
  let discoveryTask ← IO.asTask (serverDiscoveryThread pool) Task.Priority.dedicated

  -- Start server health monitoring thread
  IO.println "Starting server health monitoring thread..."
  let healthTask ← IO.asTask (serverHealthMonitorThread pool) Task.Priority.dedicated

  -- Give time for initial server discovery and health checks
  IO.println "Waiting for initial server discovery and health checks..."
  IO.sleep 2000

  -- Run batch traversal with concurrency control
  IO.println s!"Executing {numTraversals} traversals with max {maxSteps} steps each (max {MAX_CONCURRENT_REQUESTS} concurrent requests)"
  let startTime ← IO.monoMsNow
  let proofs ← batchTraverseWithConcurrency dfa actions pool maxSteps numTraversals
  let endTime ← IO.monoMsNow

  -- Print detailed results
  IO.println s!"Batch execution completed in {endTime - startTime}ms"
  printProofStats proofs

  -- Final server pool status
  let finalServers ← pool.mutex.atomically get
  IO.println "Final server pool status:"
  for server in finalServers do
    IO.println s!"  {server}"

  -- Clean shutdown
  IO.println "Shutting down scheduler..."
  IO.cancel discoveryTask
  IO.cancel healthTask
  IO.sleep 1000

  IO.println "Scheduler terminated successfully"
  pure ()
