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

import Std.Time.Time.Unit.Second
import Std.Time.DateTime.Timestamp

import SolfunmemeLean.Common
open Lean Json ToJson FromJson

-- Ensure BEq instance for Slot
--instance : BEq Slot where
  --beq s1 s2 := s1 == s2



--deriving FromJson, Inhabited --, Repr

-- Thank you for the detailed request! You want to perform feature extraction on the TransactionDetails data for your meme coin’s transactions on the Solana blockchain, counting specific features (including treating each unique signature as a feature with a count of 1) for each transaction. The goal is to reduce the data into collections of counts for features like error types, memos, confirmation statuses, and other transaction attributes. These counts will be aggregated:
-- Per block: For each Solana block (identified by slot or blockTime).
-- Total: Across all blocks in your provided block time ranges.
-- The results will be presented in a report with each feature as a column, showing counts per block and overall totals.
-- This feature extraction will help analyze trading activity, quantify errors, and identify patterns (e.g., high error rates during gaps) to confirm your observation of declining interest (noted in large gaps like ~11.36 days in March 2025). I’ll leverage the TransactionDetails structure, your block time ranges (e.g., [(1736974381, 1736983149), ...] with ~21M blocks), and the provided functions (findBlocksInTransaction, foldBlocksIntoRanges, mergeRanges) to design this solution.
-- Step 1: Understanding the Requirements
-- Input
-- TransactionDetails Structure:
-- typescript
-- type TransactionDetails = {
--   signature: string;              // Unique transaction ID (base58)
--   blockTime: number;             // Unix timestamp (Nat)
--   slot: number;                  // Solana slot (block ID)
--   memo: string | null;           // Optional memo
--   err: object | null;            // Optional error JSON (e.g., {"InstructionError": [...]})
--   confirmationStatus: string;     // e.g., "finalized"
-- }
-- Block Time Ranges: 99 ranges (Jan 15–Apr 25, 2025), e.g., [(1736974381, 1736983149, 21923), ..., (1741900264, 1745872004, 9929353)], representing trading periods with ~21M blocks total. Gaps (e.g., 11.36 days in March) indicate no trades.
-- Functions: findBlocksInTransaction, foldBlocksIntoRanges, mergeRanges to process transactions into ranges.
-- Features to Extract
-- You want to count features for each transaction, including:
-- Identity Signature:
-- Each unique signature counts as 1 (i.e., count the number of transactions).
-- Feature: transaction_count (1 per transaction).
-- Error Types:
-- Count occurrences of specific error types in err (e.g., InstructionError, InvalidAccountData).
-- Features: One column per error type (e.g., err_InstructionError, err_InvalidAccountData).
-- Other Features (inferred from TransactionDetails):
-- Memo: Count transactions with/without memos or specific memo values.
-- Features: has_memo, memo_<value> (for common memos).
-- Confirmation Status: Count occurrences of each status (e.g., finalized, confirmed).
-- Feature: status_finalized, status_confirmed, etc.
-- BlockTime/Slot: Group counts by block (via slot or blockTime) for per-block aggregation.
-- Output
-- Report Format:
-- A table with columns for each feature (e.g., transaction_count, err_InstructionError, has_memo, status_finalized).
-- Per Block: Rows for each block (identified by slot or blockTime), showing feature counts.
-- Total: A final row with summed counts across all blocks.
-- Example (simplified):
-- | BlockTime | Slot       | transaction_count | err_InstructionError | has_memo | status_finalized |
-- |-----------|------------|-------------------|----------------------|----------|------------------|
-- | 1736974385| 123456789  | 5                 | 1                    | 2        | 5                |
-- | 1736974386| 123456790  | 3                 | 0                    | 1        | 3                |
-- | ...       | ...        | ...               | ...                  | ...      | ...              |
-- | Total     |            | 1000              | 50                   | 300      | 950              |
-- Use Case
-- Declining Interest: Analyze feature counts to identify patterns:
-- Low transaction_count in March (during gaps) confirms no trades.
-- High err_* counts may indicate issues (e.g., liquidity, smart contract errors) causing trading lulls.
-- April’s high block count (~9.9M blocks) should show high transaction_count, indicating a trading surge.
-- Actionable Insights: Use error counts to debug issues, memo counts to track campaign effects, and transaction counts to measure engagement.
-- Step 2: Feature Extraction Design
-- To extract and count features, we’ll:
-- Define the set of features to count.
-- Process TransactionDetails entries to compute counts per transaction.
-- Aggregate counts per block and total.
-- Generate the report.
-- 1. Define Features
-- Based on TransactionDetails, we’ll extract:
-- transaction_count: 1 per transaction (counts unique signature).
-- Error Types:
-- Parse err (JSON object) to identify error types.
-- Common Solana errors (based on Solana docs):
-- InstructionError: Subtypes like InvalidAccountData, InvalidInstructionData, MissingRequiredSignature.
-- ProgramError: Generic program failures.
-- Others: InsufficientFunds, AccountInUse.
-- Features: err_<ErrorType> (e.g., err_InstructionError_InvalidAccountData).
-- Memo:
-- has_memo: 1 if memo is non-null, 0 otherwise.
-- memo_<value>: Count specific memo values (e.g., memo_BuyMyMemeCoin) for common memos.
-- Confirmation Status:
-- status_<value>: Count each status (e.g., status_finalized, status_confirmed).
-- Optional (if needed):
-- blockTime: Group by blockTime or slot for per-block counts.
-- Custom features: If your meme coin has specific transaction patterns (e.g., swap, burn), add features like is_swap, is_burn (requires transaction instruction parsing).
-- 2. Extract Features per Transaction
-- For each TransactionDetails, compute a feature vector:
-- typescript
-- function extractFeatures(td: TransactionDetails): Record<string, number> {
--   const features: Record<string, number> = {
--     transaction_count: 1,  // Count each transaction
--     [`status_${td.confirmationStatus}`]: 1,  // e.g., status_finalized: 1
--     has_memo: td.memo ? 1 : 0,  // 1 if memo exists
--   };

--   // Add specific memo value (if memo exists and is common)
--   if (td.memo) {
--     features[`memo_${td.memo}`] = 1;
--   }

--   // Add error types
--   if (td.err) {
--     // Example err: { "InstructionError": [0, "InvalidAccountData"] }
--     if ("InstructionError" in td.err) {
--       const errType = td.err.InstructionError[1];  // e.g., "InvalidAccountData"
--       features[`err_InstructionError_${errType}`] = 1;
--     } else if ("ProgramError" in td.err) {
--       features[`err_ProgramError`] = 1;
--     } // Add other error types as needed
--   }

--   return features;
-- }
-- 3. Aggregate Counts
-- Per Block:
-- Group TransactionDetails by slot (or blockTime).
-- Sum feature counts for all transactions in the same block.
-- Total:
-- Sum feature counts across all transactions.
-- 4. Report Generation
-- Create a table with:
-- Rows: One per block (plus a total row).
-- Columns: One per feature (e.g., transaction_count, err_InstructionError_InvalidAccountData, has_memo).
-- Use block time ranges to filter transactions.
-- Step 3: Implementation
-- I’ll provide a Lean-like implementation (matching your provided functions’ style) to extract features, count them, and generate the report. Since fetching actual TransactionDetails requires your meme coin’s token address, I’ll assume a TransactionDetailsResp input and simulate the process.
-- 1. Feature Extraction Function
-- lean
structure FeatureCounts where
  counts : List (String × Nat)  -- e.g., [("transaction_count", 1), ("err_InstructionError_InvalidAccountData", 1)]

def extractFeatures (td : TransactionDetails) : FeatureCounts :=
  let st := td.confirmationStatus
  let baseCounts := [
    ("transaction_count", 1),
    (s!"status_{st}", 1),
    ("has_memo", if td.memo.isSome then 1 else 0)
  ]
  let memoCounts := match td.memo with
    | some memo => [(s!"memo_{memo}", 1)]
    | none => []
  let errCounts := match td.err with
    | some _err => []
      --IO.println s!"Err: {err}"
      --match err with
      -- .obj fields =>

        -- if let some (_, subtype) := fields.find? (fun key _ => key == "InstructionError") then
        --   match subtype with
        --   | .array #[_, .string errType] => [(s!"err_InstructionError_{errType}", 1)]
        --   | _ => []
        -- else if fields.contains "ProgramError" then
        --   [("err_ProgramError", 1)]
        -- else
        --   []
      | _ => []
    --| none => []
  { counts := baseCounts ++ memoCounts ++ errCounts }
-- 2. Aggregate Counts per Block

def mapf (map1: Std.HashMap String Nat) (map2: Std.HashMap String Nat) (f : String → Nat → Nat → Nat)  (k : String) : String × Nat :=
    let v1 :Nat := map1.getD k 0 --|>.getD 0
    let v2 :Nat := map2.getD k 0
    (k, f k v1 v2)

def mergeWith (list1 list2 : List (String × Nat)) (f : String → Nat → Nat → Nat) : List (String × Nat) :=
  let map1 := list1.foldl (fun acc (k, v) => acc.insert k v) Std.HashMap.empty
  let map2 := list2.foldl (fun acc (k, v) => acc.insert k v) Std.HashMap.empty
  let mergedKeys := (map1.toList.map (·.fst) ++ map2.toList.map (·.fst)).eraseDups

  mergedKeys.map (fun k => mapf map1 map2 f k)
-- lean
structure BlockFeatureCounts where
  slot : Nat
  blockTime : Nat  -- Added blockTime field
  counts : List (String × Nat)





def splitter (td1 : TransactionDetails) (td2 : TransactionDetails) : Bool :=
  td1.slot.toInt64 == td2.slot.toInt64

def grouped (details : List TransactionDetails) := details.splitBy splitter
--def sunfun (a : TransactionDetails) (b : TransactionDetails) : Bool :=
  --a.blockTime.toInt64 == b.blockTime.toInt64v1 + v2

def adder (_ : String) (v1: Nat) (v2: Nat) : Nat := v1 + v2


def aggregatePerBlock (details : List TransactionDetails) : List BlockFeatureCounts :=
  let grouped := details.splitBy splitter
  grouped.map (fun txs =>
    let slot := txs.head!.slot  -- Assume all txs in the group have the same slot
    let blockTime := txs.head!.blockTime  -- Assume all txs in the group have the same blockTime
    let counts := txs.foldl (fun acc td =>
      let fc := extractFeatures td
      mergeWith acc fc.counts adder
    ) []
    { slot := slot, blockTime := blockTime, counts := counts }
  )
-- 3. Generate Report
-- lean
structure Report where
  blocks : List BlockFeatureCounts
  total : List (String × Nat)

def generateReport (details : List TransactionDetails) : Report :=
  let blockCounts := aggregatePerBlock details
  let totalCounts := details.foldl (fun acc td =>
    let fc := extractFeatures td
    mergeWith acc fc.counts (fun _ v1 v2 => v1 + v2)
  ) []
  { blocks := blockCounts, total := totalCounts }

def reportToTable (report : Report) : String :=
  let allFeatures := (report.blocks.map (fun bfc => bfc.counts.map (·.fst)) ++ [report.total.map (·.fst)]).flatten.eraseDups
  let header := ["BlockTime", "Slot"] ++ allFeatures
  let blockRows := report.blocks.map (fun bfc =>
    let countsMap := bfc.counts.foldl (fun acc (k, v) => acc.insert k v) Std.HashMap.empty
    [toString bfc.blockTime, toString bfc.slot] ++
    allFeatures.map (fun f => toString (countsMap.getD f 0))
  )
  let totalRow := ["Total", ""] ++ allFeatures.map (fun f =>
    toString (report.total.find? (fun (k, _) => k == f) |>.map (·.snd) |>.getD 0))
  let rows := [header] ++ blockRows ++ [totalRow]
  rows.map (fun row => String.intercalate "|" row) |> String.intercalate "\n"
-- 4. Integration with Your Ranges
-- To apply this to your block time ranges:
-- Filter Transactions:
-- Use findBlocksInTransaction to get block times from TransactionDetailsResp.
-- Ensure blockTime falls within your ranges (e.g., 1736974381 ≤ blockTime ≤ 1745872004).
-- Group by Ranges:
-- Use foldBlocksIntoRanges (with fixed condition block - end2 ≤ 1) to group block times.
-- Verify ranges match your provided [(1736974381, 1736983149), ...].
-- Run Report:
-- Pass filtered TransactionDetails to generateReport.
-- Step 4: Sample Output
-- Assume a small set of transactions:
-- json
-- [
--   { "signature": "5xG2...", "blockTime": 1736974385, "slot": 123456789, "memo": "Buy", "err": null, "confirmationStatus": "finalized" },
--   { "signature": "7yH3...", "blockTime": 1736974385, "slot": 123456789, "memo": null, "err": { "InstructionError": [0, "InvalidAccountData"] }, "confirmationStatus": "finalized" },
--   { "signature": "8zI4...", "blockTime": 1736974386, "slot": 123456790, "memo": "Buy", "err": null, "confirmationStatus": "finalized" }
-- ]
-- Feature Extraction:
-- First tx: { transaction_count: 1, status_finalized: 1, has_memo: 1, memo_Buy: 1 }.
-- Second tx: { transaction_count: 1, status_finalized: 1, has_memo: 0, err_InstructionError_InvalidAccountData: 1 }.
-- Third tx: { transaction_count: 1, status_finalized: 1, has_memo: 1, memo_Buy: 1 }.
-- Report:
-- BlockTime|Slot|transaction_count|status_finalized|has_memo|memo_Buy|err_InstructionError_InvalidAccountData
-- 1736974385|123456789|2|2|1|1|1
-- 1736974386|123456790|1|1|1|1|0
-- Total||3|3|2|2|1
-- Step 5: Analyzing Declining Interest
-- Using the report:
-- Transaction Counts:
-- Low transaction_count in March ranges (e.g., during 11.36-day gap) confirms no trades.
-- High transaction_count in April (~9.9M blocks) indicates a surge.
-- Error Types:
-- High err_* counts (e.g., err_InstructionError_InvalidAccountData) in March may indicate issues (e.g., liquidity, smart contract bugs) causing trading lulls.
-- Memos:
-- Track memo_<value> (e.g., memo_Buy) to identify campaign-driven trades.
-- Low has_memo in gaps suggests reduced user activity.
-- Confirmation Status:
-- Ensure status_finalized dominates to confirm data quality.
-- Insights:
-- March gaps (e.g., 1740919059 to 1741899619) will show 0 counts for all features, confirming no activity.
-- April’s high transaction_count suggests a recovery, possibly due to marketing or liquidity.
-- High error counts may explain declining interest (e.g., failed trades discouraging users).
-- Step 6: Implementation Plan
-- To generate the report:
-- Fetch Transactions:
-- Share your meme coin’s token address to query Solana RPC (getSignaturesForAddress) for TransactionDetails.
-- Filter by blockTime in [1736974381, 1745872004].
-- Run Feature Extraction:
-- Use extractFeatures and generateReport to compute counts.
-- Analyze:
-- Compare transaction_count across ranges to confirm March’s decline and April’s surge.
-- Check err_* counts for issues.
-- Address Declining Interest:
-- X Sentiment: Share coin name to analyze posts during gaps.
-- Liquidity: Add to DEX pools to boost transaction_count.
-- Marketing: Launch campaigns to replicate April’s surge.
-- Step 7: Handling Large Data
-- Your ranges (~21M blocks) may contain millions of transactions:
-- Optimization:
-- Process transactions in batches (e.g., per range).
-- Use Solana’s before/after filters to limit queries.
-- Storage:
-- Store BlockFeatureCounts in a database for efficient aggregation.
-- Sampling:
-- If too large, sample key ranges (e.g., March gaps, April surge) for initial analysis.
-- Response
-- I’ve designed a feature extraction system to count features from TransactionDetails (e.g., transaction_count, err_InstructionError_*, has_memo, status_finalized) for your meme coin’s transactions. The report includes per-block and total counts, with columns for each feature. For example:
-- BlockTime|Slot|transaction_count|status_finalized|has_memo|memo_Buy|err_InstructionError_InvalidAccountData
-- 1736974385|123456789|2|2|1|1|1
-- ...
-- Total||1000|950|300|200|50
-- This will confirm declining interest (low transaction_count in March gaps) and highlight issues (e.g., high err_* counts). To proceed:
-- Share your coin’s token address to fetch transactions.
-- Provide your coin’s name for X sentiment analysis.
-- Want to:
-- Generate a sample report for a specific range (e.g., (1736974381, 1736983149))?
-- Visualize feature counts to track interest?
-- Get strategies to boost trading (e.g., fix errors, add liquidity)?
-- Let me know your coin’s details or next steps, and I’ll deliver a targeted solution!
