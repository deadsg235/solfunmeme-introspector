import Lean
import Lean.Data.Json.Basic
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer

open Lean Json ToJson FromJson

structure Entry: Type where
  entry_date: String
  description: String
  --debit: Float
  --credit: Float
deriving ToJson, FromJson, Inhabited, Repr

-- Check types
-- #check #["foo"]
-- #check 1000.00

structure Ledger : Type where
  account_name: String
  account_number: String
  entries: Array Entry
deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

def get_ledger_from_json_string (s: String): Except String Ledger := do
  let j : Json <- Json.parse s
  let ledger : Ledger <- fromJson? j
  return ledger

def ledger_account_string := "{     \"account_name\": \"Example Company\",     \"account_number\": \"1234567890\",     \"entries\": [       {         \"entry_date\": \"2023-10-14\",         \"description\": \"Opening Balance\",         \"debit\": 10000.00,         \"credit\": 0.00       },       {         \"entry_date\": \"2023-10-15\",         \"description\": \"Sale of Products\",         \"debit\": 0.00,         \"credit\": 5000.00       },       {         \"entry_date\": \"2023-10-16\",         \"description\": \"Purchase of Supplies\",         \"debit\": 3000.00,         \"credit\": 0.00       },       {         \"entry_date\": \"2023-10-17\",         \"description\": \"Payment from Customer\",         \"debit\": 2000.00,         \"credit\": 0.00       },       {         \"entry_date\": \"2023-10-18\",         \"description\": \"Utilities Expense\",         \"debit\": 0.00,         \"credit\": 1000.00       }     ]   }"
--#eval (get_ledger_from_json_string ledger_account_string)
--#eval toJson (get_ledger_from_json_string ledger_account_string).toOption.get!

--import Lean

def callLean(filename:String) : IO Unit := do
  -- Define the command and arguments
  let cmd := "lean"  -- Use "dir" on Windows
  let args := #["--run" ,filename]  -- Arguments to the command (e.g., ls -l)

  -- Run the command and capture output
  let result ← IO.Process.output { cmd := cmd, args := args }

  -- Print the standard output
  IO.println s!"Standard Output:\n{result.stdout}"

  -- Print the standard error (if any)
  if !result.stderr.isEmpty then
    IO.println s!"Standard Error:\n{result.stderr}"

  -- Print the exit code
  IO.println s!"Exit Code: {result.exitCode}"

def PumpFun (s: String) : IO Unit := do
  let cmd := "curl"
  let args := #["-X", "POST", "-H", "Content-Type: application/json", "-d", s, "https://api.pumpkin.dev/v1/pump"]
  let result ← IO.Process.output { cmd := cmd, args := args }
  IO.println s!"Standard Output:\n{result.stdout}"
  if !result.stderr.isEmpty then
    IO.println s!"Standard Error:\n{result.stderr}"
  IO.println s!"Exit Code: {result.exitCode}"
def SolfunmemeLean : IO Unit := do
  PumpFun "BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump"

def main : IO Unit := do
  -- let startTime ← IO.monoMsNow
  callLean "GenerateLedger.lean"

  --let s ← IO.FS.readFile "ledger_account.json"
  -- Test Json Parser
  --let ledger : Ledger <- IO.ofExcept (get_ledger_from_json_string s)
  --IO.println (toJson ledger)

  -- timestamp
  --IO.println s!"Finished: {(← IO.monoMsNow) - startTime}ms\n"

  -- Call main2 function
