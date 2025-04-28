-- -- This module serves as the root of the `SolfunmemeLean` library.
-- -- Import modules here that should be built as part of the library.

-- import Lean
-- --import Lean.JsonRpc
-- --import IO.FS.Stream

-- open Lean
-- open Lean.JsonRpc
-- open IO.FS.Stream

-- -- Define a type for the params field
-- structure GetTransactionParams where
--   transactionId : String
--   encoding : String
--   deriving ToJson, FromJson

-- def main : IO Unit := do
--   -- Create IO.Ref for response buffer
--   let response ← IO.mkRef { : IO.FS.Stream.Buffer }

--   -- Construct JSON-RPC request
--   let params : GetTransactionParams := {
--     transactionId := "BrFtu8Pf9ocd8yxKnvPDdqtoSHB9qjdddZ4AwX4FRg4V94fSskYxmbeZCWhsxcSCpnBuniudUC2yJjiay7ndtUB",
--     encoding := "json"
--   }
--   let request : Request GetTransactionParams := {
--     id := RequestID.str "1",
--     method := "getTransaction",
--     param := params
--   }

--   -- Convert request to JSON
--   --let jsonPayload := toJson (request : Message)

--   -- -- Perform HTTP POST request
--   -- let result ← IO.ofExcept <| curl_easy_perform_with_options #[
--   --   CurlOption.URL "https://api.mainnet-beta.solana.com",
--   --   CurlOption.COPYPOSTFIELDS (jsonPayload.compress),
--   --   CurlOption.HTTPHEADER #["Accept: application/json", "Content-Type: application/json"],
--   --   CurlOption.WRITEDATA response,
--   --   CurlOption.WRITEFUNCTION Curl.writeBytes
--   -- ]

--   -- Handle curl result
--   -- match result with
--   -- | .ok _ => do
--   --     -- Get response bytes
--   --     let bytes ← response.get
--   --     -- Convert to string and parse as JSON
--   --     match String.fromUTF8 bytes.data with
--   --     | .ok str => do
--   --         match fromJson? (← parseJson str) with
--   --         | .ok (msg : Message) => match msg with
--   --           | .response id result =>
--   --               if id == RequestID.str "1" then
--   --                 IO.println s!"Response: {result.pretty}"
--   --               else
--   --                 IO.println s!"Error: Unexpected ID {id}, expected \"1\""
--   --           | .responseError id code message data? =>
--   --               IO.println s!"JSON-RPC Error: ID={id}, Code={code}, Message={message}, Data={data?.getD null}"
--   --           | _ => IO.println "Error: Expected JSON-RPC response"
--   --         | .error err => IO.println s!"JSON Parsing Error: {err}"
--   --     | .error _ => IO.println "Error: Invalid UTF-8 response"
--   -- | .error err => IO.println s!"Curl error: {err}"
