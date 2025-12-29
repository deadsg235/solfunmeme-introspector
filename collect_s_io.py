#!/usr/bin/env python3
"""
S_IO Token Data Collector for Hugging Face Dataset
Collects signatures and transactions for token: Fuj6EDWQHBnQ3eEvYDujNQ4rPLSkhm3pBySbQ79Bpump
"""

import json
import os
from solfunmeme_introspector import SolanaRPC

# S_IO Token Configuration
S_IO_TOKEN = "Fuj6EDWQHBnQ3eEvYDujNQ4rPLSkhm3pBySbQ79Bpump"
DATASET_DIR = "rpc_cache/s_io_dataset"
SOLANA_URL = "https://api.mainnet-beta.solana.com"

class SIODataCollector:
    def __init__(self):
        self.rpc = SolanaRPC(SOLANA_URL)
        self.token_address = S_IO_TOKEN
        os.makedirs(DATASET_DIR, exist_ok=True)
    
    def collect_all_signatures(self, max_signatures=10000):
        """Collect all transaction signatures for S_IO token"""
        print(f"Collecting signatures for S_IO token: {self.token_address}")
        
        all_signatures = []
        before = None
        batch_size = 1000
        
        while len(all_signatures) < max_signatures:
            print(f"Fetching batch {len(all_signatures)//batch_size + 1}...")
            
            params = [self.token_address, {"limit": batch_size}]
            if before:
                params[1]["before"] = before
            
            response = self.rpc.call("getSignaturesForAddress", params)
            
            if "result" not in response or not response["result"]:
                break
                
            batch_sigs = [tx["signature"] for tx in response["result"]]
            all_signatures.extend(batch_sigs)
            
            # Set before for next batch
            before = batch_sigs[-1] if batch_sigs else None
            
            print(f"Total signatures collected: {len(all_signatures)}")
            
            # Break if we got less than requested (end of data)
            if len(batch_sigs) < batch_size:
                break
        
        # Save signatures list
        sig_file = os.path.join(DATASET_DIR, "signatures.json")
        with open(sig_file, 'w') as f:
            json.dump(all_signatures, f, indent=2)
        
        print(f"Saved {len(all_signatures)} signatures to {sig_file}")
        return all_signatures
    
    def collect_transaction_details(self, signatures):
        """Collect detailed transaction data"""
        print(f"Collecting transaction details for {len(signatures)} signatures...")
        
        transactions = []
        for i, sig in enumerate(signatures):
            if i % 100 == 0:
                print(f"Processing transaction {i+1}/{len(signatures)}")
            
            params = [sig, {"maxSupportedTransactionVersion": 0}]
            tx_data = self.rpc.call("getTransaction", params)
            
            if "result" in tx_data and tx_data["result"]:
                transactions.append({
                    "signature": sig,
                    "data": tx_data["result"]
                })
        
        # Save transaction details
        tx_file = os.path.join(DATASET_DIR, "transactions.json")
        with open(tx_file, 'w') as f:
            json.dump(transactions, f, indent=2)
        
        print(f"Saved {len(transactions)} transactions to {tx_file}")
        return transactions
    
    def create_dataset_metadata(self, signatures, transactions):
        """Create metadata for Hugging Face dataset"""
        metadata = {
            "dataset_name": "S-IO_DATASET",
            "token_address": self.token_address,
            "huggingface_url": "https://huggingface.co/datasets/Deadsg/S-IO_DATASET",
            "total_signatures": len(signatures),
            "total_transactions": len(transactions),
            "collection_date": "2024-12-19",
            "blockchain": "solana",
            "data_types": ["signatures", "transactions", "token_balances", "program_logs"]
        }
        
        meta_file = os.path.join(DATASET_DIR, "metadata.json")
        with open(meta_file, 'w') as f:
            json.dump(metadata, f, indent=2)
        
        print(f"Dataset metadata saved to {meta_file}")

def main():
    collector = SIODataCollector()
    
    # Collect signatures
    signatures = collector.collect_all_signatures(max_signatures=5000)  # Start with 5k
    
    # Collect transaction details
    transactions = collector.collect_transaction_details(signatures[:100])  # Start with 100 detailed
    
    # Create metadata
    collector.create_dataset_metadata(signatures, transactions)
    
    print(f"\nS_IO Dataset Collection Complete!")
    print(f"Output directory: {DATASET_DIR}")
    print(f"Files created:")
    print(f"  - signatures.json ({len(signatures)} signatures)")
    print(f"  - transactions.json ({len(transactions)} detailed transactions)")
    print(f"  - metadata.json (dataset info)")
    print(f"\nReady for upload to: https://huggingface.co/datasets/Deadsg/S-IO_DATASET")

if __name__ == "__main__":
    main()