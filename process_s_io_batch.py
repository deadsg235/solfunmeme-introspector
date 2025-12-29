#!/usr/bin/env python3
"""
S_IO Batch Processor - Process cached signatures in batches
"""

import json
import os
import time
from solfunmeme_introspector import SolanaRPC

DATASET_DIR = "rpc_cache/s_io_dataset"
SOLANA_URL = "https://api.mainnet-beta.solana.com"

def process_cached_signatures():
    """Process signatures from cache in small batches"""
    
    # Load cached signatures
    sig_file = os.path.join(DATASET_DIR, "signatures.json")
    with open(sig_file, 'r') as f:
        signatures = json.load(f)
    
    print(f"Processing {len(signatures)} cached signatures...")
    
    # Load existing transactions if any
    tx_file = os.path.join(DATASET_DIR, "transactions.json")
    existing_transactions = []
    if os.path.exists(tx_file):
        with open(tx_file, 'r') as f:
            existing_transactions = json.load(f)
    
    processed_sigs = {tx["signature"] for tx in existing_transactions}
    
    rpc = SolanaRPC(SOLANA_URL)
    batch_size = 10
    delay = 2.0  # 2 second delay between batches
    
    for i in range(0, len(signatures), batch_size):
        batch = signatures[i:i+batch_size]
        print(f"Processing batch {i//batch_size + 1}: signatures {i+1}-{min(i+batch_size, len(signatures))}")
        
        for sig in batch:
            if sig in processed_sigs:
                continue
                
            params = [sig, {"maxSupportedTransactionVersion": 0}]
            tx_data = rpc.call("getTransaction", params)
            
            if "result" in tx_data and tx_data["result"]:
                existing_transactions.append({
                    "signature": sig,
                    "data": tx_data["result"]
                })
                processed_sigs.add(sig)
        
        # Save progress after each batch
        with open(tx_file, 'w') as f:
            json.dump(existing_transactions, f, indent=2)
        
        print(f"Total transactions collected: {len(existing_transactions)}")
        
        # Rate limiting delay
        if i + batch_size < len(signatures):
            print(f"Waiting {delay} seconds...")
            time.sleep(delay)
    
    print(f"Final count: {len(existing_transactions)} transactions")
    return existing_transactions

def create_huggingface_ready_dataset():
    """Create dataset files ready for Hugging Face upload"""
    
    # Load data
    with open(os.path.join(DATASET_DIR, "signatures.json"), 'r') as f:
        signatures = json.load(f)
    
    with open(os.path.join(DATASET_DIR, "transactions.json"), 'r') as f:
        transactions = json.load(f)
    
    # Create simplified dataset for HF
    hf_data = {
        "token_info": {
            "address": "Fuj6EDWQHBnQ3eEvYDujNQ4rPLSkhm3pBySbQ79Bpump",
            "name": "S_IO",
            "blockchain": "solana"
        },
        "signatures": signatures,
        "transaction_count": len(transactions),
        "sample_transactions": transactions[:10] if len(transactions) > 10 else transactions
    }
    
    # Save HF-ready file
    hf_file = os.path.join(DATASET_DIR, "s_io_dataset.json")
    with open(hf_file, 'w') as f:
        json.dump(hf_data, f, indent=2)
    
    print(f"Hugging Face dataset ready: {hf_file}")
    return hf_file

if __name__ == "__main__":
    print("S_IO Batch Processor")
    print("=" * 30)
    
    # Process signatures in batches
    transactions = process_cached_signatures()
    
    # Create HF-ready dataset
    hf_file = create_huggingface_ready_dataset()
    
    print(f"\nDataset Summary:")
    print(f"- Signatures: 2000")
    print(f"- Transactions: {len(transactions)}")
    print(f"- HF Dataset: {hf_file}")
    print(f"- Upload to: https://huggingface.co/datasets/Deadsg/S-IO_DATASET")