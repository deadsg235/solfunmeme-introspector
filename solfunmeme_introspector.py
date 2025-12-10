#!/usr/bin/env python3
"""
SOLFUNMEME Introspector - Minimal implementation to get the repo running
Collects and analyzes Solana transaction data for the SOLFUNMEME token
"""

import json
import os
import requests
import time
from typing import Dict, List, Optional, Any
import hashlib

# Constants
TOKEN_ADDRESS = "BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump"
CACHE_DIR = "rpc_cache"
SOLANA_URL = "https://api.mainnet-beta.solana.com"

class SolanaRPC:
    def __init__(self, url: str):
        self.url = url
        self.session = requests.Session()
        
    def call(self, method: str, params: List[Any]) -> Dict:
        """Make RPC call with caching"""
        cache_key = self._generate_cache_key(method, params)
        cached = self._check_cache(cache_key)
        
        if cached:
            print(f"Using cached result for {method}")
            return cached
            
        print(f"Making RPC call: {method}")
        payload = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": method,
            "params": params
        }
        
        try:
            response = self.session.post(self.url, json=payload, timeout=30)
            response.raise_for_status()
            result = response.json()
            
            # Cache the result
            self._save_cache(cache_key, result)
            time.sleep(1.0)  # Increased rate limiting
            return result
            
        except Exception as e:
            print(f"RPC call failed: {e}")
            return {"error": str(e)}
    
    def _generate_cache_key(self, method: str, params: List[Any]) -> str:
        """Generate cache key from method and params"""
        params_str = json.dumps(params, sort_keys=True)
        hash_obj = hashlib.md5(f"{method}_{params_str}".encode())
        return f"{method}_{hash_obj.hexdigest()}"
    
    def _check_cache(self, cache_key: str) -> Optional[Dict]:
        """Check if cached result exists"""
        cache_file = os.path.join(CACHE_DIR, f"{cache_key}.json")
        if os.path.exists(cache_file):
            try:
                with open(cache_file, 'r') as f:
                    return json.load(f)
            except:
                pass
        return None
    
    def _save_cache(self, cache_key: str, data: Dict):
        """Save result to cache"""
        os.makedirs(CACHE_DIR, exist_ok=True)
        cache_file = os.path.join(CACHE_DIR, f"{cache_key}.json")
        try:
            with open(cache_file, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            print(f"Failed to save cache: {e}")

class SolfunmemeIntrospector:
    def __init__(self):
        self.rpc = SolanaRPC(SOLANA_URL)
        self.token_address = TOKEN_ADDRESS
        
    def get_token_info(self) -> Dict:
        """Get token account information"""
        params = [self.token_address, {"encoding": "jsonParsed"}]
        return self.rpc.call("getAccountInfo", params)
    
    def get_transaction_signatures(self, limit: int = 1000, before: str = None) -> List[str]:
        """Get transaction signatures for the token address"""
        params = [self.token_address, {"limit": limit}]
        if before:
            params[1]["before"] = before
            
        response = self.rpc.call("getSignaturesForAddress", params)
        
        if "result" in response and response["result"]:
            return [tx["signature"] for tx in response["result"]]
        return []
    
    def get_transaction_details(self, signature: str) -> Dict:
        """Get detailed transaction information"""
        params = [signature, {"maxSupportedTransactionVersion": 0}]
        return self.rpc.call("getTransaction", params)
    
    def analyze_transaction(self, tx_data: Dict) -> Dict:
        """Analyze transaction for SOLFUNMEME-related activity"""
        if "result" not in tx_data or not tx_data["result"] or "error" in tx_data:
            return {
                "type": "unknown", 
                "details": "No transaction data",
                "status": "failed",
                "signature": "unknown",
                "slot": 0,
                "blockTime": 0,
                "fee": 0,
                "token_changes": []
            }
        
        result = tx_data["result"]
        meta = result.get("meta", {})
        
        # Check for token balance changes
        pre_balances = meta.get("preTokenBalances", [])
        post_balances = meta.get("postTokenBalances", [])
        
        analysis = {
            "signature": result.get("transaction", {}).get("signatures", [""])[0],
            "slot": result.get("slot", 0),
            "blockTime": result.get("blockTime", 0),
            "fee": meta.get("fee", 0),
            "status": "success" if meta.get("err") is None else "failed",
            "token_changes": []
        }
        
        # Analyze token balance changes
        for post_balance in post_balances:
            if post_balance["mint"] == self.token_address:
                # Find corresponding pre-balance
                pre_balance = None
                for pre in pre_balances:
                    if (pre["accountIndex"] == post_balance["accountIndex"] and 
                        pre["mint"] == post_balance["mint"]):
                        pre_balance = pre
                        break
                
                if pre_balance:
                    pre_amount = int(pre_balance["uiTokenAmount"]["amount"])
                    post_amount = int(post_balance["uiTokenAmount"]["amount"])
                    change = post_amount - pre_amount
                    
                    if change != 0:
                        analysis["token_changes"].append({
                            "account": post_balance["owner"],
                            "change": change,
                            "type": "buy" if change > 0 else "sell",
                            "amount": abs(change) / (10 ** post_balance["uiTokenAmount"]["decimals"])
                        })
        
        return analysis
    
    def run_introspection(self):
        """Main introspection process"""
        print(f"Starting SOLFUNMEME Introspection for token: {self.token_address}")
        print(f"Using cache directory: {CACHE_DIR}")
        
        # Create cache directory
        os.makedirs(CACHE_DIR, exist_ok=True)
        
        # Get token info
        print("\nFetching token information...")
        token_info = self.get_token_info()
        if "result" in token_info and token_info["result"]:
            print("Token info retrieved successfully")
        else:
            print("Failed to retrieve token info")
            print(f"Error: {token_info.get('error', 'Unknown error')}")
            return
        
        # Get transaction signatures
        print("\nFetching transaction signatures...")
        signatures = self.get_transaction_signatures(limit=100)  # Start with 100 for testing
        print(f"Retrieved {len(signatures)} transaction signatures")
        
        if not signatures:
            print("No transactions found")
            return
        
        # Analyze transactions
        print(f"\nAnalyzing transactions...")
        analyses = []
        
        for i, signature in enumerate(signatures[:10]):  # Analyze first 10 for testing
            print(f"Processing transaction {i+1}/10: {signature[:16]}...")
            tx_data = self.get_transaction_details(signature)
            analysis = self.analyze_transaction(tx_data)
            analyses.append(analysis)
        
        # Generate report
        self.generate_report(analyses)
        
    def generate_report(self, analyses: List[Dict]):
        """Generate analysis report"""
        print("\nSOLFUNMEME Transaction Analysis Report")
        print("=" * 50)
        
        total_transactions = len(analyses)
        successful_transactions = sum(1 for a in analyses if a.get("status") == "success")
        total_token_changes = sum(len(a["token_changes"]) for a in analyses)
        
        print(f"Total Transactions Analyzed: {total_transactions}")
        print(f"Successful Transactions: {successful_transactions}")
        print(f"Token Balance Changes: {total_token_changes}")
        
        # Show recent activity
        print(f"\nRecent Activity:")
        for analysis in analyses[:5]:
            if analysis["token_changes"]:
                for change in analysis["token_changes"]:
                    print(f"  {change['type'].upper()}: {change['amount']:.2f} SFM by {change['account'][:16]}...")
        
        # Save detailed report
        report_file = os.path.join(CACHE_DIR, "analysis_report.json")
        with open(report_file, 'w') as f:
            json.dump(analyses, f, indent=2)
        print(f"\nDetailed report saved to: {report_file}")

def main():
    """Main entry point"""
    introspector = SolfunmemeIntrospector()
    introspector.run_introspection()

if __name__ == "__main__":
    main()