import json
import os

def analyze_transaction(file_path):
    with open(file_path, 'r') as f:
        data = json.load(f)
    result = data.get('result', {})
    meta = result.get('meta', {})
    logs = meta.get('logMessages', [])
    pre_balances = meta.get('preTokenBalances', [])
    post_balances = meta.get('postTokenBalances', [])

    # Check for Raydium transactions
    if not any('675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8' in log for log in logs):
        return

    # Analyze SFM balance changes
    for post in post_balances:
        if post['mint'] == 'BwUTq7fS6sfUmHDwAiCQZ3asSiPEapW5zDrsbwtapump':
            pre = next((p for p in pre_balances if p['accountIndex'] == post['accountIndex'] and p['mint'] == post['mint']), None)
            if pre:
                pre_amount = int(pre['uiTokenAmount']['amount'])
                post_amount = int(post['uiTokenAmount']['amount'])
                if post_amount > pre_amount:
                    print(f"Buy: Account {post['accountIndex']} gained {post['uiTokenAmount']['uiAmountString']} SFM")
                elif post_amount < pre_amount:
                    print(f"Sell: Account {post['accountIndex']} lost {pre['uiTokenAmount']['uiAmountString']} SFM")

for file in os.listdir('rpc_cache'):
    if file.startswith('method_getTransaction_signature_'):
        analyze_transaction(os.path.join('rpc_cache', file))
