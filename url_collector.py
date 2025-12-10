#!/usr/bin/env python3
"""
URL Collector for SOLFUNMEME README
Extracts and resolves URLs from the README as requested
"""

import re
import requests
import json
from urllib.parse import urlparse
import time

def extract_urls_from_readme():
    """Extract all URLs from the README.md file"""
    try:
        with open('README.md', 'r', encoding='utf-8') as f:
            content = f.read()
    except:
        print("Could not read README.md")
        return []
    
    # Find all URLs using regex
    url_pattern = r'https?://[^\s\)]+|www\.[^\s\)]+'
    urls = re.findall(url_pattern, content)
    
    # Clean up URLs (remove trailing punctuation)
    cleaned_urls = []
    for url in urls:
        url = url.rstrip('.,;:!?)')
        if not url.startswith('http'):
            url = 'https://' + url
        cleaned_urls.append(url)
    
    return list(set(cleaned_urls))  # Remove duplicates

def resolve_url(url):
    """Resolve a URL and get basic information"""
    try:
        print(f"Resolving: {url}")
        response = requests.get(url, timeout=10, allow_redirects=True)
        
        parsed = urlparse(url)
        domain = parsed.netloc
        
        result = {
            "original_url": url,
            "final_url": response.url,
            "status_code": response.status_code,
            "domain": domain,
            "title": "",
            "content_type": response.headers.get('content-type', ''),
            "size": len(response.content),
            "accessible": response.status_code == 200
        }
        
        # Try to extract title for HTML pages
        if 'text/html' in result['content_type'] and response.status_code == 200:
            title_match = re.search(r'<title[^>]*>([^<]+)</title>', response.text, re.IGNORECASE)
            if title_match:
                result['title'] = title_match.group(1).strip()
        
        # Categorize by domain
        if 'solana' in domain.lower():
            result['category'] = 'solana'
        elif 'twitter.com' in domain or 'x.com' in domain:
            result['category'] = 'social_twitter'
        elif 'discord' in domain:
            result['category'] = 'social_discord'
        elif 'telegram' in domain or 't.me' in domain:
            result['category'] = 'social_telegram'
        elif 'github.com' in domain or 'codeberg.org' in domain:
            result['category'] = 'code_repository'
        elif 'opensea.io' in domain:
            result['category'] = 'nft_marketplace'
        elif 'coinmarketcap.com' in domain:
            result['category'] = 'crypto_data'
        elif 'streamflow.finance' in domain:
            result['category'] = 'defi'
        else:
            result['category'] = 'other'
        
        return result
        
    except Exception as e:
        return {
            "original_url": url,
            "error": str(e),
            "accessible": False,
            "category": "error"
        }

def main():
    """Main function to collect and resolve URLs"""
    print("SOLFUNMEME URL Collector")
    print("=" * 40)
    
    # Extract URLs from README
    urls = extract_urls_from_readme()
    print(f"Found {len(urls)} unique URLs in README.md")
    
    # Resolve each URL
    results = []
    for i, url in enumerate(urls, 1):
        print(f"[{i}/{len(urls)}] Processing: {url[:60]}...")
        result = resolve_url(url)
        results.append(result)
        time.sleep(0.5)  # Be nice to servers
    
    # Categorize results
    categories = {}
    accessible_count = 0
    
    for result in results:
        category = result.get('category', 'unknown')
        if category not in categories:
            categories[category] = []
        categories[category].append(result)
        
        if result.get('accessible', False):
            accessible_count += 1
    
    # Generate report
    print(f"\nURL Analysis Report")
    print("=" * 40)
    print(f"Total URLs: {len(urls)}")
    print(f"Accessible URLs: {accessible_count}")
    print(f"Failed URLs: {len(urls) - accessible_count}")
    
    print(f"\nBy Category:")
    for category, items in categories.items():
        print(f"  {category}: {len(items)}")
    
    # Save detailed results
    output_file = "url_analysis.json"
    with open(output_file, 'w') as f:
        json.dump({
            "summary": {
                "total_urls": len(urls),
                "accessible_urls": accessible_count,
                "failed_urls": len(urls) - accessible_count,
                "categories": {cat: len(items) for cat, items in categories.items()}
            },
            "results": results
        }, f, indent=2)
    
    print(f"\nDetailed results saved to: {output_file}")
    
    # Show some key findings
    print(f"\nKey URLs by Category:")
    for category in ['solana', 'social_twitter', 'code_repository', 'nft_marketplace']:
        if category in categories:
            print(f"\n{category.upper()}:")
            for item in categories[category][:3]:  # Show first 3
                status = "OK" if item.get('accessible') else "FAIL"
                print(f"  {status} {item['original_url']}")
                if item.get('title'):
                    print(f"    Title: {item['title'][:60]}...")

if __name__ == "__main__":
    main()