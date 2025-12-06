---
title: Cryptocurrency
aliases:
  - Crypto
  - Digital Currency
  - Cryptocurrencies
tags:
  - blockchain
  - finance
  - cryptocurrency
  - concept
type: reference
status: complete
created: 2025-11-30
---

# Cryptocurrency

Digital or virtual currencies secured by cryptography, operating on decentralized blockchain networks without central authority control.

## Overview

| Aspect | Details |
|--------|---------|
| **Definition** | Decentralized digital assets using cryptography for secure, peer-to-peer transactions |
| **Core Technology** | Blockchain, distributed ledger, cryptographic signatures |
| **Key Innovation** | Trustless value transfer without intermediaries |
| **First Implementation** | Bitcoin (2009) |
| **Major Categories** | Payment coins, smart contract platforms, stablecoins, tokens |
| **Market Cap** | $1T+ (varies significantly) |

## Core Concepts

### Account Models

**UTXO Model (Unspent Transaction Output)**
- Used by Bitcoin, Cardano, Litecoin
- Transactions consume inputs and create outputs
- Each output can only be spent once
- Better privacy (new addresses per transaction)
- Parallel transaction processing
- Simpler to verify entire history

**Account Model**
- Used by Ethereum, most smart contract platforms
- Track balances like traditional bank accounts
- Single balance per address
- More intuitive for developers
- Easier to implement complex smart contracts
- Higher risk of state conflicts

### Transaction Lifecycle

```mermaid
graph LR
    A[Create Transaction] --> B[Sign with Private Key]
    B --> C[Broadcast to Network]
    C --> D[Mempool Queue]
    D --> E[Miner/Validator Selection]
    E --> F[Block Inclusion]
    F --> G[Block Confirmation]
    G --> H[Finalized]
```

**Key Stages:**
1. **Creation** - User constructs transaction with recipient, amount, fee
2. **Signing** - Private key proves ownership and authorizes transfer
3. **Broadcasting** - Transaction shared with network nodes
4. **Mempool** - Pending transactions await inclusion
5. **Validation** - Miners/validators verify and include in block
6. **Confirmation** - Additional blocks provide security guarantees
7. **Finality** - Transaction considered irreversible (varies by chain)

### Gas Fees

Transaction costs paid to network validators for computation and storage.

| Component | Description |
|-----------|-------------|
| **Gas Limit** | Maximum computational units allocated for transaction |
| **Gas Price** | Amount paid per unit of gas (in cryptocurrency) |
| **Base Fee** | Minimum fee required (EIP-1559 on Ethereum) |
| **Priority Fee** | Optional tip to validators for faster inclusion |
| **Total Cost** | Gas Used × (Base Fee + Priority Fee) |

**Factors Affecting Fees:**
- Network congestion (demand for block space)
- Transaction complexity (simple transfers vs. smart contracts)
- Data storage requirements
- Market volatility and speculation
- Layer 2 solutions reduce fees significantly

## Major Cryptocurrencies

| Name | Model | Consensus | TPS | Smart Contracts | Primary Use |
|------|-------|-----------|-----|-----------------|-------------|
| **Bitcoin (BTC)** | UTXO | Proof of Work | 7 | Limited (Script) | Store of value, payments |
| **Ethereum (ETH)** | Account | Proof of Stake | 15-30 | ✅ Full (EVM) | Smart contracts, DeFi, NFTs |
| **Binance Coin (BNB)** | Account | Proof of Stake | 2,000+ | ✅ EVM-compatible | Exchange utility, DeFi |
| **Cardano (ADA)** | UTXO | Proof of Stake | 250+ | ✅ Plutus/Marlowe | Academic approach, sustainability |
| **Solana (SOL)** | Account | Proof of History + PoS | 65,000+ | ✅ Rust/C | High throughput, low fees |
| **Ripple (XRP)** | Account | Consensus Protocol | 1,500 | Limited | Cross-border payments |
| **Polkadot (DOT)** | Account | Nominated PoS | 1,000+ | ✅ WASM | Interoperability, parachains |

## Token Standards

### Ethereum Token Standards

**ERC-20 (Fungible Tokens)**
- Standard for interchangeable tokens (currencies, governance)
- Core functions: `transfer()`, `balanceOf()`, `approve()`, `transferFrom()`
- Examples: USDT, USDC, UNI, LINK, DAI
- Use cases: Stablecoins, governance tokens, utility tokens

**ERC-721 (Non-Fungible Tokens)**
- Unique, indivisible assets
- Each token has distinct identifier and metadata
- Core functions: `ownerOf()`, `transferFrom()`, `tokenURI()`
- Examples: CryptoPunks, Bored Ape Yacht Club, ENS domains
- Use cases: Digital art, collectibles, gaming items, identity

**ERC-1155 (Multi-Token Standard)**
- Single contract manages multiple token types
- Mix fungible and non-fungible in one contract
- Batch operations reduce gas costs
- Examples: Gaming inventories, bundle assets
- Use cases: Games with items/currency, efficient NFT collections

## Stablecoins

Cryptocurrencies designed to maintain stable value relative to reference asset (typically USD).

| Type | Mechanism | Examples | Backing | Volatility |
|------|-----------|----------|---------|------------|
| **Fiat-Collateralized** | 1:1 reserve of fiat currency | USDT, USDC, BUSD | Fiat in bank accounts | ✅ Low |
| **Crypto-Collateralized** | Over-collateralized with crypto | DAI, sUSD | ETH, other crypto assets | Medium |
| **Algorithmic** | Supply adjustment via algorithms | UST (failed), FRAX | Algorithm + partial collateral | ❌ High risk |
| **Commodity-Backed** | Backed by gold, oil, etc. | PAXG, DGX | Physical commodities | Low-Medium |

**Key Considerations:**
- Centralization risk (fiat-backed require trust in issuer)
- Transparency of reserves (regular audits critical)
- Regulatory compliance and oversight
- Depegging risk during market stress
- Algorithmic stablecoins have history of catastrophic failures

## Layer 2 Solutions

Scaling solutions built on top of Layer 1 blockchains to increase throughput and reduce fees.

| Solution | Type | Security Model | Example | TPS | Fee Reduction |
|----------|------|----------------|---------|-----|---------------|
| **Rollups (Optimistic)** | Execute off-chain, fraud proofs | Inherit L1 security | Arbitrum, Optimism | 2,000-4,000 | 10-100× |
| **Rollups (ZK)** | Zero-knowledge proofs | Inherit L1 security | zkSync, StarkNet, Polygon zkEVM | 2,000+ | 100-1000× |
| **State Channels** | Off-chain transactions, on-chain settlement | Trust between parties | Lightning Network (Bitcoin) | Unlimited | Near-zero |
| **Sidechains** | Separate blockchain with bridge | Independent security | Polygon PoS, Ronin | 7,000+ | 100× |
| **Plasma** | Hierarchical child chains | L1 + exit mechanisms | OMG Network | Varies | 100× |

## Wallets

Software or hardware managing private keys and enabling transaction signing.

### Hot Wallets (Internet-Connected)

| Type | Access | Examples | Security | Use Case |
|------|--------|----------|----------|----------|
| **Web Wallets** | Browser-based | MetaMask, Coinbase Wallet | Medium | DeFi, NFTs, daily use |
| **Mobile Wallets** | Smartphone app | Trust Wallet, Exodus | Medium | Payments, portability |
| **Desktop Wallets** | Desktop software | Electrum, Atomic Wallet | Medium-High | Power users |
| **Exchange Wallets** | Custodial (exchange controls keys) | Binance, Krainbase | ❌ Low (not your keys) | Trading |

### Cold Wallets (Offline Storage)

| Type | Format | Examples | Security | Use Case |
|------|--------|----------|----------|----------|
| **Hardware Wallets** | Physical device | Ledger, Trezor | ✅ Very High | Long-term storage |
| **Paper Wallets** | Printed keys | Custom-generated | High (if stored safely) | Offline backup |
| **Steel Wallets** | Metal backup | Cryptosteel, Billfodl | ✅ Very High | Disaster-proof backup |

**Security Best Practices:**
- Never share private keys or seed phrases
- Use hardware wallets for significant holdings
- Enable multi-factor authentication on exchange accounts
- Verify addresses before transactions (phishing risk)
- Test with small amounts first
- Backup seed phrases in multiple secure locations

## Exchanges

### Centralized Exchanges (CEX)

**Characteristics:**
- Custodial (exchange controls funds)
- High liquidity and trading volume
- Fiat on/off ramps
- Order books and matching engines
- KYC/AML compliance required
- Single point of failure risk

**Major Players:** Binance, Coinbase, Kraken, Gemini

**Advantages:**
- ✅ User-friendly interfaces
- ✅ High liquidity
- ✅ Advanced trading features (margin, futures)
- ✅ Customer support
- ✅ Fiat deposits/withdrawals

**Disadvantages:**
- ❌ Custody risk (not your keys, not your coins)
- ❌ Regulatory risk and potential seizures
- ❌ Privacy concerns (KYC required)
- ❌ Hacking targets
- ❌ Withdrawal limits and restrictions

### Decentralized Exchanges (DEX)

**Characteristics:**
- Non-custodial (users control funds)
- Automated Market Makers (AMMs) or order books
- No KYC requirements
- Smart contract-based
- Permissionless access

**Major Players:** Uniswap, PancakeSwap, Curve, dYdX, SushiSwap

**Advantages:**
- ✅ User controls private keys
- ✅ No KYC/AML barriers
- ✅ Transparent on-chain operations
- ✅ Permissionless listing of tokens
- ✅ Composability with DeFi protocols

**Disadvantages:**
- ❌ Lower liquidity for most pairs
- ❌ Higher slippage on large trades
- ❌ Gas fees for transactions
- ❌ Smart contract risk
- ❌ No fiat on/off ramps
- ❌ Slower execution than CEX

## Tokenomics Basics

Economic model governing cryptocurrency supply, distribution, and incentives.

### Supply Mechanics

| Model | Description | Examples | Inflation |
|-------|-------------|----------|-----------|
| **Fixed Supply** | Hard cap on total tokens | Bitcoin (21M BTC) | Deflationary |
| **Inflationary** | Continuous issuance | Ethereum (post-merge variable), Polkadot | Inflationary or stable |
| **Deflationary** | Token burning reduces supply | BNB (burn mechanism), ETH (EIP-1559) | Deflationary |
| **Elastic Supply** | Supply adjusts algorithmically | Ampleforth | Neutral (targets stable price) |

### Distribution Mechanisms

**Initial Distribution:**
- **Mining/Staking Rewards** - Gradual distribution to network participants
- **Presale/ICO** - Early investor allocation
- **Airdrop** - Free distribution to users (marketing/decentralization)
- **Team/Foundation** - Allocated to developers (usually vested)
- **Treasury** - Reserved for ecosystem development

**Key Metrics:**
- **Circulating Supply** - Tokens currently in circulation
- **Total Supply** - All minted tokens (including locked)
- **Max Supply** - Hard cap (if exists)
- **Market Cap** - Price × Circulating Supply
- **Fully Diluted Valuation (FDV)** - Price × Max Supply

## Consensus Mechanisms

| Mechanism | Energy Use | Security Model | Examples | Pros | Cons |
|-----------|------------|----------------|----------|------|------|
| **Proof of Work (PoW)** | ❌ Very High | Computational power | Bitcoin, Litecoin | Battle-tested, decentralized | Energy intensive, slow |
| **Proof of Stake (PoS)** | ✅ Low | Economic stake | Ethereum, Cardano | Energy efficient, scalable | Wealth concentration risk |
| **Delegated PoS (DPoS)** | ✅ Low | Voted validators | EOS, Tron | Fast, efficient | More centralized |
| **Proof of History (PoH)** | Medium | Verifiable time ordering | Solana | Very high throughput | Novel, less tested |
| **Proof of Authority (PoA)** | ✅ Low | Trusted validators | VeChain, xDai | Fast, predictable | Centralized |

## Use Cases

### Payments and Remittances
- Cross-border transfers without intermediaries
- Lower fees than traditional wire transfers
- 24/7 availability
- Challenges: Volatility, regulatory uncertainty, user experience

### Decentralized Finance (DeFi)
- Lending and borrowing without banks
- Decentralized exchanges (DEXs)
- Yield farming and liquidity provision
- Synthetic assets and derivatives
- Challenges: Smart contract risk, complexity, regulatory concerns

### Non-Fungible Tokens (NFTs)
- Digital art and collectibles
- Gaming items and metaverse assets
- Identity and credentials
- Real-world asset tokenization
- Challenges: Speculation, environmental concerns, copyright issues

### Store of Value
- Alternative to gold or traditional assets
- Hedge against inflation
- Sovereign resistance (censorship-proof)
- Challenges: Volatility, regulatory risk, adoption barriers

### Governance
- Decentralized Autonomous Organizations (DAOs)
- Protocol governance tokens
- Community-driven decision making
- Challenges: Voter apathy, plutocracy risk, coordination problems

## Key Concepts Summary

**Blockchain Fundamentals:**
- Distributed ledger maintained by network of nodes
- Cryptographic hashing ensures immutability
- Consensus mechanisms validate transactions
- Transparency and auditability of all transactions

**Cryptographic Security:**
- Public/private key pairs control ownership
- Digital signatures prove transaction authenticity
- Hash functions create unique transaction identifiers
- Merkle trees enable efficient verification

**Decentralization:**
- No central authority controls network
- Censorship resistance and permissionless access
- Trade-offs with efficiency and governance
- Varying degrees across different cryptocurrencies

**Trustless Transactions:**
- Mathematical proofs replace institutional trust
- Smart contracts enable programmable agreements
- Reduced counterparty risk
- Still requires trust in protocol security

## Decision Guide

### When to Use Cryptocurrency

| Scenario | Recommended | Considerations |
|----------|-------------|----------------|
| **Cross-border payments** | ✅ Yes | Faster and cheaper than traditional methods |
| **Remittances** | ✅ Yes | Significant fee savings for international transfers |
| **DeFi participation** | Consider | High returns but significant smart contract risk |
| **Store of value** | Consider | Volatility risk, only allocate risk capital |
| **Daily payments** | ❌ Not yet | Volatility, tax implications, limited acceptance |
| **Privacy** | Consider | Some cryptocurrencies offer privacy (Monero), most are pseudonymous not anonymous |
| **Smart contracts** | ✅ Yes | Ethereum ecosystem mature for development |
| **NFTs/Digital collectibles** | Consider | Speculative market, environmental concerns |

### Choosing a Cryptocurrency

**For Payments:**
- **Low volatility needed** → Stablecoins (USDC, DAI)
- **Maximum decentralization** → Bitcoin
- **Speed and low fees** → Solana, Polygon
- **Privacy** → Monero, Zcash

**For Smart Contracts:**
- **Mature ecosystem** → Ethereum
- **Low fees** → Polygon, Arbitrum (L2), Avalanche
- **High throughput** → Solana, Binance Smart Chain
- **Academic rigor** → Cardano

**For Store of Value:**
- **Digital gold** → Bitcoin
- **Yield generation** → Staking assets (ETH, SOL, ADA)
- **Stable value** → Stablecoins (but no appreciation)

## Risks and Challenges

**Technical Risks:**
- Smart contract vulnerabilities and exploits
- Private key loss or theft
- Network attacks (51%, eclipse, MEV)
- Software bugs and protocol failures

**Market Risks:**
- Extreme price volatility
- Liquidity constraints
- Market manipulation
- Exchange insolvency

**Regulatory Risks:**
- Unclear or evolving regulations
- Potential bans or restrictions
- Tax complexity and reporting requirements
- Securities classification uncertainty

**Operational Risks:**
- User error (wrong address, lost keys)
- Phishing and social engineering
- Centralized exchange custody risk
- Irreversible transactions

**Environmental Concerns:**
- Proof of Work energy consumption
- E-waste from mining hardware
- Carbon footprint of network operations
- Transition to more sustainable consensus mechanisms

## Related

- [[Blockchain Fundamentals]]
- [[Smart Contracts]]
- [[Distributed Systems]]
- [[Cryptography]]
- [[Consensus Algorithms]]
