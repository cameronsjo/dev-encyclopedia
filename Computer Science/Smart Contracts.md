---
title: Smart Contracts
aliases:
  - Smart Contract Development
  - Blockchain Programming
  - Solidity
tags:
  - blockchain
  - ethereum
  - development
  - concept
type: reference
status: complete
created: 2025-11-30
---

# Smart Contracts

Self-executing programs deployed on blockchain networks that automatically enforce and execute agreements when predefined conditions are met.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Trustless, automated execution of business logic on blockchain |
| **Primary Language** | Solidity (Ethereum), Rust (Solana), Move (Aptos) |
| **Execution Environment** | EVM (Ethereum Virtual Machine), SVM (Solana), WASM |
| **Key Properties** | Immutable after deployment, deterministic, transparent |
| **State Storage** | On-chain (expensive), off-chain (IPFS, centralized) |
| **Typical Uses** | DeFi protocols, NFTs, DAOs, token standards, governance |

## Core Concepts

### Solidity Fundamentals

**Contract Structure:**

- State variables (stored on blockchain)
- Functions (external, public, internal, private)
- Modifiers (reusable access control)
- Events (logging for off-chain indexing)
- Constructors (one-time initialization)

**Visibility Specifiers:**

- `external` - Only callable from outside contract
- `public` - Callable from anywhere, auto-generates getter
- `internal` - Only this contract and derived contracts
- `private` - Only this contract

**State Mutability:**

- `view` - Reads state but doesn't modify
- `pure` - Neither reads nor modifies state
- `payable` - Can receive ETH

### EVM (Ethereum Virtual Machine)

**Execution Model:**

- Stack-based architecture (256-bit words)
- Opcodes compiled from Solidity
- Deterministic execution across all nodes
- Gas metering for every operation

**Transaction Lifecycle:**

1. Transaction submitted to mempool
2. Miner/validator picks transaction
3. EVM executes bytecode
4. State changes applied or reverted
5. Events emitted, gas consumed

### Gas

| Aspect | Description |
|--------|-------------|
| **Purpose** | Prevents infinite loops, compensates validators |
| **Gas Limit** | Max units willing to spend on transaction |
| **Gas Price** | Wei per gas unit (fluctuates with demand) |
| **Gas Fee** | `gas used × gas price` |
| **Optimization** | Minimize storage writes, use events, pack variables |

**Gas Costs (approximate):**

- Storage write: 20,000 gas
- Storage update: 5,000 gas
- Transfer ETH: 21,000 gas
- Deploy contract: 32,000 + bytecode cost

### Storage vs Memory vs Calldata

| Location | Persistence | Cost | Use Case |
|----------|-------------|------|----------|
| **Storage** | Permanent (blockchain) | Very high | State variables, persistent data |
| **Memory** | Temporary (function scope) | Moderate | Function parameters, local arrays |
| **Calldata** | Temporary (read-only) | Low | External function parameters |
| **Stack** | Temporary (EVM only) | Very low | Local variables, limited to 16 slots |

**Best Practice:** Use `calldata` for external function parameters, `memory` for internal processing, minimize `storage` writes.

## Common Design Patterns

### Proxy Pattern

**Purpose:** Enable contract upgrades while preserving state and address.

| Pattern | Description | Pros | Cons |
|---------|-------------|------|------|
| **Transparent Proxy** | Separates admin and user calls | Simple upgrade mechanism | Higher gas costs |
| **UUPS (ERC-1822)** | Upgrade logic in implementation | Lower gas | Implementation must handle upgrades |
| **Beacon Proxy** | Multiple proxies share one implementation | Efficient for many instances | More complex architecture |
| **Diamond (EIP-2535)** | Modular facets with shared storage | Maximum flexibility | High complexity |

**Key Insight:** Use `delegatecall` to execute implementation logic in proxy's storage context.

### Factory Pattern

**Purpose:** Deploy multiple contract instances from a single factory contract.

**Benefits:**

- Standardized deployment
- Event tracking for all instances
- Reduced bytecode duplication with `create2` deterministic addresses
- Registry of deployed contracts

### Access Control

| Pattern | Use Case | Implementation |
|---------|----------|----------------|
| **Ownable** | Single admin | OpenZeppelin's `Ownable` |
| **Role-Based (RBAC)** | Multiple permission levels | `AccessControl` with roles |
| **Multi-Sig** | Shared ownership | Gnosis Safe, threshold signatures |
| **Timelock** | Delayed execution | Governance delay for security |

## Security Vulnerabilities

### Critical Vulnerabilities

| Vulnerability | Description | Mitigation |
|---------------|-------------|------------|
| **Reentrancy** | External call allows re-entering function before state update | ✅ Checks-Effects-Interactions pattern<br>✅ ReentrancyGuard modifier<br>✅ Use `transfer()` not `call()` for ETH |
| **Integer Overflow/Underflow** | Arithmetic wraps around (pre-0.8.0) | ✅ Use Solidity 0.8.0+ (auto checks)<br>✅ SafeMath library for older versions |
| **Front-Running** | Attacker sees pending tx, submits higher gas | ✅ Commit-reveal schemes<br>✅ Use private mempools (Flashbots) |
| **Access Control** | Unauthorized function execution | ✅ Proper modifiers on all functions<br>✅ Test permission boundaries |
| **Denial of Service** | Unbounded loops, block gas limit | ✅ Avoid loops over user data<br>✅ Pull over push payments |
| **Delegate Call** | Malicious contract changes proxy state | ✅ Only delegatecall trusted implementations<br>✅ Storage layout compatibility |
| **Unchecked Returns** | Ignoring return values from calls | ✅ Check all external call returns<br>✅ Use SafeERC20 for tokens |
| **Timestamp Manipulation** | Miners can manipulate block.timestamp | ✅ Don't rely on exact timestamps<br>✅ Use block numbers for ordering |

### Checks-Effects-Interactions Pattern

**Always follow this order:**

1. **Checks** - Validate conditions (require statements)
2. **Effects** - Update contract state
3. **Interactions** - External calls to other contracts

**Why:** Prevents reentrancy by updating state before external calls.

## Auditing & Testing

### Testing Frameworks

| Framework | Language | Features | Best For |
|-----------|----------|----------|----------|
| **Foundry** | Solidity | Fast, fuzz testing, gas profiling | ✅ Modern projects, performance |
| **Hardhat** | JavaScript/TS | Rich plugin ecosystem, mainnet forking | ✅ Full-stack dApps, debugging |
| **Truffle** | JavaScript | Established, Ganache integration | Legacy projects |
| **Brownie** | Python | Pythonic testing, hypothesis testing | Python developers |
| **Anchor** | Rust | Solana framework with built-in testing | Solana contracts |

**Foundry Advantages:**

- Written in Rust (extremely fast)
- Tests in Solidity (same language as contracts)
- Built-in fuzzing and invariant testing
- Gas snapshots and profiling
- Mainnet forking without external dependencies

### Testing Best Practices

**Coverage Areas:**

- Unit tests for individual functions
- Integration tests for contract interactions
- Fuzz testing for unexpected inputs
- Invariant testing (properties that must always hold)
- Gas benchmarking
- Mainnet fork testing with real state

**Security Checklist:**

- [ ] All external calls checked for reentrancy
- [ ] Access control on privileged functions
- [ ] Integer overflow protection (Solidity 0.8.0+)
- [ ] Input validation on all parameters
- [ ] Proper event emission for state changes
- [ ] Gas optimization for user transactions
- [ ] Emergency pause mechanism
- [ ] Upgrade mechanism tested
- [ ] External contract calls handle failures
- [ ] No hardcoded addresses (use config/registry)

### Audit Process

**Phases:**

1. **Automated Analysis** - Slither, Mythril, Echidna
2. **Manual Review** - Line-by-line code inspection
3. **Invariant Testing** - Property-based testing
4. **Economic Analysis** - Game theory, incentive alignment
5. **Report & Remediation** - Fix critical/high issues
6. **Re-audit** - Verify fixes

**Top Audit Firms:** Trail of Bits, OpenZeppelin, ConsenSys Diligence, Quantstamp, Sigma Prime

## Platform Comparison

| Platform | Language | VM | Consensus | TPS | Gas Model | Strengths | Considerations |
|----------|----------|----|-----------|----|-----------|-----------|----------------|
| **Ethereum** | Solidity, Vyper | EVM | PoS | 15-30 | High, variable | ✅ Largest ecosystem<br>✅ Most tooling<br>✅ Highest security | ❌ Expensive<br>❌ Slower |
| **Solana** | Rust | SVM | PoH + PoS | 3,000+ | Low, predictable | ✅ High throughput<br>✅ Low fees<br>✅ Fast finality | ❌ Complex programming model<br>❌ Less mature |
| **Polygon** | Solidity | EVM | PoS | 65,000 | Very low | ✅ EVM compatible<br>✅ Fast & cheap<br>✅ Ethereum security bridge | ❌ More centralized<br>❌ Security depends on validators |
| **Arbitrum** | Solidity | EVM | Optimistic Rollup | 4,000 | Low | ✅ Full EVM compatibility<br>✅ Ethereum security<br>✅ Lower fees than L1 | ❌ 7-day withdrawal delay<br>❌ Still developing |
| **Optimism** | Solidity | OVM (EVM) | Optimistic Rollup | 2,000 | Low | ✅ EVM compatible<br>✅ Ethereum security<br>✅ Sequencer revenue sharing | ❌ Withdrawal delay<br>❌ Centralized sequencer |
| **Avalanche** | Solidity | EVM | Avalanche | 4,500 | Low | ✅ Sub-second finality<br>✅ Subnet customization<br>✅ EVM compatible | ❌ Different consensus model<br>❌ Smaller ecosystem |
| **Base** | Solidity | EVM | Optimistic Rollup | 2,000+ | Very low | ✅ Coinbase backing<br>✅ EVM compatible<br>✅ OP Stack | ❌ Newer platform<br>❌ Smaller ecosystem |
| **Aptos** | Move | MoveVM | PoS | 160,000 | Low | ✅ Move language safety<br>✅ Parallel execution<br>✅ High throughput | ❌ New ecosystem<br>❌ Learning curve |

**Decision Guide:**

| Priority | Recommended Platform |
|----------|---------------------|
| **Maximum Security** | Ethereum L1 |
| **Low Cost, High Speed** | Solana, Polygon, Base |
| **EVM Compatibility** | Polygon, Arbitrum, Optimism, Avalanche |
| **Developer Tooling** | Ethereum, Polygon |
| **Novel Language Safety** | Aptos (Move), Solana (Rust) |
| **Enterprise/Consortium** | Hyperledger Fabric, Corda |

## Token Standards

| Standard | Purpose | Key Functions |
|----------|---------|---------------|
| **ERC-20** | Fungible tokens | `transfer()`, `approve()`, `transferFrom()` |
| **ERC-721** | Non-fungible tokens (NFTs) | `ownerOf()`, `safeTransferFrom()`, metadata URI |
| **ERC-1155** | Multi-token (fungible + NFT) | Batch transfers, gas efficient |
| **ERC-4626** | Tokenized vaults | Standardized yield-bearing tokens |

## Development Workflow

**Typical Stack:**

1. **Development** - Foundry/Hardhat for compilation and testing
2. **Local Network** - Anvil (Foundry) or Hardhat Network
3. **Deployment** - Forge scripts or Hardhat deploy
4. **Verification** - Etherscan API for source code verification
5. **Monitoring** - Tenderly, Defender for transaction monitoring
6. **Frontend** - ethers.js/viem + wagmi for React integration

**Environment Progression:**

- Local (Anvil/Hardhat) → Testnet (Sepolia/Goerli) → Mainnet

## Best Practices

**Security:**

- Start with audited libraries (OpenZeppelin)
- Fail fast with `require()` statements
- Use events for off-chain monitoring
- Implement emergency pause mechanisms
- Never trust user input or external contracts

**Gas Optimization:**

- Pack storage variables (use uint128 over uint256 when possible)
- Use `immutable` for constructor-set constants
- Use `calldata` instead of `memory` for external parameters
- Cache storage reads in memory
- Batch operations to reduce transaction count

**Maintainability:**

- Document all functions with NatSpec comments
- Use descriptive variable and function names
- Keep functions small and focused
- Emit events for all state changes
- Version contracts and track deployments

## Related

- [[Blockchain Fundamentals]]
- [[Ethereum]]
- [[Web3 Development]]
- [[Cryptography]]
- [[Distributed Systems]]
