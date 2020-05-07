# Prosomo 0.7

# A blockchain testnet client for development of the Topl protocol

Prosomo, short for *Prosomoiotís* - the Greek word for simulator, is a stand-alone application that executes the Ouroboros Genesis protocol.  The goal is to have a persistent Proof-of-Stake (PoS) blockchain come to consensus among a system of Akka actors interfaced on a peer to peer network.  Under the conditions guaranteed by PoS consensus, the actors maintain a live and robust ledger of randomly generated transactions.

Ouroboros has been presented in a very rigorous and formal mathematical style that stands out among other protocol designs. The security properties of the protocol are formulated from probabilistic arguments drawn from mathematical proofs.  These proofs use a self consistent framework to make statements about the operation of several ideal functionalities acting in a specified order that completely defines the protocol.  Formal analysis in a self consistent framework assures that the protocol is mathematically sound but makes it difficult to conceptualize the procedure and how it operates over a real-world network.  We wish to form a better intuition about the protocol’s behavior under different circumstances to put theory to the test.  The papers specifying Ouroboros provide almost no data on the execution of the protocol, containing only one plot regarding transaction throughput.   Prosomo will remedy this by providing data from which metrics of the network can be directly calculated under controlled conditions.  Work has been done involving simulation of adversarial manipulation, network connectivity conditions, and chain visualization.

The simulation can be executed locally or across many nodes.  Local communication is handled by a simplified global network model that adds artificial delay.  Scala and Akka were chosen as a platform so this implementation can be incorporated into Bifrost, Topl’s production blockchain client.  All cryptographic and consensus routines are executed by Stakeholder actors, serving as a test bed for the primitives that have been implemented for Prosomo and Bifrost.  Data output consists of block information, global position, network connectivity graphs, and stake distribution over time.  Block tree diagrams, i.e. the set of all tines, can be constructed from the database of blocks; no blocks are deleted so we may study the structure of tines produced by the protocol.  An account based transaction model is used for the ledger and stake is tracked as a ledger balance.  The simulation has many configuration options and simulations with 1 to ~100 Stakeholders per node can be executed provided enough system RAM.  About 0.5 GB per Stakeholder provides sufficient overhead for long term execution.

## Key Features
- Uses the Akka actor system for concurrent execution and communication between peers
- Stakeholders are Akka actors that autonomously carry out the protocol, there can be many Stakeholders per node
- A Coordinator actor gives commands to the nodes Stakeholders, calculates the genesis block, and synchronizes all Stakeholders
- A Router actor that collects messages and sends them to the recipient Stakeholder, using the Akka system scheduler if the destination is local and using a dedicated network controller if the destination is in a remote node
- A simple gossip protocol that acts between Stakeholders to efficiently broadcast transactions and blocks over the network
- Transactions are issued, broadcast, and added to forged blocks so the stake distribution changes over time
- Command scheduling and flexible configuration options that can be modified at run-time

The consensus protocol used in Prosomo is Ouroboros Genesis.  Ouroboros, the original protocol, has gone through several revisions.  The protocols we are concerned with have been developed and published by IOHK and Cardano in the following chronological order:


- Ouroboros - original definition of slots and epochs with a verifiable pseudo-random leader election process based on a commit-reveal scheme that requires all participants to register round by round
- Ouroboros Praos - a relaxed version of Ouroboros that replaces the leader election mechanism with a verifiable random function allowing the protocol to operate under dynamic availability
- Ouroboros Genesis - the protocol of Praos with a modified chain selection rule that allows participants to bootstrap from the genesis block without any dynamic checkpoint system
- Ouroboros Chronos - the protocol of Genesis with a synchronization functionality built into the block headers, eliminating the need for a global synchronization functionality

Of the four protocols, Praos, Genesis, and Chronos are of particular interest.  Chronos focuses on synchronization and serves to replace the global clock functionality in Genesis; we have chosen Genesis as a starting point which we can update to Chronos at a later time.  Ouroboros Praos, the previous specification, establishes many of the concepts and terminology which carry over to Genesis.  Since the three specifications use the same staking procedure with differences in chain selection and synchronization, we shed light on the properties of all of the above protocols by studying Prosomo.  The paper on Ouroboros Genesis https://eprint.iacr.org/2018/378.pdf encompasses the academic specification of the protocol along with robust mathematical analysis that quantifies the probability that the blockchain produced by the protocol will sustain a live and persistent ledger.


## Design Constraints For Prosomo
- The Stakeholders should mimic the dynamic stake protocol given in Ouroboros Genesis as closely as possible
- The execution should be resilient against adversarial tampering assuming honest majority
- Stakeholders should reach consensus
- Should achieve a persistent and live ledger
- Simulation must have a mode that generates reproducible deterministic output
- Deterministic output must be a subset of possible output of concurrent execution
- Testnet requirements: simulation must be able to connect to a bootstrap node and discover peers dynamically, network communication must be handled on same footing as local communication

In a nutshell the procedure of the Ouroboros Genesis protocol is a round by round execution of a leader election process that is publicly verifiable and cannot be biased by any one party.  Blocks are forged in a chronological fashion where time is segmented into rounds corresponding to integer slots.  Each round corresponds to one slot and epochs represent a fixed interval of slots.

Slot leaders correspond to valid block headers that satisfy a comparison condition that is uniquely determined by a Stakeholders public address.  Leader election corresponds to the staking procedure: a pseudo-random value is generated from the slot number, epoch nonce and a private key.  If this value is less than a threshold calculated from a Stakeholder’s net stake according to the ledger given by the protocol then a valid block may be forged. Valid blocks are verified with a Stakeholders public address, provided that the threshold and block header pass the staking procedure comparison.  To guarantee that leader election cannot be biased, the staking threshold is only modified by stake distributions in the past.  The epoch randomness is also sampled from the past so everyone agrees on the leadership eligibility no matter what.

These constraints play out updating the randomness and stake distribution in intervals called epochs.  Epochs should be long enough to ensure that no one party can bias the stake distribution and randomness in future epochs.  The upper bound of an epoch interval is constrained by the desired market response so that stake may be freely traded.  The default configuration for Prosomo has a slot time of 1 second and an epoch of 1 day.  The length of the epoch must be adjusted by the network delay tolerance and is parameterized to guarantee certain security and chain quality properties.  The stake distribution and epoch randomness is calculated from the blockchain at the beginning of each epoch.  The epoch nonce represents the seed-randomness for which all stakeholders calculate slot leader eligibility and future nonces.  Leader eligibility changes every slot and is uniquely determined from the epoch nonce, the slot, and net stake.  

The threshold, a function of the net stake, is held constant over the course of an epoch.  The stake distribution for calculating the threshold is sampled from a snapshot of the stake two epochs in the past, ensuring that transactions cannot affect the leader eligibility in the current epoch.  

The epoch nonce is generated from a set of seeds sampled the block headers of the first two-thirds of the previous epoch.  This range of sampling for epoch nonce generation is designed to coincide with the parameterization of the Ouroboros Genesis chain selection rule so no one party may bias the epoch nonce.


## Communication Between Peers

Stakeholders participate in rounds by diffusing messages and registering the other parties present on the network.  The diffuse functionality lets Stakeholders assign a public key used for ledger tracking and forging to other peers .  Each set of remote and local stakeholders use a simplified gossip protocol to share transactions and blocks.  Stakeholders broadcast Hello messages to their neighbor and populate a list of gossipers based on who responds first.  The number of gossipers has a sinusoidal variation to allow bootstrapping Stakeholders to connect.  When the simulation begins, the initial set of stakeholders are all in one party that can be split into sub parties with scripted commands.  The sub parties can be later rejoined to emulate a network split scenario.  This scenario can also be scripted and carried out over a network.


## Global Delay Model

Global delay is added to messages passed between local stakeholders.  Remote stakeholders from other nodes have no artificial delay added since we wish to measure the delay induced by remote communication.  A more complete model would use empirical data from real world measurements of operational networks.  With the recently added network functionality, we can start to explore real world sources of network delay.

In the simple global network delay model, each stakeholder is assigned random global latitude and longitude.  The network delay is calculated from the distance between each actors coordinates using a proportionality constant in ms/km.  Additionally a delay is added that is proportionate to the size of the message using a constant in ms/byte.  A randomly determined delay is added to each message up to a maximum delay given in ms to emulate random network delay.  The net delay on a message is

    delay = delay_ms_km*distance + delay_ms_byte*messageBytes.size + randDouble*delay_ms_noise

This serves as a crude representation of the effects of network latency, bandwidth, and dynamic availability giving a first order approximation of global delay in a peer to peer network.


# Cryptographic Primitives

Three distinct cryptographic functionalities are used to identify a Stakeholder with a single public address.  A Signature scheme, a Verifiable Random Function, and a Key Evolving Signature scheme are simultaneously used to validate transactions, leader eligibility, and block header authenticity respectively.  The three functionalities form a triad of public private keypairs so a Stakeholder’s public address corresponds to all three public keys concatenated together.

## Digital Signatures

A Signature Functionality (SIG) is used to determine transaction validity and identity verification.  We have two requirements for a signature routine:


- The signature routine must be EUF-CMA (Existential Unforgeability under Chosen Message Attack) secure
- Strong signatures: signature information **s** for a message **m** must be deterministic and collision resistant such that another signature **s’** cannot be found that verifies with **m** and the public key

We use the BouncyCastle java implementation of the Ed25519 signature routine specified in https://tools.ietf.org/html/rfc8032.  This scheme gives strong signatures with deterministic output based on elliptic curve cryptography.  Other signing and verification routines use this implementation of Ed25519 as a sub-scheme; their composition retains the security properties of the underlying sub-scheme.  In Ed25519, the private key is a secret bit-string that is chosen from uniform randomness.  The public key corresponds to a mapping of that private key to an elliptic-curve point.  It is impossible to recover the private key from this curve point.


## Verifiable Random Function

A Verifiable Random Function (VRF) is used to carry out the staking procedure and determine the validity of block headers.  VRF output is a pseudo-random value that is deterministic and is generated from a proof.  The proof is committed to a message with the secret key and verified with the message and a public key.  A VRF must satisfy the following properties:


- Given input message **a**, compute **b = VRF(SK,a)** and proof **p = P(SK,a)** where **SK** is the secret key
- Given the public key **PK**, compute **v = Verify(PK, b, p)** where **Verify** will return **true** if **b** and **p** were made with **SK** that corresponds to **PK**, **false** if otherwise
- Uniformly Random: **b** must be indistinguishable from randomness
- Uniqueness: **p = p’** if and only if **a = a’** for **p = P(SK,a)** and **p’ = P(SK,a’)**
- The above constraints require a function **ProofToOutput** to securely map the proof to the output such that **VRF(SK,a) == ProofToOutput(P(SK,a))**
- We need only specify **P(SK,a)** and **ProofToOutput(p)**

A VRF has been implemented in Scala that uses the Ed25519 implementation specified in the SIG functionality.  This implementation is used for slot leader election and epoch nonce generation.  It corresponds to ECVRF-ED25519-SHA512-TAI specified in https://tools.ietf.org/html/draft-irtf-cfrg-vrf-06.  The public-private keypair for the VRF in this implementation is equivalent to a keypair for Ed25519.  This implementation satisfies the above requirements and specifies **P(SK,a)** and **ProofToOutput(p)**.


## Key Evolving Scheme

The Key Evolving Signature functionality (KES) is used for block header signing and verification. Ouroboros enforces a chronological ordering of slots, so headers are signed with a KES scheme such that the slot of a header cannot be modulated once the signature is forged. The KES must have the following properties:


- The private key evolves in time steps and has a configuration that uniquely determines the current time step of the key
- The public key is static and uniquely determined by the private key
- Forward Security: the private keys corresponding to time steps lower then the current time step cannot be recovered from the current private key configuration
- Secure erasure: the protocol must store the evolved private key in a configuration corresponding to the time step of the current global slot and erase any past copies
- A signature is verifiable with a message, a time step, and the public key

Ordinary public-private keypair schemes are inherently vulnerable to exposure of the secret key.  Once the secret key is known, all signatures previously generated with that keypair may be altered.  Forward key evolution ensures that past signatures remain secure in the event that the private key is discovered by an adversary.  Forward key evolution allows future private keys to be calculated from an exposed private key, but previous private keys cannot be recovered from the current private key.  The adversary who gains access to a private key evolved to time step **t** will only be able to forge new signatures at **t’>= t**, and cannot forge past signatures for **t’< t**.  This provides a layer of security for block header signatures, and protects the integrity of the network form Long-Range attacks.

Prosomo uses a Scala implementation of the Malkin-Micciancio-Miner (MMM) construction specified in https://dl.acm.org/citation.cfm?id=647087.715826 for the KES functionality using the SIG functionality as a base signature scheme.  The private key consists of two binary trees and three byte strings.  One binary tree corresponds to the super scheme **L** and the other corresponds to the sub-scheme **Si**.  The first string is a signature of the root public key of **Si** signed with **L** and is dynamically updated with each increment of **L**.  The second string corresponds to the public key of **Si** and is dynamically updated with each increment of **L**.  The third string corresponds to the seed for the next sub-scheme Si that will be generated once **L** increments to the next time step. This is also used as the seed for the pseudo random number generator that in turn generates the next seed for **Si**.  With each step of **Si**, the binary tree for **Si** is updated, and with each step of **L**, **Si** is reseeded with the stored seed and new set of signature, public key, and seed are calculated.

Each node of the binary tree contains a byte string that contains three values of fixed length.  In order it is the seed for the right child node **R1**, the public key of the left child node **PK0**, and the public key of the right child node **PK1**, such that the byte array at each node is **R1||PK0||PK1**.  The public key of a node is the hash of **PK0||PK1**, so the public key of the entire scheme is **Hash(PK0||PK1)** of the root node of **L**.  The public key of a leaf is the Hash of the public key of the underlying SIG scheme.  The leaf contains just a single byte string that contains **SK||PK** of the underlying SIG functionality.

Key generation consists of several steps.  First the entire binary tree is generated with a length doubling pseudo-random number generator from the initial **seed**, where **seed -> R0,R1** which are fed to the child nodes of each node, where the left node is seeded with **R0** and the right node is seeded with **R1**.  **R1** is saved at each node.  The leaves then contain **R0** for the left leaf and R1 for the right leaf which they inherit from their parent node. Then each leaf is populated with a keypair of the underlying scheme seeded with the **R** value that was given.  Then the Merkle tree is produced from the hash of the public keys of the underlying scheme.  The public key of each node, i.e. **Hash(PK0||PK1)**, is fed to its parent node.  The public key of the entire scheme is the root of the Merkle tree and can be calculated anytime from the root node of the binary tree.  After hashes are computed the tree is complete and from the root down the right child nodes and leafs are erased leaving only the leftmost leaf and leftmost node per level.  The information lost is recovered deterministically from the **R1** value stored at each node.

Key update consists of modifying the binary tree structure and reproducing the public private keypairs for the next leaf of the scheme.  First if switching from left leaf to right leaf, the right leaf is seeded with **R1** from the parent node and a keypair of the underlying scheme is generated.  When switching from a left node to a right node, the key generation scheme is seeded with **R1**, leaving a right node with a left only child branch.  This left-only child branch is then incremented in the same fashion in a recursive way.  With this procedure, the time step is determined by the left-right positions of the nodes with respect to their level in the tree.  For example, say an 8 level tree has a configuration LLRLRLRL.  Then the time step is {00101010} = 42.  In the MMM construction, the sub-scheme is updated until it runs out of leaves and then the super-scheme is updated.  Each sub-scheme has **tl** leaves where **tl** is the current time-step of **L**, so the current time step is calculated as **t = 2^tl+ti-1** where **ti** is the current time-step of **Si**.

A signature in the MMM scheme consists of the signature and public key of the sub-scheme given in the key file as well as the signature of the message signed by the sub-scheme.  The sub-scheme signature is a sum composition signature of the message concatenated with the time step bytes, along with the public keys **PK0||PK1** of each node of the private key, in order.

Signature verification consists of two sum composition verifications, one for the public key of **PKL** and the signature of **PKSi**, the other for the public key **PKSi** and the signature of the message **M||step**.  The step bytes are used to verify the Merkle tree witness path **PK0||PK1||PK0'||PK1'**... that is given with each signature.  Finally the public keys are confirmed to correspond to the two schemes **L** and **Si**.

## Hash Function

The execution of the protocol requires a sufficient realization of a global random oracle, a trusted source of randomness that is available and verifiable globally.  A hash function with the following properties is sufficient to realize this ideal funcitonality:
  
- Trapdoor function: Given a hash function **H**, input message **m**, and output **b**, the input **m** must be non-recoverable from the output **b = H(m)** provided **b** alone
- Collision resistance: It must be impossible to find input **m'** such that **H(m) = H(m')** for a given **m**
- Uniform randomness: Given an arbitrary input **m**, the output **b=H(m)** must be indistiguishable from randomness for all **m**
  
Prosomo uses a hash function to produce epoch nonces, seeds in the key evolving scheme, and block identification.  The hash function currently used is the Blake2b 256-bit java implementation of RFC7693 https://www.ietf.org/rfc/rfc7693.txt.  This specification was chosen for its speed and established security.



## Global Clock

Since blocks are chronologically sorted according to slots, Stakeholders executing the protocol must agree on the time length of slots and the current global slot at all times.  Block headers with a slot greater than the current global slot are not valid.  The global slot is determined by synchronized clock functionality.

A global clock is represented with the system clock that is adjusted using the NIST Internet Time Servers https://tf.nist.gov/tf-cgi/servers.cgi.  The default time server is time-a.nist.gov.  The Coordinator behaves as a global clock, giving each stakeholder its system time passed through an Akka message.  The current global slot is determined from time given by the Coordinator.


# Forging Blocks

Block forging proceeds at the beginning of the slot.  The staking procedure is carried out and if the test value passes the comparison the block header is forged and broadcast to the network.  Verifiable block headers contain the following information:


    block = (
      parent_block_hash,
      ledger,
      slot,
      certificate,
      vrf_nonce,
      vrf_proof,
      kes_signature,
      kes_public_key,
      block_number,
      parent_block_slot
    )

The parent block hash is the block identifier of the head of the blockchain during the time of forging.  The ledger entry is a commitment to a set of transactions, i.e. the block body, and is uniquely determined by the body.  The certificate contains the VRF test information and public keys so that the threshold of leader eligibility can be calculated and verified when the block is received.

Future epoch nonces are generated by hashing the concatenation of the previous epoch nonce with the VRF nonces from the first 2/3 of the block headers of the previous epoch.  All Stakeholders keep a view of local state from which the stake distribution and epoch threshold can be calculated for block validation.  In a given epoch, the relative stake for block validation and leader election is sampled from the stake distribution from two epochs prior. Every stakeholder will need the exact same snapshot of staking distribution to validate blocks that were forged in a given epoch.


# Tine Construction

Blocks are broadcast among stakeholders as they are forged and when a new block is received a holder will try to build a tine from it.  Tines represent candidates that may be adopted if they are validated and have a common block somewhere on the stakeholders local chain.  When an actor hears about a new block, it adds it to its database.  Blocks are identified by their hash.  If the blocks slot is at or below the current slot, its considered the head of a new tine.  The parent hash included in the new block is used to identify the parent block.  If a parent block is not found in the actors local database, the actor will request that block by querying the sender.  In return if any parent block is not found for subsequently fetched blocks, the sender will be queried again until a parent block is found that is on the local chain of the actor.  Once this common ancestor block is found the tine is then a candidate for chain validation.  If no common ancestor is found, the tine is discarded.


# Chain Adoption

  
Chain selection occurs in accordance with *maxvalid-bg* specified in Ouroboros Genesis as new tines are built and confirmed to have a common ancestor in the local chain.  The ancestor block for both tines represent the head of the prefix to both tines.  Any tine with a prefix above a certain depth is considered.  This depth is parameterized by **k** and any tine that has a common prefix above **k** blocks deep is selected by longest chain rule, i.e. the tine with a head containing a higher block number than the head of the local chain.  If a common prefix below **k** blocks is found an alternative selection rule is used.  This selection rule prefers the tine with a higher number of blocks in a window of slots starting at the prefix slot and ending at prefix slot + **s** where **s** is the slot window interval.  Both **k** and **s < k** are parameterized to satisfy chain quality properties in the honest majority setting.  If either of these conditions are satisfied then the tine is validated block by block.

The tines with appropriate block number are validated per the isValidChain specification in Ouroboros Genesis.  The transactions included in each ledger are verified and a new state is calculated for each block on the tine.  If an invalid state is produced then the entire tine is discarded.  If the resulting state is valid, then each block is verified according to the VRF proofs, KES signatures, and slot leader eligibility according to the forger public keys and the local staking state.  Once the tine has passed all tests it is adopted as the local chain.

The implementation of chain validation in Prosomo has an additional check not specified in Ouroboros Genesis.  The threshold calculated upon forging is included in the block certificate.  The validator calculates this same threshold for the forger from its local staking state, and the locally calculated threshold must be equal to the certificate threshold for the block to be valid.  In Ouroboros Genesis, the locally calculated threshold is evaluated in an inequality against the VRF test nonce, and there is no assurance that the forger and verifier calculated the exact same threshold value.


# Transactions and State

During the execution of the simulation, transactions are randomly generated at a specified rate. 
Transactions may also be scheduled between specific actors with commands.  Transactions that are issued and new transactions that are discovered are broadcast to the set of gossipers.  An account based model is used for tracking state transitions and each transaction has a unique ID and a nonce.  State consists of a balance along with the next expected transaction nonce.  For a transaction to be applied to state it must have a nonce equal to the expected nonce and it must not deplete the account balance to below zero.  If those conditions are satisfied, then the new state balance is calculated and the next expected nonce is incremented by one.

The state of the blockchain in Prosomo consists a ledger balance and a transaction counter.  Following the account based transaction model, the transaction counter enforces the ordering of transactions issued by stakeholders and prevents double counting state transitions when blocks are applied to state.  Block validity is predicated on the state validity check, so that all transactions on the block ledger are valid state transitions.

# History and Storage

Each block that produces a valid state is stored in a history object that the actors use to store copies of state.  When the epoch is updated, the snapshot of state used for the staking distribution is collected from history by querying it based on the hash of the block associated with that state.  History is also used to revert the local state to the common prefix and apply new state transitions when checking a new tine.  The staking state is collected from history when the epoch is incremented.

Prosomo uses a database to store block and state information on disk. A java implementation of LevelDB maintained at https://github.com/dain/leveldb is used to store and index blocks and state based on what epoch they correspond to.  Since there can be no empty epochs, each epoch will be represented as individual LevelDB databases.  When the database is queried with a block ID, the slot number must be included so the appropriate epoch database can be opened and checked.

# Wallet

Each stakeholder has a wallet object that is used to track pending transactions.  The pending transactions are only confirmed after they have appeared in a block at or below the confirmation depth given as a parameter.  This is achieved by sending the wallet a copy of the state at the confirmation depth and removing any pending transactions with a nonce that is lower than the nonce contained in that state.  The balance and total balance can be queried by a command for individual actors.

When tines are adopted the wallet is updated.  All transactions pertaining to the wallet on the local chain from above the common prefix to the head are added to the set of pending transactions.  The new tine that is adopted is applied and the state at confirmation depth is used to trim the set of pending transactions.  The local state is then used to see which transactions appear in the unconfirmed interval of tine that was just adopted.  Any pending transaction that has a nonce greater than or equal to the local state nonce is rebroadcast to the gossiper set.  This ensures that any pending transaction that is missed by a portion of the network will eventually hear about it and either forge it in a block or adopt it in a tine.

# Mempool Management

The mempool contains pending transactions that are to be applied to the local state upon block forging.  If a new transaction is discovered, i.e. it’s unique ID does not appear in the mempool, its transaction nonce is checked against the expected nonce recorded in local state for that transactions sender.  If the new transaction nonce is greater than or equal to the local state nonce, then the transaction is added to the mempool and broadcast to the set of gossipers.  The mempool accumulates new transactions until either a block is forged, or individual transactions are encountered on the ledgers of adopted tines.

When a tine is adopted, all transactions on the local chain from above the common prefix to the head are added to the mempool.  The new local state is used to trim the mempool of all old transactions that have a transaction nonce lower than the state nonce.  When a tine is discarded, all transactions from that tine are added to the mempool and then the mempool is trimmed with the local state nonces.

# Ledger Selection

Transactions that are broadcast on the network file into stakeholders mempools and are statefully tracked as eligible ledger entries according to local state.  When a block is forged, the ledger for that block is created from entries in the mempool.  First a buffer is created from the mempool that consists of a sorted list of transactions that are sorted by their nonces.  The lowest nonce is applied first, beyond that there is no order.  Once a maximum number of valid transactions have been parsed, the mempool returns the sorted list and the block is forged with that ledger including the forger reward for the block.  The size of the ledger and the forger reward are configuration parameters not specified in Ouroboros.

# Executing the Simulation

The project can be run either in the scala build tool (sbt) console or staged for deployment to a server by running ‘sbt stage’ in the project directory.  This creates the executable Prosomo/target/universal/stage/bin/prosomo that can be run independently of sbt.  A command line script for interacting with the simulation is provided in the project directory.  To execute the command line, run Prosomo/cmd.sh in a separate terminal.  This is used to pass commands to Prosomo via a file and can be used to queue commands at a later slot.  Just enter the desired slot number next to the command.  The command line input is useful for debugging, setting up specific network conditions, and manipulating repeated simulation runs in real time.

Prosomo has command line configuration capabilities.  The *.conf files are HOCON formatted configuration files that specify the simulation parameters and commands.  To run a simulation with a given input command, they can be entered into an array in the .conf file.  The default input.conf file is given below, and is included in the project directory:


    input{
      params {
        //your parameters go here
      }
    
      command {
        //your commands go here
        cmd = []
      }
    }

The command line also accepts HOCON formatted strings in the command line to enable scripts to set values in batch jobs.  An example of a parameter sweep run in the bin directory is given below (using GNU parallel):


    parallel --jobs 32 "./prosomo input \"input{params{f_s={1},delay_ms_km={2},inputSeed={3}}}\"" ::: 0.15 0.25 0.35 ::: 0.0 0.1 0.2 0.3 0.4 ::: seed1 seed2 seed3

The prosomo executable will look for a file called input.conf in the local directory and load those values, then it loads the values specified in the string.  The first argument of the prosomo executable specifies the file to look for, e.g. ./prosomo run1 will look for a file called run1.conf


## Commands

Commands may be specified to execute in a given slot in the configuration file.
Several commands are available:


    status_all

Prints the status of each stakeholder, including the chain hash and number of transactions in the mempool and on the chain.  Individual holders can be addressed by replacing all with the holder index.


    verify_all

Prints the status of each stakeholder, including the chain hash, transaction in mempool, and verifies the chain from the genesis block.  Individual holders can be addressed by replacing all with the holder index.


    inbox

Stakeholders print their inbox which represents a list of all known parties which they are aware of and communicating with.


    print_0

Specifies the holder of the given index to print to console if printing is on.  Default holder to print is holder 0, e.g. print_23 will tell the stakeholder at index 23 to print to console.


    write

Clears the file writer buffer and writes it to the data directory.  Run before plotting.


    kill

Stops all execution.


    stall

Stalls all actors except for the coordinator.  The global clock will continue to tick.


    stall_0

Stalls the holder with the given index.  e.g. stall_0 will stall only the 0th holder stall_2 will stall the 2nd holder etc.


    pause

Pauses the coordinator.  The global clock will stop but all stakeholders will continue to respond to messages they receive.


    randtx

Turns on or off the  coordinator issuing randomized transactions.


    graph

Outputs the network connectivity matrix of the system to the data directory, for plotting with obGraph.py


    tree

Outputs all block data, chain history, and parameters of the printing holder to the data directory.


    tree_all

Same as tree command but outputs all holders in the simulation.


    split

Splits all stakeholders into two randomly selected parties.


    split_stake_0.5

Splits the holders into two parties based on the relative stake.  The two parties will have a net relative stake split by the specified double value between 0.0 and 1.0, e.g. split_stake_0.4 would make two parties with 40% and 60% of the stake, respectively.


    bridge

Splits all stakeholders into two randomly selected parties with one stakeholder in both parties.


    bridge_stake_0.5

Splits all stakeholders into two randomly selected parties with one stakeholder in both parties but splits the parties in the specified stake ratio in the same fashion as split_stake_0.5.  Different value between 0.0 and 1.0 can be used.


    join

Joins the parties back together and resets all actors gossipers.


    new_holder_0

Creates a new holder with the specified index that bootstraps from the genesis block.  If the holder index is not on the genesis block, the new holder will not have any stake initially and won’t be able to forge blocks until future epochs.


## Configuration Files

In order to schedule commands and set parameters, spdecify the command in the cmd list of the input.conf file.  For example:


    input{
      params {
        //your parameters go here
        numHolders = 32
      }
    
      command {
        //your commands go here
        cmd = ["split_stake_0.3 100","join 200"]
      }
    }

would set the number of holders in the simulation to 32.  The simulation would evolve until slot 100 and the 32 holders would be split based on their stake into parties consisting of ~30% and ~70% of the net stake respectively.  Then once slot 200 is reached, the parties are joined back together.  This kind of control allows specific network conditions to be emulated to study how the protocol responds to these scenarios.  The plan is to model adversarial behavior with commands that stall parties and change the network connectivity.

The parameters and their default values available to be set in the *.conf format files are listed below:


    //bootstrap IP enabling peer discovery
    knownPeer = "35.192.11.126:9084"
    //Remote Procedure Calls port
    rpcPort = "9085"
    //bind address
    bindAddress = "0.0.0.0:9084"
    //declared address broadcast to other peers
    myAddress = ""
    //time server for NTP sync
    timeServer = "time-a.nist.gov"
    //seed for pseudo random runs
    inputSeed = "prosomo_testnet"
    //number genesis of stakeholders
    numHolders = 8
    //the minumum index of local holders, set to -1 for no holders
    holderIndexMin = 0
    //the maximum index of local holders, set to -1 for no holders
    holderIndexMax = 7
    //time scale for slot time and delay parameters
    timeScale = 1.0
    //duration of slot in milliseconds
    slotT = 1000
    //delay in milliseconds per killometer in router model
    delay_ms_km = 0.02
    //delay in ms per byte in router model
    delay_ms_byte = 2.0e-4
    //delay random noise
    delay_ms_noise = 100.0
    //use router if true, use direct communication if false
    useRouting = true
    //use network delay parameterization if true
    useDelayParam = false
    //alert stake ratio
    alpha_s = 1.0
    //participating stake ratio
    beta_s = 1.0
    //epoch paramter
    epsilon_s = 0.081775
    // checkpoint depth in slots, k > 192*delta/epsilon*beta useDelayParam = true
    k_s = 23480
    // epoch length, R >= 3k/2f if useDelayParam = true
    epochLength = 86400
    // slot window for chain selection, s = k/4f if useDelayParam = true
    slotWindow = 14400
    //active slot coefficient, f <= 1-exp(1/(delta_s+1))*(1+epsilon_s)/(2.0*alpha_s) if useDelayParam = true
    f_s = 0.2
    //order of accuracy for convergent series
    o_n = 16
    //simulation runtime in slots
    L_s = 2000000
    //number of holders on gossip list for sending new blocks and transactions
    numGossipers = 6
    //use gossiper protocol
    useGossipProtocol = true
    //max number of tries for a tine to ask for parent blocks
    tineMaxTries = 10
    //max depth in multiples of confirmation depth that can be returned from an actor
    tineMaxDepth = 10
    //time out for dropped messages from coordinator, in seconds
    waitTime = 60
    //duration between update tics that stakeholder actors send to themselves, in milliseconds
    updateTime = 10
    //duration between update tics that coordinator and router actors send to themselves, in milliseconds
    commandUpdateTime = 10
    //Issue transactions if true
    transactionFlag = true
    // p = txProbability => chance of issuing transaction per coordinator update
    txProbability = 0.6
    //number of txs per block
    txPerBlock = 3000
    //max number of transactions to be issued over lifetime of simulation
    txMax = 20000000
    //transaction confirmation depth in blocks
    confirmationDepth = 10
    //max initial stake
    initStakeMax = 1.0e9
    //min initial stake
    initStakeMin = 1000.0
    //max random transaction delta
    maxTransfer = 5.0e6
    //reward for forging blocks
    forgerReward = 1000000
    //ratio of transaction amount taken as fee by the forger
    transactionFee = 0.01
    //uses randomness for public key seed and initial stake, set to false for deterministic run
    randomFlag = false
    //use fencing and action based round progression to enforce deterministic runs, set true for deterministic run
    useFencing = false
    //when true, if system cpu load is too high the coordinator will stall to allow stakeholders to catch up
    performanceFlag = false
    //threshold of cpu usage above which coordinator will stall if performanceFlag = true
    systemLoadThreshold = 0.95
    //number of values to average for load threshold
    numAverageLoad = 3
    //print Stakeholder 0 status per slot if true
    printFlag = true
    //print Stakeholder 0 execution time per slot if true
    timingFlag = true
    //Record data if true, plot data points with ./cmd.sh and enter command: plot
    dataOutFlag = false
    //use LSMStore to store block data to disk
    storageFlag = true
    //max number of entries in cache for block and state storage
    cacheSize = 50
    //database refresh interval in slots
    refreshInterval = 1800
    //path for data output files
    dataFileDir = "data"
    //bifrost settings
    settingsFilename = "bootstrap.json"
    //type of genesis stake distribution
    stakeDistribution = "flat"
    //exponential scale factor for stake distribution
    stakeScale = 0.5

