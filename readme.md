# Prosomo 0.7

## A blockchain testnet client for development of the Topl protocol

Prosomo is a testnet client and research tool that executes the Ouroboros Genesis protocol.  The goal is to have a persistent Proof-of-Stake (PoS) blockchain come to consensus among a system of Akka actors interfaced on a peer to peer network.  Under the conditions guaranteed by PoS consensus, the actors maintain a live and robust ledger of randomly generated transactions.

The simulation can be executed locally or across many nodes.  Local communication is handled by a simplified global network model that adds artificial delay.  Scala and Akka were chosen as a platform so this implementation can be incorporated into Bifrost, Topl’s production blockchain client.  Remote communication is handled by a network controller provided in Scorex 2, an open source blockchain testbed that provides peer connections and dynamic peer synchronization.  All cryptographic and consensus routines are executed by Stakeholder actors that operate with a given genesis block. An account based transaction model is used for the ledger and stake is tracked as a ledger balance.  The simulation has many configuration options and simulations with 1 to ~100 Stakeholders per node can be executed provided enough system RAM.  The default configuration includes a user interface designed to connect to the testnet.

## Key Features
- Uses the Akka actor system for concurrent execution and communication between peers
- Stakeholders are Akka actors that autonomously carry out the protocol, there can be many Stakeholders per node, each one has a separate node view
- A Coordinator actor gives commands to the nodes Stakeholders, calculates the genesis block, and synchronizes all Stakeholders
- A Router actor that collects messages and sends them to the recipient Stakeholder, using the Akka system scheduler if the destination is local and using a dedicated network controller if the destination is in a remote node
- A simple gossip protocol that acts between Stakeholders to efficiently broadcast transactions and blocks over the network
- Confirmed transactions are added to forged blocks giving the forger a reward
- Command scheduling and flexible configuration options that can be modified at run-time
- A user interface that helps users track local files, manage keys, and connect to a bootstrap server to discover peers


## Persistent Testnet

The testnet is live and can be accessed with the latest release.  The distribution contains a jar file that will require an installation of a JVM to run.  The user interface will be configured to connect to the Prosomo testnet.  Once connected, peers will discover your public name along with the public address generated for your account.  This testnet is purely experimental and stake has no value.  A faucet node that with genesis stakeholders will be active for the duration of the testnet.  The forgers will detect new public addresses and issue random transactions to new users that bootstrap to the network.  Once the faucet node has discovered a new peer, transactions will be issued to that peer while they are online.  The public address will accumulate stake and 2 days later that peer will be able to forge blocks.