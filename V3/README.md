# Cardano Plinth (PlutusTx) Examples
### Sections
- Setup
- Compile Plinth
- Running Local Devnet
- Execute Test Cases
- Profiling a Smart Contract


## Setup
Install docker desktop for either Mac, Windows or Linux https://docs.docker.com/desktop/

Run the docker desktop appication to confirm that docker engine is able to start
![image](https://github.com/user-attachments/assets/54aa5ab4-aa1f-453a-9a60-3d07a0b7a711)

In a terminal window on the host machine
```
$ mkdir ~/src
$ cd ~/src
$ git clone https://github.com/gimbalabs/ppbl2024-plutus-examples.git
$ cd ppbl2024-plutus-examples/V3
$ docker run \
  -v /absolute-path-to-src-directory/src/ppbl2024-plutus-examples/V3:/workspaces/ppbl2024-plutus-examples/V3  \
  -it ghcr.io/input-output-hk/devx-devcontainer:x86_64-linux.ghc96-iog
```
## Compiling Haskell Files
Inside the container run the following commands
```
[workspaces] cd ppbl2024-plutus-examples/V3
[workspaces/ppbl2024-plutus-examples/V3] cabal update
[workspaces/ppbl2024-plutus-examples/V3] cabal build
```
## Generating Blueprints
The cabal build will take a while and once it is completed, you should be able to execute the following commands to generate the blueprint files.
```
[workspaces/ppbl2024-plutus-examples/V3] cabal run gen-faucet-validator-blueprint -- ./off-chain/faucet-validator-blueprint.json
```

## Running a local Cardano devnet
In new a terminal window, download the yaci-devkit Github repo
```
$ cd ~/src
$ git clone https://github.com/bloxbean/yaci-devkit.git
$ cd yaci-devkit
$ git checkout v0.10.0-preview5
```

Now run the following commands to start up the yaci devkit
```
$ ./bin/devkit.sh start
yaci-cli:>create-node -o --era conway
devnet:default>start
```
This will start up a local cardano node and devnet.  Wait for the ```Yaci Store Started``` to appear on the terminal


You can find out the API and URLs by using the ```info``` command
```
devnet:default>info

###### Node Details (Container) ######
[💡 Node port] 3001
[💡 Node Socket Paths] 
/clusters/nodes/default/node/node.sock
[💡 Submit Api Port] 8090
[💡 Protocol Magic] 42
[💡 Block Time] 1.0 sec
[💡 Slot Length] 1.0 sec
[💡 Start Time] 1738176728
[💡 Epoch Length] 600
[💡 Security Param] 300
[💡 SlotsPerKESPeriod] 129600


#################### URLS (Host) ####################
[💡 Yaci Viewer] http://localhost:5173
[💡 Yaci Store Swagger UI] http://localhost:8080/swagger-ui.html
[💡 Yaci Store Api URL] http://localhost:8080/api/v1/
[💡 Pool Id] pool1wvqhvyrgwch4jq9aa84hc8q4kzvyq2z3xr6mpafkqmx9wce39zy


#################### Other URLS ####################
[💡 Ogmios Url (Optional)] ws://localhost:1337
[💡 Kupo Url   (Optional)] http://localhost:1442


#################### Node Ports ####################
[💡 n2n port] localhost:3001
[💡 n2c port (socat)] localhost:3333
```

## Test the contracts
Launch a new terminal window on your host machine
```
$ cd ~/src
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
```
Close and then reopen a new terminal window
```
$ nvm install v22.13.0
$ cd ppbl2024-plutus-examples/V3/test
$ npm install
```

Notes: There will be some deprecated and security warnings that you can ignore for now since this only used for testing purposes.

Now run the faucet end-to-end integration test cases using the yaci devkit local devnet.
```
$ npm run test-faucet
```

The tests should run successfully and you should see the following output
```
 PASS  test/faucet.test.ts (30.199 s)
  E2E Faucet Test
    ✓ Mint access token (3510 ms)
    ✓ Mint faucet tokens (3104 ms)
    ✓ Lock faucet tokens (3182 ms)
    ✓ Withdrawal Faucet Token (3175 ms)
    ✓ Withdrawal Faucet Token (3070 ms)
    ✓ Check Wallet Balance (18 ms)

Test Suites: 1 passed, 1 total
Tests:       6 passed, 6 total
Snapshots:   0 total
Time:        30.607 s
Ran all test suites.
```

In the terminal window runnign yaci devkit, you can query the utxos at and address using the following command
```
devnet:default>utxos addr_test1qz3y5kacdctuxzczarxu2t3c9jswawm4xtp3n26t6l9qelyh40hlltg24lyeuw9mk3e6p7fs58sv852zmp5suuk85s9qmzzvlm
1. 4ce6cf1d49c1108c980151e0ab793ecedab297a90aeeba478e61eb3a131e0c47#1 : [Amount(unit=lovelace, quantity=2000000), Amount(unit=b10b3a5a819392a156d0190ba4a5c34f1706bad47c8ed404dfe193e16163636573732d746f6b656e, quantity=1), Amount(unit=bb07253073ca06fb0b2704c41ad26a869ec303dd12f39d4409639a366661756365742d746f6b656e, quantity=100)]
--------------------------------------------------------------------------------------
2. 4ce6cf1d49c1108c980151e0ab793ecedab297a90aeeba478e61eb3a131e0c47#2 : [Amount(unit=lovelace, quantity=5000000)]
```

You can also go to the Yaci viewer http://localhost:5173/ and view the transactions as well.

![image](https://github.com/user-attachments/assets/871ee952-9945-4d79-9ad3-ad569252a911)

![image](https://github.com/user-attachments/assets/9c6c96e5-ae4b-4a92-8a81-54c97d47387f)


## Profiling the contract
In a new terminal window
```
$ cd ~/src/ppbl2024-plutus-examples/V3
$ sudo apt update
$ sudo apt install wget
$ sudo apt install jq
$ wget https://github.com/IntersectMBO/plutus/releases/download/1.40.0.0/uplc-x86_64-linux-ghc96
$ chmod u+x uplc-x86_64-linux-ghc96
$ cat off-chain/faucet-validator-blueprint.json | jq -jr .validators[0].compiledCode > faucet.cbor
```

Convert the cbor into a flat hex format using the following website
- https://cbor.nemo157.com/
- copy and paste the contents for faucet.cbor
- unselect the annotate button on the top right side
- select and copy the flat hex content on the right hand side

![image](https://github.com/user-attachments/assets/54e9bf03-e5ff-4c32-89d7-2f2fc07a908e)
- create a new file called ```faucet.hex``` and paste the flat hex contents into it

Now issue the following commands
```
$ xxd -r -p faucet.hex faucet.flat
$ ./uplc-x86_64-linux-ghc96 convert -i faucet.flat --if flat > faucet.uplc
$ ./uplc-x86_64-linux-ghc96 evaluate -t -i faucet.flat --if flat
CPU budget:    4544100
Memory budget: 28500

Const                32000              200
Var                 880000             5500
LamAbs             1920000            12000
Apply              1488000             9300
Delay               192000             1200
Force                16000              100
Builtin                  0                0
Constr               16000              100
Case                     0                0

startup                100              100
compute            4544000            28400
AST nodes             4911



Total builtin costs:                 0                0
Time spent executing builtins:  0.00%

Total budget spent:            4544100            28500
Predicted execution time: 4.544 μs
```

## Confirming the protocol parameters
```
$ docker ps
CONTAINER ID   IMAGE                                                              COMMAND                  CREATED      STATUS        PORTS                                                                                                                                                                      NAMES
c619fbc1c318   ghcr.io/input-output-hk/devx-devcontainer:x86_64-linux.ghc96-iog   "/bin/bash"              7 days ago   Up 12 hours                                                                                                                                                                              gallant_varahamihira
af9e019fe37a   bloxbean/yaci-cli:0.10.0-preview5                                  "sleep infinity"         9 days ago   Up 12 hours   0.0.0.0:1337->1337/tcp, 0.0.0.0:1442->1442/tcp, 0.0.0.0:3001->3001/tcp, 0.0.0.0:3333->3333/tcp, 0.0.0.0:8080->8080/tcp, 0.0.0.0:8090->8090/tcp, 0.0.0.0:10000->10000/tcp   node1-yaci-cli-1
b74ba32968d4   bloxbean/yaci-viewer:0.10.0-preview5                               "docker-entrypoint.s…"   9 days ago   Up 12 hours   0.0.0.0:5173->5173/tcp                                                                                                                                                     node1-yaci-viewer-1
$ docker exec -it node1-yaci-cli-1 bash
# cardano-cli query protocol-parameters --testnet-magic 42

...
    "maxTxExecutionUnits": {
        "memory": 14000000,
        "steps": 10000000000
    },
    "maxTxSize": 16384,

```

