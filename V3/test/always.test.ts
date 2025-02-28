import { describe, expect, it, beforeAll, afterAll } from '@jest/globals';
import { 
    MeshTx,
    newWallet,
    provider
} from "./offchain";

/*
const seed = [
    'rally',      'tape',     'wrestle',
    'enroll',     'alter',    'orange',
    'isolate',    'genuine',  'lunar',
    'february',   'island',   'curve',
    'jealous',    'stay',     'search',
    'session',    'grid',     'inside',
    'present',    'recall',   'lava',
    'often',      'above',    'rubber',
]
const wallet = newWallet(seed);
*/

const wallet = newWallet();
const networkId = 0; // set to 0 for testnet, 1 for mainnet


describe('E2E AlwaysSucceeds Test', () => {
    console.log("AlwaysSucceeds E2E Test Started");

    // Only use for yaci devkit testing
    beforeAll(async () => {
        const address = wallet.getChangeAddress();
        console.log("Wallet Address:", address);
        await provider.addressTopup(address, "200_000") 
        await provider.addressTopup(address, "5_000")
        await sleep(2);
    }, 100000);

    afterAll(async () => {
        console.log("AlwaysSucceeds E2E Test Finished");
    }); 

    it('Lock Ada', async() => {
        console.log('\n--- Before Lock Ada Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.lockAda(2_000_000n);
        console.log('\n--- After Lock Ada Tx ---');
        console.log({ result });
        await sleep(2);
    }, 100000);

    it('Unlock Ada', async() => {
        console.log('\n--- Before Unlock Ada Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.unlockAda(2_000_000n);
        console.log('\n--- After Unlock Ada Tx ---');
        console.log({ result });
        await sleep(2);
    }, 100000);

}); 

const sleep = (second: number) =>
    new Promise((resolve) => setTimeout(resolve, second * 1000));


