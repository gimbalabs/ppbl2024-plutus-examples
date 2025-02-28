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


describe('Secret Number E2E Test', () => {
    console.log("Secret Number E2E Test Started");
    let accessTokenHex: string;
    let accessTokenPolicy: string;


    // Only use for yaci devkit testing
    beforeAll(async () => {
        const address = wallet.getChangeAddress();
        console.log("Wallet Address:", address);
        await provider.addressTopup(address, "200_000") 
        await provider.addressTopup(address, "5_000")
        await sleep(2);
    }, 100000);

    afterAll(async () => {
        console.log("Secret Number E2E Test Finished");
    }); 

    it('Mint access token', async() => {
        console.log('\n--- Before Mint Access Token Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.mintSecretNumber("access-token", 1n);
        console.log('\n--- After Mint Access Token Tx ---');
        console.log({ result });
        accessTokenHex = result.tokenNameHex;
        accessTokenPolicy = result.mintTokenPolicy;
        await sleep(2);
    }, 100000);


    it('Check Wallet Balance', async() => {
        const balance = await wallet.getBalance();
        console.log({ balance });
        expect(balance.find(b => b.unit === accessTokenPolicy + accessTokenHex)?.quantity).toBe('1');
    }, 100000);

}); 

const sleep = (second: number) =>
    new Promise((resolve) => setTimeout(resolve, second * 1000));


