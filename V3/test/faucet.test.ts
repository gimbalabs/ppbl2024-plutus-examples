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


describe('E2E Faucet Test', () => {
    console.log("Faucet E2E Test Started");
    let accessTokenHex: string;
    let accessTokenPolicy: string;
    let faucetTokenHex: string;
    let faucetTokenPolicy: string;
    const withdrawalAmount = 100n;
    const faucetLockedAmount = 1000000n;

    // Only use for yaci devkit testing
    beforeAll(async () => {
        const address = wallet.getChangeAddress();
        console.log("Wallet Address:", address);
        await provider.addressTopup(address, "200_000") 
        await provider.addressTopup(address, "5_000")
        await sleep(2);
    },100000);

    afterAll(async () => {
        console.log("Faucet E2E Test Finished");
    }); 

    it('Mint access token', async() => {
        console.log('\n--- Before Mint Access Token Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.mint("access-token", 1n);
        console.log('\n--- After Mint Access Token Tx ---');
        console.log({ result });
        accessTokenHex = result.tokenNameHex;
        accessTokenPolicy = result.mintTokenPolicy;
        await sleep(2);
    }, 100000);

    it('Mint faucet tokens', async() => {
        console.log('\n--- Before Mint Faucet Token Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.mint("faucet-token", 1000000n);
        console.log('\n--- After Mint Faucet Token Tx ---');
        console.log({ result });
        faucetTokenHex = result.tokenNameHex;
        faucetTokenPolicy = result.mintTokenPolicy;
        await sleep(2);
    }, 100000);

    it('Lock faucet tokens', async() => {
        console.log('\n--- Before Lock Faucet Token Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.lockFaucet(withdrawalAmount, faucetLockedAmount, faucetTokenHex, faucetTokenPolicy, accessTokenPolicy);
        console.log('\n--- After Lock Faucet Token Tx ---');
        console.log({ result });
        await sleep(2);
    }, 100000);
    
    it('Withdrawal Faucet Token', async() => {
        console.log('\n--- Before Withdrawal Faucet Token Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.withdrawalFaucet(withdrawalAmount, faucetTokenHex, faucetTokenPolicy, accessTokenHex, accessTokenPolicy);
        console.log('\n--- After Withdrawal Faucet Token Tx ---');
        console.log({ result });
        await sleep(2);
    }, 100000);

    it('Withdrawal Faucet Token', async() => {
        console.log('\n--- Before Withdrawal Faucet Token Tx ---');
        const tx = new MeshTx(wallet, provider, networkId);
        const result = await tx.withdrawalFaucet(withdrawalAmount, faucetTokenHex, faucetTokenPolicy, accessTokenHex, accessTokenPolicy);
        console.log('\n--- After Withdrawal Faucet Token Tx ---');
        console.log({ result });
        await sleep(2);
    }, 100000);

    it('Check Wallet Balance', async() => {
        const balance = await wallet.getBalance();
        console.log({ balance });
        expect(balance.find(b => b.unit === faucetTokenPolicy + faucetTokenHex)?.quantity).toBe('200');
        expect(balance.find(b => b.unit === accessTokenPolicy + accessTokenHex)?.quantity).toBe('1');
    });

}); 

const sleep = (second: number) =>
    new Promise((resolve) => setTimeout(resolve, second * 1000));


