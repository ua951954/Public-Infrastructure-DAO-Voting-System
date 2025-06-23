import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
  name: "Allows citizen registration with sufficient stake",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const wallet_1 = accounts.get("wallet_1")!;

    let block = chain.mineBlock([
      Tx.contractCall("dao-voting", "register-citizen", [], wallet_1.address)
    ]);

    assertEquals(block.receipts[0].result, "(ok true)");
  },
});

Clarinet.test({
  name: "Allows proposal creation by registered citizens",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const wallet_1 = accounts.get("wallet_1")!;

    let block = chain.mineBlock([
      Tx.contractCall("dao-voting", "register-citizen", [], wallet_1.address),
      Tx.contractCall("dao-voting", "create-proposal", [
        types.ascii("Road Repair"),
        types.ascii("Fix Main Street"),
        types.ascii("Infrastructure"),
        types.uint(1000000),
        types.uint(144)
      ], wallet_1.address)
    ]);

    assertEquals(block.receipts[1].result, "(ok u1)");
  },
});

Clarinet.test({
  name: "Allows voting on active proposals",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const wallet_1 = accounts.get("wallet_1")!;
    const wallet_2 = accounts.get("wallet_2")!;

    let block = chain.mineBlock([
      Tx.contractCall("dao-voting", "register-citizen", [], wallet_1.address),
      Tx.contractCall("dao-voting", "create-proposal", [
        types.ascii("Road Repair"),
        types.ascii("Fix Main Street"),
        types.ascii("Infrastructure"),
        types.uint(1000000),
        types.uint(144)
      ], wallet_1.address),
      Tx.contractCall("dao-voting", "vote", [
        types.uint(1),
        types.bool(true)
      ], wallet_2.address)
    ]);

    assertEquals(block.receipts[2].result, "(ok true)");
  },
});
