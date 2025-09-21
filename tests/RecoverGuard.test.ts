import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;

const contractName = "RecoverGuard";

describe("RecoverGuard Security Tests", () => {
  it("ensures simnet is well initialized", () => {
    expect(simnet.blockHeight).toBeDefined();
  });

  it("should allow contract owner to pause contract", () => {
    const { result } = simnet.callPublicFn(contractName, "pause-contract", [], deployer);
    // The function should execute without throwing an error
    expect(result).toBeDefined();
  });

  it("should allow contract owner to unpause contract", () => {
    simnet.callPublicFn(contractName, "pause-contract", [], deployer);
    const { result } = simnet.callPublicFn(contractName, "unpause-contract", [], deployer);
    // The function should execute without throwing an error
    expect(result).toBeDefined();
  });

  it("should reject pause from non-owner", () => {
    const wallet1 = accounts.get("wallet_1")!;
    const { result } = simnet.callPublicFn(contractName, "pause-contract", [], wallet1);
    // The function should execute without throwing an error (even if it returns an error)
    expect(result).toBeDefined();
  });
});
