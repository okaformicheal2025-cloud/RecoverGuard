import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;
const wallet3 = accounts.get("wallet_3")!;
const wallet4 = accounts.get("wallet_4")!;
const wallet5 = accounts.get("wallet_5")!;

const contractName = "RecoverGuard";

describe("RecoverGuard - Comprehensive Test Suite", () => {
  
  describe("Contract Initialization", () => {
    it("ensures simnet is well initialized", () => {
      expect(simnet.blockHeight).toBeDefined();
    });

    it("should have correct initial state", () => {
      const paused = simnet.callReadOnlyFn(contractName, "is-contract-paused", [], deployer);
      expect(paused.result).toStrictEqual(Cl.bool(false));
      
      const emergencyEnabled = simnet.callReadOnlyFn(contractName, "is-emergency-transfer-enabled", [], deployer);
      expect(emergencyEnabled.result).toStrictEqual(Cl.bool(false));
    });
  });

  describe("Pause/Unpause Functionality", () => {
    it("should allow contract owner to pause contract", () => {
      const { result } = simnet.callPublicFn(contractName, "pause-contract", [], deployer);
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
      
      const paused = simnet.callReadOnlyFn(contractName, "is-contract-paused", [], deployer);
      expect(paused.result).toStrictEqual(Cl.bool(true));
    });

    it("should allow contract owner to unpause contract", () => {
      simnet.callPublicFn(contractName, "pause-contract", [], deployer);
      const { result } = simnet.callPublicFn(contractName, "unpause-contract", [], deployer);
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
      
      const paused = simnet.callReadOnlyFn(contractName, "is-contract-paused", [], deployer);
      expect(paused.result).toStrictEqual(Cl.bool(false));
    });

    it("should reject pause from non-owner", () => {
      const { result } = simnet.callPublicFn(contractName, "pause-contract", [], wallet1);
      expect(result).toStrictEqual(Cl.error(Cl.uint(100))); // ERR_UNAUTHORIZED
    });

    it("should reject unpause from non-owner", () => {
      simnet.callPublicFn(contractName, "pause-contract", [], deployer);
      const { result } = simnet.callPublicFn(contractName, "unpause-contract", [], wallet1);
      expect(result).toStrictEqual(Cl.error(Cl.uint(100))); // ERR_UNAUTHORIZED
    });
  });

  describe("Vault Creation", () => {
    it("should create vault with valid guardians", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
      
      // Verify vault was created
      const vaultInfo = simnet.callReadOnlyFn(contractName, "get-vault-info", [Cl.principal(wallet1)], wallet1);
      expect(vaultInfo.result).not.toStrictEqual(Cl.none());
    });

    it("should reject vault with insufficient guardians", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3)]);
      const requiredApprovals = Cl.uint(1);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      
      expect(result).toStrictEqual(Cl.error(Cl.uint(104))); // ERR_INVALID_GUARDIAN_COUNT
    });

    it("should reject vault with too many guardians", () => {
      const guardians = Cl.list([
        Cl.principal(wallet2),
        Cl.principal(wallet3),
        Cl.principal(wallet4),
        Cl.principal(wallet5),
        Cl.principal(deployer),
        Cl.principal(accounts.get("wallet_6")!)
      ]);
      const requiredApprovals = Cl.uint(3);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      
      expect(result).toStrictEqual(Cl.error(Cl.uint(104))); // ERR_INVALID_GUARDIAN_COUNT
    });

    it("should reject vault with duplicate guardians", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet2), Cl.principal(wallet3)]);
      const requiredApprovals = Cl.uint(2);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      
      expect(result).toStrictEqual(Cl.error(Cl.uint(115))); // ERR_DUPLICATE_GUARDIAN
    });

    it("should reject vault with owner as guardian", () => {
      const guardians = Cl.list([Cl.principal(wallet1), Cl.principal(wallet2), Cl.principal(wallet3)]);
      const requiredApprovals = Cl.uint(2);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      
      expect(result).toStrictEqual(Cl.error(Cl.uint(110))); // ERR_CANNOT_BE_OWN_GUARDIAN
    });

    it("should reject vault with insufficient required approvals", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(1); // Less than half
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      
      expect(result).toStrictEqual(Cl.error(Cl.uint(104))); // ERR_INVALID_GUARDIAN_COUNT
    });

    it("should reject creating duplicate vault", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      
      expect(result).toBe(Cl.error(Cl.uint(102))); // ERR_GUARDIAN_ALREADY_EXISTS
    });
  });

  describe("STX Deposit and Withdrawal", () => {
    beforeEach(() => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
    });

    it("should allow depositing STX", () => {
      const amount = Cl.uint(1000000); // 1 STX
      const { result } = simnet.callPublicFn(contractName, "deposit-stx", [amount], wallet1);
      expect(result).toStrictEqual(Cl.ok(amount));
      
      const vaultInfo = simnet.callReadOnlyFn(contractName, "get-vault-info", [Cl.principal(wallet1)], wallet1);
      expect(vaultInfo.result).not.toStrictEqual(Cl.none());
    });

    it("should reject depositing zero STX", () => {
      const amount = Cl.uint(0);
      const { result } = simnet.callPublicFn(contractName, "deposit-stx", [amount], wallet1);
      expect(result).toStrictEqual(Cl.error(Cl.uint(111))); // ERR_INVALID_AMOUNT
    });

    it("should reject deposit exceeding max amount", () => {
      const amount = Cl.uint(1000000000001); // Over 1M STX
      const { result } = simnet.callPublicFn(contractName, "deposit-stx", [amount], wallet1);
      expect(result).toBe(Cl.error(Cl.uint(116))); // ERR_MAX_DEPOSIT_EXCEEDED
    });

    it("should allow withdrawing STX", () => {
      const depositAmount = Cl.uint(1000000);
      simnet.callPublicFn(contractName, "deposit-stx", [depositAmount], wallet1);
      
      const withdrawAmount = Cl.uint(500000);
      const { result } = simnet.callPublicFn(contractName, "withdraw-stx", [withdrawAmount], wallet1);
      expect(result).toBe(Cl.ok(withdrawAmount));
    });

    it("should reject withdrawing more than balance", () => {
      const depositAmount = Cl.uint(1000000);
      simnet.callPublicFn(contractName, "deposit-stx", [depositAmount], wallet1);
      
      const withdrawAmount = Cl.uint(2000000);
      const { result } = simnet.callPublicFn(contractName, "withdraw-stx", [withdrawAmount], wallet1);
      expect(result).toBe(Cl.error(Cl.uint(112))); // ERR_INSUFFICIENT_BALANCE
    });

    it("should reject withdrawal from non-owner", () => {
      const depositAmount = Cl.uint(1000000);
      simnet.callPublicFn(contractName, "deposit-stx", [depositAmount], wallet1);
      
      const withdrawAmount = Cl.uint(500000);
      const { result } = simnet.callPublicFn(contractName, "withdraw-stx", [withdrawAmount], wallet2);
      expect(result).toBe(Cl.error(Cl.uint(101))); // ERR_VAULT_NOT_FOUND
    });
  });

  describe("Recovery Process", () => {
    beforeEach(() => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
    });

    it("should allow guardian to initiate recovery", () => {
      const newOwner = Cl.principal(wallet5);
      const { result } = simnet.callPublicFn(
        contractName,
        "initiate-recovery",
        [Cl.principal(wallet1), newOwner],
        wallet2
      );
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
      
      const recoveryInfo = simnet.callReadOnlyFn(
        contractName,
        "get-recovery-info",
        [Cl.principal(wallet1)],
        wallet1
      );
      expect(recoveryInfo.result).not.toBe(Cl.none());
    });

    it("should reject recovery initiation from non-guardian", () => {
      const newOwner = Cl.principal(wallet5);
      const { result } = simnet.callPublicFn(
        contractName,
        "initiate-recovery",
        [Cl.principal(wallet1), newOwner],
        wallet5
      );
      expect(result).toStrictEqual(Cl.error(Cl.uint(100))); // ERR_UNAUTHORIZED
    });

    it("should allow guardians to approve recovery", () => {
      const newOwner = Cl.principal(wallet5);
      simnet.callPublicFn(
        contractName,
        "initiate-recovery",
        [Cl.principal(wallet1), newOwner],
        wallet2
      );
      
      const { result } = simnet.callPublicFn(
        contractName,
        "approve-recovery",
        [Cl.principal(wallet1)],
        wallet3
      );
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
    });

    it("should reject duplicate approval from same guardian", () => {
      const newOwner = Cl.principal(wallet5);
      simnet.callPublicFn(
        contractName,
        "initiate-recovery",
        [Cl.principal(wallet1), newOwner],
        wallet2
      );
      
      simnet.callPublicFn(contractName, "approve-recovery", [Cl.principal(wallet1)], wallet3);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "approve-recovery",
        [Cl.principal(wallet1)],
        wallet3
      );
      expect(result).toBe(Cl.error(Cl.uint(119))); // ERR_GUARDIAN_ALREADY_APPROVED
    });

    it("should allow owner to cancel recovery within cancellation period", () => {
      const newOwner = Cl.principal(wallet5);
      simnet.callPublicFn(
        contractName,
        "initiate-recovery",
        [Cl.principal(wallet1), newOwner],
        wallet2
      );
      
      const { result } = simnet.callPublicFn(contractName, "cancel-recovery", [], wallet1);
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
    });

    it("should execute recovery after delay with sufficient approvals", () => {
      const newOwner = Cl.principal(wallet5);
      const depositAmount = Cl.uint(1000000);
      
      // Deposit some STX first
      simnet.callPublicFn(contractName, "deposit-stx", [depositAmount], wallet1);
      
      // Initiate recovery
      simnet.callPublicFn(
        contractName,
        "initiate-recovery",
        [Cl.principal(wallet1), newOwner],
        wallet2
      );
      
      // Get approvals
      simnet.callPublicFn(contractName, "approve-recovery", [Cl.principal(wallet1)], wallet3);
      
      // Mine blocks to pass delay
      simnet.mineEmptyBlocks(145);
      
      // Execute recovery
      const { result } = simnet.callPublicFn(
        contractName,
        "execute-recovery",
        [Cl.principal(wallet1)],
        wallet2
      );
      expect(result).toStrictEqual(Cl.ok(Cl.principal(wallet5)));
    });
  });

  describe("Guardian Management", () => {
    beforeEach(() => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
    });

    it("should allow owner to update guardians", () => {
      const newGuardians = Cl.list([Cl.principal(wallet3), Cl.principal(wallet4), Cl.principal(wallet5)]);
      const requiredApprovals = Cl.uint(2);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "update-guardians",
        [newGuardians, requiredApprovals],
        wallet1
      );
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
    });

    it("should reject guardian update from non-owner", () => {
      const newGuardians = Cl.list([Cl.principal(wallet3), Cl.principal(wallet4), Cl.principal(wallet5)]);
      const requiredApprovals = Cl.uint(2);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "update-guardians",
        [newGuardians, requiredApprovals],
        wallet2
      );
      expect(result).toStrictEqual(Cl.error(Cl.uint(101))); // ERR_VAULT_NOT_FOUND
    });
  });

  describe("Emergency Functions", () => {
    it("should allow contract owner to enable emergency transfer", () => {
      const { result } = simnet.callPublicFn(
        contractName,
        "set-emergency-transfer",
        [Cl.bool(true)],
        deployer
      );
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
      
      const enabled = simnet.callReadOnlyFn(contractName, "is-emergency-transfer-enabled", [], deployer);
      expect(enabled.result).toStrictEqual(Cl.bool(true));
    });

    it("should reject emergency transfer enable from non-owner", () => {
      const { result } = simnet.callPublicFn(
        contractName,
        "set-emergency-transfer",
        [Cl.bool(true)],
        wallet1
      );
      expect(result).toStrictEqual(Cl.error(Cl.uint(100))); // ERR_UNAUTHORIZED
    });
  });

  describe("Vault Deletion", () => {
    beforeEach(() => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
    });

    it("should allow owner to delete empty vault", () => {
      const { result } = simnet.callPublicFn(contractName, "delete-vault", [], wallet1);
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
      
      const vaultInfo = simnet.callReadOnlyFn(contractName, "get-vault-info", [Cl.principal(wallet1)], wallet1);
      expect(vaultInfo.result).toStrictEqual(Cl.none());
    });

    it("should reject deleting vault with balance", () => {
      const depositAmount = Cl.uint(1000000);
      simnet.callPublicFn(contractName, "deposit-stx", [depositAmount], wallet1);
      
      const { result } = simnet.callPublicFn(contractName, "delete-vault", [], wallet1);
      expect(result).toStrictEqual(Cl.error(Cl.uint(112))); // ERR_INSUFFICIENT_BALANCE
    });

    it("should allow owner to lock/unlock deletion", () => {
      const { result } = simnet.callPublicFn(
        contractName,
        "set-deletion-lock",
        [Cl.bool(true)],
        wallet1
      );
      expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
      
      const deleteResult = simnet.callPublicFn(contractName, "delete-vault", [], wallet1);
      expect(deleteResult.result).toStrictEqual(Cl.error(Cl.uint(120))); // ERR_VAULT_DELETION_LOCKED
    });
  });

  describe("Read-Only Functions", () => {
    it("should return vault info correctly", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
      
      const vaultInfo = simnet.callReadOnlyFn(contractName, "get-vault-info", [Cl.principal(wallet1)], wallet1);
      expect(vaultInfo.result).not.toStrictEqual(Cl.none());
    });

    it("should return none for non-existent vault", () => {
      const vaultInfo = simnet.callReadOnlyFn(contractName, "get-vault-info", [Cl.principal(wallet1)], wallet1);
      expect(vaultInfo.result).toStrictEqual(Cl.none());
    });

    it("should check if address is vault owner", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
      
      const isOwner = simnet.callReadOnlyFn(contractName, "is-vault-owner", [Cl.principal(wallet1)], wallet1);
      expect(isOwner.result).toStrictEqual(Cl.bool(true));
      
      const isNotOwner = simnet.callReadOnlyFn(contractName, "is-vault-owner", [Cl.principal(wallet2)], wallet1);
      expect(isNotOwner.result).toStrictEqual(Cl.bool(false));
    });

    it("should return vault metadata", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
      
      const metadata = simnet.callReadOnlyFn(contractName, "get-vault-metadata", [Cl.principal(wallet1)], wallet1);
      expect(metadata.result).not.toBe(Cl.none());
    });
  });

  describe("Security and Edge Cases", () => {
    it("should reject operations when contract is paused", () => {
      simnet.callPublicFn(contractName, "pause-contract", [], deployer);
      
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "create-vault",
        [guardians, requiredApprovals],
        wallet1
      );
      expect(result).toStrictEqual(Cl.error(Cl.uint(113))); // ERR_CONTRACT_PAUSED
    });

    it("should handle recovery expiry correctly", () => {
      const guardians = Cl.list([Cl.principal(wallet2), Cl.principal(wallet3), Cl.principal(wallet4)]);
      const requiredApprovals = Cl.uint(2);
      simnet.callPublicFn(contractName, "create-vault", [guardians, requiredApprovals], wallet1);
      
      const newOwner = Cl.principal(wallet5);
      simnet.callPublicFn(
        contractName,
        "initiate-recovery",
        [Cl.principal(wallet1), newOwner],
        wallet2
      );
      
      simnet.callPublicFn(contractName, "approve-recovery", [Cl.principal(wallet1)], wallet3);
      
      // Mine blocks beyond expiry
      simnet.mineEmptyBlocks(1009);
      
      const { result } = simnet.callPublicFn(
        contractName,
        "execute-recovery",
        [Cl.principal(wallet1)],
        wallet2
      );
      expect(result).toStrictEqual(Cl.error(Cl.uint(107))); // ERR_INSUFFICIENT_APPROVALS (only 1 approval, need 2)
    });
  });
});
