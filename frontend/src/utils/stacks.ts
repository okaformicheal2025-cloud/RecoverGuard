import { StacksMainnet, StacksTestnet } from '@stacks/network';
import {
  AnchorMode,
  PostConditionMode,
  uintCV,
  listCV,
  principalCV,
  FungibleConditionCode,
  makeStandardSTXPostCondition,
} from '@stacks/transactions';
import { openContractCall } from '@stacks/connect';

// Network configuration
export const NETWORK = new StacksTestnet();
export const CONTRACT_ADDRESS = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM'; // Replace with your deployed contract address
export const CONTRACT_NAME = 'RecoverGuard';

// Helper to create STX post condition
export const createSTXPostCondition = (
  address: string,
  amount: bigint,
  code: FungibleConditionCode = FungibleConditionCode.LessEqual
) => {
  return makeStandardSTXPostCondition(address, code, amount);
};

// Contract call wrapper
export const callContract = async (
  functionName: string,
  functionArgs: any[],
  userAddress: string,
  postConditions: any[] = []
) => {
  const options = {
    network: NETWORK,
    contractAddress: CONTRACT_ADDRESS,
    contractName: CONTRACT_NAME,
    functionName,
    functionArgs,
    postConditions,
    postConditionMode: PostConditionMode.Deny,
    anchorMode: AnchorMode.Any,
    onFinish: (data: any) => {
      console.log('Transaction submitted:', data);
      return data;
    },
    onCancel: () => {
      console.log('Transaction cancelled');
    },
  };

  try {
    await openContractCall(options);
  } catch (error) {
    console.error('Contract call error:', error);
    throw error;
  }
};

// Format STX amount (microSTX to STX)
export const formatSTX = (microSTX: bigint): string => {
  return (Number(microSTX) / 1_000_000).toFixed(6);
};

// Parse STX amount (STX to microSTX)
export const parseSTX = (stx: string): bigint => {
  return BigInt(Math.floor(parseFloat(stx) * 1_000_000));
};

// Shorten address for display
export const shortenAddress = (address: string): string => {
  if (!address) return '';
  return `${address.slice(0, 6)}...${address.slice(-4)}`;
};

// Error messages mapping
export const ERROR_MESSAGES: { [key: number]: string } = {
  100: 'Unauthorized access',
  101: 'Vault not found',
  102: 'Guardian already exists',
  103: 'Guardian not found',
  104: 'Invalid guardian count',
  105: 'Recovery not initiated',
  106: 'Recovery already initiated',
  107: 'Insufficient approvals',
  108: 'Recovery period not ended',
  109: 'Recovery expired',
  110: 'Cannot be own guardian',
  111: 'Invalid amount',
  112: 'Insufficient balance',
  113: 'Contract paused',
  114: 'Rate limit exceeded',
  115: 'Duplicate guardian',
  116: 'Max deposit exceeded',
  117: 'Reentrancy detected',
  118: 'Invalid new owner',
  119: 'Guardian already approved',
  120: 'Vault deletion locked',
  121: 'Emergency transfer cooldown',
  122: 'Invalid guardian address',
  123: 'Token transfer failed',
};

export const getErrorMessage = (errorCode: number): string => {
  return ERROR_MESSAGES[errorCode] || `Unknown error (${errorCode})`;
};
