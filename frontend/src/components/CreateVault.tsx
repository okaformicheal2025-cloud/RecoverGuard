import { useState } from 'react';
import { Plus, X } from 'lucide-react';
import { useAuth } from '../hooks/useAuth';
import { callContract, parseSTX } from '../utils/stacks';
import { uintCV, listCV, principalCV } from '@stacks/transactions';

export default function CreateVault() {
  const { address } = useAuth();
  const [guardians, setGuardians] = useState<string[]>(['', '', '']);
  const [requiredApprovals, setRequiredApprovals] = useState(2);
  const [isLoading, setIsLoading] = useState(false);

  const addGuardian = () => {
    if (guardians.length < 5) {
      setGuardians([...guardians, '']);
    }
  };

  const removeGuardian = (index: number) => {
    if (guardians.length > 3) {
      setGuardians(guardians.filter((_, i) => i !== index));
    }
  };

  const updateGuardian = (index: number, value: string) => {
    const newGuardians = [...guardians];
    newGuardians[index] = value;
    setGuardians(newGuardians);
  };

  const handleCreateVault = async () => {
    if (!address) return;

    const validGuardians = guardians.filter(g => g.trim() !== '');
    if (validGuardians.length < 3) {
      alert('Please add at least 3 guardians');
      return;
    }

    setIsLoading(true);
    try {
      const guardianPrincipals = validGuardians.map(g => principalCV(g));
      await callContract(
        'create-vault',
        [listCV(guardianPrincipals), uintCV(requiredApprovals)],
        address
      );
      alert('Vault created successfully!');
    } catch (error) {
      console.error('Error creating vault:', error);
      alert('Failed to create vault');
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="card max-w-2xl mx-auto">
      <h2 className="text-2xl font-bold mb-6 text-gray-900 dark:text-white">Create New Vault</h2>
      
      <div className="space-y-6">
        <div>
          <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
            Guardians (3-5 required)
          </label>
          {guardians.map((guardian, index) => (
            <div key={index} className="flex items-center space-x-2 mb-2">
              <input
                type="text"
                value={guardian}
                onChange={(e) => updateGuardian(index, e.target.value)}
                placeholder="ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM"
                className="input-field flex-1"
              />
              {guardians.length > 3 && (
                <button
                  onClick={() => removeGuardian(index)}
                  className="p-2 text-red-500 hover:bg-red-50 dark:hover:bg-red-900 rounded"
                >
                  <X className="w-5 h-5" />
                </button>
              )}
            </div>
          ))}
          {guardians.length < 5 && (
            <button
              onClick={addGuardian}
              className="mt-2 flex items-center space-x-2 text-primary-600 hover:text-primary-700"
            >
              <Plus className="w-5 h-5" />
              <span>Add Guardian</span>
            </button>
          )}
        </div>

        <div>
          <label className="block text-sm font-medium mb-2 text-gray-700 dark:text-gray-300">
            Required Approvals: {requiredApprovals}
          </label>
          <input
            type="range"
            min={Math.ceil(guardians.filter(g => g).length / 2)}
            max={guardians.filter(g => g).length}
            value={requiredApprovals}
            onChange={(e) => setRequiredApprovals(parseInt(e.target.value))}
            className="w-full"
          />
          <p className="text-sm text-gray-500 mt-1">
            At least {requiredApprovals} guardians must approve recovery
          </p>
        </div>

        <button
          onClick={handleCreateVault}
          disabled={isLoading}
          className="btn-primary w-full"
        >
          {isLoading ? 'Creating...' : 'Create Vault'}
        </button>
      </div>
    </div>
  );
}
