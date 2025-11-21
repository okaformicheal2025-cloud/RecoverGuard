import { Shield, Wallet } from 'lucide-react';
import { useAuth } from '../hooks/useAuth';
import { shortenAddress } from '../utils/stacks';

export default function Header() {
  const { isConnected, address, connectWallet, disconnectWallet } = useAuth();

  return (
    <header className="bg-gray-800 border-b border-gray-700">
      <div className="container mx-auto px-4 py-4">
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-3">
            <Shield className="w-8 h-8 text-primary-500" />
            <h1 className="text-2xl font-bold text-white">RecoverGuard</h1>
          </div>

          <div>
            {isConnected ? (
              <div className="flex items-center space-x-4">
                <div className="px-4 py-2 bg-gray-700 rounded-lg text-white">
                  {shortenAddress(address)}
                </div>
                <button
                  onClick={disconnectWallet}
                  className="btn-secondary"
                >
                  Disconnect
                </button>
              </div>
            ) : (
              <button
                onClick={connectWallet}
                className="btn-primary flex items-center space-x-2"
              >
                <Wallet className="w-5 h-5" />
                <span>Connect Wallet</span>
              </button>
            )}
          </div>
        </div>
      </div>
    </header>
  );
}
