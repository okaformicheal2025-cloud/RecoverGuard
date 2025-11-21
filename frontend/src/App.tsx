import { useState } from 'react';
import { Shield, Users, Lock, AlertTriangle } from 'lucide-react';
import { useAuth } from './hooks/useAuth';
import Header from './components/Header';
import CreateVault from './components/CreateVault';
import VaultDashboard from './components/VaultDashboard';
import RecoveryManager from './components/RecoveryManager';

function App() {
  const { isConnected, isLoading } = useAuth();
  const [activeTab, setActiveTab] = useState<'dashboard' | 'create' | 'recovery'>('dashboard');

  if (isLoading) {
    return (
      <div className="min-h-screen bg-gradient-to-br from-gray-900 via-gray-800 to-gray-900 flex items-center justify-center">
        <div className="text-white text-xl">Loading...</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-900 via-gray-800 to-gray-900">
      <Header />
      
      <main className="container mx-auto px-4 py-8">
        {!isConnected ? (
          <div className="max-w-4xl mx-auto">
            <div className="card text-center py-16">
              <Shield className="w-24 h-24 mx-auto mb-6 text-primary-500" />
              <h1 className="text-4xl font-bold mb-4 text-gray-900 dark:text-white">
                Welcome to RecoverGuard
              </h1>
              <p className="text-xl text-gray-600 dark:text-gray-300 mb-8">
                Secure your digital assets with social recovery
              </p>
              <div className="grid md:grid-cols-3 gap-6 mt-12 text-left">
                <div className="p-6 bg-gray-50 dark:bg-gray-700 rounded-lg">
                  <Users className="w-12 h-12 text-primary-500 mb-4" />
                  <h3 className="text-lg font-semibold mb-2 text-gray-900 dark:text-white">
                    Trusted Guardians
                  </h3>
                  <p className="text-gray-600 dark:text-gray-300">
                    Select 3-5 trusted individuals to help recover your vault
                  </p>
                </div>
                <div className="p-6 bg-gray-50 dark:bg-gray-700 rounded-lg">
                  <Lock className="w-12 h-12 text-primary-500 mb-4" />
                  <h3 className="text-lg font-semibold mb-2 text-gray-900 dark:text-white">
                    Secure Storage
                  </h3>
                  <p className="text-gray-600 dark:text-gray-300">
                    Store STX and SIP-010 tokens safely in your vault
                  </p>
                </div>
                <div className="p-6 bg-gray-50 dark:bg-gray-700 rounded-lg">
                  <AlertTriangle className="w-12 h-12 text-primary-500 mb-4" />
                  <h3 className="text-lg font-semibold mb-2 text-gray-900 dark:text-white">
                    Emergency Recovery
                  </h3>
                  <p className="text-gray-600 dark:text-gray-300">
                    Recover your assets with guardian approval if needed
                  </p>
                </div>
              </div>
            </div>
          </div>
        ) : (
          <>
            <div className="flex justify-center mb-8">
              <div className="inline-flex rounded-lg border border-gray-700 p-1 bg-gray-800">
                <button
                  onClick={() => setActiveTab('dashboard')}
                  className={`px-6 py-2 rounded-md transition-colors ${
                    activeTab === 'dashboard'
                      ? 'bg-primary-600 text-white'
                      : 'text-gray-300 hover:text-white'
                  }`}
                >
                  Dashboard
                </button>
                <button
                  onClick={() => setActiveTab('create')}
                  className={`px-6 py-2 rounded-md transition-colors ${
                    activeTab === 'create'
                      ? 'bg-primary-600 text-white'
                      : 'text-gray-300 hover:text-white'
                  }`}
                >
                  Create Vault
                </button>
                <button
                  onClick={() => setActiveTab('recovery')}
                  className={`px-6 py-2 rounded-md transition-colors ${
                    activeTab === 'recovery'
                      ? 'bg-primary-600 text-white'
                      : 'text-gray-300 hover:text-white'
                  }`}
                >
                  Recovery
                </button>
              </div>
            </div>

            <div className="max-w-6xl mx-auto">
              {activeTab === 'dashboard' && <VaultDashboard />}
              {activeTab === 'create' && <CreateVault />}
              {activeTab === 'recovery' && <RecoveryManager />}
            </div>
          </>
        )}
      </main>

      <footer className="mt-16 py-8 border-t border-gray-700">
        <div className="container mx-auto px-4 text-center text-gray-400">
          <p>© 2024 RecoverGuard. Secure your digital future.</p>
        </div>
      </footer>
    </div>
  );
}

export default App;
