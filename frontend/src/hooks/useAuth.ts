import { useState, useEffect } from 'react';
import { AppConfig, UserSession, showConnect } from '@stacks/connect';

const appConfig = new AppConfig(['store_write', 'publish_data']);
const userSession = new UserSession({ appConfig });

export const useAuth = () => {
  const [userData, setUserData] = useState<any>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    if (userSession.isSignInPending()) {
      userSession.handlePendingSignIn().then((userData) => {
        setUserData(userData);
        setIsLoading(false);
      });
    } else if (userSession.isUserSignedIn()) {
      setUserData(userSession.loadUserData());
      setIsLoading(false);
    } else {
      setIsLoading(false);
    }
  }, []);

  const connectWallet = () => {
    showConnect({
      appDetails: {
        name: 'RecoverGuard',
        icon: window.location.origin + '/logo.png',
      },
      redirectTo: '/',
      onFinish: () => {
        setUserData(userSession.loadUserData());
      },
      userSession,
    });
  };

  const disconnectWallet = () => {
    userSession.signUserOut();
    setUserData(null);
  };

  return {
    userData,
    isLoading,
    isConnected: !!userData,
    address: userData?.profile?.stxAddress?.testnet || userData?.profile?.stxAddress?.mainnet,
    connectWallet,
    disconnectWallet,
  };
};
