{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Pos.Crypto.Signing.Safe (emptyPassphrase)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.BackupPhrase (BackupPhrase (..))
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Backup (getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (CId (..), Wal, CHash (..), CWallet(..), CWalletMeta(..),
                                             CWalletInit(..), CWalletAssurance(..))
import           Pos.Wallet.Web.Methods.Backup (restoreWalletFromBackup)
-- import           Pos.Wallet.Web.Methods.Logic (deleteWallet)
import           Pos.Wallet.Web.Methods.Restore (newWallet)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.Pos.Util (assertProperty, withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "restoreAddressFromWalletBackup" $ modifyMaxSuccess (const 10) $ do
           restoreWalletAddressFromBackupSpec

restoreWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => Spec
restoreWalletAddressFromBackupSpec = walletPropertySpec restoreWalletAddressFromBackupDesc $ do
    -- _ <- lift $ deleteWallet (cwId nWallet)
    restoredWallet <- lift $ restoreWalletFromBackup wBackup
    assertProperty(nWallet == restoredWallet) $ "Exported wallet is not the same as imported one!"
    where
        restoreWalletAddressFromBackupDesc =
            "Create new wallet; " <>
            "Create wallet backup; " <>
            "Delete created wallet; " <>
            "Check if the wallet is restored from backup; "