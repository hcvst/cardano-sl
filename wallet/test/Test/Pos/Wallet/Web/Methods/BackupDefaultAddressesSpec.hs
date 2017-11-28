module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Data.List ((!!))
import           Formatting (build, sformat, (%))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account (myRootAddresses)
import           Pos.Wallet.Web.ClientTypes (CFilePath(..))
import           Pos.Wallet.Web.Methods.Backup (importWalletJSON,
                                                exportWalletJSON)
import           Pos.Wallet.Web.Methods.Logic (getAccounts, getWallet)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.Pos.Util (assertProperty, maybeStopProperty,
                                withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.Pos.Wallet.Web.Util (importSomeWallets,
                                           mostlyEmptyPassphrases)
import           Test.QuickCheck (choose)
import           Test.QuickCheck.Monadic (pick)


spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "restoreAddressFromWalletBackup" $ modifyMaxSuccess (const 10) $ do
           restoreWalletAddressFromBackupSpec

restoreWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => Spec
restoreWalletAddressFromBackupSpec = walletPropertySpec restoreWalletAddressFromBackupDesc $ do
    passphrases <- importSomeWallets mostlyEmptyPassphrases
    let l = length passphrases
    rootsWIds <- lift myRootAddresses
    idx <- pick $ choose (0, l - 1)
    let walId = rootsWIds !! idx
    let noOneAccount = sformat ("There is no one account for wallet: "%build) walId
    _ <- maybeStopProperty noOneAccount =<< (lift $ head <$> getAccounts (Just walId))
    let filePath = CFilePath ("walletExport.json" :: Text)
    wallet <- lift $ getWallet walId
    _ <- lift $ exportWalletJSON walId filePath
    walletImport <- lift $ importWalletJSON filePath
    assertProperty(walletImport == wallet) $ "Exported wallet is not the same as imported one!"
    where
        restoreWalletAddressFromBackupDesc =
            "Get arbitrary wallet and check if it has at least one account;" <>
            "Export wallet and then import it;" <>
            "Check if the imported wallet is the same one; "
