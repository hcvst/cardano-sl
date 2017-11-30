module Pos.Wallet.Web.Backup
       ( WalletMetaBackup (..)
       , AccountMetaBackup (..)
       , WalletBackup (..)
       , TotalBackup (..)
       , currentBackupFormatVersion
       , getWalletBackup
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.SemVer as V

import           Pos.Crypto (EncryptedSecretKey, PublicKey (..))
import           Pos.Crypto.Signing.Safe (emptyPassphrase, safeDeterministicKeyGen)
import           Pos.Wallet.Web.ClientTypes (CWalletAssurance (..))
import           Pos.Util.Util (maybeThrow)
import           Pos.Wallet.Web.Account (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountMeta (..), CId,
                                             CWalletMeta (..), Wal)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (getAccountMeta, getWalletMeta)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Gen (Gen)

currentBackupFormatVersion :: V.Version
currentBackupFormatVersion = V.initial & V.major .~ 1

newtype WalletMetaBackup = WalletMetaBackup CWalletMeta
newtype AccountMetaBackup = AccountMetaBackup CAccountMeta

data WalletBackup = WalletBackup
    { wbSecretKey :: !EncryptedSecretKey
    , wbMeta      :: !WalletMetaBackup
    , wbAccounts  :: !(HashMap Int AccountMetaBackup)
    }

data TotalBackup = TotalBackup WalletBackup

instance Arbitrary WalletBackup where
    arbitrary = do
        cwNameT <- arbitrary
        caNameT <- arbitrary
        wbInt   <- arbitrary
        let hm = HM.empty
        (_, esk) <- genEncryptedSecretKey
        let wMetaBackup = WalletMetaBackup $ CWalletMeta { cwName = (cwNameT :: Text ), cwAssurance = CWANormal, cwUnit = 1 }
        return $ WalletBackup
            { wbSecretKey = esk
            , wbMeta = wMetaBackup
            , wbAccounts = HM.insert wbInt (AccountMetaBackup $ CAccountMeta { caName = caNameT }) hm
            }

genEncryptedSecretKey :: Gen (PublicKey, EncryptedSecretKey)
genEncryptedSecretKey = do
    x <- arbitrary
    return $ safeDeterministicKeyGen x emptyPassphrase

getWalletBackup :: AccountMode ctx m => CId Wal -> m WalletBackup
getWalletBackup wId = do
    sk <- getSKById wId
    meta <- maybeThrow (InternalError "Wallet have no meta") =<<
            getWalletMeta wId
    accountIds <- getWalletAccountIds wId
    accountMetas <- forM accountIds $
        maybeThrow (InternalError "Account have no meta") <=<
        getAccountMeta

    let accountsMap = HM.fromList $ zip
            (map (fromInteger . fromIntegral . aiIndex) accountIds)
            (map AccountMetaBackup accountMetas)

    return WalletBackup
        { wbSecretKey = sk
        , wbMeta = WalletMetaBackup meta
        , wbAccounts = accountsMap
        }
