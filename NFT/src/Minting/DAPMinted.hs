{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Minting.DAPMinted where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import qualified PlutusTx.Builtins   as Builtins
import           Minting.Types


-- QUESTIONS: How to I make this policy CIP25 compliant but lock down minting to strictly only when updating metadata?
-- CIP 68 updateable NFTs (datum) - Requires same policy ID!

-- PlutusTx.makeLift ''MinterRedeemer

{-# INLINABLE mkPolicy #-}

mkPolicy :: MinterRedeemer -> ScriptContext -> Bool
mkPolicy (MRdmr oref tn amt) ctx = traceIfFalse "There can only be ONE UTxO, to rule it all!" hasUTxO      &&
                              traceIfFalse "The intended amount has not been minted"    checkMintedAmount &&
                              traceIfFalse "Not signed by DigitalArkPool"               signedByDAP 
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount =
            case flattenValue (txInfoMint info) of
              [(_, tn', amt')] ->
                if tn' == tn then
                traceIfFalse "Exact amount of token must be minted" (amt' == amt)
                else
                traceError "Bad token name."

              _              ->
                traceError "Exactly 1 type of asset must be minted."

        signedByDAP :: Bool
        signedByDAP = all (== True) $ map (\pkh -> elem pkh $ txInfoSignatories info) (addrDAP)

            where
                addrDAP :: [PubKeyHash]
                addrDAP = ["466c29e23bee42206aa24a0e09f6e24c8d46ad9dcac43fcbb11f9d24","bff4347ee151015986a3446bb567e27d3bd6a1c0bcccb01189a64e27"]
                
policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])
    
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- -- OFF-CHAIN

-- Parameters to get used to compile the script
data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    , npAmount  :: !Integer
    } deriving (Generic, FromJSON, ToJSON, Show)

-- {-# INLINABLE manager #-}
-- --- These magic numbers for the hardcoded PubKeyHash can be found by using  [indexByteString (getPubKeyHash p) i | i <- [0..27]] where p is the PubKeyHash in this case let (p :: PubKeyHash) = "80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7"
-- manager :: PubKeyHash
-- manager = PubKeyHash { getPubKeyHash = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [128,164,244,91,86,184,141,17,57,218,35,188,76,60,117,236,109,50,148,60,8,127,37,11,134,25,60,167]) }
-- --manage2 = PubKeyHash { getPubKeyHash =  (consByteString (consByteString emptyByteString 167) 60) ...  [128,164,244,91,86,184,141,17,57,218,35,188,76,60,117,236,109,50,148,60,8,127,37,11,134,25,60,167] }

byteString = [indexByteString (getPubKeyHash p) i | i <- [0..27]]

p1 :: Integer
p1 = 466c29e23bee42206aa24a0e09f6e24c8d46ad9dcac43fcbb11f9d24

p2 :: Integer
p2 = bff4347ee151015986a3446bb567e27d3bd6a1c0bcccb01189a64e27

-- -- Wrapping Integer to be used as a redeemer
-- data Typed                                            
-- instance Scripts.ValidatorTypes Typed where   
--     type instance RedeemerType Typed = Integer


-- Add endpoint of burn if that is necessary to update the metadata, will also need to be in the NFTSchema and create the burn function
-- type NFTSchema = Endpoint "mint" NFTParams

-- mint :: NFTParams -> Contract w NFTSchema Text ()
-- mint _ = Contract () NFTSchema "" ()
-- mint np = do
--     utxos <- utxosAt $ npAddress np
--     case Map.keys utxos of
--         []       -> Contract.logError @String $ printf "no utxo found" 
--         oref : _ -> do
--             let tn      = npToken np  
--                 amt     = npAmount np         
--             let val     = Value.singleton curSymbol tn amt 
--                 lookups = Constraints.mintingPolicy policy <> Constraints.unspentOutputs utxos
--                 tx      = (Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData (MRdmr oref tn amt)) val) <> Constraints.mustSpendPubKeyOutput oref
--             ledgerTx <- submitTxConstraintsWith @Void lookups tx
--             void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--             Contract.logInfo @String $ printf "forged %s" (show val)

-- Setup function to be able to burn and mint
-- upgrademd :: NFTParams -> Contract w NFTSchema Text ()
-- upgrademd np = do
--     utxos <- utxosAt $ npAddress np
--     case Map.keys utxos of
--         []       -> Contract.logError @String "no utxo found"
--         oref : _ -> do
--             let tn      = npToken np
--                 amnt    = npAmount np 
--             let val     = Value.singleton (curSymbol oref tn) tn amnt
--                 lookups = Constraints.mintingPolicy (policy nftparams oref tn) <> Constraints.unspentOutputs utxos
--                 tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
--             ledgerTx <- submitTxConstraintsWith @Void lookups tx
--             void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--             Contract.logInfo @String $ printf "forged %s" (show val)

-- endpoints :: Contract () NFTSchema Text ()
-- endpoints = mint' >> endpoints
--   where
--     mint' = awaitPromise $ endpoint @"mint" mint

-- test :: IO ()
-- test = runEmulatorTraceIO $ do
--     let tn1 = "Vasil"
--         tn2 = "RareBloom"
--         w1  = knownWallet 1
--         w2  = knownWallet 2
--     h1 <- activateContractWallet w1 endpoints
--     h2 <- activateContractWallet w2 endpoints
--     callEndpoint @"mint" h1 $ NFTParams
--         { npToken   = "Vasil"
--         , npAddress = mockWalletAddress w1
--         , npAmount  = 1
--         }
--     void $ Emulator.waitNSlots 1
--     callEndpoint @"mint" h2 $ NFTParams
--         { npToken   = "Vasil"
--         , npAddress = mockWalletAddress w2
--         , npAmount  = 1
--         }
--     void $ Emulator.waitNSlots 1
--     callEndpoint @"mint" h1 $ NFTParams
--         { npToken   = "RareBloom"
--         , npAddress = mockWalletAddress w1
--         , npAmount  = 10
--         }
--     void $ Emulator.waitNSlots 1
--     callEndpoint @"mint" h2 $ NFTParams
--         { npToken   = "RareBloom"
--         , npAddress = mockWalletAddress w2
--         , npAmount  = 1
--         }
--     void $ Emulator.waitNSlots 1