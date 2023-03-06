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
addrDAP = ["466c29e23bee42206aa24a0e09f6e24c8d46ad9dcac43fcbb11f9d24","bff4347ee151015986a3446bb567e27d3bd6a1c0bcccb01189a64e27"]
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
mkPolicy (MinterRedeemer oref tn amt) ctx = traceIfFalse "There can only be ONE UTxO, to rule it all!" hasUTxO      &&
                              traceIfFalse "The intended amount has not been minted"    checkMintedAmount &&
                              traceIfFalse "Not signed by DigitalArkPool"               signedByDAP 


-- mkPolicy :: TxOutRef -> TokenName -> Integer -> ScriptContext -> Bool
-- mkPolicy oref tn amt ctx = traceIfFalse "There can only be ONE UTxO, to rule it all!" hasUTxO           &&
--                             traceIfFalse "The intended amount has not been minted"    checkMintedAmount &&
--                             traceIfFalse "Not signed by DigitalArkPool"               signedByDAP    
                        --   traceIfFalse "some error for updating metadata"                      burntoMint
    where

        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- When you compile this script with parameters it will lock this minting policy to a utxo, once spent it cannot be used again to mint again
        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        -- Check that only amt was minted, cannot mint more or less
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


        -- Which one makes more sense to use and why?

            -- signedBypkh :: Bool
            -- signedBypkh = txSignedBy info $ unPaymentPubKeyHash addrDAP

            -- signedByDAP = any (\sig -> sig == filter `elem` addrDAP) $ txInfoSignatories info

            -- signedByDAP = all (== True) $ map (\pkh -> elem pkh $ txInfoSignatories info) (addrDAP)



        -- Element of the list is evaluated against each element in (txSignatories info). Decide if all or any, leaning on any. 
        signedByDAP :: Bool
        signedByDAP = all (== True) $ map (\pkh -> elem pkh $ txInfoSignatories info) (addrDAP)

            where
                addrDAP = ["466c29e23bee42206aa24a0e09f6e24c8d46ad9dcac43fcbb11f9d24","bff4347ee151015986a3446bb567e27d3bd6a1c0bcccb01189a64e27"]
                -- needs to be multisig pkh. List of elements to compare
                -- addrDAP :: [PubKeyHash]

-- Add endpoint to burn so we can mint again with updated metadata, refer to CIP 68 for alternatives
    -- burntoMint :: Bool
    -- burntoMint = 

-- This is making the mkPolicy wrapped to be compiled
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


-- -- Wrapping Integer to be used as a redeemer
-- data Typed                                            
-- instance Scripts.ValidatorTypes Typed where   
--     type instance RedeemerType Typed = Integer


-- Add endpoint of burn if that is necessary to update the metadata, will also need to be in the NFTSchema and create the burn function
type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ npAddress np
    -- pkh <- Contract.ownPaymentPubKeyHash
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np  
                amt     = npAmount np         
            let val     = Value.singleton curSymbol tn amt 
                lookups = Constraints.mintingPolicy policy <> Constraints.unspentOutputs utxos
                tx      = (Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData (MinterRedeemer oref tn amt)) val) <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

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

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn1 = "Vasil"
        tn2 = "RareBloom"
        w1  = knownWallet 1
        w2  = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn1
        , npAddress = mockWalletAddress w1
        , npAmount  = 1
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn1
        , npAddress = mockWalletAddress w2
        , npAmount  = 1
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn2
        , npAddress = mockWalletAddress w1
        , npAmount  = 10
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn2
        , npAddress = mockWalletAddress w2
        , npAmount  = 1
        }
    void $ Emulator.waitNSlots 1