{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Minting.Types where

import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified PlutusTx
import           Ledger             (TxOutRef, TokenName)


data MinterRedeemer = MRdmr {
    infoOutRef :: TxOutRef
    , tname :: TokenName
    , amnt :: Integer
    }
PlutusTx.makeIsDataIndexed ''MinterRedeemer  [('MRdmr,0)]
