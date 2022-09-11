cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--tx-out $MINTER+"1500000 + $MINTAMOUNT $POLICYID.$TOKENNAME1 + $MINTAMOUNT $POLICYID.$TOKENNAME2" \
--change-address $MINTER \
--mint "$MINTAMOUNT $POLICYID.$TOKENNAME1 + $MINTAMOUNT $POLICYID.$TOKENNAME2" \
--mint-script-file $SCRIPTFILE \
--out-file mint-token-plutus.raw


# --tx-in-collateral $COLLATERAL \
# --protocol-params-file protocol.json \
# --mint-redeemer-file $REDEEMERFILE \


cardano-cli transaction sign \
--signing-key-file $MINTERKEY \
--testnet-magic 1097911063 \
--tx-body-file /home/relativity/multisig/txs/mint-token-plutus.raw \
--out-file /home/relativity/multisig/txs/mint-token-plutus.signed

cardano-cli transaction submit \
--tx-file /home/relativity/multisig/txs/mint-token-plutus.signed \
$TESTNET