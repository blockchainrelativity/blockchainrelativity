cardano-cli transaction build-raw \
--babbage-era \
--tx-in $TXIN \
--tx-out $CLIWALLET+50000000 \
--tx-out 
--fee 174885 \
--change-address $CLIWALLET \
--out-file /home/relativity/testnet/tmp/tx_ump4d14kq.raw \
--protocol-params-file /home/relativity/testnet/tmp/protocolParams.json

cardano-cli transaction sign \
--signing-key-file payment4.skey $TESTNET \
--tx-body-file /home/relativity/testnet/tmp/tx_ump4d14kq.raw \
--out-file  /home/relativity/testnet/tmp/tx_ump4d14kq.signed


cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--tx-in $TXIN2 \
--tx-out $MINTER+1500000+"$MINTAMOUNT $POLICYID.$TOKENNAME" \
--change-address $MINTER \
--mint "$MINTAMOUNT $POLICYID.$TOKENNAME" \
--mint-script-file $SCRIPTFILE \
--mint-redeemer-file $REDEEMERFILE \
--tx-in-collateral $COLLATERAL \
--protocol-params-file protocol.json \
--out-file mint-token-plutus.raw



cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $SENDER+"1500000 + <NUMBER OF TOKENS> <POLICY ID>.<TOKEN NAME>" \
--change-address $SENDER \
--protocol-params-file protocol.json \
--out-file send-tokens.raw

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--tx-out $CLIWALLET+50000000 \
--change-address $CLIWALLET \
--protocol-params-file protocol.json \
--out-file txs/send-tokens.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file txs/send-tokens.raw \
--out-file txs/send-tokens.signed

cardano-cli transaction submit \
--tx-file txs/send-tokens.signed \
$TESNET
