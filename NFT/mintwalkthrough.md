#  203.4 - Using CIP-25 to Mint a Cardano NFT

If you were able to mint a token with cardano-cli in **201.3 Minting Native Assets on cardano-cli** and to add metadata to a transaction in **203 Completion Assignment: Hello Testnet!**, then you've got the background knowledge necessary to mint a Cardano NFT.

## Review Cardano Improvement Proposal 25:
- Start here: [https://cips.cardano.org/cips/cip25/](https://cips.cardano.org/cips/cip25/)
- Then, use CIP-25 to create a `.json` file that adheres to the Cardano NFT Metatdata standard.
- In the "721" metadata, you'll need to include a Policy Id and Asset Name for your NFT.

## Create a Policy ID
- To see how, review lesson [201.3](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-02/docs/201-3.md)
- Extension: [You can use scripts to create policies with additional parameters](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md).

## Set Variables
- Remember that $ASSETNAME must be a Hex string.

```
TXIN="a7a4c8cc2759510e2cfe42434a742b995efe2850f50729d39fb93c90c1b9ac43#0"
MINTERADDRESS="addr_test1vprxc20z80hyygr25f9quz0kufxg634dnh9vg07tky0e6fqnsrvuy"
MINTERKEY="/home/relativity/playground/wallets/pay1/payment1.skey"
POLICYID="77e6cb8d302948cc52604fe2f5895791938a7a2655bda7980e371a85"
ASSETNAME="41524b"
MINT_SCRIPT_FILE="/home/relativity/playground/scripts/native/singlekeyhash.script"
METADATA_JSON_FILE="/home/relativity/playground/assets/arknft.json"
```

## Example Minting Transaction:
```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 2 \
--tx-in $TXIN \
--tx-out $MINTERADDRESS+"2000000 + 1 $POLICYID.$ASSETNAME" \
--mint "1 $POLICYID.$ASSETNAME" \
--mint-script-file $MINT_SCRIPT_FILE \
--change-address $MINTERADDRESS \
--metadata-json-file $METADATA_JSON_FILE \
--protocol-params-file ~/preview/protocol.json \
--out-file ~/playground/txs/mint-ark-nft.raw

cardano-cli transaction sign \
--signing-key-file $MINTERKEY \
--testnet-magic 2 \
--tx-body-file ~/playground/txs/mint-ark-nft.raw \
--out-file ~/playground/txs/mint-ark-nft.signed

cardano-cli transaction submit \
--tx-file ~/playground/txs/mint-ark-nft.signed \
--testnet-magic 2
```

## A great way to check your understanding: try to mint two NFTs in one transaction.
- In `nft.json`, note that addtional NFTs can created on the same policyId.
- [Lesson 201.3](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-02/docs/201-3.md) shows you how to mint multiple tokens at once.
- Try to mint several NFTs in one transaction - and bring your questions to Live Coding.

---

## Tip: Send your NFT to a browser wallet to view:

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $ETERNL+"2000000 + 1 $POLICYID.$ASSETNAME" \
--change-address $MINTERADDRESS \
--out-file tx.draft

cardano-cli transaction sign \
--tx-body-file tx.draft \
--signing-key-file $MINTERKEY \
--testnet-magic 1097911063 \
--out-file tx.signed

cardano-cli transaction submit \
--testnet-magic 1097911063 \
--tx-file tx.signed
```