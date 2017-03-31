# Review of cardano-sl project

View [progress here](https://fpco.github.io/cardano-sl/).

This is the `master` branch, it contains a README explaining how review works.

The second branch is [`upstream`](https://github.com/fpco/cardano-sl/tree/upstream), which contains a recent version of the cardano-sl master branch.

The third branch is [`reviewed`](https://github.com/fpco/cardano-sl/tree/reviewed), which contains fragments of the cardano project which have actually been reviewed by a pull request process.

Finally, sections of the project are committed into a branch and opened as a PR to be reviewed by one or more members of the team. Once reviews are complete and the code is merged, that part of the code is considered "reviewed". We may add notes to this master branch from our findings of reviewing the different components of the codebase.

The team recommends starting at this points:

- [Node entry point](https://github.com/fpco/cardano-sl/blob/upstream/src/node/Main.hs)
- [Core node logic](https://github.com/fpco/cardano-sl/blob/upstream/src/Pos/Launcher.hs), and the exported modules.
- [Listeners](https://github.com/fpco/cardano-sl/blob/upstream/src/Pos/Communication/)
- [Workers](https://github.com/fpco/cardano-sl/blob/upstream/src/Pos/Worker/)

## Modules

```
bench
┣━Bench
┃ ┗━Pos
┃    ┗━Criterion
┃       ┣━FollowTheSatoshiBench.hs:1340
┃       ┗━TxSigningBench.hs:1296
┗━Local
   ┗━Criterion.hs:347

core
┣━Pos
┃ ┣━Binary
┃ ┃ ┣━Class.hs:31122
┃ ┃ ┣━Core
┃ ┃ ┃ ┣━Address.hs:2376
┃ ┃ ┃ ┣━Coin.hs:5962
┃ ┃ ┃ ┣━Script.hs:646
┃ ┃ ┃ ┣━Types.hs:2283
┃ ┃ ┃ ┗━Version.hs:1018
┃ ┃ ┣━Core.hs:253
┃ ┃ ┗━Crypto.hs:8547
┃ ┣━Core
┃ ┃ ┣━Address.hs:5856
┃ ┃ ┣━Block.hs:3965
┃ ┃ ┣━Class.hs:5082
┃ ┃ ┣━Coin.hs:3304
┃ ┃ ┣━Constants.hs:3652
┃ ┃ ┣━Script.hs:4257
┃ ┃ ┣━Slotting.hs:2994
┃ ┃ ┣━Timestamp.hs:866
┃ ┃ ┣━Types.hs:13192
┃ ┃ ┗━Version.hs:1609
┃ ┣━Core.hs:463
┃ ┣━Crypto
┃ ┃ ┣━AsBinary.hs:2906
┃ ┃ ┣━Hashing.hs:5006
┃ ┃ ┣━Random.hs:2486
┃ ┃ ┣━RedeemSigning.hs:3975
┃ ┃ ┣━SafeSigning.hs:4042
┃ ┃ ┣━SecretSharing.hs:6847
┃ ┃ ┗━Signing.hs:13663
┃ ┣━Crypto.hs:570
┃ ┣━Data
┃ ┃ ┗━Attributes.hs:3946
┃ ┣━Exception.hs:2528
┃ ┣━Merkle.hs:4200
┃ ┗━Util
┃    ┣━Config
┃    ┃ ┗━Path.hs:2019
┃    ┣━Config.hs:10180
┃    ┣━Iterator.hs:1956
┃    ┣━Modifier.hs:4814
┃    ┗━Util.hs:2608
┗━Setup.hs:46

db
┣━Pos
┃ ┣━DB
┃ ┃ ┣━Class.hs:2411
┃ ┃ ┣━Error.hs:525
┃ ┃ ┣━Functions.hs:6397
┃ ┃ ┣━GState
┃ ┃ ┃ ┣━Balances.hs:2129
┃ ┃ ┃ ┗━Common.hs:3951
┃ ┃ ┣━Holder.hs:2970
┃ ┃ ┣━Iterator
┃ ┃ ┃ ┣━Class.hs:330
┃ ┃ ┃ ┗━DBIterator.hs:6713
┃ ┃ ┣━Iterator.hs:212
┃ ┃ ┣━Limits.hs:1502
┃ ┃ ┣━Misc
┃ ┃ ┃ ┗━Common.hs:585
┃ ┃ ┗━Types.hs:1749
┃ ┗━DB.hs:442
┗━Setup.hs:46

infra
┣━Pos
┃ ┣━Binary
┃ ┃ ┣━Infra
┃ ┃ ┃ ┣━Communication.hs:564
┃ ┃ ┃ ┣━DHTModel.hs:740
┃ ┃ ┃ ┗━Slotting.hs:780
┃ ┃ ┗━Infra.hs:126
┃ ┣━Communication
┃ ┃ ┣━BiP.hs:856
┃ ┃ ┣━Constants.hs:798
┃ ┃ ┣━Limits
┃ ┃ ┃ ┣━Instances.hs:3348
┃ ┃ ┃ ┗━Types.hs:4773
┃ ┃ ┣━MessagePart.hs:225
┃ ┃ ┣━PeerState.hs:5022
┃ ┃ ┣━Protocol.hs:11967
┃ ┃ ┣━Relay
┃ ┃ ┃ ┣━Class.hs:2294
┃ ┃ ┃ ┣━Logic.hs:14128
┃ ┃ ┃ ┗━Types.hs:1177
┃ ┃ ┣━Relay.hs:221
┃ ┃ ┣━Types
┃ ┃ ┃ ┣━Protocol.hs:6530
┃ ┃ ┃ ┣━Relay.hs:1454
┃ ┃ ┃ ┣━State.hs:456
┃ ┃ ┃ ┗━SysStart.hs:527
┃ ┃ ┗━Util.hs:8043
┃ ┣━DHT
┃ ┃ ┣━Constants.hs:886
┃ ┃ ┣━MemState
┃ ┃ ┃ ┣━Class.hs:600
┃ ┃ ┃ ┗━Types.hs:171
┃ ┃ ┣━MemState.hs:198
┃ ┃ ┣━Model
┃ ┃ ┃ ┣━Class.hs:1734
┃ ┃ ┃ ┣━Neighbors.hs:3910
┃ ┃ ┃ ┣━Types.hs:2537
┃ ┃ ┃ ┗━Util.hs:908
┃ ┃ ┣━Model.hs:379
┃ ┃ ┣━Real
┃ ┃ ┃ ┣━Real.hs:8887
┃ ┃ ┃ ┗━Types.hs:4379
┃ ┃ ┣━Real.hs:378
┃ ┃ ┗━Workers.hs:2334
┃ ┣━DHT.hs:129
┃ ┣━Infra
┃ ┃ ┗━Constants.hs:1917
┃ ┣━Reporting
┃ ┃ ┣━Exceptions.hs:327
┃ ┃ ┣━MemState
┃ ┃ ┃ ┣━Class.hs:713
┃ ┃ ┃ ┗━Types.hs:193
┃ ┃ ┣━MemState.hs:166
┃ ┃ ┗━Methods.hs:11301
┃ ┣━Reporting.hs:269
┃ ┣━Shutdown
┃ ┃ ┣━Class.hs:670
┃ ┃ ┣━Logic.hs:1301
┃ ┃ ┗━Types.hs:325
┃ ┣━Shutdown.hs:177
┃ ┣━Slotting
┃ ┃ ┣━Class.hs:1978
┃ ┃ ┣━Constants.hs:966
┃ ┃ ┣━Error.hs:926
┃ ┃ ┣━MemState
┃ ┃ ┃ ┣━Class.hs:1391
┃ ┃ ┃ ┗━Holder.hs:4971
┃ ┃ ┣━MemState.hs:225
┃ ┃ ┣━Ntp.hs:13580
┃ ┃ ┣━Types.hs:1086
┃ ┃ ┗━Util.hs:6910
┃ ┣━Slotting.hs:492
┃ ┗━Util
┃    ┣━TimeLimit.hs:5786
┃    ┗━TimeWarp.hs:1290
┗━Setup.hs:46

lrc
┣━Pos
┃ ┗━Lrc
┃    ┣━Class.hs:1622
┃    ┣━Consumer.hs:2184
┃    ┣━Core.hs:2955
┃    ┣━DB
┃    ┃ ┣━Common.hs:3115
┃    ┃ ┣━Issuers.hs:1743
┃    ┃ ┗━RichmenBase.hs:2073
┃    ┣━Error.hs:930
┃    ┣━Fts.hs:2045
┃    ┗━Types.hs:837
┗━Setup.hs:46

Setup.hs:56

src
┣━analyzer
┃ ┣━AnalyzerOptions.hs:1126
┃ ┗━Main.hs:5830
┣━avvmmigrate
┃ ┗━Main.hs:3326
┣━checks
┃ ┗━Main.hs:5408
┣━dht-keygen
┃ ┗━Main.hs:1018
┣━genupdate
┃ ┗━Main.hs:4202
┣━keygen
┃ ┗━Main.hs:4805
┣━launcher
┃ ┗━Main.hs:15058
┣━node
┃ ┣━Main.hs:12028
┃ ┗━NodeOptions.hs:6545
┣━Pos
┃ ┣━Aeson
┃ ┃ ┣━ClientTypes.hs:1677
┃ ┃ ┣━CompileConfig.hs:307
┃ ┃ ┣━Crypto.hs:404
┃ ┃ ┗━Types.hs:2031
┃ ┣━Aeson.hs:277
┃ ┣━Binary
┃ ┃ ┣━Block
┃ ┃ ┃ ┣━Block.hs:487
┃ ┃ ┃ ┗━Types.hs:5568
┃ ┃ ┣━Block.hs:135
┃ ┃ ┣━Communication.hs:7191
┃ ┃ ┣━Explorer.hs:519
┃ ┃ ┣━Genesis.hs:1598
┃ ┃ ┣━Merkle.hs:787
┃ ┃ ┣━Relay.hs:1996
┃ ┃ ┣━Ssc
┃ ┃ ┃ ┗━GodTossing
┃ ┃ ┃    ┗━Core.hs:3261
┃ ┃ ┣━Ssc.hs:2237
┃ ┃ ┣━Txp.hs:4106
┃ ┃ ┗━Update.hs:7107
┃ ┣━Binary.hs:555
┃ ┣━Block
┃ ┃ ┣━Arbitrary.hs:17142
┃ ┃ ┣━Error.hs:200
┃ ┃ ┣━Logic
┃ ┃ ┃ ┗━Internal.hs:6260
┃ ┃ ┣━Logic.hs:37557
┃ ┃ ┣━Network
┃ ┃ ┃ ┣━Announce.hs:4585
┃ ┃ ┃ ┣━Listeners.hs:5464
┃ ┃ ┃ ┣━Retrieval.hs:30155
┃ ┃ ┃ ┗━Types.hs:2876
┃ ┃ ┣━Network.hs:426
┃ ┃ ┣━Types.hs:1530
┃ ┃ ┗━Worker.hs:9675
┃ ┣━CLI.hs:11046
┃ ┣━Communication
┃ ┃ ┣━Limits.hs:9459
┃ ┃ ┣━Message.hs:4005
┃ ┃ ┣━Methods.hs:2118
┃ ┃ ┣━Server
┃ ┃ ┃ ┗━SysStart.hs:2333
┃ ┃ ┣━Server.hs:2549
┃ ┃ ┣━Specs.hs:1898
┃ ┃ ┗━Types.hs:585
┃ ┣━Communication.hs:599
┃ ┣━CompileConfig
┃ ┃ ┣━Parser.hs:501
┃ ┃ ┗━Type.hs:4670
┃ ┣━CompileConfig.hs:208
┃ ┣━Constants.hs:11303
┃ ┣━Context
┃ ┃ ┣━Class.hs:1145
┃ ┃ ┣━Context.hs:5394
┃ ┃ ┣━Functions.hs:3985
┃ ┃ ┗━Holder.hs:5444
┃ ┣━Context.hs:374
┃ ┣━Crypto
┃ ┃ ┣━Arbitrary
┃ ┃ ┃ ┗━Unsafe.hs:2268
┃ ┃ ┗━Arbitrary.hs:7267
┃ ┣━DB
┃ ┃ ┣━Block.hs:8575
┃ ┃ ┣━DB.hs:4546
┃ ┃ ┣━GState
┃ ┃ ┃ ┣━BlockExtra.hs:5235
┃ ┃ ┃ ┣━Explorer.hs:1526
┃ ┃ ┃ ┗━GState.hs:2127
┃ ┃ ┣━GState.hs:810
┃ ┃ ┗━Misc.hs:3461
┃ ┣━Delegation
┃ ┃ ┣━Class.hs:3067
┃ ┃ ┣━DB.hs:3442
┃ ┃ ┣━Holder.hs:4033
┃ ┃ ┣━Listeners.hs:6984
┃ ┃ ┣━Logic.hs:20786
┃ ┃ ┣━Lrc.hs:497
┃ ┃ ┣━Methods.hs:2459
┃ ┃ ┣━Types.hs:2441
┃ ┃ ┗━Worker.hs:2122
┃ ┣━Delegation.hs:595
┃ ┣━DHT
┃ ┃ ┗━Arbitrary.hs:534
┃ ┣━Genesis
┃ ┃ ┣━Arbitrary.hs:1126
┃ ┃ ┣━Parser.hs:1057
┃ ┃ ┗━Types.hs:1158
┃ ┣━Genesis.hs:9415
┃ ┣━Launcher
┃ ┃ ┣━Launcher.hs:1647
┃ ┃ ┣━Options.hs:903
┃ ┃ ┣━Param.hs:3062
┃ ┃ ┣━Runner.hs:20788
┃ ┃ ┗━Scenario.hs:4443
┃ ┣━Launcher.hs:487
┃ ┣━Lrc
┃ ┃ ┣━Arbitrary.hs:3319
┃ ┃ ┣━Consumers.hs:613
┃ ┃ ┣━DB
┃ ┃ ┃ ┣━Leaders.hs:1947
┃ ┃ ┃ ┣━Lrc.hs:964
┃ ┃ ┃ ┗━Richmen.hs:4954
┃ ┃ ┣━DB.hs:496
┃ ┃ ┣━FtsPure.hs:2126
┃ ┃ ┣━Logic.hs:4327
┃ ┃ ┗━Worker.hs:9221
┃ ┣━Lrc.hs:471
┃ ┣━SafeCopy
┃ ┃ ┣━GodTossing
┃ ┃ ┃ ┣━Base.hs:779
┃ ┃ ┃ ┣━Types.hs:390
┃ ┃ ┃ ┗━VssCertData.hs:379
┃ ┃ ┣━GodTossing.hs:253
┃ ┃ ┣━Types.hs:5892
┃ ┃ ┗━Update.hs:269
┃ ┣━SafeCopy.hs:355
┃ ┣━Script
┃ ┃ ┗━Examples.hs:6995
┃ ┣━Script.hs:5696
┃ ┣━Security
┃ ┃ ┣━Class.hs:431
┃ ┃ ┣━CLI.hs:644
┃ ┃ ┣━Util.hs:926
┃ ┃ ┗━Workers.hs:8715
┃ ┣━Security.hs:243
┃ ┣━Ssc
┃ ┃ ┣━Arbitrary.hs:578
┃ ┃ ┣━Class
┃ ┃ ┃ ┣━Helpers.hs:534
┃ ┃ ┃ ┣━Listeners.hs:741
┃ ┃ ┃ ┣━LocalData.hs:1870
┃ ┃ ┃ ┣━Storage.hs:2244
┃ ┃ ┃ ┣━Types.hs:1854
┃ ┃ ┃ ┗━Workers.hs:668
┃ ┃ ┣━Class.hs:987
┃ ┃ ┣━Extra
┃ ┃ ┃ ┣━Class.hs:918
┃ ┃ ┃ ┣━Holder.hs:5312
┃ ┃ ┃ ┣━Logic.hs:11671
┃ ┃ ┃ ┗━Types.hs:428
┃ ┃ ┣━Extra.hs:479
┃ ┃ ┣━GodTossing
┃ ┃ ┃ ┣━Arbitrary.hs:8505
┃ ┃ ┃ ┣━Core
┃ ┃ ┃ ┃ ┣━Core.hs:11436
┃ ┃ ┃ ┃ ┗━Types.hs:8513
┃ ┃ ┃ ┣━Core.hs:261
┃ ┃ ┃ ┣━Error.hs:1936
┃ ┃ ┃ ┣━Functions.hs:6081
┃ ┃ ┃ ┣━Genesis.hs:2314
┃ ┃ ┃ ┣━GState.hs:8116
┃ ┃ ┃ ┣━Listeners.hs:5238
┃ ┃ ┃ ┣━LocalData
┃ ┃ ┃ ┃ ┣━Logic.hs:11273
┃ ┃ ┃ ┃ ┗━Types.hs:699
┃ ┃ ┃ ┣━LocalData.hs:323
┃ ┃ ┃ ┣━Richmen.hs:432
┃ ┃ ┃ ┣━SecretStorage.hs:2002
┃ ┃ ┃ ┣━Seed.hs:6058
┃ ┃ ┃ ┣━Shares.hs:3050
┃ ┃ ┃ ┣━Toss
┃ ┃ ┃ ┃ ┣━Base.hs:19064
┃ ┃ ┃ ┃ ┣━Class.hs:5611
┃ ┃ ┃ ┃ ┣━Failure.hs:4402
┃ ┃ ┃ ┃ ┣━Logic.hs:5195
┃ ┃ ┃ ┃ ┣━Pure.hs:4248
┃ ┃ ┃ ┃ ┣━Trans.hs:5717
┃ ┃ ┃ ┃ ┗━Types.hs:2412
┃ ┃ ┃ ┣━Toss.hs:741
┃ ┃ ┃ ┣━Type.hs:1872
┃ ┃ ┃ ┣━Types
┃ ┃ ┃ ┃ ┣━Message.hs:1430
┃ ┃ ┃ ┃ ┗━Types.hs:4680
┃ ┃ ┃ ┣━Types.hs:318
┃ ┃ ┃ ┣━VssCertData.hs:8909
┃ ┃ ┃ ┗━Workers.hs:17475
┃ ┃ ┣━GodTossing.hs:1727
┃ ┃ ┣━NistBeacon.hs:2602
┃ ┃ ┗━SscAlgo.hs:424
┃ ┣━Statistics
┃ ┃ ┣━Helpers.hs:752
┃ ┃ ┣━MonadStats.hs:8619
┃ ┃ ┣━StatEntry.hs:2115
┃ ┃ ┗━Tx.hs:1943
┃ ┣━Statistics.hs:425
┃ ┣━Txp
┃ ┃ ┣━Arbitrary.hs:729
┃ ┃ ┣━Core
┃ ┃ ┃ ┣━Core.hs:1664
┃ ┃ ┃ ┣━Tx.hs:13214
┃ ┃ ┃ ┗━Types.hs:9547
┃ ┃ ┣━Core.hs:275
┃ ┃ ┣━DB
┃ ┃ ┃ ┣━Balances.hs:5136
┃ ┃ ┃ ┗━Utxo.hs:7762
┃ ┃ ┣━DB.hs:209
┃ ┃ ┣━Error.hs:848
┃ ┃ ┣━Holder.hs:6361
┃ ┃ ┣━Logic
┃ ┃ ┃ ┣━Global.hs:5374
┃ ┃ ┃ ┗━Local.hs:4631
┃ ┃ ┣━Logic.hs:205
┃ ┃ ┣━MemState
┃ ┃ ┃ ┣━Class.hs:3139
┃ ┃ ┃ ┣━Holder.hs:5082
┃ ┃ ┃ ┗━Types.hs:1240
┃ ┃ ┣━MemState.hs:279
┃ ┃ ┣━Network
┃ ┃ ┃ ┣━Listeners.hs:2880
┃ ┃ ┃ ┗━Types.hs:877
┃ ┃ ┣━Network.hs:243
┃ ┃ ┣━Toil
┃ ┃ ┃ ┣━Class.hs:5425
┃ ┃ ┃ ┣━DBTxp.hs:4259
┃ ┃ ┃ ┣━Failure.hs:1055
┃ ┃ ┃ ┣━Logic.hs:7557
┃ ┃ ┃ ┣━Trans.hs:4978
┃ ┃ ┃ ┣━Types.hs:3536
┃ ┃ ┃ ┣━Utxo
┃ ┃ ┃ ┃ ┣━Functions.hs:3575
┃ ┃ ┃ ┃ ┗━Pure.hs:4142
┃ ┃ ┃ ┗━Utxo.hs:246
┃ ┃ ┗━Toil.hs:675
┃ ┣━Txp.hs:519
┃ ┣━Types
┃ ┃ ┣━Arbitrary
┃ ┃ ┃ ┗━Unsafe.hs:1307
┃ ┃ ┣━Arbitrary.hs:16744
┃ ┃ ┣━Block
┃ ┃ ┃ ┣━Functions.hs:23300
┃ ┃ ┃ ┣━Instances.hs:21960
┃ ┃ ┃ ┗━Types.hs:6076
┃ ┃ ┣━Block.hs:397
┃ ┃ ┣━Explorer.hs:559
┃ ┃ ┗━SharedSeed.hs:741
┃ ┣━Types.hs:945
┃ ┣━Update
┃ ┃ ┣━Arbitrary
┃ ┃ ┃ ┣━Core.hs:1824
┃ ┃ ┃ ┣━Network.hs:1614
┃ ┃ ┃ ┗━Poll.hs:919
┃ ┃ ┣━Arbitrary.hs:251
┃ ┃ ┣━DB.hs:14068
┃ ┃ ┣━Download.hs:5431
┃ ┃ ┣━Logic
┃ ┃ ┃ ┣━Global.hs:8550
┃ ┃ ┃ ┗━Local.hs:11985
┃ ┃ ┣━Logic.hs:241
┃ ┃ ┣━Lrc.hs:420
┃ ┃ ┣━MemState
┃ ┃ ┃ ┣━Class.hs:1000
┃ ┃ ┃ ┣━Functions.hs:1680
┃ ┃ ┃ ┣━Holder.hs:5458
┃ ┃ ┃ ┗━Types.hs:1943
┃ ┃ ┣━MemState.hs:433
┃ ┃ ┣━Network
┃ ┃ ┃ ┣━Listeners.hs:4248
┃ ┃ ┃ ┗━Types.hs:534
┃ ┃ ┣━Network.hs:265
┃ ┃ ┣━Poll
┃ ┃ ┃ ┣━Class.hs:9115
┃ ┃ ┃ ┣━DBPoll.hs:5219
┃ ┃ ┃ ┣━Failure.hs:6792
┃ ┃ ┃ ┣━Logic
┃ ┃ ┃ ┃ ┣━Apply.hs:16761
┃ ┃ ┃ ┃ ┣━Base.hs:16694
┃ ┃ ┃ ┃ ┣━Normalize.hs:5086
┃ ┃ ┃ ┃ ┣━Rollback.hs:1876
┃ ┃ ┃ ┃ ┣━Softfork.hs:7671
┃ ┃ ┃ ┃ ┗━Version.hs:5405
┃ ┃ ┃ ┣━Logic.hs:607
┃ ┃ ┃ ┣━Modifier.hs:1454
┃ ┃ ┃ ┣━RollTrans.hs:3556
┃ ┃ ┃ ┣━Trans.hs:9729
┃ ┃ ┃ ┗━Types.hs:10165
┃ ┃ ┣━Poll.hs:740
┃ ┃ ┗━Worker.hs:1010
┃ ┣━Update.hs:664
┃ ┣━Util
┃ ┃ ┣━Arbitrary.hs:3960
┃ ┃ ┣━BackupPhrase.hs:3088
┃ ┃ ┣━Binary.hs:6410
┃ ┃ ┣━JsonLog.hs:3258
┃ ┃ ┣━LogSafe.hs:2069
┃ ┃ ┣━Undefined.hs:644
┃ ┃ ┗━UserSecret.hs:5141
┃ ┣━Util.hs:13913
┃ ┣━Wallet
┃ ┃ ┣━Context
┃ ┃ ┃ ┣━Class.hs:2020
┃ ┃ ┃ ┣━Context.hs:387
┃ ┃ ┃ ┗━Holder.hs:3798
┃ ┃ ┣━Context.hs:297
┃ ┃ ┣━KeyStorage.hs:9117
┃ ┃ ┣━Launcher
┃ ┃ ┃ ┣━Param.hs:530
┃ ┃ ┃ ┗━Runner.hs:3919
┃ ┃ ┣━Launcher.hs:215
┃ ┃ ┣━State
┃ ┃ ┃ ┣━Acidic.hs:2144
┃ ┃ ┃ ┣━Holder.hs:3866
┃ ┃ ┃ ┣━Limits.hs:624
┃ ┃ ┃ ┣━State.hs:2805
┃ ┃ ┃ ┣━Storage
┃ ┃ ┃ ┃ ┣━Block.hs:2486
┃ ┃ ┃ ┃ ┗━Tx.hs:1596
┃ ┃ ┃ ┗━Storage.hs:2560
┃ ┃ ┣━State.hs:244
┃ ┃ ┣━Tx
┃ ┃ ┃ ┗━Pure.hs:10279
┃ ┃ ┣━Tx.hs:3199
┃ ┃ ┣━Update.hs:1766
┃ ┃ ┣━WalletMode.hs:15971
┃ ┃ ┣━Web
┃ ┃ ┃ ┣━Api.hs:6331
┃ ┃ ┃ ┣━ClientTypes.hs:10378
┃ ┃ ┃ ┣━Doc.hs:12225
┃ ┃ ┃ ┣━Error.hs:493
┃ ┃ ┃ ┣━Server
┃ ┃ ┃ ┃ ┣━Full.hs:5751
┃ ┃ ┃ ┃ ┣━Lite.hs:3595
┃ ┃ ┃ ┃ ┣━Methods.hs:25146
┃ ┃ ┃ ┃ ┗━Sockets.hs:7170
┃ ┃ ┃ ┣━Server.hs:405
┃ ┃ ┃ ┣━State
┃ ┃ ┃ ┃ ┣━Acidic.hs:2848
┃ ┃ ┃ ┃ ┣━Holder.hs:3465
┃ ┃ ┃ ┃ ┣━Limits.hs:662
┃ ┃ ┃ ┃ ┣━State.hs:4902
┃ ┃ ┃ ┃ ┗━Storage.hs:5173
┃ ┃ ┃ ┗━State.hs:268
┃ ┃ ┗━Web.hs:520
┃ ┣━Wallet.hs:314
┃ ┣━Web
┃ ┃ ┣━Api.hs:1553
┃ ┃ ┣━Doc.hs:4747
┃ ┃ ┣━Server.hs:7317
┃ ┃ ┗━Types.hs:236
┃ ┣━Web.hs:189
┃ ┣━Worker
┃ ┃ ┣━Stats.hs:1246
┃ ┃ ┗━SysStart.hs:2043
┃ ┣━Worker.hs:2311
┃ ┗━WorkMode.hs:7354
┣━smart-generator
┃ ┣━GenOptions.hs:4068
┃ ┣━Main.hs:13061
┃ ┣━TxAnalysis.hs:4489
┃ ┣━TxGeneration.hs:6294
┃ ┗━Util.hs:973
┣━wallet
┃ ┣━Command.hs:4077
┃ ┣━Main.hs:13050
┃ ┗━WalletOptions.hs:3768
┣━wallet-purescript
┃ ┣━Main.hs:2505
┃ ┗━PSTypes.hs:281
┣━wallet-web-docs
┃ ┗━Main.hs:200
┗━web-docs
   ┗━Main.hs:171

test
┣━Spec.hs:69
┣━Test
┃ ┗━Pos
┃    ┣━Arbitrary
┃    ┃ ┣━Infra
┃    ┃ ┃ ┣━Communication.hs:1300
┃    ┃ ┃ ┗━DHT.hs:452
┃    ┃ ┗━Infra.hs:160
┃    ┣━BinarySpec.hs:2217
┃    ┣━Block
┃    ┃ ┗━Identity
┃    ┃    ┣━BinarySpec.hs:3363
┃    ┃    ┗━SafeCopySpec.hs:2482
┃    ┣━Communication
┃    ┃ ┗━Identity
┃    ┃    ┗━BinarySpec.hs:871
┃    ┣━CryptoSpec.hs:9384
┃    ┣━DHT
┃    ┃ ┗━Identity
┃    ┃    ┗━BinarySpec.hs:492
┃    ┣━FollowTheSatoshiSpec.hs:7836
┃    ┣━Genesis
┃    ┃ ┗━Identity
┃    ┃    ┗━BinarySpec.hs:491
┃    ┣━MerkleSpec.hs:1336
┃    ┣━Script
┃    ┃ ┗━Identity
┃    ┃    ┗━BinarySpec.hs:378
┃    ┣━Ssc
┃    ┃ ┗━GodTossing
┃    ┃    ┣━ComputeSharesSpec.hs:3258
┃    ┃    ┣━Identity
┃    ┃    ┃ ┣━BinarySpec.hs:3169
┃    ┃    ┃ ┗━SafeCopySpec.hs:653
┃    ┃    ┣━SeedSpec.hs:12080
┃    ┃    ┣━Types
┃    ┃    ┃ ┗━BaseSpec.hs:2921
┃    ┃    ┗━VssCertDataSpec.hs:7677
┃    ┣━Txp
┃    ┃ ┗━Identity
┃    ┃    ┗━BinarySpec.hs:1071
┃    ┣━Types
┃    ┃ ┣━AddressSpec.hs:1214
┃    ┃ ┣━BlockSpec.hs:6031
┃    ┃ ┣━CoinSpec.hs:7816
┃    ┃ ┣━Identity
┃    ┃ ┃ ┣━BinarySpec.hs:761
┃    ┃ ┃ ┣━SafeCopySpec.hs:797
┃    ┃ ┃ ┗━TimestampSpec.hs:552
┃    ┃ ┣━SlottingSpec.hs:918
┃    ┃ ┣━TxSpec.hs:23443
┃    ┃ ┗━UtxoSpec.hs:3983
┃    ┣━Update
┃    ┃ ┗━Identity
┃    ┃    ┣━BinarySpec.hs:2154
┃    ┃    ┗━SafeCopySpec.hs:511
┃    ┣━Util.hs:7521
┃    ┗━UtilSpec.hs:4768
┗━Test.hs:131

update
┣━Pos
┃ ┗━Update
┃    ┣━Core
┃    ┃ ┗━Types.hs:13484
┃    ┣━Core.hs:145
┃    ┗━Error.hs:875
┗━Setup.hs:46
```
