module PWPs.PiecewiseSpec where

import PWPs.Piecewise (Piece (..), Pieces (..), mergePieces)
import PWPs.PolyDeltas (PolyDelta (..))
import PWPs.SimplePolynomials (Poly (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Merge pieces" $ do
    it "keeps merging mergeable pieces" $ do
      let pieces :: Pieces Double (PolyDelta Double)
          pieces =
            Pieces
              [ Piece 1 (Pd (Poly [1]))
              , Piece 2 (Pd (Poly [1]))
              , Piece 3 (Pd (Poly [1]))
              , Piece 4 (Pd (Poly [2]))
              ]
      mergePieces pieces `shouldBe` Pieces [Piece 1 (Pd (Poly [1])), Piece 4 (Pd (Poly [2]))]

multihops =
  [
    ( 0.6
    , PDF
        ( Pieces
            { getPieces =
                [ Piece{basepoint = 0.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 1.3333333333333333, object = D 4.1472e-8}
                , Piece{basepoint = 1.3333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 1.6666666666666665, object = D 7.966080000000001e-7}
                , Piece{basepoint = 1.6666666666666665, object = Pd (Poly [0.0])}
                , Piece{basepoint = 2.0, object = D 8.471088000000001e-6}
                , Piece{basepoint = 2.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 2.333333333333333, object = D 5.774835600000001e-5}
                , Piece{basepoint = 2.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 2.6666666666666665, object = D 2.7872934300000006e-4}
                , Piece{basepoint = 2.6666666666666665, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.0, object = D 9.517248270000002e-4}
                , Piece{basepoint = 3.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.333333333333333, object = D 2.300707257e-3}
                , Piece{basepoint = 3.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.6666666666666665, object = D 3.5652384290000005e-3}
                , Piece{basepoint = 3.6666666666666665, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.0, object = D 3.0576724120000005e-3}
                , Piece{basepoint = 4.0, object = Pd (Poly [0.0])}
                ]
            }
        )
    )
  ,
    ( 9.18
    , PDF
        ( Pieces
            { getPieces =
                [ Piece{basepoint = 0.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 2.6666666666666665, object = D 1.7199267840000002e-15}
                , Piece{basepoint = 2.6666666666666665, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.0, object = D 6.607385395200001e-14}
                , Piece{basepoint = 3.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.333333333333333, object = D 1.3372102287360005e-12}
                , Piece{basepoint = 3.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.666666666666666, object = D 4.789879640064001e-12}
                , Piece{basepoint = 3.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.6666666666666665, object = D 1.3496272939008005e-11}
                , Piece{basepoint = 3.6666666666666665, object = Pd (Poly [0.0])}
                , Piece{basepoint = 3.9999999999999996, object = D 9.200560475289603e-11}
                , Piece{basepoint = 3.9999999999999996, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.0, object = D 9.487825852953602e-11}
                , Piece{basepoint = 4.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.333333333333333, object = D 1.5013987240504324e-9}
                , Piece{basepoint = 4.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.666666666666666, object = D 9.764287290871347e-9}
                , Piece{basepoint = 4.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.0, object = D 5.227785092644728e-8}
                , Piece{basepoint = 5.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.333333333333333, object = D 2.3252393265849042e-7}
                , Piece{basepoint = 5.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.666666666666666, object = D 8.615458245066751e-7}
                , Piece{basepoint = 5.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.0, object = D 2.651906330886059e-6}
                , Piece{basepoint = 6.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.333333333333333, object = D 6.719904672158095e-6}
                , Piece{basepoint = 6.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.666666666666666, object = D 1.3784031781531993e-5}
                , Piece{basepoint = 6.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.0, object = D 2.2225265347737914e-5}
                , Piece{basepoint = 7.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.333333333333333, object = D 2.6780543271252584e-5}
                , Piece{basepoint = 7.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.666666666666666, object = D 2.1802662373111047e-5}
                , Piece{basepoint = 7.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.0, object = D 9.3493605791059e-6}
                , Piece{basepoint = 8.0, object = Pd (Poly [0.0])}
                ]
            }
        )
    )
  ,
    ( 75.03999999999999
    , PDF
        ( Pieces
            { getPieces =
                [ Piece{basepoint = 0.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.0, object = D 7.132880358604801e-23}
                , Piece{basepoint = 4.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.333333333333333, object = D 4.110322306646017e-21}
                , Piece{basepoint = 4.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.666666666666666, object = D 1.2266139439595526e-19}
                , Piece{basepoint = 4.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 4.999999999999999, object = D 1.9864588843273427e-19}
                , Piece{basepoint = 4.999999999999999, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.0, object = D 2.2839901727623754e-18}
                , Piece{basepoint = 5.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.333333333333332, object = D 3.815656440312105e-18}
                , Piece{basepoint = 5.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.333333333333333, object = D 3.4124362596393636e-17}
                , Piece{basepoint = 5.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.666666666666666, object = D 4.633181073826191e-16}
                , Piece{basepoint = 5.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.999999999999999, object = D 6.815521971785843e-16}
                , Piece{basepoint = 5.999999999999999, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.0, object = D 3.9980843645324316e-15}
                , Piece{basepoint = 6.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.333333333333332, object = D 1.442656179259609e-14}
                , Piece{basepoint = 6.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.333333333333333, object = D 2.5558223205559723e-14}
                , Piece{basepoint = 6.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.666666666666666, object = D 2.935163908418456e-13}
                , Piece{basepoint = 6.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.999999999999999, object = D 6.106216778415069e-13}
                , Piece{basepoint = 6.999999999999999, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.0, object = D 1.2604470715719645e-12}
                , Piece{basepoint = 7.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.333333333333332, object = D 3.636662703119137e-12}
                , Piece{basepoint = 7.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.333333333333333, object = D 6.798082221817281e-12}
                , Piece{basepoint = 7.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.666666666666666, object = D 5.1158086232443324e-11}
                , Piece{basepoint = 7.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.999999999999999, object = D 7.278927297950928e-11}
                , Piece{basepoint = 7.999999999999999, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.0, object = D 1.4830730738959641e-10}
                , Piece{basepoint = 8.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.333333333333332, object = D 8.421643346514407e-10}
                , Piece{basepoint = 8.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.333333333333334, object = D 9.217262045013868e-13}
                , Piece{basepoint = 8.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.666666666666666, object = D 2.83397181473949e-9}
                , Piece{basepoint = 8.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.0, object = D 8.374451389916683e-9}
                , Piece{basepoint = 9.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.333333333333332, object = D 2.1649073719674732e-8}
                , Piece{basepoint = 9.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.333333333333334, object = D 3.8773668193667994e-13}
                , Piece{basepoint = 9.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.666666666666666, object = D 4.860164797058107e-8}
                , Piece{basepoint = 9.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.0, object = D 9.373487236800627e-8}
                , Piece{basepoint = 10.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.333333333333332, object = D 1.5292911608949568e-7}
                , Piece{basepoint = 10.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.666666666666666, object = D 2.06355690274445e-7}
                , Piece{basepoint = 10.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.0, object = D 2.2249616484677253e-7}
                , Piece{basepoint = 11.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.333333333333332, object = D 1.811279598186678e-7}
                , Piece{basepoint = 11.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.666666666666666, object = D 9.999809886961817e-8}
                , Piece{basepoint = 11.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.0, object = D 2.858728191257246e-8}
                , Piece{basepoint = 12.0, object = Pd (Poly [0.0])}
                ]
            }
        )
    )
  ,
    ( 99.99
    , PDF
        ( Pieces
            { getPieces =
                [ Piece{basepoint = 0.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.333333333333333, object = D 2.9581481423205833e-30}
                , Piece{basepoint = 5.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.666666666666666, object = D 2.2728438226829816e-28}
                , Piece{basepoint = 5.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 5.999999999999999, object = D 5.087013348389057e-27}
                , Piece{basepoint = 5.999999999999999, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.0, object = D 3.8785482041648e-27}
                , Piece{basepoint = 6.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.333333333333332, object = D 1.059512903520555e-25}
                , Piece{basepoint = 6.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.333333333333333, object = D 1.3365966355530381e-25}
                , Piece{basepoint = 6.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.666666666666665, object = D 1.582429038926236e-25}
                , Piece{basepoint = 6.666666666666665, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.666666666666666, object = D 4.689214565564446e-24}
                , Piece{basepoint = 6.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.999999999999998, object = D 3.0395824456041457e-24}
                , Piece{basepoint = 6.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 6.999999999999999, object = D 2.7980969222157886e-23}
                , Piece{basepoint = 6.999999999999999, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.0, object = D 4.7745174813095975e-23}
                , Piece{basepoint = 7.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.333333333333332, object = D 4.7533180844549275e-22}
                , Piece{basepoint = 7.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.333333333333333, object = D 5.908517133432816e-22}
                , Piece{basepoint = 7.333333333333333, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.666666666666665, object = D 8.186462571513814e-22}
                , Piece{basepoint = 7.666666666666665, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.666666666666666, object = D 1.150165521921094e-20}
                , Piece{basepoint = 7.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.999999999999998, object = D 1.25558499491983e-20}
                , Piece{basepoint = 7.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 7.999999999999999, object = D 4.5173323590307306e-20}
                , Piece{basepoint = 7.999999999999999, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.0, object = D 6.592807325269952e-20}
                , Piece{basepoint = 8.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.333333333333332, object = D 1.0395076714594502e-18}
                , Piece{basepoint = 8.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.333333333333334, object = D 5.2273260952232514e-20}
                , Piece{basepoint = 8.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.666666666666664, object = D 8.418889347172822e-19}
                , Piece{basepoint = 8.666666666666664, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.666666666666666, object = D 7.718646410962982e-18}
                , Piece{basepoint = 8.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 8.999999999999998, object = D 6.931704417778403e-18}
                , Piece{basepoint = 8.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.0, object = D 5.3109876863382975e-17}
                , Piece{basepoint = 9.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.333333333333332, object = D 3.726169090772976e-16}
                , Piece{basepoint = 9.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.333333333333334, object = D 6.150600652061343e-18}
                , Piece{basepoint = 9.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.666666666666664, object = D 2.4320258784143104e-16}
                , Piece{basepoint = 9.666666666666664, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.666666666666666, object = D 1.914767550861743e-15}
                , Piece{basepoint = 9.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.666666666666668, object = D 3.8225829153081516e-20}
                , Piece{basepoint = 9.666666666666668, object = Pd (Poly [0.0])}
                , Piece{basepoint = 9.999999999999998, object = D 1.735953584752323e-15}
                , Piece{basepoint = 9.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.0, object = D 9.401781517213182e-15}
                , Piece{basepoint = 10.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.333333333333332, object = D 5.182334015568888e-14}
                , Piece{basepoint = 10.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.333333333333334, object = D 3.5511327183286193e-16}
                , Piece{basepoint = 10.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.666666666666664, object = D 5.700050208028198e-14}
                , Piece{basepoint = 10.666666666666664, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.666666666666666, object = D 1.6509901562622427e-13}
                , Piece{basepoint = 10.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.666666666666668, object = D 5.3244253207748177e-17}
                , Piece{basepoint = 10.666666666666668, object = Pd (Poly [0.0])}
                , Piece{basepoint = 10.999999999999998, object = D 2.649473066355823e-13}
                , Piece{basepoint = 10.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.0, object = D 5.949515407134883e-13}
                , Piece{basepoint = 11.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.333333333333332, object = D 3.020113591684896e-12}
                , Piece{basepoint = 11.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.333333333333334, object = D 4.767886890919919e-15}
                , Piece{basepoint = 11.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.666666666666664, object = D 3.187772012553167e-12}
                , Piece{basepoint = 11.666666666666664, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.666666666666666, object = D 6.468774425192424e-12}
                , Piece{basepoint = 11.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.666666666666668, object = D 2.143013323606145e-15}
                , Piece{basepoint = 11.666666666666668, object = Pd (Poly [0.0])}
                , Piece{basepoint = 11.999999999999998, object = D 9.15857330118586e-12}
                , Piece{basepoint = 11.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.0, object = D 1.8779007586928046e-11}
                , Piece{basepoint = 12.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.333333333333332, object = D 7.296957068034289e-11}
                , Piece{basepoint = 12.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.333333333333334, object = D 1.2414716361984453e-14}
                , Piece{basepoint = 12.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.666666666666664, object = D 5.863958605288517e-11}
                , Piece{basepoint = 12.666666666666664, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.666666666666666, object = D 1.1283713892472878e-10}
                , Piece{basepoint = 12.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.666666666666668, object = D 8.920685979368205e-16}
                , Piece{basepoint = 12.666666666666668, object = Pd (Poly [0.0])}
                , Piece{basepoint = 12.999999999999998, object = D 1.1995422961264942e-10}
                , Piece{basepoint = 12.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 13.0, object = D 2.4044131430851533e-10}
                , Piece{basepoint = 13.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 13.333333333333332, object = D 6.726553600431706e-10}
                , Piece{basepoint = 13.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 13.333333333333334, object = D 2.3711435109564105e-15}
                , Piece{basepoint = 13.333333333333334, object = Pd (Poly [0.0])}
                , Piece{basepoint = 13.666666666666664, object = D 3.6230496909886033e-10}
                , Piece{basepoint = 13.666666666666664, object = Pd (Poly [0.0])}
                , Piece{basepoint = 13.666666666666666, object = D 7.420749963087072e-10}
                , Piece{basepoint = 13.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 13.999999999999998, object = D 5.9571443883446e-10}
                , Piece{basepoint = 13.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 14.0, object = D 9.7914658261022e-10}
                , Piece{basepoint = 14.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 14.333333333333332, object = D 1.9171201665407293e-9}
                , Piece{basepoint = 14.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 14.666666666666664, object = D 4.167224116004134e-10}
                , Piece{basepoint = 14.666666666666664, object = Pd (Poly [0.0])}
                , Piece{basepoint = 14.666666666666666, object = D 1.5273587660808833e-9}
                , Piece{basepoint = 14.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 14.999999999999998, object = D 6.457643629118824e-10}
                , Piece{basepoint = 14.999999999999998, object = Pd (Poly [0.0])}
                , Piece{basepoint = 15.0, object = D 9.37593962715958e-10}
                , Piece{basepoint = 15.0, object = Pd (Poly [0.0])}
                , Piece{basepoint = 15.333333333333332, object = D 9.761179976504496e-10}
                , Piece{basepoint = 15.333333333333332, object = Pd (Poly [0.0])}
                , Piece{basepoint = 15.666666666666666, object = D 4.076819042214399e-10}
                , Piece{basepoint = 15.666666666666666, object = Pd (Poly [0.0])}
                , Piece{basepoint = 16.0, object = D 8.741054323813941e-11}
                , Piece{basepoint = 16.0, object = Pd (Poly [0.0])}
                ]
            }
        )
    )
  ]
