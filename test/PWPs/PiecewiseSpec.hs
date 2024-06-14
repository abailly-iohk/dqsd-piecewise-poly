module PWPs.PiecewiseSpec where

import PWPs.Piecewise (Piece (..), Pieces (..), mergePieces)
import PWPs.PolyDeltas (PolyDelta (..), aggregate)
import PWPs.SimplePolynomials (Poly (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
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
      mergePieces pieces
        `shouldBe` Pieces [Piece 1 (Pd (Poly [1])), Piece 4 (Pd (Poly [2]))]

  describe "Aggregate" $ do
    it "discard 0 polynomial when points are equal" $ do
      let deltas1 =
            [ (1.0 :: Double, Pd (Poly [0.0]))
            , (2.0, Pd (Poly [1.0]))
            , (2.0, Pd (Poly [0.0]))
            ]
          deltas2 =
            [ (1.0 :: Double, Pd (Poly [0.0]))
            , (1.5 :: Double, Pd (Poly [2.0]))
            , (2.0, Pd (Poly [0.0]))
            , (2.0, Pd (Poly [1.0]))
            ]

      aggregate deltas1
        `shouldBe` [(1.0, Pd (Poly [0.0])), (2.0, Pd (Poly [1.0]))]
      aggregate deltas2
        `shouldBe` [(1.0, Pd (Poly [0.0])), (1.5 :: Double, Pd (Poly [2.0])), (2.0, Pd (Poly [1.0]))]
