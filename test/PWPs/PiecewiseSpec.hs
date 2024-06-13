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
