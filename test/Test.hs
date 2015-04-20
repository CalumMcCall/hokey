module Test where
import Test.HUnit
import qualified Distribution.TestSuite as TS
import qualified Distribution.TestSuite.HUnit as H
import Card

testHasPair = TestCase $ assertEqual "Should get a list of cards with a pair here" Nothing (hasPair [(Card Two Spades), (Card Two Clubs), (Card Three Spades)])

testCases :: [(String, Test)]
testCases = [("Test hasPair", testHasPair)]

tests :: IO [TS.Test]
tests = return $ map (uncurry H.test) testCases

--checkRes :: TS.Progress -> Bool
--checkRes (TS.Finished TS.Pass) = True
--checkRes _                     = False
--
--runHUnitTests :: HU.Test -> IO TS.Progress
--runHUnitTests tests = do
--   (HU.Counts cases tried errors failures) <- HU.runTestTT tests
--   return $ if errors > 0
--      then TS.Finished $ TS.Error "There were errors in the HUnit tests"
--      else if failures > 0
--         then TS.Finished $ TS.Fail "There were failures in the HUnit tests"
--         else TS.Finished TS.Pass
-- 
--tests :: IO [TS.Test]
--tests = return [ TS.Test hunit ]
--  where
--    hunit = TS.TestInstance
--        { TS.run = runHUnitTests hunitTests
--        , TS.name = "HUnit Test Cases"
--        , TS.tags = ["hunit"]
--        , TS.options = []
--        , TS.setOption = \_ _ -> Right hunit
--        }
