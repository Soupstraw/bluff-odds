module Main where

import Control.Monad
import Control.Lens

import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Linear.V2

import Plots
import Diagrams.Backend.SVG.CmdLine

import qualified Options.Applicative as Opt (Parser)
import Options.Applicative hiding (Parser)

import System.Random.Shuffle

import Debug.Trace

data PokerHand
  = HighCard Card
  | OnePair (Card, Card)
  | TwoPair (Card, Card) (Card, Card)
  | ThreeOfAKind (Card, Card, Card)
  | Straight (Card, Card, Card, Card, Card)
  | Flush (Card, Card, Card, Card, Card)
  | FullHouse (Card, Card, Card) (Card, Card)
  | FourOfAKind (Card, Card, Card, Card)
  | StraightFlush (Card, Card, Card, Card, Card)
  deriving (Show, Eq, Ord)

constrEnum :: PokerHand -> Int
constrEnum HighCard{}      = 0
constrEnum OnePair{}       = 1
constrEnum TwoPair{}       = 2
constrEnum ThreeOfAKind{}  = 3
constrEnum Straight{}      = 4
constrEnum Flush{}         = 5
constrEnum FullHouse{}     = 6
constrEnum FourOfAKind{}   = 7
constrEnum StraightFlush{} = 8

follows :: Card -> Card -> Bool
follows x y
  | _cRank x == R2 && _cRank y == RA = True
  | _cRank y == RA = False
  | otherwise = _cRank x == succ (_cRank y)

handStrength :: [Card] -> PokerHand
handStrength hand = fromMaybe (highCard hand) $ asum
  [ straightFlush sorted
  , fourOfAKind sorted
  , fullHouse sorted
  , flush sorted
  , straight sorted
  , threeOfAKind sorted
  , twoPair sorted
  , onePair sorted
  ]
  where
    sorted = sort hand
    highCard = HighCard . maximum
    fullHouse :: [Card] -> Maybe PokerHand
    fullHouse hand =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) hand
        xIdx <- findIndex ((>=3) . length) ranked
        let (l, r') = splitAt xIdx ranked
        let r = tail r'
        yIdx <- findIndex ((>=2) . length) $ l <> r
        let x1:x2:x3:_ = ranked !! xIdx
        let y1:y2:_    = (l <> r) !! yIdx
        return $ FullHouse (x1, x2, x3) (y1, y2)

    straightFlush :: [Card] -> Maybe PokerHand
    straightFlush hand =
      do
        let suited = groupBy (\x y -> _cSuit x == _cSuit y) $ sortOn _cSuit hand
        let straightsBySuit = catMaybes $ straight <$> suited
        Straight x <- if null straightsBySuit
                        then Nothing
                        else Just $ maximum straightsBySuit
        return $ StraightFlush x
    flush :: [Card] -> Maybe PokerHand
    flush hand = 
      do
        let suited = groupBy (\x y -> _cSuit x == _cSuit y) $ sortOn _cSuit hand
        [x1, x2, x3, x4, x5] <- find ((>= 5) . length) suited
        return $ Flush (x1, x2, x3, x4, x5)
    fourOfAKind :: [Card] -> Maybe PokerHand
    fourOfAKind hand =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) hand
        [x1, x2, x3, x4] <- find ((==4) . length) ranked
        return $ FourOfAKind (x1, x2, x3, x4)

    threeOfAKind :: [Card] -> Maybe PokerHand
    threeOfAKind hand =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) hand
        [x1, x2, x3] <- find ((==3) . length) ranked
        return $ ThreeOfAKind (x1, x2, x3)
    onePair :: [Card] -> Maybe PokerHand
    onePair hand =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) hand
        [x1, x2] <- find ((==2) . length) ranked
        return $ OnePair (x1, x2)
    twoPair :: [Card] -> Maybe PokerHand
    twoPair hand =
      do
        let ranked = groupBy (\x y -> _cRank x == _cRank y) hand
        let idxs = findIndices ((==2) . length) ranked
        guard $ length idxs >= 2
        let x:y:_ = idxs
        let p1 = ranked !! x
        let p2 = ranked !! y
        return $ TwoPair (head p1, last p1) (head p2, last p2)
    straight :: [Card] -> Maybe PokerHand
    straight hand' = 
      do
        let aces = filter ((==RA) . _cRank) hand'
        let hand = aces <> hand'
        let checkStraight (x1:x2:x3:x4:x5:t)
              | x1 `follows` x2 &&
                x2 `follows` x3 &&
                x3 `follows` x4 &&
                x4 `follows` x5 = Just $ Straight (x1, x2, x3, x4, x5)
              | otherwise = checkStraight $ x2:x3:x4:x5:t
            checkStraight _ = Nothing
        --traceShowM $ sortOn (Down . _cRank) hand
        --traceShowM $ reverse hand
        checkStraight $ reverse hand

data Rank
  = R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | RT
  | RJ
  | RQ
  | RK
  | RA
  deriving (Enum, Eq, Ord, Show)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Enum, Eq, Ord)

instance Show Suit where
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"
  show Spades   = "♠"

data Card = Card 
  { _cRank :: Rank 
  , _cSuit :: Suit
  } deriving (Eq, Ord)

instance Show Card where
  show c = show (_cRank c) <> show (_cSuit c)

instance Enum Card where
  fromEnum (Card r s) = fromEnum r * 4 + fromEnum s
  toEnum x = Card (toEnum $ x `div` 4) (toEnum $ x `mod` 4)

newtype Config = Config
  { _cfgInput :: FilePath
  }

parseArgs :: Opt.Parser Config
parseArgs = Config
  <$> strOption ( mconcat
        [ long "input"
        ])

deck :: [Card]
deck = Card <$> [R2 ..] <*> [Clubs ..]

axis :: [[Double]] -> Axis B V2 Double
axis res = r2Axis &~
  do
    heatMap res $ return ()

main :: IO ()
main = 
  do
--    cfg <- execParser $ info parseArgs fullDesc
--    inp <- if null $ _cfgInput cfg
--      then return []
--      else readFile $ _cfgInput cfg
    let iterations = 50000
    res <- forM [1..52] $ \cardCount -> do
      print cardCount
      let b = M.fromList ([0..8] `zip` repeat 0)
      x <- forM [0..iterations] $ \_ -> do
        shuffled <- take cardCount <$> shuffleM deck
        return $ M.singleton (constrEnum $ handStrength shuffled) 1
      return . fmap (/iterations) . M.elems $ M.unionsWith (+) (b:x)
    r2AxisMain $ axis res

