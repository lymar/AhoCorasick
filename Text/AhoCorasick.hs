{-# LANGUAGE RankNTypes, ScopedTypeVariables, ExistentialQuantification #-}
-- Module:      Text.Hastache
-- Copyright:   Sergey S Lymar (c) 2012
-- License:     BSD3
-- Maintainer:  Sergey S Lymar <sergey.lymar@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Aho-Corasick string matching algorithm

{- | Aho-Corasick string matching algorithm

Simplest example:

@
example1 = mapM_ print $ findAll simpleSM \"ushers\" where
    simpleSM = makeSimpleStateMachine [\"he\",\"she\",\"his\",\"hers\"]
@

@
Position {pIndex = 1, pLength = 3, pVal = \"she\"}
Position {pIndex = 2, pLength = 2, pVal = \"he\"}
Position {pIndex = 2, pLength = 4, pVal = \"hers\"}
@

With data:

@
example2 = mapM_ print $ findAll sm \"ushers\" where
    sm = makeStateMachine [(\"he\",0),(\"she\",1),(\"his\",2),(\"hers\",3)]
@

@
Position {pIndex = 1, pLength = 3, pVal = 1}
Position {pIndex = 2, pLength = 2, pVal = 0}
Position {pIndex = 2, pLength = 4, pVal = 3}
@

Step-by-step state machine evaluation:

@
example3 = mapM_ print $ next sm \"ushers\" where
    sm = makeSimpleStateMachine [\"he\",\"she\",\"his\",\"hers\"]
    next _ [] = []
    next sm (s:n) = let (SMStepRes match nextSM) = stateMachineStep sm s in
        (s, match) : next nextSM n
@

@
('u',[])
('s',[])
('h',[])
('e',[(3,\"she\"),(2,\"he\")])
('r',[])
('s',[(4,\"hers\")])
@
-}
module Text.AhoCorasick (
      makeStateMachine
    , makeSimpleStateMachine
    , findAll
    , Position(..)
    , stateMachineStep
    , KeyLength
    , SMStepRes(..)
    , resetStateMachine
    ) where

import Control.Monad.State.Lazy (execStateT, get, put)
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.Trans (lift)
import Data.Array.IArray (Array, array, (!))
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import qualified Data.HashMap.Strict as M

import Text.AhoCorasick.Internal.Deque (mkDQ, pushBack, popFront, dqLength, DQ)

data (Eq keySymb, Hashable keySymb) => TNode keySymb s = 
    TNode {
          tnId          :: Int
        , tnLinks       :: M.HashMap keySymb (STRef s (TNode keySymb s))
        , tnFail        :: Maybe (STRef s (TNode keySymb s))
        , tnValuesIds   :: [Int]
    }

type KeyLength = Int

data (Eq keySymb, Hashable keySymb) => TTree keySymb val s = 
    TTree {
          ttRoot        :: STRef s (TNode keySymb s)
        , ttLastId      :: STRef s Int
        , ttValues      :: DQ (KeyLength, val) s
    }

type NodeIndex = Int

data (Eq keySymb, Hashable keySymb) => SMElem keySymb = 
    SMElem {
          smeLinks      :: M.HashMap keySymb NodeIndex
        , smeFail       :: NodeIndex
        , smeValuesIds  :: [Int]
    }
    
data (Eq keySymb, Hashable keySymb) => StateMachine keySymb val = 
    StateMachine {
          smStates      :: Array NodeIndex (SMElem keySymb)
        , smValues      :: Array Int (KeyLength, val)
        , smState       :: Int
    }

data (Eq keySymb, Hashable keySymb) => SMStepRes keySymb val = 
    SMStepRes {
          smsrMatch     :: [(KeyLength, val)]
        , smsrNextSM    :: StateMachine keySymb val
    }

data Position val =
    Position {
          pIndex        :: Int
        , pLength       :: Int
        , pVal          :: val
    }

instance (Eq keySymb, Hashable keySymb, Show keySymb) => 
                                                    Show (SMElem keySymb) where
    show (SMElem l f v) = concat ["SMElem {smeLinks = ", show l,
        ", smeFail = ", show f,", smeValuesIds = ", show v, "}"]

instance (Eq keySymb, Hashable keySymb, Show keySymb, Show val) => 
                                        Show (StateMachine keySymb val) where
    show (StateMachine st vals state) = concat [
        "StateMachine {smStates = ", show st,
        ", smValues = ", show vals, ", smState = ", show state,"}"]

instance (Eq keySymb, Hashable keySymb, Show keySymb, Show val) => 
                                            Show (SMStepRes keySymb val) where
    show (SMStepRes f n) = concat [
        "StateMachineStepRes {smsrFound = ", show f,
        ", smsrNewSM = ", show n,"}"]

instance (Show val) => Show (Position val) where
    show (Position i l v) = concat [
        "Position {pIndex = ", show i,
        ", pLength = ", show l,
        ", pVal = ", show v,"}"]

x ~> f = f x
infixl 9 ~>

rootNodeId :: Int
rootNodeId = 0

initNewTTree :: (Eq keySymb, Hashable keySymb) => ST s (TTree keySymb a s)
initNewTTree = do
    root <- newSTRef $ TNode rootNodeId M.empty Nothing []
    lid <- newSTRef rootNodeId
    kw <- mkDQ
    return $ TTree root lid kw
    
mkNewTNode :: (Eq keySymb, Hashable keySymb) => 
    TTree keySymb a s -> ST s (TNode keySymb s)
mkNewTNode tree = do
    modifySTRef lid (+1)
    lv <- readSTRef lid
    return $ TNode lv M.empty Nothing []
    where
    lid = ttLastId tree

addKeyVal :: forall val s keySymb. (Eq keySymb, Hashable keySymb) => 
    TTree keySymb val s -> [keySymb] -> val -> ST s ()
addKeyVal tree key val = addSymb (ttRoot tree) key
    where
    addSymb :: STRef s (TNode keySymb s) -> [keySymb] -> ST s ()
    addSymb node [] = do
        vi <- dqLength (ttValues tree)
        pushBack (ttValues tree) (length key, val)
        modifySTRef node (\r -> r { tnValuesIds = [vi] })
    addSymb node (c:nc) = do
        n <- readSTRef node
        let nlnks = tnLinks n
        case M.lookup c nlnks of
            Just tn -> addSymb tn nc
            Nothing -> do
                nnd <- mkNewTNode tree
                refNewN <- newSTRef nnd
                writeSTRef node (n {tnLinks = M.insert c refNewN nlnks})
                addSymb refNewN nc

findFailures :: (Eq keySymb, Hashable keySymb) => TTree keySymb val s -> ST s ()
findFailures tree = do
    modifySTRef root (\n -> n {tnFail = Just root})
    dq <- mkDQ
    pushBack dq root
    procAll dq
    where
    root = ttRoot tree
    procAll dq = do
        n <- popFront dq
        case n of
            Nothing -> return ()
            Just node -> do
                procNode dq node
                procAll dq
    procNode dq nodeRef = do
        node <- readSTRef nodeRef
        mapM_ (\(symb, link) -> do
            pushBack dq link
            fRef <- findParentFail link (tnFail node) symb
            f <- readSTRef fRef
            modifySTRef link (\n -> n {tnFail = Just fRef, 
                tnValuesIds = (tnValuesIds n) ++ (tnValuesIds f)})
            ) $ tnLinks node ~> M.toList
        return ()
    findParentFail link (Just cfRef) symb = do
        cf <- readSTRef cfRef
        case (M.lookup symb (tnLinks cf), cfRef == root) of
            (Just nl, _) -> if nl == link
                then return root
                else return nl
            (Nothing, True) -> return root
            _ -> findParentFail link (tnFail cf) symb

convertToStateMachine :: forall val s keySymb. (Eq keySymb, Hashable keySymb) => 
    TTree keySymb val s -> 
    ST s (StateMachine keySymb val)
convertToStateMachine tree = do
    size <- readSTRef $ ttLastId tree
    nds <- execStateT (convertNode $ ttRoot tree) []
    
    vlsSize <- dqLength $ ttValues tree
    vls <- mapM (\i -> do
        k <- popFront (ttValues tree)
        return (i,fromJust k)
        ) [0..(vlsSize-1)]
    
    StateMachine (array (0, size) nds) (array (0, vlsSize-1) vls) rootNodeId
        ~> return
    where
    convertNode node = do
        (n,l,fail) <- lift $ do
            n <- readSTRef node
            l <- tnLinks n ~> convertLinks
            fail <- tnFail n ~> fromJust ~> readSTRef >>= return . tnId
            return (n,l,fail)
        v <- get
        put $ (tnId n, SMElem l fail (tnValuesIds n)) : v
        M.toList (tnLinks n) ~> map snd ~> mapM_ convertNode

    convertLinks :: M.HashMap keySymb (STRef s (TNode keySymb s)) ->
        ST s (M.HashMap keySymb Int)
    convertLinks lnksMap = do
        nl <- mapM (\(symb, link) -> do
            l <- readSTRef link
            return $ (symb, tnId l)
            ) $ M.toList lnksMap
        return $ M.fromList nl

resetStateMachine :: (Eq keySymb, Hashable keySymb) => 
    StateMachine keySymb val -> StateMachine keySymb val
resetStateMachine m = m { smState = rootNodeId }

stateMachineStep :: (Eq keySymb, Hashable keySymb) => 
    StateMachine keySymb val -> keySymb -> SMStepRes keySymb val
stateMachineStep sm symb =
    case (M.lookup symb links, currentState == rootNodeId) of
        (Just nextState, _) -> SMStepRes
            ((smStates sm) ! nextState ~> smeValuesIds ~> convertToVals)
            (sm { smState = nextState })
        (Nothing, True) -> SMStepRes [] sm
        (Nothing, False) -> stateMachineStep 
            (sm { smState = smeFail currentNode}) symb            
    where
    currentState = smState sm
    currentNode = (smStates sm) ! currentState
    links = smeLinks currentNode
    convertToVals idx = map (\i -> smValues sm ! i) idx

findAll :: (Eq keySymb, Hashable keySymb) => 
    StateMachine keySymb val -> [keySymb] -> [Position val]
findAll sm str = 
    step (resetStateMachine sm) (zip [0..] str) ~> concat
    where
    step _ [] = []
    step csm ((idx,symb):next) = case stateMachineStep csm symb of
        SMStepRes [] newsm -> step newsm next
        SMStepRes r newsm -> (map (cnvToPos idx) r) : (step newsm next)      
    cnvToPos idx (keyLength, val) = Position (idx - keyLength + 1) keyLength val

makeSimpleStateMachine :: (Eq keySymb, Hashable keySymb) => 
    [[keySymb]] -> StateMachine keySymb [keySymb]
makeSimpleStateMachine keys = runST $ do
    tree <- initNewTTree
    mapM_ (\s -> addKeyVal tree s s) keys
    findFailures tree
    convertToStateMachine tree

makeStateMachine :: (Eq keySymb, Hashable keySymb) => 
    [([keySymb], val)] -> StateMachine keySymb val
makeStateMachine kv = runST $ do
    tree <- initNewTTree
    mapM_ (\(s,v) -> addKeyVal tree s v) kv
    findFailures tree
    convertToStateMachine tree
