{-# OPTIONS_GHC -Wall #-}

module HighLevel ( CPXENV(..)
                 , CPXLP(..)
                 , ObjSense(..)
                 , Sense(..)
                 , Row(..)
                 , Col(..)
                   -- * high level bindings
                 , openCPLEX
                 , closeCPLEX
                 , createProb
                 , freeProb
                 , setIntParam
                 , copyLp
                   -- * low level stuff
                 , cpxGetErrorString
                   -- * convenience wrappers
                 , withEnv
                 , withLp
                 ) where

import qualified Data.Map as M
import Data.Vector.Storable ( Vector )
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import Foreign.C
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr

import CPLEX.Bindings
import CPLEX.Param

newtype CPXENV = CPXENV (Ptr CPXENV')
newtype CPXLP = CPXLP (Ptr CPXLP')

data ObjSense = CPX_MIN | CPX_MAX
instance Enum ObjSense where
  fromEnum CPX_MIN = 1
  fromEnum CPX_MAX = -1
  toEnum 1 = CPX_MIN
  toEnum (-1) = CPX_MAX
  toEnum k = error $ "ObjSense: toEnum: unhandled value: " ++ show k

data Sense = L Double
           | E Double
           | G Double
           | R (Double,Double)

cpx_INFBOUND :: CDouble
cpx_INFBOUND = 1.1e20

cpxGetErrorString :: CPXENV -> CInt -> IO String
cpxGetErrorString (CPXENV env) status = do
  msgPtr <- mallocArray 4096
  _ <- c_CPXgeterrorstring env status msgPtr
  msg <- peekCString msgPtr
  free msgPtr
  return msg

openCPLEX :: IO (Either String CPXENV)
openCPLEX = do
  statusPtr <- malloc
  putStrLn "opening CPLEX..."
  env <- c_CPXopenCPLEX statusPtr
  status <- peek statusPtr
  free statusPtr

  if env == nullPtr
    then do
      -- env is NULL, throw error
      msg <- cpxGetErrorString (CPXENV env) status
      return (Left msg)
    else do
      return (Right (CPXENV env))

closeCPLEX :: CPXENV -> IO ()
closeCPLEX env@(CPXENV env') = do
  -- free env
  putStrLn "closing CPLEX..."
  envPtr <- new env'
  status <- c_CPXcloseCPLEX envPtr
  free envPtr
  case status of
    -- closed successfully
    0 -> do
      return ()
    -- close failed, print error message
    k -> do
      msg <- cpxGetErrorString env k
      error $ "error calling CPXcloseCPLEX: " ++ msg

createProb :: CPXENV -> String -> IO (Either String CPXLP)
createProb env@(CPXENV env') name = do
  statusPtr <- malloc
  putStrLn "creating problem..."
  namePtr <- newCString name
  lp <- c_CPXcreateprob env' statusPtr namePtr
  status <- peek statusPtr
  free statusPtr
  free namePtr

  if lp == nullPtr
    then do
      -- lp is NULL, return error message
      msg <- cpxGetErrorString env status
      return (Left msg)
    else return (Right (CPXLP lp))


freeProb :: CPXENV -> CPXLP -> IO ()
freeProb env@(CPXENV env') (CPXLP lp) = do
  -- free env
  lpPtr <- new lp
  status <- c_CPXfreeprob env' lpPtr
  free lpPtr
  
  case status of
    -- freed successfully
    0 -> return ()
    -- freeing failed, print error message
    k -> do
      msg <- cpxGetErrorString env k
      error $ "error calling CPXfreeprob: " ++ msg



setIntParam :: CPXENV -> CPX_PARAM -> CInt -> IO ()
setIntParam env@(CPXENV env') param val = do
  status <- c_CPXsetintparam env' (paramToInt param) val
  case status of
    0 -> return ()
    k -> do
      putStrLn $ "CPXsetintparam failure settng " ++ show param
      msg <- cpxGetErrorString env k
      error $ "error calling CPXsetintparam: " ++ msg

newtype Row = Row {unRow :: Int}
newtype Col = Col Int deriving (Ord, Eq)

toColForm :: Int -> [(Row,Col,Double)] -> (Vector CInt, Vector CInt, Vector CInt, Vector CDouble)
toColForm numcols amat = (matbeg, matcnt, matind, matval)
  where
    matbeg = SV.fromList $ map fromIntegral begs
    matcnt = SV.fromList $ map fromIntegral cnts
    matind = SV.fromList $ map (fromIntegral . unRow) inds
    matval = SV.fromList $ map realToFrac vals

    -- sort colMap into the from CPLEX wants
    inds :: [Row]
    vals :: [Double]
    (inds,vals) = unzip $ concat rows
    
    begs :: [Int]
    cnts :: [Int]
    rows :: [[(Row,Double)]]
    (begs,cnts,rows) = unzip3 $ colMapInfo' 0 $ M.elems colMap
    
    colMapInfo' :: Int -> [[(Row,Double)]] -> [(Int,Int,[(Row,Double)])]
    colMapInfo' beg (row:xs) = (beg,cnt,row) : colMapInfo' (beg + cnt) xs
      where
        cnt = length row
    colMapInfo' _ [] = []

    -- add Columns with no entries in case some are missing
    colMap = M.union colMap' emptyColMap
    
    emptyColMap :: M.Map Col [(Row,Double)]
    emptyColMap = M.fromList $ take numcols $ zip (map Col [0..]) (repeat [])

    -- a map from Col to all (Row,Double) pairs
    colMap' :: M.Map Col [(Row,Double)]
    colMap' = M.fromListWith (++) preorder

    -- reorganize the (Row,Col,Double) into (Col, [(Row,Double)]) with only 1 (Row,Double)
    preorder :: [(Col,[(Row,Double)])]
    preorder = map (\(row,col,val) -> (col, [(row, val)])) amat


copyLp :: CPXENV -> CPXLP -> ObjSense -> V.Vector Double -> V.Vector Sense -> [(Row,Col,Double)] -> V.Vector (Maybe Double, Maybe Double) -> IO (Maybe String)
copyLp env lp objsense objcoeffs senseRhsRngVal aMat xbnds =
  copyLp' env lp  numcols numrows objsense (SV.fromList (V.toList (V.map realToFrac objcoeffs))) rhs sense matbeg matcnt matind matval lb ub rngval
  where
    numcols = V.length objcoeffs -- or xbnds
    numrows = V.length senseRhsRngVal

    toBnds :: (Maybe Double, Maybe Double) -> (CDouble, CDouble)
    toBnds (Nothing, Nothing) = (-cpx_INFBOUND,  cpx_INFBOUND)
    toBnds ( Just x, Nothing) = ( realToFrac x,  cpx_INFBOUND)
    toBnds (Nothing,  Just y) = (-cpx_INFBOUND,  realToFrac y)
    toBnds ( Just x,  Just y) = ( realToFrac x,  realToFrac y)

    lb = SV.fromList $ V.toList lb'
    ub = SV.fromList $ V.toList ub'
    (lb',ub') = V.unzip $ V.map toBnds xbnds

    toRhs :: Sense -> (CChar, CDouble, CDouble)
    toRhs (L x)     = (castCharToCChar 'L', realToFrac  x,               0)
    toRhs (E x)     = (castCharToCChar 'E', realToFrac  x,               0)
    toRhs (G x)     = (castCharToCChar 'G', realToFrac  x,               0)
    toRhs (R (l,u)) = (castCharToCChar 'R', realToFrac l, realToFrac (u-l))

    sense  = SV.fromList $ V.toList sense'
    rngval = SV.fromList $ V.toList rngval'
    rhs    = SV.fromList $ V.toList rhs'
    (sense', rhs', rngval') = V.unzip3 $ V.map toRhs senseRhsRngVal

    (matbeg, matcnt, matind, matval) = toColForm numcols aMat


copyLp' :: CPXENV -> CPXLP -> Int -> Int -> ObjSense -> Vector CDouble -> Vector CDouble -> Vector CChar -> Vector CInt -> Vector CInt -> Vector CInt -> Vector CDouble -> Vector CDouble -> Vector CDouble -> Vector CDouble -> IO (Maybe String)
copyLp' env@(CPXENV env') (CPXLP lp) numcols numrows objsense obj rhs sense matbeg matcnt matind matval lb ub rngval = do
--  setIntParam env CPX_PARAM_SCRIND cpx_ON
--  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  let objsense' = fromIntegral (fromEnum objsense)
      numcols' = fromIntegral numcols
      numrows' = fromIntegral numrows
      
  status <-
    SV.unsafeWith (SV.map realToFrac obj   )   $ \obj' ->
    SV.unsafeWith (SV.map realToFrac rhs   )   $ \rhs' ->
    SV.unsafeWith (sense )                    $ \sense' ->
    SV.unsafeWith (SV.map fromIntegral matbeg) $ \matbeg' ->
    SV.unsafeWith (SV.map fromIntegral matcnt) $ \matcnt' ->
    SV.unsafeWith (SV.map fromIntegral matind) $ \matind' ->
    SV.unsafeWith (SV.map realToFrac matval)   $ \matval' ->
    SV.unsafeWith (SV.map realToFrac lb    )   $ \lb' ->
    SV.unsafeWith (SV.map realToFrac ub    )   $ \ub' ->
    SV.unsafeWith (SV.map realToFrac rngval)   $ \rngval' ->
    c_CPXcopylp env' lp numcols' numrows' objsense' obj' rhs' sense' matbeg' matcnt' matind' matval' lb' ub' rngval'
  
  case status of 0 -> return Nothing
                 k -> do
                   msg <- cpxGetErrorString env k
                   return $ Just $ "CPXcopylp error: " ++ msg
-------------------------------------------------

withEnv :: (CPXENV -> IO a) -> IO a
withEnv f = do
  env' <- openCPLEX
  case env' of Left msg -> error msg
               Right env -> do
                 ret <- f env
                 closeCPLEX env
                 return ret

withLp :: CPXENV -> String -> (CPXLP -> IO a) -> IO a
withLp env name f = do
  lp' <- createProb env name
  case lp' of Left msg -> error msg
              Right lp -> do
                ret <- f lp
                freeProb env lp
                return ret
