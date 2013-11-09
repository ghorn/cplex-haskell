{-# OPTIONS_GHC -Wall #-}

module CPLEX ( openCPLEX
             , closeCPLEX
             , createProb
             , freeProb
               
             , withEnv
             , withLp
             , run
             ) where

import qualified Data.Vector as V

import Foreign.C
import Foreign.Marshal
import Foreign.Storable

import CPLEX.Bindings
import CPLEX.Param
import HighLevel

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0

run :: IO ()
run = withEnv $ \env@(CPXENV env') -> do
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "testprob" $ \lp@(CPXLP lp') -> do
    let numcols = 3
        numrows = 2
        objsen = CPX_MAX
        obj = V.fromList [1,2,3]
        rhs = V.fromList [L 20, L 30]
        xbnds = [(Just 0, Just 40), (Just 0, Nothing), (Just 0, Nothing)]
        amat = [ (Row 0, Col 0, -1)
               , (Row 1, Col 0, 1)
               , (Row 0, Col 1, 1)
               , (Row 1, Col 1, -3)
               , (Row 0, Col 2, 1)
               , (Row 1, Col 2, 1)
               ]
    status <- copyLp env lp objsen obj rhs amat (V.fromList xbnds)

    case status of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    ------------------------
    qmatbeg <- newArray [0,2,5]
    qmatcnd <- newArray [2,3,2]
    qmatind <- newArray [0,1,0,1,2,1,2]
    qmatval <- newArray [-33,6,6,-22,11.5,11.5,-11]
    status' <- c_CPXcopyquad env' lp' qmatbeg qmatcnd qmatind qmatval
    free qmatbeg
    free qmatcnd
    free qmatind
    free qmatval
    case status' of 0 -> return ()
                    k -> do
                      msg <- cpxGetErrorString env k
                      error $ "CPXcopyquad error: " ++ msg
    ------------------------
    status'' <- c_CPXqpopt env' lp'
    case status'' of 0 -> return ()
                     k -> do
                       msg <- cpxGetErrorString env k
                       error $ "CPXqpopt error: " ++ msg
    ----------------------------
    solstatPtr <- malloc
    objvalPtr <- malloc
    xPtr <- mallocArray numcols
    piPtr <- mallocArray numrows
    slackPtr <- mallocArray numrows
    djPtr <- mallocArray numcols
    
    status''' <- c_CPXsolution env' lp' solstatPtr objvalPtr xPtr piPtr slackPtr djPtr
    
    solstat <- peek solstatPtr
    objval <- peek objvalPtr
    x <- peekArray numcols xPtr
    pi' <- peekArray numrows piPtr
    slack <- peekArray numrows slackPtr
    dj <- peekArray numcols djPtr
    
    free solstatPtr
    free objvalPtr
    free xPtr
    free piPtr
    free slackPtr
    free djPtr
    case status''' of 0 -> return ()
                      k -> do
                        msg <- cpxGetErrorString env k
                        error $ "CPXsolution error: " ++ msg

    putStrLn $ "x      : " ++ show x
    putStrLn $ "pi'    : " ++ show pi'
    putStrLn $ "slack  : " ++ show slack
    putStrLn $ "dj     : " ++ show dj
    putStrLn $ "solstat: " ++ show solstat
    putStrLn $ "objval : " ++ show objval 
    return ()
