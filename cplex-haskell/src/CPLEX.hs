{-# OPTIONS_GHC -Wall #-}

module CPLEX ( openCPLEX
             , closeCPLEX
             , createProb
             , freeProb
               
             , withEnv
             , withLp
             , run
             ) where

import Foreign.C
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr

import CPLEX.Bindings
import CPLEX.Param

cpxMessageBufSize :: Int
cpxMessageBufSize = 1024

cpxGetErrorString :: Ptr CPXENV -> CInt -> IO String
cpxGetErrorString env status = do
  msgPtr <- mallocArray cpxMessageBufSize
  c_CPXgeterrorstring env status msgPtr
  msg <- peekCString msgPtr
  free msgPtr
  return msg

openCPLEX :: IO (Ptr CPXENV)
openCPLEX = do
  statusPtr <- malloc
  putStrLn "opening CPLEX..."
  env <- c_CPXopenCPLEX statusPtr

  if env == nullPtr
    then do
      -- env is NULL, throw error
      putStrLn "opening CPLEX failed"
      status <- peek statusPtr
      free statusPtr
      msg <- cpxGetErrorString env status
      error $ "error calling CPXopenCPLEX: " ++ msg
    else do
      putStrLn "opening CPLEX succeeded, calling user function"
      free statusPtr
      return env

closeCPLEX :: Ptr CPXENV -> IO ()
closeCPLEX env = do
  -- free env
  putStrLn "closing CPLEX..."
  envPtr <- new env
  status <- c_CPXcloseCPLEX envPtr
  free envPtr
  case status of
    -- closed successfully
    0 -> do
      putStrLn "closing CPLEX succeeded"
    -- close failed, print error message
    k -> do
      putStrLn "closing CPLEX failed"
      msg <- cpxGetErrorString env k
      error $ "error calling CPXcloseCPLEX: " ++ msg


createProb :: Ptr CPXENV -> String -> IO (Ptr CPXLP)
createProb env name = do
  statusPtr <- malloc
  putStrLn "creating problem..."
  namePtr <- newCString name
  lp <- c_CPXcreateprob env statusPtr namePtr

  if lp == nullPtr
    then do
      -- lp is NULL, throw error
      putStrLn "creating problem failed"
      status <- peek statusPtr
      free statusPtr
      msg <- cpxGetErrorString env status
      error $ "error calling CPXcreateprob: " ++ msg
      
    else free statusPtr >> return lp

freeProb :: Ptr CPXENV -> Ptr CPXLP -> IO ()
freeProb env lp = do
  -- free env
  putStrLn "freeing problem..."
  lpPtr <- new lp
  status <- c_CPXfreeprob env lpPtr
  free lpPtr
  case status of
    -- freed successfully
    0 -> do
      putStrLn "freeing problem succeeded"
    -- freeing failed, print error message
    k -> do
      putStrLn "freeing problem failed"
      msg <- cpxGetErrorString env k
      error $ "error calling CPXfreeprob: " ++ msg


withEnv :: (Ptr CPXENV -> IO a) -> IO a
withEnv f = do
  env <- openCPLEX
  ret <- f env
  closeCPLEX env
  return ret

withLp :: Ptr CPXENV -> String -> (Ptr CPXLP -> IO b) -> IO b
withLp env name f = do
  lp <- createProb env name
  ret <- f lp
  freeProb env lp
  return ret

setIntParam :: Ptr CPXENV -> CPX_PARAM -> CInt -> IO ()
setIntParam env param val = do
  status <- c_CPXsetintparam env (paramToInt param) val
  case status of
    0 -> return ()
    k -> do
      putStrLn $ "CPXsetintparam failure settng " ++ show param
      msg <- cpxGetErrorString env k
      error $ "error calling CPXsetintparam: " ++ msg

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0
cpx_MAX :: CInt
cpx_MAX = -1
cpx_MIN :: Integer
cpx_MIN =  1
cpx_INFBOUND :: CDouble
cpx_INFBOUND = 1.0e20

run :: IO ()
run = withEnv $ \env -> do
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "testprob" $ \lp -> do
    let numcols = 3
        numrows = 2
        objsen = cpx_MAX
    obj <- newArray [1,2,3]
    rhs <- newArray [20, 30]
    sense <- newArray $ map castCharToCChar ['L','L']
    matbeg <- newArray [0,2,4]
    matcnt <- newArray [2,2,2]
    matind <- newArray [0,1,0,1,0,1]
    matval <- newArray [-1,1,1,-3,1,1,20,30]
    lb <- newArray [0,0,0]
    ub <- newArray [40, cpx_INFBOUND, cpx_INFBOUND]
        
    status <- c_CPXcopylp env lp (fromIntegral numcols) (fromIntegral numrows) objsen obj rhs sense matbeg matcnt matind matval lb ub nullPtr

    free obj
    free rhs
    free sense
    free matbeg
    free matcnt
    free matind
    free matval
    free lb
    free ub

    case status of 0 -> return ()
                   k -> do
                     msg <- cpxGetErrorString env k
                     error $ "CPXcopylp error: " ++ msg

    ------------------------
    qmatbeg <- newArray [0,2,5]
    qmatcnd <- newArray [2,3,2]
    qmatind <- newArray [0,1,0,1,2,1,2]
    qmatval <- newArray [-33,6,6,-22,11.5,11.5,-11]
    status' <- c_CPXcopyquad env lp qmatbeg qmatcnd qmatind qmatval
    free qmatbeg
    free qmatcnd
    free qmatind
    free qmatval
    case status' of 0 -> return ()
                    k -> do
                      msg <- cpxGetErrorString env k
                      error $ "CPXcopyquad error: " ++ msg
    ------------------------
    status'' <- c_CPXqpopt env lp
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
    
    status''' <- c_CPXsolution env lp solstatPtr objvalPtr xPtr piPtr slackPtr djPtr
    
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
