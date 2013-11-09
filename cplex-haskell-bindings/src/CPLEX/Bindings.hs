{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module CPLEX.Bindings ( CPXENV'
                      , CPXLP'
                      , c_CPXopenCPLEX
                      , c_CPXcloseCPLEX
                      , c_CPXcreateprob
                      , c_CPXfreeprob
                      , c_CPXnewrows
                      , c_CPXaddrows
                      , c_CPXnewcols
                      , c_CPXaddcols
                      , c_CPXchgcoeflist
                      , c_CPXchgcoef
                      , c_CPXchgrhs
                      , c_CPXchgrngval
                      , c_CPXchgbds
                      , c_CPXchgobj
                      , c_CPXgeterrorstring
                      , c_CPXgetstatstring
                      , c_CPXsetintparam
                      , c_CPXsetdblparam
                      , c_CPXgetnumcols
                      , c_CPXgetnumrows
                      , c_CPXcopylp
                      , c_CPXcopyquad
                      , c_CPXqpopt
                      , c_CPXsolution
                      ) where

import Foreign.C ( CInt(..), CDouble(..), CChar(..) )
import Foreign.Ptr ( Ptr )

data CPXENV'
data CPXLP'

foreign import ccall unsafe "cplex.h CPXopenCPLEX" c_CPXopenCPLEX :: Ptr CInt -> IO (Ptr CPXENV')
foreign import ccall unsafe "cplex.h CPXcloseCPLEX" c_CPXcloseCPLEX :: Ptr (Ptr CPXENV') -> IO CInt

foreign import ccall unsafe "cplex.h CPXcreateprob" c_CPXcreateprob ::
  Ptr CPXENV' -> Ptr CInt -> Ptr CChar -> IO (Ptr CPXLP')
foreign import ccall unsafe "cplex.h CPXfreeprob" c_CPXfreeprob ::
  Ptr CPXENV' -> Ptr (Ptr CPXLP') -> IO CInt

foreign import ccall unsafe "cplex.h CPXnewrows" c_CPXnewrows ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> Ptr CDouble -> Ptr CChar -> Ptr CDouble -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXaddrows" c_CPXaddrows ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr (Ptr CChar) -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXnewcols" c_CPXnewcols ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXaddcols" c_CPXaddcols ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgcoeflist" c_CPXchgcoeflist ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgcoef" c_CPXchgcoef ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> CInt -> CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgrhs" c_CPXchgrhs ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgobj" c_CPXchgobj ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgbds" c_CPXchgbds ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> Ptr CInt -> Ptr CChar -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXchgrngval" c_CPXchgrngval ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXgeterrorstring" c_CPXgeterrorstring ::
  Ptr CPXENV' -> CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import ccall unsafe "cplex.h CPXgetstatstring" c_CPXgetstatstring ::
  Ptr CPXENV' -> CInt -> Ptr CChar -> IO (Ptr CChar)

foreign import ccall unsafe "cplex.h CPXsetintparam" c_CPXsetintparam ::
  Ptr CPXENV' -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "cplex.h CPXsetdblparam" c_CPXsetdblparam ::
  Ptr CPXENV' -> CInt -> CDouble -> IO CInt

foreign import ccall unsafe "cplex.h CPXgetnumcols" c_CPXgetnumcols ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt

foreign import ccall unsafe "cplex.h CPXgetnumrows" c_CPXgetnumrows ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt

foreign import ccall unsafe "cplex.h CPXcopylp" c_CPXcopylp ::
  Ptr CPXENV' -> Ptr CPXLP' -> CInt -> -- (CPXCENVptr env, CPXLP'ptr lp, int numcols,
  CInt -> CInt -> Ptr CDouble ->     --  int numrows, int objsense, const double *objective,
  Ptr CDouble -> Ptr CChar ->        --  const double *rhs, const char *sense,
  Ptr CInt -> Ptr CInt ->            --  const int *matbeg, const int *matcnt,
  Ptr CInt -> Ptr CDouble ->         --  const int *matind, const double *matval,
  Ptr CDouble -> Ptr CDouble ->      --  const double *lb, const double *ub,
  Ptr CDouble                        --  const double *rngval);
  -> IO CInt


foreign import ccall unsafe "cplex.h CPXcopyquad" c_CPXcopyquad ::
  Ptr CPXENV' -> Ptr CPXLP' -> Ptr CInt -> -- (CPXCENVptr env, CPXLP'ptr lp, const int *qmatbeg,
  Ptr CInt -> Ptr CInt ->                --  const int *qmatcnt, const int *qmatind,
  Ptr CDouble ->                         --  const double *qmatval);
  IO CInt

foreign import ccall unsafe "cplex.h CPXqpopt" c_CPXqpopt ::
  Ptr CPXENV' -> Ptr CPXLP' -> IO CInt

foreign import ccall unsafe "cplex.h CPXsolution" c_CPXsolution ::
  Ptr CPXENV' -> Ptr CPXLP' -> Ptr CInt ->       -- (CPXCENVptr env, CPXCLPptr lp, int *lpstat_p,
  Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> --  double *objval_p, double *x, double *pi,
  Ptr CDouble -> Ptr CDouble -> IO CInt        --  double *slack, double *dj);
