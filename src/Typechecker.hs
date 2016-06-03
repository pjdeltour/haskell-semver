{-
This module was copied from the https://hackage.haskell.org/package/hermit
package

Copyright (c) 2011, The University of Kansas
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}
module Typechecker
    (
      initTcFromModGuts
    , tcLookupGlobal
    ) where

import RdrName
import TcRnMonad
import ErrUtils
import VarEnv
import NameEnv
import SrcLoc
import HscTypes
import Outputable
import Data.IORef ( newIORef, readIORef )

import TcEnv ( tcLookupGlobal )
import TcType   ( topTcLevel )

import FastString
import Bag

import Prelude hiding (mod)
import VarSet (emptyVarSet)

-- Note: the contents of this module should eventually be folded into GHC proper.

-- | Re-Setup the typechecking environment from a ModGuts
initTcFromModGuts
    :: HscEnv
    -> TcGblEnv
    -> TcM r
    -> IO (Messages, Maybe r) -- Nothing => error thrown by the thing inside
                              -- (error messages should have been printed already)
initTcFromModGuts hsc_env gbl_env do_this
 = do { errs_var     <- newIORef (emptyBag, emptyBag) ;
        tvs_var      <- newIORef emptyVarSet ;
        lie_var      <- newIORef emptyWC ;

        let {
             dflags = hsc_dflags hsc_env ;

             lcl_env = TcLclEnv {
                tcl_errs       = errs_var,
                tcl_loc        = realSrcLocSpan $ mkRealSrcLoc (fsLit "Top level") 1 1,
                tcl_ctxt       = [],
                tcl_rdr        = emptyLocalRdrEnv,
                tcl_th_ctxt    = topStage,
                tcl_th_bndrs   = emptyNameEnv,
                tcl_arrow_ctxt = NoArrowCtxt,
                tcl_env        = emptyNameEnv,
                tcl_bndrs      = [],
                tcl_tidy       = emptyTidyEnv,
                tcl_tyvars     = tvs_var,
                tcl_lie        = lie_var,
                tcl_tclvl      = topTcLevel
             } ;
        } ;

        -- OK, here's the business end!
        maybe_res <- initTcRnIf 'a' hsc_env gbl_env lcl_env $
                     do { r <- tryM do_this
                        ; case r of
                          Right res -> return (Just res)
                          Left _    -> return Nothing } ;


        -- Check for unsolved constraints
       lie <- readIORef lie_var ;
       let {unsolv_error = not $ isEmptyWC lie};

        -- Collect any error messages
       msgs <- readIORef errs_var ;

       let { final_res | unsolv_error            = Nothing
                       | errorsFound dflags msgs = Nothing
                       | otherwise               = maybe_res } ;

        return (msgs, final_res)
    }
