module ExOne.Common where

import Prelude hiding (Num)
import Common (Epsilon)
import CommonAbsSyntax (Num)

data Omega = TauTerm Epsilon 
           | TauAbort Epsilon 
           | TauOut (Num, Omega)
           | TauIn (Num -> Omega)

data TermResult = Term Epsilon | Abort Epsilon deriving (Eq, Show)
type OmegaResult = ([Num], TermResult)

evalOmega :: Omega -> [Num] -> OmegaResult
evalOmega omega inputs = (reverse outputs, finalS) where
                            (outputs, finalS) = eval' omega inputs []
                            eval' omg inp acc = case omg of 
                                TauTerm s           -> (acc, Term s)
                                TauAbort s          -> (acc, Abort s)
                                TauOut (n, omega')  -> eval' omega' inp (n : acc)
                                TauIn g             -> case inp of 
                                                            (i:is) -> eval' (g i) is acc
                                                            _      -> undefined