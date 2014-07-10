module DLL
       where

data DLL a = EmptyDLL | DLL { value :: a, left :: DLL a, right :: DLL a }

init_CDLL v = let dll = DLL v dll dll
                       in dll

init_DLL v = DLL v EmptyDLL EmptyDLL

append_to_DLL dll@(DLL vdll ldll rdll) nv = let ndll = DLL vdll  ldll ( DLL nv ndll rdll )
                                            in ndll


