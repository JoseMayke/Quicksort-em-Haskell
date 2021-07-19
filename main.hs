module QuicksortStringInt where

import Data.Char

type Nome = String
type Numero = Int
type NomeNumero = (Nome,Numero)
type ListaNN = [NomeNumero]

menor :: (ListaNN,NomeNumero) -> ListaNN
menor(resto,(nome,num)) = [(a,b)| (a,b) <- resto, map(toLower) nome > map(toLower) a ]

maior :: (ListaNN,NomeNumero) -> ListaNN
maior(resto,(nome,num)) = [(a,b)| (a,b) <- resto, map(toLower) nome < map(toLower) a || map(toLower) nome == map(toLower) a ]

quicksort :: (ListaNN) -> ListaNN
quicksort(l)
  | null l   = l
  | otherwise = let pivo = head l                
                in quicksort(menor(tail l,pivo)) ++ [pivo] ++ quicksort(maior(tail l,pivo))
