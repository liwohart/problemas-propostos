{-# LANGUAGE CPP              #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

#ifndef RELEASE
import           Control.Lens       (view, _3)
import           Data.Matrix        (mapPos)
#endif

import           Control.Monad      (forM_)
import           Data.Function      (on)
import           Data.Matrix        (Matrix, forceMatrix, matrix, submatrix,
                                     (!))
import           Data.Maybe         (listToMaybe)
import           Data.Vector        (Vector)
import qualified Data.Vector        as V (enumFromThenTo, fromList, iterateN,
                                          map, minimumBy, sum, (!))
import           System.Environment (getArgs)
import           Text.Printf        (printf)

type Mes = Int

type Estoque = Int

type Pedido = Int

type Demanda = Int

type Estado = (Estoque, Mes)

type Custo = Double

type Indice = (Int, Int)

#ifndef RELEASE
tabelaDeCustosTotais :: Matrix Custo
tabelaDeCustosTotais =
  forceMatrix $ submatrix 1 5 1 6 $ snd <$> tabela

tabelaDeCustos :: Matrix Custo
tabelaDeCustos = mapPos (custo . ixmap) tabelaDePedidos

{-# INLINE politicaOtima #-}
politicaOtima :: Estoque -> Vector Pedido
politicaOtima e = view _3 <$> tabelaDescritiva e

{-# INLINE ceilCentavos #-}
ceilCentavos :: Custo -> Custo
ceilCentavos = (/ 100) . fromIntegral @Integer . ceiling . (* 100)
#endif

{-# INLINE mes #-}
mes :: Mes -> String
mes =
  (V.fromList
     [ "Novembro"
     , "Dezembro"
     , "Janeiro"
     , "Fevereiro"
     , "Março"
     , "Abril"
     ] V.!)

{-# INLINE demanda #-}
demanda :: Vector Demanda
demanda = V.fromList [400, 200, 300, 400, 300, 200]

{-# INLINE custoEstoque #-}
custoEstoque :: Estoque -> Custo
custoEstoque = (V.fromList [0.0, 20.0, 40.0, 60.0, 80.0] V.!)

{-# INLINE custoPedido #-}
custoPedido :: Pedido -> Custo
custoPedido =
  (V.fromList [0.0, 389.5, 769.5, 1089.0, 1288.0, 1507.5] V.!)

{-# INLINE novoEstoque #-}
novoEstoque :: Estado -> Pedido -> Estoque
novoEstoque (e, m) p = e - demanda V.! m + p

{-# INLINE pedidosPossiveis #-}
pedidosPossiveis :: Estado -> Vector Pedido
pedidosPossiveis (e,m) =
  let aux = demanda V.! m - e
      inf = max 0 aux
      sup = min 500 $ aux + 400
   in V.enumFromThenTo inf (inf + 100) sup

{-# INLINE ixmap #-}
ixmap :: Indice -> Estado
ixmap t@(e, m)
  | 1 <= e && e <= 5 && 1 <= m && m <= 7 = ((e - 1) * 100, m - 1)
  | otherwise =
    error $ "ixmap: indice fora do intervalo " ++ show t

{-# INLINE invIxmap #-}
invIxmap :: Estado -> Indice
invIxmap t@(e, m)
  | 0 <= e && e <= 400 && 0 <= m && m <= 6 =
    (e `div` 100 + 1, m + 1)
  | otherwise =
    error $ "invIxmap: indice fora do intervalo " ++ show t

{-# INLINE custo #-}
custo :: Estado -> Pedido -> Custo
custo estado p =
  custoEstoque (novoEstoque estado p `div` 100) +
  custoPedido (p `div` 100)

tabela :: Matrix (Pedido, Custo)
tabela =
  matrix 5 7
    (\case
       (_, 7) -> (0, 0)
       i ->
         let indice@(_, m) = ixmap i
          in V.minimumBy (compare `on` snd) $
             V.map
               (\p ->
                  ( p
                  , custo indice p +
                    snd
                      (tabela !
                       invIxmap (novoEstoque indice p, m + 1)))) $
             pedidosPossiveis indice)

tabelaDePedidos :: Matrix Pedido
tabelaDePedidos = forceMatrix . submatrix 1 5 1 6 $ fst <$> tabela

tabelaDescritiva ::
     Estoque -> Vector (Estoque, Mes, Pedido, Demanda, Custo)
tabelaDescritiva estoque =
  V.iterateN
    6
    passo
    (estoque, 0, pedido, demanda V.! 0, custo (estoque, 0) pedido)
  where
    pedido = tabelaDePedidos ! (estoque `div` 100 + 1, 1)
    passo ::
         (Estoque, Mes, Pedido, Demanda, Custo)
      -> (Estoque, Mes, Pedido, Demanda, Custo)
    passo (e, m, p, _, _) =
      let nI@(nE, nM) = (novoEstoque (e, m) p, m + 1)
       in ( nE
          , nM
          , tabelaDePedidos ! invIxmap nI
          , demanda V.! nM
          , custo nI $ tabelaDePedidos ! invIxmap nI)

{-# INLINE roundCentavos #-}
roundCentavos :: Custo -> Custo
roundCentavos = (/ 100) . fromIntegral @Int . round . (* 100)

main :: IO ()
main = do
  iEstoque <- maybe 1 read . listToMaybe <$> getArgs
  if 1 <= iEstoque && iEstoque <= 5
    then do
      let custoTotal = snd $ tabela ! (iEstoque, 1)
          totalVendido = V.sum demanda
          precoPorProduto =
            roundCentavos $ 2.5 * custoTotal / fromIntegral totalVendido
          faturamento = fromIntegral totalVendido * precoPorProduto
          lucro = faturamento - custoTotal
      putChar '\n'
      putStrLn "\t\\begin{table}[ht]"
      putStrLn "\t\t\\centering"
      putStrLn "\t\t\\begin{tabular}{lrrrr}"
      putStrLn "\t\t\t\\toprule"
      putStrLn "\t\t\tMês       & Estoque & Demanda & Pedido &       Custo \\\\"
      putStrLn "\t\t\t\\midrule"
      forM_
        (tabelaDescritiva $ 100 * (iEstoque - 1))
        (\(e, m, p, d, c) ->
           printf
             "\t\t\t%-9s &  %3d un &  %3d un & %3d un & R\\$ %7.2G \\\\\n"
             (mes m) e d p c)
      putStrLn "\t\t\t\\bottomrule"
      putStrLn "\t\t\\end{tabular}"
      putStrLn "\t\\end{table}"
      putChar '\n'
      putStrLn "\t\\begin{table}[ht]"
      putStrLn "\t\t\\centering"
      putStrLn "\t\t\\begin{tabular}{lr}"
      putStrLn "\t\t\t\\toprule"
      printf "\t\t\tTotal vendido   & %9d un    \\\\\n" totalVendido
      printf "\t\t\tCusto total     & R\\$ %8.2G    \\\\\n" custoTotal
      printf "\t\t\tPreço de venda  & R\\$ %8.2G    \\\\\n" precoPorProduto
      printf "\t\t\tFaturamento     & R\\$ %8.2G    \\\\\n" faturamento
      printf "\t\t\tLucro           & R\\$ %8.2G    \\\\\n" lucro
      printf "\t\t\tTaxa de retorno & %13.3f\\%% \\\\\n" (lucro / faturamento * 100)
      putStrLn "\t\t\t\\bottomrule"
      putStrLn "\t\t\\end{tabular}"
      putStrLn "\t\\end{table}"
      putChar '\n'
    else do
      putStrLn "Modo de uso: ./Main [índice]"
      putStrLn "Erro de input: é preciso que 1 <= índice <= 5"
