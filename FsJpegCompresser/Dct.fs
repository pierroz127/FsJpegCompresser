namespace FsJpegCompresser

open MathNet.Numerics.LinearAlgebra

module Dct =
    let cos x = System.Math.Cos(x)
    let pi = System.Math.PI
    let sqrt2 = System.Math.Sqrt(2.)
    
    let fast1Didct (input: double[]) = 
        let rec fastidctrec (X: double[]) = 
            if X.Length = 1
            then [| X.[0] |]
            else
                let N, halfN, piDivByTwoN = X.Length, X.Length / 2, pi / (2*X.Length |> double)
                let g = Array.init halfN (fun i -> X.[2*i]) |> fastidctrec 
                let h = Array.init halfN (fun i -> if i=0 then X.[1] else X.[2*i+1] + X.[2*i-1]) |> fastidctrec
                let coeff = Array.init halfN (fun k -> 1./(2.*cos ((2.*(double k) + 1.)*piDivByTwoN)))
                Array.init N (fun k -> if k < halfN then g.[k] + coeff.[k]*h.[k] else g.[N-1-k] - coeff.[N-1-k]*h.[N-1-k])
        
        let Xprime = Array.init (input.Length) (fun i -> if i = 0 then input.[i] / sqrt2 else input.[i])
        fastidctrec Xprime

    let fast1Ddct (input: double[]) =
        let rec fastdctrec (x: double[]) = 
            if x.Length = 1 
            then [| x.[0] |]
            else 
                let N, halfN, piDivByTwoN = x.Length, x.Length / 2, pi / (2*x.Length |> double)
                let coeff = Array.init halfN (fun k -> (cos ((2.*(double k) + 1.)*piDivByTwoN)))
                let G = Array.init halfN (fun k -> (x.[k] + x.[N-1-k]) / 2.) |> fastdctrec
                let H = Array.init halfN (fun k -> coeff.[k]*(x.[k] - x.[N-1-k])) |> fastdctrec
                let X = Array.init N (fun n -> if n%2 = 0 then G.[n/2] else H.[n/2])
                for i in [1..halfN-1] do Array.set X (2*i+1)  (X.[2*i+1] - X.[2*i-1])
                X
        let XPrime = fastdctrec input
        Array.init (input.Length) (fun i -> if i = 0 then XPrime.[0]*sqrt2 else XPrime.[i]) 
        

    let fast2Didct dim (input: Matrix<double>) = 
        let idctToVector arr = arr |> fast1Didct |> Array.toList |> vector
        let temp = DenseMatrix.initColumns dim (fun j -> Array.init dim (fun k -> input.[k, j]) |> idctToVector)
        DenseMatrix.initRows dim (fun i -> Array.init dim (fun k -> temp.[i, k]) |> idctToVector)

    let fast2Ddct (input: Matrix<double>) =
        let dctToVector (v: Vector<double>) = v.ToArray() |> fast1Ddct |> Array.toList |> vector
        let rows, cols = input.RowCount, input.ColumnCount
        let temp = DenseMatrix.initRows cols (fun i -> input.Row(i) |> dctToVector)
        DenseMatrix.initColumns rows (fun j -> temp.Column(j) |> dctToVector)

    let dct (x: double[]) = 
        let piDivByTwoN, invHalfN = pi / (2*x.Length |> double), 2./(x.Length |> double)
        let e = Array.init x.Length (fun i -> if i = 0 then 1. / sqrt2 else 1.)
        Array.init x.Length (fun n  -> (invHalfN * e.[n])*([0..x.Length-1] |> List.sumBy (fun k -> x.[k]*cos((2.*(double k)+1.)*(double n)*piDivByTwoN))))
        
    let idct (X: double[]) = 
        let piDivByTwoN = pi / (2*X.Length |> double)
        let e = Array.init X.Length (fun i -> if i = 0 then 1. / sqrt2 else 1.)
        Array.init X.Length (fun k  -> [0..X.Length-1] |> List.sumBy (fun n -> e.[n]*X.[n]*cos((2.*(double k)+1.)*(double n)*piDivByTwoN)))



