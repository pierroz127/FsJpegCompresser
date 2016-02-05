#load @"..\packages\MathNet.Numerics.FSharp.3.10.0\MathNet.Numerics.fsx"

#r "System.Drawing"
#r @"bin\Debug\FsJpegCompresser.exe"

open System
open System.Drawing
open System.Diagnostics
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FsJpegCompresser.MatrixExtension
let M = DenseMatrix.init 2 3 (fun i j -> i + 2*j |> double)
M.foldij (fun i j a state -> state + a + (double (i - j))) 0.

// slow dct stuff
let pi = System.Math.PI

let sqrt x = System.Math.Sqrt(x)
let cos x = System.Math.Cos(x)

let round (n: double) = System.Math.Round(n)

let dct2ways (inv: bool) (input: Matrix<double>) = 
    let n = input.RowCount
    let nd = n |> double
    let alpha u = if u = 0 then 1. else sqrt 2.
    let fcos x u = cos ((2.*(x |> double) + 1.)*(u |> double)*pi/(2.*nd))
    let r x y u v = (alpha u) * (alpha v) * (fcos x u) * (fcos y v) / nd
    if inv
    then DenseMatrix.init n n (fun x y -> input.foldij (fun u v coeff sum -> sum + coeff * (r x y u v)) 0.)
    else DenseMatrix.init n n (fun u v -> input.foldij (fun x y coeff sum -> sum + coeff * (r x y u v)) 0.)


let toMatrix (m: double[,]) = DenseMatrix.init (m.GetUpperBound(0) + 1) (m.GetUpperBound(1) + 1) (fun i j -> m.[i, j])

let dct input = dct2ways false (input |> toMatrix)

let idct input = dct2ways true input

// compare dct algorithms

let d = [| 1.5; 1.3; 1.25; 1.5; 1.6; 1.32; 1.73; 1.405 |]
let epsilon = 0.00001
let abs (x: double) = System.Math.Abs(x)
let idct11 = DenseMatrix.init 2 2 (fun i j -> 1.) |> FsJpegCompresser.Dct.fast2Didct 2
let idct22 = DenseMatrix.init 2 2 (fun i j -> 1.) |> idct 

let firstBlock = DenseMatrix.init 8 8 (fun i j -> if i=0 && j=0 then 992. else if i=0 && j=1 then -11. else 0.)

let f1 = firstBlock.Multiply(0.25) |> FsJpegCompresser.Dct.fast2Didct 8
let f2 = firstBlock |> idct

let r = new Random((int)DateTime.Now.Ticks)
let next() = r.Next(256) |> double
let x = Array.init 8 (fun i -> next())

let block = Array2D.init 8 8 (fun i j -> next())

let B1 = dct block
let B2 = FsJpegCompresser.Dct.fast2Ddct (block |> toMatrix) |> Matrix.map (fun a -> 4.*a)

let loadBmp (file: string) = new Bitmap(file)
let img = @"<<path of your BMP file>>" |> loadBmp
let w, h = img.Width, img.Height
printfn "img: %dx%d" w h

let chrono = Stopwatch.StartNew()
let bytes = FsJpegCompresser.jpeg.compress img
open System.IO
File.WriteAllBytes(@"<<path of the fsjpg file>>", bytes)
printfn "image compressed in %d ms, buffer size: %d..." chrono.ElapsedMilliseconds bytes.Length

let readbytes = File.ReadAllBytes(@"<<path of the fsjpg file>>")
let decoded = FsJpegCompresser.jpeg.decompress readbytes
decoded.Save(@"<<path of the decoded BMP file>>")
printfn "image decompressed in %d ms" chrono.ElapsedMilliseconds

