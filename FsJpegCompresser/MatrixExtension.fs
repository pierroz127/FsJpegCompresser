namespace FsJpegCompresser

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

module MatrixExtension =
     type MathNet.Numerics.LinearAlgebra.Matrix<'T when 'T : struct and 'T : (new : unit -> 'T) 
                                                   and 'T :> System.IEquatable<'T> and 'T :> System.IFormattable 
                                                   and 'T :> System.ValueType> with 
        member this.foldij (f: int -> int -> 'T -> 'U -> 'U) (zero: 'U)= 
            let rec recursivefold i j (state: 'U) = 
                if i = this.RowCount then state
                else if j = this.ColumnCount then recursivefold (i + 1) 0 state
                else state |> f i j (this.At(i, j)) |> recursivefold i (j + 1)
            recursivefold 0 0 zero

