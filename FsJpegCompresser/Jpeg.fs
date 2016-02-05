namespace FsJpegCompresser

open System.Drawing
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open Nessos.FsPickler
open FsJpegCompresser.MatrixExtension
open FsJpegCompresser.Huffman

type Direction = Down | Up
type Block = { id: int; values: Matrix<double>; }
    with
        static member dimension block =
            if block.values.RowCount = block.values.ColumnCount
            then block.values.RowCount
            else failwith "not a square matrix"

        static member toTrimmedList block =
            let n = Block.dimension block 
            let at i j = block.values.At(i, j)
            let rec trim = 
                function
                | zero :: tail when zero = 0. -> trim tail
                | x -> x
            let rec next output = 
                function
                | (i, j, _) when i = n-1 && j = n-1 -> (at i j) :: output |> List.rev
                | (i, j, Up) when j = n-1 -> next ((at i j) :: output) (i+1, n-1, Down)
                | (i, j, Down) when i = n-1 -> next ((at i j) :: output) (n-1, j+1, Up)
                | (i, j, Up) when i = 0 -> next ((at i j) :: output) (0, j+1, Down)
                | (i, j, Down) when j = 0 -> next ((at i j) :: output) (i+1, 0, Up)
                | (i, j, Up) -> next((at i j) :: output) (i-1, j+1, Up)
                | (i, j, Down) -> next ((at i j) :: output) (i+1, j-1, Down)
            next [] (0, 0, Up) |>  List.rev |> trim |> List.rev

        static member fromTrimmedList n (values: double list) =
            let matrix = DenseMatrix.zero n n
            let rec parse k x y dir =
                if k < values.Length then 
                    matrix.[x, y] <- values.[k]
                    match (x, y, dir) with
                    | (i, j, _) when i = n-1 && j = n-1 -> ()
                    | (i, j, Up) when j = n-1 -> parse (k+1) (i+1) (n-1) Down
                    | (i, j, Down) when i = n-1 -> parse (k+1) (n-1) (j+1) Up
                    | (i, j, Up) when i = 0 -> parse (k+1) 0 (j+1) Down
                    | (i, j, Down) when j = 0 -> parse (k+1) (i+1) 0 Up
                    | (i, j, Up) -> parse (k+1) (i-1) (j+1) Up
                    | (i, j, Down) -> parse (k+1) (i+1) (j-1) Down
            parse 0 0 0 Up
            {id = 0; values = matrix }


module jpeg =     
    type JPEG = { width: int; height: int; blocks: Block[]; }
    type BlockRawData = { id: int; length: byte; data: byte[] }
    type JpegRawData = { width: int; height: int; tree: HuffmanTree<float>; rawdata: BlockRawData[]}
    
    let blockCount n = if n%8=0 then n/8 else n/8 + 1
    
    let pi = System.Math.PI
    let sqrt2 = System.Math.Sqrt(2.)
    let cos x = System.Math.Cos(x)
    let round (x: double) = System.Math.Round(x)
        
    let Q = matrix [[16.; 11.; 10.; 16.; 24.; 40.; 51.; 61.]
                    [12.; 12.; 14.; 19.; 26.; 58.; 60.; 55.]
                    [14.; 13.; 16.; 24.; 40.; 57.; 69.; 56.]
                    [14.; 17.; 22.; 29.; 51.; 87.; 80.; 62.]
                    [18.; 22.; 37.; 56.; 68.; 109.; 103.; 77.]
                    [24.; 35.; 55.; 64.; 81.; 104.; 113.; 92.]
                    [49.; 64.; 78.; 87.; 103.; 121.; 120.; 101.]
                    [72.; 92.; 95.; 98.; 112.; 100.; 103.; 99.]]

    let fastDctBlock block = { block with values = Dct.fast2Ddct block.values}

    let fastIDctBlock block = { block with values = Dct.fast2Didct 8 block.values }

    let fastDct jpg = { jpg with blocks = jpg.blocks |> Array.Parallel.map fastDctBlock }

    let fastIDct jpg = { jpg with blocks = jpg.blocks |> Array.Parallel.map fastIDctBlock }

    let compress img =       
        let toJpeg (img : Bitmap) = 
            let convertToGrayLevel (c: Color) = 
                let red, green, blue = c.R |> double, c.G |> double, c.B |> double
                0.2989*red + 0.5870*green + 0.1140*blue |> round

            let getBlockPixelValue (i: int) (j: int) (size: int)  (u: int) (v: int) =
                let x, y = j*size + v, i*size + u
                if x >= img.Width || y >= img.Height 
                then 0. 
                else convertToGrayLevel (img.GetPixel(x, y))

            let rows, cols = (img.Height |> blockCount), (img.Width |> blockCount)

            let getBlock i j size img = { id = i*cols + j; values = DenseMatrix.init size size (getBlockPixelValue i j size) }
        
            let blocks = seq { for i in [0..rows-1] do for j in [0..cols-1] do yield getBlock i j 8 img }

            { width = img.Width; height = img.Height; blocks = blocks |> Seq.toArray; }

        let quantizeBlock block =
            let n = block.values.RowCount
            { block with values = DenseMatrix.init n n (fun i j -> block.values.At(i, j) / (0.25 * Q.At(i, j)) |> round) }
        
        let quantize coeff jpg = { jpg with blocks = jpg.blocks |> Array.Parallel.map quantizeBlock }

        let serialize (jpg: JPEG) =
            let tree =
                jpg.blocks 
                |> Array.map (fun b -> b |> Block.toTrimmedList |> Array.ofList)
                |> Array.concat
                |> Array.groupBy (fun b -> b)
                |> Array.map (fun (key, group) -> (key, group.Length))
                |> Array.toList
                |> Huffman.buildTree 
    
            let codes = Huffman.toCodes tree

            let serializeBlock codes block =
                let bytes = 
                    block 
                    |> Block.toTrimmedList
                    |> Huffman.encode codes
                    |> List.toArray
                { id = block.id; length = bytes.Length |> byte; data = bytes }
                

            let jpgRawData = { width = jpg.width;
              height = jpg.height;
              tree = tree;
              rawdata = jpg.blocks |> Array.map (serializeBlock codes) }

            let binarySerializer = FsPickler.CreateBinarySerializer()
            binarySerializer.Pickle jpgRawData
        
        img 
        |> toJpeg
        |> fastDct
        |> quantize 4.
        |> serialize

    let decompress (bytes: byte[]) =
        let deserialize bytes = 
            let binarySerializer = FsPickler.CreateBinarySerializer()
            binarySerializer.UnPickle<JpegRawData> bytes
         
        let decode (jpegRawData: JpegRawData) =   
            let deserializeBlock tree (blockRawData: BlockRawData) =
                let deserializedBlock =
                    Huffman.decode tree (blockRawData.length |> int) (blockRawData.data |> Array.toList)
                    |> Block.fromTrimmedList 8
                { deserializedBlock with id = blockRawData.id }

            { width = jpegRawData.width; 
              height = jpegRawData.height; 
              blocks = jpegRawData.rawdata |> Array.map (deserializeBlock jpegRawData.tree) }

        let quantizeBlock block =
            let n = block.values.RowCount
            { block with values = DenseMatrix.init n n (fun i j -> block.values.At(i, j) * 0.25 * Q.At(i, j)) }
        
        let quantize jpg = { jpg with blocks = jpg.blocks |> Array.Parallel.map quantizeBlock }

        let toBmp (jpeg: JPEG) = 
            let image = new Bitmap(jpeg.width, jpeg.height)

            let setPixelsFromBlock idx block = 
                let blocksPerColumn = jpeg.width |> blockCount
                let i = idx / blocksPerColumn
                let j = idx - i*blocksPerColumn
                let offseti, offsetj = i*8, j*8
                for k in [0..7] do 
                    for l in [0..7] do
                        if (offseti + k) < jpeg.height && (offsetj + l) < jpeg.width 
                        then 
                            let tmp = [ System.Math.Round(block.values.At(k, l)) |> int; 255 ] |> List.min
                            let gray = [ tmp; 0] |> List.max
                            let color = Color.FromArgb(gray, gray, gray)
                            image.SetPixel(offsetj+l, offseti+k, color)

            jpeg.blocks |> Seq.iteri setPixelsFromBlock
            image

        bytes
        |> deserialize
        |> decode
        |> quantize 
        |> fastIDct
        |> toBmp