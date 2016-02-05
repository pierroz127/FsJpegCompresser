namespace FsJpegCompresser

open System.Diagnostics
open System.Drawing
open System.IO

module Program =
    let compress (source: string)  =        
        let destination = 
            let fi = FileInfo(source)
            Path.Combine(fi.DirectoryName, fi.Name.Replace(fi.Extension, "NB.fsjpg"))

        let save bytes = 
            printfn "destination: %s" destination
            File.WriteAllBytes(destination, bytes)        
        let chrono = Stopwatch.StartNew()
        new Bitmap(source)
        |> FsJpegCompresser.jpeg.compress 
        |> save
        printfn "%s compressed in %d ms" source chrono.ElapsedMilliseconds
        
    let decompress (source: string) = 
        let destination = 
            let fi = FileInfo(source)
            Path.Combine(fi.DirectoryName, fi.Name.Replace(fi.Extension, ".bmp"))
        let save (img: Bitmap) = img.Save(destination)
        let chrono = Stopwatch.StartNew()
        File.ReadAllBytes(source)
        |> FsJpegCompresser.jpeg.decompress
        |> save 
        printfn "%s decompressed in %d ms" source chrono.ElapsedMilliseconds

    [<EntryPoint>]
    let main argv = 
        match argv.[0] with
        | c when c = "-c" -> compress argv.[1] 
        | c when c = "-d" -> decompress argv.[1]
        | _ -> failwith "bad arguments"
        0
