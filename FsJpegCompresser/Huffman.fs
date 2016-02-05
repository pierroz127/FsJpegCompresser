namespace FsJpegCompresser

module Huffman =
    type HuffmanTree<'a> = 
    | Leaf of int*'a
    | Node of int * 'a HuffmanTree * 'a HuffmanTree

    type HuffmanCode<'a when 'a: comparison> = HuffmanCode of Map<'a, byte list>

    let freq = function Leaf(f, _) | Node(f, _, _) -> f
    let freqCompare a b = compare (freq a) (freq b) 
    let buildTree freqs = 
        let leaves = List.map (fun (c, f) -> Leaf (f, c)) freqs
        let freqSort = List.sortWith freqCompare 
        let rec build = function
            | a :: b :: tail ->
                let node = Node((freq a) + (freq b), a, b) 
                node :: tail |> freqSort |> build
            | [a] -> a
            | _ -> failwith "Fail with empty list"
        leaves |> freqSort |> build

    let toCodes huffmanTree = 
        let rec buildCodes = 
            function
            | code, Leaf (f, c) -> [ (c, code |> List.rev) ]
            | code, Node (_, l, r) -> buildCodes (0uy::code, l) @ buildCodes (1uy::code, r)
        buildCodes ([], huffmanTree) |> Map.ofList |> HuffmanCode

    let encode (HuffmanCode(codes)) (inputs: 'a list) =
        let rec encoderec (``in``: 'a list) (results: byte list, current: byte, index: int) =
            let rec concat (res: byte list, cur: byte, idx: int) ll =
                match ll with
                | head :: tail -> 
                    let next = (head <<< idx) ||| cur
                    if idx = 0 then concat (res @ [next], 0uy, 7) tail else concat (res, next, idx-1) tail
                | _ -> res, cur, idx
        
            match ``in`` with
            | head :: tail -> codes.[head] |> concat (results, current, index) |> encoderec tail
            | _ -> if (index = 7) then results else results @ [current]
        
        encoderec inputs ([], 0uy, 7)


    let decode tree length encoded = 
        let decodeByte (node: HuffmanTree<'a>) (element: byte) result =
            let rec next (node, (res: 'a list)) i =
                if (res.Length = length) then (tree, res) else 
                match node with 
                | Node(_, l, r) -> if (element >>> (7 - i)) &&& 1uy = 0uy then l, res else r, res
                | Leaf(_, a) -> next (tree, a :: res) i
            
            [0..7] |> List.fold next (node, result)
        
        let rec decoderec elements (node, res) =
            match elements with
            | head :: tail -> decodeByte node head res |> decoderec tail
            | [] -> match node with Leaf(_, a) when res.Length < length -> a :: res | _ -> res
        
        decoderec encoded (tree, []) |> List.rev
