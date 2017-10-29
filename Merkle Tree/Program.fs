open System
open System.Collections.Generic
open System.Text
open System.Security.Cryptography


module MerkleTree =
    let private charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let private sha = SHA256.Create()
    let private rnd = new Random()

    let salt =
        let salts = [|for c in 1..7 -> charset.[rnd.Next(charset.Length)]|]
        new String(salts)

    let stringToSha (s:string) = 
        s |> Encoding.ASCII.GetBytes |> sha.ComputeHash
    
    let shaToString (b: byte[]) =
        b |> Encoding.ASCII.GetString
    
    let combineHashes (hash1: byte[]) (hash2: byte[]) = 
        Array.concat [| hash1 ; hash2 |]
        |> sha.ComputeHash

    type INode =
        abstract Id : string
        abstract Hash : byte[]
        abstract Children : List<INode>

    type Node(msg:string) =
        static let mutable count = 1
        
        let hash = stringToSha(msg)
        let mutable id = string count
        let mutable children = List<INode>()

        do count <- count + 1
        interface INode with
            member this.Hash with get() = hash
            member this.Id with get() = id
            member this.Children with get() = children
    
    type RootNode() =
        let _hash = ""
        let children = List<INode>()

        interface INode with
            member this.Id with get() = "Root"
            member this.Hash with get() = 
                let mutable hashPool = Array.zeroCreate 0
                let rec buildHash (node:INode) =
                    let mutable leaf = Array.zeroCreate 0
                    for c in node.Children do
                        if c.Children.Count <> 0 then
                            buildHash c
                        leaf <- leaf |> combineHashes c.Hash
                    hashPool <- hashPool |> combineHashes leaf
                buildHash this
                hashPool
            member this.Children with get() = children
    
    let Root = RootNode()

    let addChild child =
        child |> (Root :> INode).Children.Add

    let rec private descend (startingNode : INode, isVerbose : bool) =
        if isVerbose then
            printfn "Current Node: %s" startingNode.Id
            printfn "%s's Children:" startingNode.Id
        for c in startingNode.Children do
            if isVerbose then
                printfn "\t%s: %A" c.Id (c.Hash)
            if c.Children.Count <> 0 then
                descend(c, isVerbose)
                if isVerbose then
                    printfn "\n%s's next sibling:" c.Id

    let printTree node =
        descend (node, true)



open MerkleTree
[<EntryPoint>]
let main argv = 
    Console.OutputEncoding <- Encoding.ASCII
    printfn "| | | - Merkle Tree Demo - | | |"
    
    "Hello"
        |> MerkleTree.stringToSha 
        |> Encoding.ASCII.GetString
        |> printfn "%A"
    let fooNode = new Node "FooNode"
    let barNode = new Node "BarNode"
    addChild fooNode
    addChild barNode
    let nodes = [for i in 1..10 -> new Node(string i)] |> Seq.cast<INode>
    (fooNode :> INode).Children.AddRange(nodes)
    (barNode :> INode).Children.Add(new Node("Hello World"))
    printTree Root
    (Root:>INode).Hash
    |> shaToString
    |> printfn "Root's Hash As String: %A" 
    0 // return an integer exit code
