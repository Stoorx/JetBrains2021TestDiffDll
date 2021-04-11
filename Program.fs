open Mono.Cecil
open Mono.Cecil.Cil

type AssemblyPaths = { source: string; destination: string }

let readArg argv index name: Result<string, string> =
    argv
    |> Array.tryItem index
    |> function
    | Some r -> Ok r
    | None -> Error(sprintf "Argument '%s' has not set" name)

let readArgs argv: Result<AssemblyPaths, string []> =
    let s = readArg argv 0 "source"
    let d = readArg argv 1 "destination"

    match (s, d) with
    | (Ok s, Ok d) -> Ok { source = s; destination = d }
    | (Ok _, Error d) -> Error [| d |]
    | (Error s, Ok _) -> Error [| s |]
    | (Error s, Error d) -> Error [| s; d |]

let transformDecimalInstruction (inst: Instruction) =
    match inst.Operand with
    | :? MethodReference as mr when mr.DeclaringType.FullName = "System.Decimal"
                                    && mr.Name = "op_Addition" -> mr.Name <- "op_Subtraction" // I tried to do things immutable but Cecil has no FP-friendly API
    | _ -> ()

let transformInstruction (inst: Instruction) =
    match inst with
    | i when i.OpCode = OpCodes.Add -> inst.OpCode <- OpCodes.Sub // Same
    | i when i.OpCode = OpCodes.Call -> transformDecimalInstruction inst
    | _ -> ()

let processAssembly (assemblyPaths: AssemblyPaths) =
    let assembly =
        ModuleDefinition.ReadModule assemblyPaths.source

    assembly.Types.ToArray()
    |> Array.collect (fun td -> td.Methods.ToArray())
    |> Array.collect (fun md -> md.Body.Instructions.ToArray())
    |> Array.iter transformInstruction

    assembly.Write assemblyPaths.destination


[<EntryPoint>]
let main argv =
    readArgs argv
    |> function
    | Ok asmPaths -> processAssembly asmPaths |> fun _ -> 0
    | Error e -> e |> String.concat "\n" |> printf "%s" |> fun _ -> -1
