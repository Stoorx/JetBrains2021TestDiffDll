open System.IO
open Mono.Cecil
open Mono.Cecil.Cil

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


[<EntryPoint>]
let main argv =
    let file =
        new FileStream(@"TestApp\bin\Debug\net5.0\TestApp.dll", FileMode.Open, FileAccess.ReadWrite)

    let assembly = ModuleDefinition.ReadModule file

    assembly.Types.ToArray()
    |> Array.collect (fun td -> td.Methods.ToArray())
    |> Array.collect (fun md -> md.Body.Instructions.ToArray())
    |> Array.iter transformInstruction

    assembly.Write(file)
    0
