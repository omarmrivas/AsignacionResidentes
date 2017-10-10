module Restricciones

open System.IO
open System.Text.RegularExpressions

(*
    Número de asesorados + número de revisiones <= impuesto por profesor

    Sumatoria de asesorados = residentes
    Sumatoria de revisiones = residentes
*)

let to_smt (profesores : Profesores.Profesor list) user (carrera, periodo) =
    let alumnos = Kardex.obtener_kardex user (carrera, periodo)

    let cids = profesores |> List.map (fun profesor -> profesor.profesor)
                          |> set
    let cprofesores = "Profesores.txt" |> Library.leerProfesores
                                       |> Array.filter (fun (_, id, _, _) -> Set.exists (fun x -> x = id) cids)
    let asignaciones = Library.leerAsignaciones "AsignacionParcial.txt"

    (* declare-const *)
    let cabecera cprofesores =
        cprofesores
            |> Array.collect (fun (_, id, _, _) -> 
                            [|"a" + string id
                              "r" + string id|])
            |> Array.map (fun name -> "(declare-const " + name + " Int)")
            |> Array.toList
    (* assert *)
    let restricciones cprofesores =
        cprofesores
            |> Array.collect (fun (_, id, a, r) -> 
                            let ac = "a" + string id
                            let rc = "r" + string id
                            [|"(assert (<= " + string ac + " " + string a + "))"
                              "(assert (<= " + string rc + " " + string r + "))"|])
            |> Array.toList
    let restricciones_total cprofesores =
        cprofesores
            |> Array.map (fun (_, id, _, _) -> 
                            ("a" + string id, "r" + string id))
            |> (fun arr -> let asesorados = arr |> Array.map (fun (a,_) -> a)
                                                |> String.concat " "
                                                |> (fun str -> "(+ " + str + ")")
                                                |> (fun str -> "(assert (= " + str + " " + string alumnos.Length + "))")
                           let revisiones = arr |> Array.map (fun (_,r) -> r)
                                                               |> String.concat " "
                                                               |> (fun str -> "(+ " + str + ")")
                                                               |> (fun str -> "(assert (= " + str + " " + string alumnos.Length + "))")
                           (asesorados, revisiones))
    let (asesorados, revisiones) = restricciones_total cprofesores
    (cabecera cprofesores) @
    (restricciones cprofesores) @
    [ asesorados
      revisiones
      "(check-sat)"
      "(get-model)" ]
      |> String.concat "\n"


let execute_z3 profesores user (carrera, periodo) =
    let smt_str = to_smt profesores user (carrera, periodo)
    let outFile = new StreamWriter("z3.smt")
    outFile.WriteLine smt_str
    outFile.Flush()
    outFile.Close()
    let output = Library.execute_command "z3" "-smt2 z3.smt"
    output


let parse_z3_output output = 
    let regex s = new Regex(s)
    let pattern = @"(\(define\-fun\s+(?<variable>\w+)\s+\(\)\s+Int\s+(?<valor>\d+)\))+"
    let regex = new Regex(pattern)
    [ for m in regex.Matches output do
        yield (m.Groups.["variable"].Value, int m.Groups.["valor"].Value) ]


let genera_asignacion user (carrera, periodo) =
    let profesores = Profesores.obtener_profesores user (carrera, periodo)
    let output = execute_z3 profesores user (carrera, periodo)
    let cids = profesores |> List.map (fun profesor -> (profesor.profesor, profesor))
                          |> Map.ofList
    let to_id (name : string) = 
        1 |> name.Substring
          |> int
    let es_asesorias (name : string) =
        name.[0] = 'a'
    let resultado = parse_z3_output output
    resultado |> List.map (fun (name, valor) -> (Map.find (to_id name) cids, valor, es_asesorias name))
              |> List.iter (printfn "%A")
