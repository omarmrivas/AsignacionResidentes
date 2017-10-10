// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO

let periodo () =
    string System.DateTime.Now.Year + 
    (match System.DateTime.Now.Month with
          1 | 2 | 3 | 4 | 5 | 6 -> "1S"
          | _ -> "3S")

[<EntryPoint>]
let main argv =
    if Array.length argv > 0 &&
       argv.[0] = "-csv"
    then printf "Archivo de asignación parcial: "
         let archivo = System.Console.ReadLine ()
         Asignacion.to_csv archivo
    elif Array.length argv > 0 &&
         argv.[0] = "-mail"
    then printf "Archivo de asignación parcial: "
         let archivo = System.Console.ReadLine ()
         let matriculas = Asignacion.matriculas archivo
         List.iter (printfn "%s@upslp.edu.mx") matriculas
    else 
        printf "Usuario: "
        let usuario = System.Console.ReadLine ()
        printf "Contraseña: "
        let password = Library.readPassword ()
        printf "Carrera: "
        let carrera = System.Console.ReadLine ()
        let periodo = periodo ()
        printfn "Periodo: %s" periodo
        printf "Archivo de asignación parcial: "
        let parcial = System.Console.ReadLine ()


        let user = IntranetAccess.login usuario password

        match user with
            Some user -> let asignaciones = Asignacion.hacer_asignacion parcial user (carrera, periodo)
                         let outFile = new StreamWriter(carrera + ".txt")
                         Array.iter (outFile.WriteLine : string -> unit) asignaciones
                         outFile.Flush()
                         outFile.Close()
                         let asignaciones = Asignacion.nivelar_asesorias user (carrera, periodo)
                         let outFile = new StreamWriter(carrera + ".txt")
                         Array.iter (outFile.WriteLine : string -> unit) asignaciones
                         outFile.Flush()
                         outFile.Close()
                         printfn "Alumnos asesorados: %d" (Asignacion.contar_asesorados (carrera + ".txt"))
                         printfn "Alumnos revisados: %d" (Asignacion.contar_revisiones (carrera + ".txt"))
                         Asignacion.profesores_con_menos_alumnos (carrera + ".txt")
          | None -> ()

    0 // return an integer exit code

