module Profesores

open System
open FSharp.Data
open System.Net
open System.Collections.Generic

type Profesor =
    {profesor  : int
     periodo   : string
     nombre    : string
     apellidos : string
     tipo      : string}

let hacerProfesor profesor periodo nombre apellidos tipo =
    {profesor  = profesor
     periodo   = periodo
     nombre    = nombre
     apellidos = apellidos
     tipo      = tipo}


[<Literal>]
let TablaProfesores = """
<table>
<row>
<column>664</column>
<column>Castillo Galván / Patricia</column>
<column>A1</column>
<column>19976</column>
<column>C163-32</column>
<column>Proyecto Profesional</column>
<column>ISTI. </column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column> - </column>
<column>3</column>
<column>8</column>
<column>08/08/16</column>
<column>18/10/16</column>
<column>Profesores de Tiempo Completo</column>
</row><row>
<column>1184</column>
<column>Castro López / Dora Luz</column>
<column>A66</column>
<column>18861</column>
<column>C163-12</column>
<column>Sistemas Integradores de Producción II</column>
<column>ISTI. </column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column>16:00 - 17:00</column>
<column> - </column>
<column>3</column>
<column>7</column>
<column>02/05/16</column>
<column>08/08/16</column>
<column>Profesores Asignatura</column>
</row>
</table>"""

type Profesores = XmlProvider<TablaProfesores>

let extraerInfo (info : string) =
    match info.Split [|'/'|] with
        | [| apellidos; nombre |] -> (apellidos.Trim(), nombre.Trim())
        | _ -> printfn "Información de profesor inválida: %s" info
               ("", "")

let obtener_profesores user (plan, periodo) =
    let rec aux cookie =
     try
      let f () = IntranetAccess.request_string' ("http://intranet.upslp.edu.mx:9080/Users/periodo.do",
                                                 [("6578706f7274","1"); ("aula", "0"); ("aulanom", "*"); ("cveMateria", "0");
                                                  ("cveprof","0"); ("d-1782-e","3"); ("method","horarios"); ("nommat","*"); ("nomprof", "*");
                                                  ("pdo",periodo); ("plan",plan); ("rep", "si"); ("ultimo","20013S")],
                                                 cookie)
      let intranet = Library.recursive_timeout Library.val_timeout f ()
      let profesores = Profesores.Parse(intranet)
      (cookie, profesores)
     with | :? System.Xml.XmlException -> let cookie = Option.get (IntranetAccess.newCookie user)
                                          aux cookie
          | :? System.Net.WebException -> let cookie = Option.get (IntranetAccess.newCookie user)
                                          aux cookie
    let (cookie, profesores) = aux (IntranetAccess.getCookie user)
    printfn "Actualizando profesores de la carrera %s en el periodo %s..." plan periodo
    Array.fold (fun profesores (profesor : Profesores.Row) -> 
        let valores = [| for campo in profesor.Columns do
                            match campo.String with
                                Some s -> yield s
                                | _ -> yield "" |]
        let profesor = int (valores.[0])
        let (apellidos, nombre) = extraerInfo (valores.[1])
        let tipo = valores.[18]
// //printfn "profesor:%s periodo:%s nombre:%s apellidos:%s tipo:%s" profesor periodo nombre apellidos tipo
        let profr = hacerProfesor profesor periodo nombre apellidos tipo
        profr :: profesores) [] profesores.Rows
(*        |> Map.iter (fun _ (profesor, nombre, apellidos, tipo) -> BaseDatos.actualiza_profesores profesor periodo nombre apellidos tipo)
    cookie*)


