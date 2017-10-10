module Kardex

open FSharp.Data
open System.Net
open System.Collections.Generic
open System.Xml

type kardex = 
    {matricula      : string
     materia        : string option
     grupo          : string
     c1             : string option
     i1             : uint32
     c2             : string option
     i2             : uint32
     c3             : string option
     i3             : uint32
     efinal         : string option
     final          : string option
     inasistencias  : uint32
     extraordinario : string option
     regularizacion : string option}

let kardex_revalidado matricula materia final = 
             {matricula      = matricula
              materia        = materia
              grupo          = Option.get materia
              c1             = None
              i1             = uint32 0
              c2             = None
              i2             = uint32 0
              c3             = None
              i3             = uint32 0
              efinal         = None
              final          = final
              inasistencias  = uint32 0
              extraordinario = None
              regularizacion = None}

[<Literal>]
let Kardex = """
    <table>
        <row>
            <column>080675</column>
            <column>Osuna Mart&#237;nez Fernando</column>
            <column>9</column>
            <column>Inteligencia Artificial II</column>
            <column>E15-643</column>
            <column>20151S</column>
            <column>9.6</column>
            <column> </column>
            <column> </column>
            <column>7</column>
            <column>Aprobado </column>
        </row>
        <row>
            <column>080675</column>
            <column>Osuna Mart&#237;nez Fernando</column>
            <column>9</column>
            <column>Residencia Profesional</column>
            <column>E15-942</column>
            <column>20151S</column>
            <column></column>
            <column></column>
            <column></column>
            <column></column>
            <column>*No-Evaluada</column>
        </row>
    </table>"""

[<Literal>]
let Parciales = """
    <table>
        <row>
            <column>100251</column>
            <column>Narváez Carrizalez Imelda</column>
            <column>ITI</column>
            <column>20151S</column>
            <column>Compiladores</column>
            <column>E15-837 - Reyes  / Alejandro </column>
            <column>7.5</column>
            <column>0</column>
            <column>5.0</column>
            <column>2</column>
            <column>8.0</column>
            <column>0</column>
            <column>8.0</column>
            <column>7.3</column>
            <column>2</column>
            <column> </column>
            <column> </column>
        </row>
        <row>
            <column>100251</column>
            <column>Narváez Carrizalez Imelda</column>
            <column>ITI</column>
            <column>20151S</column>
            <column>Inteligencia Artificial II</column>
            <column>E15-643 - Montaño Rivas / Omar</column>
            <column>6.5</column>
            <column>4</column>
            <column>4.0</column>
            <column>0</column>
            <column>10.0</column>
            <column>0</column>
            <column>10.0</column>
            <column>8.1</column>
            <column>4</column>
            <column> </column>
            <column> </column>
        </row>
    </table>"""

type TParciales = XmlProvider<Parciales>
type TKardex = XmlProvider<Kardex>

let rec calificacion cal =
    match cal with
        "" -> None
      | str -> if str.Length > 5
               then (try
                        let v = double str
                        if v > 10.0
                        then let str' = string (v / 10.0)
                             printfn "Cambiando calificación '%s' por '%s'" str str'
                             Option.get (calificacion str')
                        else printfn "Algun error en el valor numérico de '%s' (truncando)" str
                             str.Substring(0,4)
                     with | :? System.FormatException -> str.Substring(0,4)) |> Some
               else Some str

let extraerGrupo (info : string) =
    match info.Split [|'-'|] |> Array.toList with
        | code1 :: code2 :: _ -> let grupo = code1.Trim() + "-" + code2.Trim()
                                 grupo
        | _ -> printfn "No se pudo extraer la información del grupo! %s" info
               ""

// http://intranet.upslp.edu.mx:9080/Users/kardex.do?cveMateria=0&gpo=*&matricula=100251&method=parciales&nomalu=*&nommat=*&pdo=20151S&plan=ITI&rep=si


//let obtener_kardex planes mapa_extra_codigo ((carrera, ((mapa_ser : Dictionary<string, string>), (mapa_sem : Dictionary<int, string list>))), periodo) =
let obtener_kardex user (carrera, periodo) =
    let rec aux cookie =
     try
      let f () = 
        IntranetAccess.request_string' 
                                   ("http://intranet.upslp.edu.mx:9080/Users/kardex.do",
                                    [("6578706f7274","1");
                                     ("aprobo","*");
                                     ("cveMateria","70");
                                     ("d-1782-e","3");
                                     ("gpo","*");
                                     ("matricula","*");
                                     ("method","list");
                                     ("nomalu","*");
                                     ("nommat","*");
                                     ("pdo",periodo);
                                     ("plan",carrera);
                                     ("rep","si");
                                     ("ultimo","20013S")],
                                    cookie)
      let intranet = Library.recursive_timeout Library.val_timeout f ()
      let materias = TKardex.Parse(intranet)
      (cookie, materias)
     with | :? System.Xml.XmlException -> let cookie = Option.get (IntranetAccess.newCookie user)
                                          aux cookie
          | :? System.Net.WebException -> let cookie = Option.get (IntranetAccess.newCookie user)
                                          aux cookie
          | :? System.AggregateException -> let cookie = Option.get (IntranetAccess.newCookie user)
                                            aux cookie
    let (cookie, materias) = aux (IntranetAccess.getCookie user)
//    printfn "Actualizando el kardex del alumno con matrícula %s de la carrera %s en el periodo %s..." matricula carrera periodo
    Array.map (fun (materia : XmlProvider<Kardex>.Row) -> 
        let valores = [| for campo in materia.Columns do
                            match campo.String with
                                Some s -> yield s
                                | _ -> yield "" |]
        let matricula = valores.[0]
        let nombre = valores.[1]
        (matricula, nombre)) materias.Rows
        |> Array.toList
(*                BaseDatos.actualiza_kardex matricula kardex.grupo kardex.materia semestre periodo kardex.c1 kardex.i1 
                                           kardex.c2 kardex.i2 kardex.c3 kardex.i3 kardex.efinal kardex.final 
                                           kardex.inasistencias kardex.extraordinario kardex.regularizacion estatus*)


