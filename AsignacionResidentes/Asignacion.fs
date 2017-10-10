module Asignacion

open System.IO

let hacer_asignacion parcial user (carrera, periodo) =
    let alumnos = Kardex.obtener_kardex user (carrera, periodo) // Alumnos de Residencias Profesionales
                    |> List.map (fun (matricula, _) -> (matricula, carrera))
    let profesores = Profesores.obtener_profesores user (carrera, periodo) // Profesores dando clases en carrera
    let mapa_profesores = profesores |> List.map (fun profesor -> (profesor.profesor, profesor))
                                     |> Map.ofList
    let cids = profesores |> List.map (fun profesor -> profesor.profesor)
                          |> set
    let cprofesores = "Profesores.txt" |> Library.leerProfesores
                                       |> Array.filter (fun (_, id, _, _) -> Set.contains id cids)
                                       |> Array.map (fun (_, id, a, r) -> (id, (a,r)))
                                       |> Map.ofArray
    let (asignaciones, resto) = 
            parcial
                |> Library.leerAsignaciones
                |> Array.partition (fun (_, id, _, _) -> Set.contains id cids)
    let alumnos_asesorados = 
            alumnos |> 
                List.filter (fun (matricula, plan) -> 
                    not (Array.exists (fun (_, _, xs, _) -> List.exists (fun (mat,_) -> mat = matricula) xs) asignaciones))
    let alumnos_revisados = 
            alumnos |> 
                List.filter (fun (matricula, plan) -> 
                    not (Array.exists (fun (_, _, _, xs) -> List.exists (fun (mat,_) -> mat = matricula) xs) asignaciones))
    let estado = Map.ofArray (Array.map (fun (nombre, id, a, r) -> (id, (a, r))) asignaciones)
    let pendiente_asesorados matricula estado =
        estado |> Map.filter (fun id (asesorados, revisiones) -> 
                            List.length asesorados < fst (Map.find id cprofesores) &&
                            List.forall (fun mat -> mat <> matricula) revisiones)
               |> Map.toList
               |> (fun l -> if List.isEmpty l
                            then None
                            else (Some << fst) (List.minBy (fun (id, (asesorados, _)) -> List.length asesorados) l))
(*        Map.tryFindKey (fun id (asesorados, _) -> 
                            List.length asesorados < fst (Map.find id cprofesores)) estado*)
    let pendiente_revisiones matricula estado =
        estado |> Map.filter (fun id (asesorados, revisiones) -> 
                            List.length revisiones < snd (Map.find id cprofesores) &&
                            List.forall (fun mat -> mat <> matricula) asesorados)
               |> Map.toList
               |> (fun l -> if List.isEmpty l
                            then None
                            else (Some << fst) (List.minBy (fun (id, (_, revisiones)) -> List.length revisiones) l))
(*        Map.tryFindKey (fun id (_, revisiones) -> 
                            List.length revisiones < snd (Map.find id cprofesores)) estado*)
    let profesor_asesorar estado =
        Map.tryFindKey (fun id (a,_) -> not (Map.containsKey id estado) && 
                                        a > 0) cprofesores
    let profesor_revisar estado =
        Map.tryFindKey (fun id (_,r) -> not (Map.containsKey id estado) && 
                                        r > 0) cprofesores
    let encontrar_asesor matricula estado =
        match profesor_asesorar estado with
            Some id -> Some (id, ([], []))
          | None    -> match pendiente_asesorados matricula estado with
                            Some id -> Some (id, Map.find id estado)
                          | None -> None
    let encontrar_revisor matricula estado =
        match profesor_revisar estado with
            Some id -> Some (id, ([], []))
          | None    -> match pendiente_revisiones matricula estado with
                            Some id -> Some (id, Map.find id estado)
                          | None -> None
    let rec asignar alumnos_asesorados alumnos_revisados estado =
        match alumnos_asesorados with
            matricula :: alumnos_asesorados ->
                match encontrar_asesor matricula estado with
                    Some (id, (a, r)) -> asignar alumnos_asesorados alumnos_revisados (Map.add id (matricula :: a, r) estado)
                  | None -> failwith "Imposible asignación!"
           | [] -> match alumnos_revisados with
                    matricula :: alumnos_revisados ->
                        match encontrar_revisor matricula estado with
                            Some (id, (a, r)) -> asignar alumnos_asesorados alumnos_revisados (Map.add id (a, matricula :: r) estado)
                          | None -> failwith "Imposible asignación!"
                   | [] -> estado
    let matriculas_txt matriculas =
        matriculas |> List.map (fun (mat, plan) -> "(" + mat + "," + plan + ")")
                   |> (fun matriculas -> "<" + String.concat "," matriculas + ">")
    estado |> asignar alumnos_asesorados alumnos_revisados
           |> Map.toList
           |> List.map (fun (id, (xs, ys)) -> let profesor = Map.find id mapa_profesores
                                              (profesor.nombre + " " + profesor.apellidos, id, xs, ys))
           |> List.toArray
           |> Array.append resto
           |> Array.map (fun (nombre, id, xs, ys) -> 
                    String.concat "," [nombre
                                       string id
                                       matriculas_txt xs
                                       matriculas_txt ys])

let contar_asesorados parcial =
    parcial
        |> Library.leerAsignaciones
        |> Array.sumBy (fun (_, _, xs, _) -> List.length xs)

let contar_revisiones parcial =
    parcial
        |> Library.leerAsignaciones
        |> Array.sumBy (fun (_, _, xs, _) -> List.length xs)

let profesores_con_menos_alumnos parcial =
    let cprofesores = "Profesores.txt" |> Library.leerProfesores
    let asignaciones = Library.leerAsignaciones parcial
    let profs = Array.filter (fun (nombre, id, a, r) -> 
                    Array.forall (fun (_, id', _, _) -> id <> id') asignaciones ||
                    Array.exists (fun (_, id', xs, ys) -> 
                                          id = id' &&
                                          (List.length xs < a ||
                                           List.length ys < r)) asignaciones) cprofesores
    profs |> Array.iter (fun (nombre, id, a, r) -> printfn "Profesor %s!" nombre)

let nivelar_asesorias user (carrera, periodo) =
    let restricciones = Library.leerAsignaciones "AsignacionParcial.txt"
                            |> Array.toList
                            |> List.map (fun (_,_,xs,ys) -> List.map fst (xs @ ys))
                            |> List.concat
                            |> set
    let cprofesores = "Profesores.txt" |> Library.leerProfesores
                                       |> Array.map (fun (_, id, a, r) -> (id, (a,r)))
                                       |> Map.ofArray
    let asignaciones = Library.leerAsignaciones (carrera + ".txt")
                         |> Array.map (fun ((_, id, _, _) as r) -> (id,r))
                         |> Map.ofArray
    let ordenar_asesor ((n, i, xs, ys),(n', i', xs', ys')) =
        if List.length xs < List.length xs'
        then ((n, i, xs, ys),(n', i', xs', ys'))
        else ((n', i', xs', ys'),(n, i, xs, ys))
    let ordenar_revisor ((n, i, xs, ys),(n', i', xs', ys')) =
        if List.length ys < List.length ys'
        then ((n, i, xs, ys),(n', i', xs', ys'))
        else ((n', i', xs', ys'),(n, i, xs, ys))
        // Caso 1 los dos profesores tienen alumnos en carrera comun
        // Solucion: ceder alumno de un profesor a otro en carrera comun
    let caso1_asesor par =
        let (((n, i, xs, ys),(n', i', xs', ys')) as par) = ordenar_asesor par
        let s1 = set (List.map snd xs)
        let s2 = set (List.map snd xs')
        let inter = Set.intersect s1 s2
        Set.exists (fun plan -> 
            List.exists (fun (mat,plan') ->
                            plan = plan' &&
                            not (Set.contains mat restricciones) &&
                            List.forall (fun (mat',_) -> mat <> mat') ys
                            ) xs'
            ) inter
    let caso1_revisor par =
        let (((n, i, xs, ys),(n', i', xs', ys')) as par) = ordenar_revisor par
        let s1 = set (List.map snd ys)
        let s2 = set (List.map snd ys')
        let inter = Set.intersect s1 s2
        Set.exists (fun plan -> 
            List.exists (fun (mat,plan') ->
                            plan = plan' &&
                            not (Set.contains mat restricciones) &&
                            List.forall (fun (mat',_) -> mat <> mat') xs
                            ) ys'
            ) inter
    let sol1_asesor par =
        let (((n, i, xs, ys),(n', i', xs', ys')) as par) = ordenar_asesor par
        printfn "Caso 1 (asesor): %A" par
        let s1 = set (List.map snd xs)
        let s2 = set (List.map snd xs')
        let inter = Set.intersect s1 s2
        let alumno = 
            inter |> Set.toList
                  |> List.pick (fun plan -> 
                           List.pick (fun (mat,plan') ->
                               if plan = plan' &&
                                  not (Set.contains mat restricciones) &&
                                  List.forall (fun (mat',_) -> mat <> mat') ys
                               then Some (mat,plan')
                               else None
                           ) xs'
                           |> Some)
        ((n, i, alumno :: xs, ys),(n', i', List.filter (fun e -> e <> alumno) xs', ys'))
            |> Library.tap (printfn "Sol asesor: %A")
    let sol1_revisor par =
        let (((n, i, xs, ys),(n', i', xs', ys')) as par) = ordenar_revisor par
        printfn "Caso 1 (revisor): %A" par
        let s1 = set (List.map snd ys)
        let s2 = set (List.map snd ys')
        let inter = Set.intersect s1 s2
        let alumno = 
            inter |> Set.toList
                  |> List.pick (fun plan -> 
                           List.pick (fun (mat,plan') ->
                               if plan = plan' &&
                                  not (Set.contains mat restricciones) &&
                                  List.forall (fun (mat',_) -> mat <> mat') xs
                               then Some (mat,plan')
                               else None
                           ) ys'
                           |> Some)
        ((n, i, xs, alumno :: ys),(n', i', xs', List.filter (fun e -> e <> alumno) ys'))
            |> Library.tap (printfn "Sol revisor: %A")
    // Caso 2 los dos profesores no tienen alumnos en carrera comun
    // pero existe un profesor (pivote) que tiene alumnos en las dos carreras
    // Solucion: ceder alumno de un profesor al pivote y este a su vez cede al otro profesor
    let caso2_asesor asignaciones par =
        let (((n1, id1, xs1, ys1),(n2, id2, xs2, ys2)) as par) = ordenar_asesor par
        let s1 = set (List.map snd xs1)
        let s2 = set (List.map snd xs2)
        Map.toArray asignaciones
            |> Array.exists (fun (id, (n, i, xs, ys)) -> 
                                let s3 = set (List.map snd xs)
                                let inter_s3s1 = Set.intersect s3 s1
                                let inter_s3s2 = Set.intersect s3 s2
                                id <> id1 &&
                                id <> id2 &&
                                not (Set.isEmpty inter_s3s1) &&
                                not (Set.isEmpty inter_s3s2) &&
                                Set.exists (fun plan -> 
                                    List.exists (fun (mat,plan') ->
                                                    plan = plan' &&
                                                    not (Set.contains mat restricciones) &&
                                                    List.forall (fun (mat',_) -> mat <> mat') ys
                                                    ) xs2
                                    ) inter_s3s2 &&
                                Set.exists (fun plan -> 
                                    List.exists (fun (mat,plan') ->
                                                    plan = plan' &&
                                                    not (Set.contains mat restricciones) &&
                                                    List.forall (fun (mat',_) -> mat <> mat') ys1
                                                    ) xs
                                    ) inter_s3s1)
    let caso2_revisor asignaciones par =
        let (((n1, id1, xs1, ys1),(n2, id2, xs2, ys2)) as par) = ordenar_revisor par
        let s1 = set (List.map snd ys1)
        let s2 = set (List.map snd ys2)
        Map.toArray asignaciones
            |> Array.exists (fun (id, (n, i, xs, ys)) -> 
                                let s3 = set (List.map snd ys)
                                let inter_s3s1 = Set.intersect s3 s1
                                let inter_s3s2 = Set.intersect s3 s2
                                id <> id1 &&
                                id <> id2 &&
                                not (Set.isEmpty inter_s3s1) &&
                                not (Set.isEmpty inter_s3s2) &&
                                Set.exists (fun plan -> 
                                    List.exists (fun (mat,plan') ->
                                                    plan = plan' &&
                                                    not (Set.contains mat restricciones) &&
                                                    List.forall (fun (mat',_) -> mat <> mat') xs
                                                    ) ys2
                                    ) inter_s3s2 &&
                                Set.exists (fun plan -> 
                                    List.exists (fun (mat,plan') ->
                                                    plan = plan' &&
                                                    not (Set.contains mat restricciones) &&
                                                    List.forall (fun (mat',_) -> mat <> mat') xs1
                                                    ) ys
                                    ) inter_s3s1)
    let sol2_asesor saignaciones par =
        let (((n1, id1, xs1, ys1),(n2, id2, xs2, ys2)) as par) = ordenar_asesor par
        printfn "Caso 2 (asesor): %A" par
        let s1 = set (List.map snd xs1)
        let s2 = set (List.map snd xs2)
        let ((n, i, xs, ys), alumno2, alumno1) =
            Map.toArray asignaciones
                |> Array.pick (fun (id, (n, i, xs, ys)) -> 
                                let s3 = set (List.map snd xs)
                                let inter_s3s1 = Set.intersect s3 s1
                                let inter_s3s2 = Set.intersect s3 s2
                                if id <> id1 &&
                                   id <> id2 &&
                                   not (Set.isEmpty inter_s3s1) &&
                                   not (Set.isEmpty inter_s3s2) &&
                                   Set.exists (fun plan -> 
                                       List.exists (fun (mat,plan') ->
                                                       plan = plan' &&
                                                       not (Set.contains mat restricciones) &&
                                                       List.forall (fun (mat',_) -> mat <> mat') ys
                                                       ) xs2
                                       ) inter_s3s2 &&
                                   Set.exists (fun plan -> 
                                       List.exists (fun (mat,plan') ->
                                                       plan = plan' &&
                                                       not (Set.contains mat restricciones) &&
                                                       List.forall (fun (mat',_) -> mat <> mat') ys1
                                                       ) xs
                                       ) inter_s3s1
                                then let alumno2 = 
                                          inter_s3s2
                                            |> Set.toList
                                            |> List.pick (fun plan -> 
                                                if List.exists (fun (mat,plan') ->
                                                                   plan = plan' &&
                                                                   not (Set.contains mat restricciones) &&
                                                                   List.forall (fun (mat',_) -> mat <> mat') ys
                                                                   ) xs2
                                                then List.pick (fun (mat,plan') ->
                                                                   if plan = plan' &&
                                                                      not (Set.contains mat restricciones) &&
                                                                      List.forall (fun (mat',_) -> mat <> mat') ys
                                                                   then Some (mat,plan')
                                                                   else None
                                                               ) xs2
                                                               |> Some
                                                else None
                                                )
                                     let alumno1 = 
                                          inter_s3s1
                                            |> Set.toList
                                            |> List.pick (fun plan -> 
                                                if List.exists (fun (mat,plan') ->
                                                                    plan = plan' &&
                                                                    not (Set.contains mat restricciones) &&
                                                                    List.forall (fun (mat',_) -> mat <> mat') ys1
                                                               ) xs
                                                then List.pick (fun (mat,plan') ->
                                                                    if plan = plan' &&
                                                                       not (Set.contains mat restricciones) &&
                                                                       List.forall (fun (mat',_) -> mat <> mat') ys1
                                                                    then Some (mat,plan')
                                                                    else None
                                                               ) xs
                                                               |> Some
                                                else None
                                             )
                                     Some ((n, i, xs, ys), alumno2, alumno1)
                                else None)
        printfn "Pivote y alumnos: %A" ((n, i, xs, ys), alumno2, alumno1)
        ((n1, id1, alumno1 :: xs1, ys1),
         (n2, id2, List.filter (fun e -> e <> alumno2) xs2, ys2),
         (n, i, alumno2 :: List.filter (fun e -> e <> alumno1) xs, ys))
            |> Library.tap (printfn "Sol 2 asesor: %A")
    let sol2_revisor asignaciones par =
        let (((n1, id1, xs1, ys1),(n2, id2, xs2, ys2)) as par) = ordenar_revisor par
        printfn "Caso 2 (revisor): %A" par
        let s1 = set (List.map snd ys1)
        let s2 = set (List.map snd ys2)
        let ((n, i, xs, ys), alumno2, alumno1) =
            Map.toArray asignaciones
                |> Array.pick (fun (id, (n, i, xs, ys)) -> 
                                let s3 = set (List.map snd ys)
                                let inter_s3s1 = Set.intersect s3 s1
                                let inter_s3s2 = Set.intersect s3 s2
                                if id <> id1 &&
                                   id <> id2 &&
                                   not (Set.isEmpty inter_s3s1) &&
                                   not (Set.isEmpty inter_s3s2) &&
                                   Set.exists (fun plan -> 
                                       List.exists (fun (mat,plan') ->
                                                       plan = plan' &&
                                                       not (Set.contains mat restricciones) &&
                                                       List.forall (fun (mat',_) -> mat <> mat') xs
                                                       ) ys2
                                       ) inter_s3s2 &&
                                   Set.exists (fun plan -> 
                                       List.exists (fun (mat,plan') ->
                                                       plan = plan' &&
                                                       not (Set.contains mat restricciones) &&
                                                       List.forall (fun (mat',_) -> mat <> mat') xs1
                                                       ) ys
                                       ) inter_s3s1
                                then let alumno2 = 
                                          inter_s3s2
                                            |> Set.toList
                                            |> List.pick (fun plan -> 
                                                if List.exists (fun (mat,plan') ->
                                                                   plan = plan' &&
                                                                   not (Set.contains mat restricciones) &&
                                                                   List.forall (fun (mat',_) -> mat <> mat') xs
                                                                   ) ys2
                                                then List.pick (fun (mat,plan') ->
                                                                   if plan = plan' &&
                                                                      not (Set.contains mat restricciones) &&
                                                                      List.forall (fun (mat',_) -> mat <> mat') xs
                                                                   then Some (mat,plan')
                                                                   else None
                                                               ) ys2
                                                               |> Some
                                                else None
                                                )
                                     let alumno1 = 
                                          inter_s3s1
                                            |> Set.toList
                                            |> List.pick (fun plan -> 
                                                if List.exists (fun (mat,plan') ->
                                                                    plan = plan' &&
                                                                    not (Set.contains mat restricciones) &&
                                                                    List.forall (fun (mat',_) -> mat <> mat') xs1
                                                               ) ys
                                                then List.pick (fun (mat,plan') ->
                                                                    if plan = plan' &&
                                                                       not (Set.contains mat restricciones) &&
                                                                       List.forall (fun (mat',_) -> mat <> mat') xs1
                                                                    then Some (mat,plan')
                                                                    else None
                                                               ) ys
                                                               |> Some
                                                else None
                                             )
                                     Some ((n, i, xs, ys), alumno2, alumno1)
                                else None)
        printfn "Pivote y alumnos: %A" ((n, i, xs, ys), alumno2, alumno1)
        ((n1, id1, xs1, alumno1 :: ys1),
         (n2, id2, xs2, List.filter (fun e -> e <> alumno2) ys2),
         (n, i, xs, alumno2 :: List.filter (fun e -> e <> alumno1) ys))
            |> Library.tap (printfn "Sol 2 revisor: %A")
    let rec nivelar_asesores asignaciones =
        let par = asignaciones
                        |> Map.toArray
                        |> Array.map snd
                        |> (fun xs -> Array.allPairs xs xs)
                        |> Array.filter (fun ((_, id1, _, _),(_, id2, _, _)) -> id1 <> id2)
                        |> Array.tryFind (fun par -> 
                                           let (((n1, id1, xs1, ys1),(n2, id2, xs2, ys2)) as par) = ordenar_asesor par
                                           List.length xs1 < fst (Map.find id1 cprofesores) && 
                                           System.Math.Abs (List.length xs1 - List.length xs2) > 1)
        match par with
            Some par -> if caso1_asesor par
                        then let (((_, id1, _, _) as p1),((_, id2, _, _) as p2)) = sol1_asesor par
                             Map.add id1 p1 (Map.add id2 p2 asignaciones)
                                |> nivelar_asesores
                        elif caso2_asesor asignaciones par
                        then let (((_, id1, _, _) as p1),((_, id2, _, _) as p2),((_, id3, _, _) as p3)) = sol2_asesor asignaciones par
                             Map.add id1 p1 (Map.add id2 p2 (Map.add id3 p3 asignaciones))
                                |> nivelar_asesores
                        else asignaciones
          | None -> asignaciones
    let rec nivelar_revisores asignaciones =
        let par = asignaciones
                        |> Map.toArray
                        |> Array.map snd
                        |> (fun xs -> Array.allPairs xs xs)
                        |> Array.filter (fun ((_, id1, _, _),(_, id2, _, _)) -> id1 <> id2)
                        |> Array.tryFind (fun par -> 
                                           let (((n1, id1, xs1, ys1),(n2, id2, xs2, ys2)) as par) = ordenar_revisor par
                                           List.length ys1 < snd (Map.find id1 cprofesores) &&
                                           System.Math.Abs (List.length ys1 - List.length ys2) > 1)
        match par with
            Some par -> if caso1_revisor par
                        then let (((_, id1, _, _) as p1),((_, id2, _, _) as p2)) = sol1_revisor par
                             Map.add id1 p1 (Map.add id2 p2 asignaciones)
                                |> nivelar_revisores
                        elif caso2_revisor asignaciones par
                        then let (((_, id1, _, _) as p1),((_, id2, _, _) as p2),((_, id3, _, _) as p3)) = sol2_revisor asignaciones par
                             Map.add id1 p1 (Map.add id2 p2 (Map.add id3 p3 asignaciones))
                                |> nivelar_revisores
                        else asignaciones
          | None -> asignaciones
    let matriculas_txt matriculas =
        matriculas |> List.map (fun (mat, plan) -> "(" + mat + "," + plan + ")")
                   |> (fun matriculas -> "<" + String.concat "," matriculas + ">")
    nivelar_asesores asignaciones
           |> nivelar_revisores
           |> Map.toList
           |> List.map snd
           |> List.toArray
           |> Array.map (fun (nombre, id, xs, ys) -> 
                    String.concat "," [nombre
                                       string id
                                       matriculas_txt xs
                                       matriculas_txt ys])

let to_csv archivo =
    let asignaciones = Library.leerAsignaciones archivo
    let matriculas_txt matriculas =
        matriculas |> List.map (fun (mat, plan) -> mat + "(" + plan + ")")
                   |> (fun matriculas -> String.concat " " matriculas)
    let csv = asignaciones
                |> Array.map (fun (nombre, id, xs, ys) -> 
                    String.concat "," [nombre
                                       matriculas_txt xs
                                       matriculas_txt ys])
    let outFile = new StreamWriter(archivo + ".csv")
    outFile.WriteLine "Profesor,Asesor,Revisor"
    Array.iter (outFile.WriteLine : string -> unit) csv
    outFile.Flush()
    outFile.Close()
    Array.iter (fun (n, _, _, _) -> printfn "%s" n) asignaciones


let matriculas archivo =
    let asignaciones = Library.leerAsignaciones archivo
    let matriculas = 
            asignaciones 
                |> Array.toList
                |> List.map (fun (_, _, xs, _) -> List.map fst xs)
                |> List.concat
                |> set
                |> Set.toList
    matriculas
