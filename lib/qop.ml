let qop_h qst qbit = let _ = qbit in qst

let qop_x qst qbit = let _ = qbit in qst

(*let qop_y qst qbit = let _ = qbit in qst

let qop_z qst qbit = let _ = qbit in qst *)

let qop_cx qst controlbit qbit = let _ = qbit in let _ = controlbit in qst

let measure qst qbits = let _ = qbits in [(List.map (fun v -> v+.1.0) qst, 0, 0.5); (List.map (fun v -> v+.2.0) qst, 1, 0.5)]