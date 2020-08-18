(* ::Package:: *)

(*
ScriptCommandLine: When accessed in the script, 
list containing the name of the script as the first element and the rest of the command line arguments.
*)
Print["Mathematica File ", ToString[$ScriptCommandLine[[1]]]];
FileIn=FileNameJoin[{$HomeDirectory, "GDC", "TAU2", ToString[$ScriptCommandLine[[2]]]}]; Print["Eingabe - Datei - Tau 2 ", FileIn];
PosFileIn=FileNameJoin[{$HomeDirectory, "GDC", "POS", ToString[$ScriptCommandLine[[3]]]}]; Print["Eingabe - Datei - POS ", PosFileIn];
boxOut=FileNameJoin[{$HomeDirectory, "GDC", ToString[$ScriptCommandLine[[4]]]}]; Print["Ausgabe - Ordner ", boxOut];
filenameOut=ToString[$ScriptCommandLine[[5]]]; Print["Ausgabe - Datei ", filenameOut];



AppendTo[$Path,FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]];
If[!MemberQ[$Path, FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]], 
AppendTo[$Path, FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]]];
Print["Package path ", FindFile[ "Argdown`"]];
Print["Package path ", FindFile[ "GDCAnalysis3`"]];
<<Argdown`
<<GDCAnalysis3`


tauListe=Get[FileIn];
tau=tauListe[[1]];
Print["tau ", tau];
Print["Anzahl S\[ADoubleDot]tze ", Length[BooleanVariables[tau]]];
idMap=tauListe[[2]];
erules=tauListe[[3]];
crules=tauListe[[4]];



modPersList=persMixFunc["CONCONCON"];
Print["modPersList ", modPersList];
posAssList=Get[PosFileIn];
bkP=posAssList[[modPersList[[2]]]][["BK"]]/.idMap;
Print["bk ", bkP];
evP=posAssList[[modPersList[[3]]]][["EV"]]/.idMap;
Print["ev ", evP];

sigmaBKEV=sigmaFunc[tau, idMap, erules, crules, bkP, evP];
Print["sigmaBKEV ", sigmaBKEV];


SetDirectory[boxOut];
Put[sigmaBKEV,filenameOut]
