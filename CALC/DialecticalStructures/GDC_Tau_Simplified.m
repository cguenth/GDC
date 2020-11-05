(* ::Package:: *)

(*
ScriptCommandLine: When accessed in the script, 
list containing the name of the script as the first element and the rest of the command line arguments.
*)
Print["Mathematica File ", ToString[$ScriptCommandLine[[1]]]];
jsonFileIn=FileNameJoin[{$HomeDirectory, "GDC", "JSON", ToString[$ScriptCommandLine[[2]]]}]; Print["Eingabe - JSON - Datei ", jsonFileIn];
boxOut=FileNameJoin[{$HomeDirectory, "GDC", ToString[$ScriptCommandLine[[3]]]}]; Print["Ausgabe - Ordner ", boxOut];
filenameOut=ToString[$ScriptCommandLine[[4]]]; Print["Ausgabe - Datei ", filenameOut];



AppendTo[$Path,FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]];
If[!MemberQ[$Path, FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]], 
AppendTo[$Path, FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]]];
Print["Package path ", FindFile[ "Argdown`"]];
Print["Package path ", FindFile[ "GDCDOJCalc4`"]];
<<Argdown`
<<GDCDOJCalc4`


argdown=Import[jsonFileIn];
idMap=IdMap[argdown];
tau=DeductiveConstraints[argdown]/.idMap;
Print["tau ", tau];
senIDs=Values[idMap];
Print["Number Sentences ", Length[senIDs]];


conpairs=Sort[Map[{#[[1]],#[[2]]}&,Cases[tau, (a_\[Xor]b_)]]];
Print["conpairs ",conpairs];
erules=RuleOutEquivalences[conpairs,senIDs];
Print["erules ", erules];
tauAndCrules=TauWithoutContradictions[erules, conpairs ,DeleteCases[tau,(a_\[Xor]b_)]];
tauNeu=tauAndCrules[[1]];
Print["tauNeu ", tauNeu];
Print["Number Sentences  ", Length[BooleanVariables[tauNeu]]];
crules=tauAndCrules[[2]];
Print["crules ", crules];


contrarypairs=Sort[Map[{#[[1]],#[[2]]}&,Cases[tauNeu, (a_\[Nand]b_)]]];
Print["contrarypairs ", contrarypairs];

contrariesNew=ContrariesNewForm[contrarypairs];
tauNeu2=DeleteCases[tauNeu, (a_\[Nand]b_)]&&contrariesNew;

Print["tauNeu2 ", tauNeu2];
Print["Number Sentences  ", Length[BooleanVariables[tauNeu2]]];


SetDirectory[boxOut];
Put[{tauNeu2,idMap,erules,crules},filenameOut]
