(* ::Package:: *)

(*
ScriptCommandLine: When accessed in the script, 
list containing the name of the script as the first element and the rest of the command line arguments.
*)
Print["Mathematica File ", ToString[$ScriptCommandLine[[1]]]];
revMod=ToString[$ScriptCommandLine[[2]]]; Print["REV ", revMod];
persMod=ToString[$ScriptCommandLine[[3]]]; Print["PERS ", persMod];
confMod=ToString[$ScriptCommandLine[[4]]]; Print["CONF ", confMod];
TauFileIn=FileNameJoin[{$HomeDirectory, "GDC", "TAU2", ToString[$ScriptCommandLine[[5]]]}]; Print["Eingabe - Datei - TAU 2 ", TauFileIn];
PosFileIn=FileNameJoin[{$HomeDirectory, "GDC", "POS", ToString[$ScriptCommandLine[[6]]]}]; Print["Eingabe - Datei - POS ", PosFileIn];
SigmaFileIn=FileNameJoin[{$HomeDirectory, "GDC", "SIG2", ToString[$ScriptCommandLine[[7]]]}]; Print["Eingabe - Datei - SIG ", SigmaFileIn];
boxOut=FileNameJoin[{$HomeDirectory, "GDC", ToString[$ScriptCommandLine[[8]]]}]; Print["Ausgabe - Ordner ", boxOut];
filenameOut=ToString[$ScriptCommandLine[[9]]]; Print["Ausgabe - Datei ", filenameOut];



AppendTo[$Path,FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]];
If[!MemberQ[$Path, FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]], 
AppendTo[$Path, FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures"}]]];
Print["Package path ", FindFile[ "Argdown`"]];
Print["Package path ",FindFile["GDCAnalysis3`"]];
Print["Package path ",FindFile["GDCTools`"]];
<<Argdown`
<<GDCAnalysis3`
<<GDCTools`


modPersList=persMixFunc[persMod];
Print["Positions - Indexe ", modPersList];


TauIn=Get[TauFileIn];
SigIn=Get[SigmaFileIn];


(*confToolsFunc[tauFile_, sigFile_, posFile_, modPersList_, revMod_, modAll_]*)
confM=confToolsFunc[TauIn, SigIn, PosFileIn, modPersList, revMod , confMod];
(*bvFunc[tauF_,posF_, modPersList_, revMod_, posMod_]*)
bvE=bvFunc[TauIn, PosFileIn, modPersList, revMod, "EVBK"];
bvHEK=bvFunc[TauIn, PosFileIn, modPersList, revMod, "CHEVBK"];


SetDirectory[boxOut];
Put[{confM,{bvE,bvHEK}},filenameOut]
