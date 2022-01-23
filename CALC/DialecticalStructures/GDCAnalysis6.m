(* ::Package:: *)

(* ::Chapter:: *)
(*Package GDCAnalysis6*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for calculating the deductive closure of a position X*)


(* Wolfram Language Package *)
BeginPackage["DialecticalStructures`GDCAnalysis6`", { "DialecticalStructures`GDCAnalysis3`",  "DialecticalStructures`GDCTools`"}]


(* Exported symbols added here with SymbolName::usage *)  
dedAbSimpFunc::usage = "dedAbSimpFunc[h_]";
dedAbCalc::usage = "dedAbCalc[direcT_, ts_,modPersList_, revMod_, posMod_]";
dedAbToString::usage = "dedAbToString[direcT_, ts_, dedAb_]";
dedAbAllInFunc::usage = "dedAbAllInFunc[direcT_,pers_,rev_, posMod_]";



Begin["`Private`"] (* Begin Private Context *) 


dedAbSimpFunc[h_]:=Module[{tauH,lPre,l,z,tauN,lOut},
tauH=h[[1]];
lPre=h[[2]];
If[Length[tauH]>1,
l=Union[Cases[tauH, _Integer], Not/@Cases[Cases[tauH, _Not][[All,1]], _Integer]];
(*Print["l ", l];*)
z=Map[If[IntegerQ[#], #-> True, First[#]->False]&,l];
(*Print["z ", z];*)
tauN=tauH/.z;,
tauN=tauH;
];
If[Length[l]==0,lOut=lPre;, lOut=Union[l,lPre];];
Return[{tauN,lOut}];
];



dedAbCalc[direcT_, ts_,modPersList_, revMod_, posMod_]:=Module[{TauFile,tauFileName, posFileName,posAR, bkCON, evCON, timeSteps, posListe, ch, bk, ev, posTot, posNeu,posRep,tauN, tauNN,h},
tauFileName=direcT<>"/TAU2/GDC_Tau_Simplified_S"<>ToString[ts]<>".txt";
posFileName=direcT<>"POS/gdc_pos_S"<>ToString[ts]<>".txt";
(*Print["tauFileName ", tauFileName];
Print["posFileName ", posFileName];*)
TauFile=Get[tauFileName];
posAR=Get[posFileName];

bkCON=posAR[[1]][["BK"]]/.TauFile[[2]];
evCON=posAR[[1]][["EV"]]/.TauFile[[2]];

timeSteps={"S0", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"};

posListe=timePosFunc[posFileName, TauFile[[2]], timeSteps, modPersList, revMod];
(*Print["ALLES OK :  TIME STEP ", ts];*)

ch=posListe[[1]];
bk=posListe[[2]];
ev=posListe[[3]];

posTot=Which[posMod=="CH", Union[ch, evCON, bkCON],
posMod=="EV", Union[ev, evCON, bkCON],
posMod=="BK", Union[bk, evCON, bkCON],
posMod=="EVBK", Union[ev, bk, evCON, bkCON], 
posMod=="CHEVBK", Union[ch, ev, bk, evCON, bkCON]];

(*Print[" posTot ", posTot];*)
posNeu= (posTot/.TauFile[[3]])/.TauFile[[4]];
(*Print["posNeu ", posNeu];*)

posRep=Map[Function[x, If[IntegerQ[x],x->True, First[x]-> False]],posNeu];
tauN=TauFile[[1]]/.posRep;
(*Print["TAU vor Vereinfachung ", tauN];*)

(*tauNN=FixedPoint[dedAbSimpFunc,tauN,Length[tauN],SameTest->(Length[#1]==Length[#2]&)];*)
h={tauN,{}};
tauNN=FixedPoint[dedAbSimpFunc,h,Length[First[h]],SameTest->(Length[First[#1]]==Length[First[#2]]&)];

(*Ge\[ADoubleDot]ndert : posTot statt posNeu !!*)
Return[{tauNN[[1]],Catenate[{tauNN[[2]],posTot}]}];];



dedAbToString[direcT_, ts_, dedAb_]:=Module[{tauFileName,tauFile,tRulesRev,dedAbNew,dedAbString, tauString, dedAbStringTat},
tauFileName=direcT<>"/TAU2/GDC_Tau_Simplified_S"<>ToString[ts]<>".txt";
(*Print["tauFileName ", tauFileName];*)
tauFile=Get[tauFileName];

testingFunc[test_]:=Which[IntegerQ[test],{True, Union[{dedAb[[1]]},dedAb[[2]]]},
BooleanQ[Head[test]==Not],If[IntegerQ[test[[1]]],{True, Union[{dedAb[[1]]},dedAb[[2]]]},dedAb],
True,dedAb];

dedAbNew=testingFunc[dedAb[[1]]];

(*Print["dedAb 1 ", dedAb[[1]]];
Print["dedAbNew 1 ", dedAbNew[[1]]];*)

dedAbString=Map[If[NumberQ[#],tauFile[[2]][[#,1]],!tauFile[[2]][[#[[1]],1]]]&,dedAbNew[[2]]];
tRulesRev=Map[#[[2]]->#[[1]]&,tauFile[[2]]];

(*HIER FEHLER: tauString=If[!BooleanQ[dedAbNew[[1]]], dedAbNew[[1]]/.tRulesRev,
True];*)
tauString=If[!BooleanQ[dedAbNew[[1]]],
dedAbNew[[1]]/.tRulesRev,
dedAbNew[[1]]];
(*Print["tauString ", tauString];*)

If[ToString[tauString]=="False",Print["Hallo"];dedAbStringTat={};,dedAbStringTat=dedAbString;];

Return[{tauString,dedAbStringTat}];
];


dedAbAllInFunc[direcT_,pers_,rev_, posMod_]:=Module[{tsAll,ts,k,data,string},
Which[rev=="AR", tsAll=Range[9]-1; k=1;,
rev=="BR", tsAll=Range[8]; k=1;
];

ts=Which[pers=="DEV", tsAll,
pers=="DLB", tsAll,
pers=="MUR", Drop[tsAll,1*k],
pers=="LYE", Drop[tsAll,1*k],
pers=="PHI", Drop[tsAll,2*k],
pers=="SED", Drop[tsAll,3*k],
pers=="AUS", Drop[tsAll,6*k]
];
data=Map[dedAbCalc[direcT, ts[[#]],persMixFunc[pers],rev, posMod]&,Range[Length[ts]]];
(*Print["BIS HIER "];*)
string=Map[dedAbToString[direcT,ts[[#]],data[[#]]]&,Range[Length[ts]]];
Return[pers->string];
];




End[] (* End Private Context *)

EndPackage[]
