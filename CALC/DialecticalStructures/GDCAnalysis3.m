(* ::Package:: *)

(* ::Chapter:: *)
(*Package GDCAnalysis3*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for calculating Sigma*)


(* ::Item:: *)
(*for calculating DOJ*)


(* ::Item:: *)
(*for calculating Z and F*)


(* Wolfram Language Package *)

BeginPackage["DialecticalStructures`GDCAnalysis3`"]
(* Exported symbols added here with SymbolName::usage *)  

persMixFunc::usage = "persMixFunc[persMod_] splits persMod into three parts and indexes them rightly";
replaceFunc::usage = "replaceFunc[pos_, erules_, crules_]";
simpFunc::usage = "simpFunc[tauH_]";
sigmaFunc::usage = "sigmaFunc[tauNeu_,idMap_, erules_, crules_, pos1_,pos2_,pos3_]";
dojFunc::usage = "dojFunc[sigmaN_, sigmaZ_]";
zFunc::usage = "zFunc[pers_] calculates Z confirmation measure";
lFunc::usage = "lFunc[dojHEK_, dojEK_,dojH_] calculates Kemeny-Openheimer confirmation measure";
fFunc::usage = "fFunc[pers_] calculates F confirmation measure";

Begin["`Private`"] (* Begin Private Context *) 


persMixFunc[persMod_]:=Module[
{f,n,n1,n2,n3,modPersList},

f=Which[#=="CON", 1,
#=="DEV", 2,
#=="DLB", 3,
#=="RP1", 4,
#=="SF1", 5,
#=="USS", 6,
#=="MUR", 7,
#=="CFP", 8,
#=="LYE", 9,
#=="LP1", 10,
#=="PHI", 11,
#=="LVF", 12,
#=="LVS", 13,
#=="AP1", 14,
#=="SED", 15,
#=="AP2", 16,
#=="LP2", 17,
#=="RP2", 18,
#=="AUS", 19]&;

(*Print["L\[ADoubleDot]nge PersMod ", StringLength[persMod]];*)

If[StringLength[persMod]==3, n=persMod; modPersList=Map[f, {n,n,n}];,
n1=StringTake[persMod,{1,3}]; n2=StringTake[persMod,{4,6}]; n3=StringTake[persMod,{7,9}]; modPersList=Map[f, {n1,n2,n3}];];

Return[modPersList];
];


replaceFunc[pos_, er_, cr_]:=Module[{posNeu,posNeuRep},
posNeu=(pos/.er)/.cr;
Print["posNeu ", posNeu];
(* OLD x\[Rule]True *)
posNeuRep=Map[Function[x, 
If[IntegerQ[x],x->True, First[x]-> False]
],posNeu
];
Return[posNeuRep];
];


simpFunc[tauH_]:=Module[{l,z,tauN},
If[Length[tauH]>1,
l=Union[Cases[tauH, _Integer], Not/@Cases[Cases[tauH, _Not][[All,1]], _Integer]];
Print["l ", l];
z=Map[If[IntegerQ[#], #-> True, First[#]->False]&,l];
(*Print["z ", z];*)
tauN=tauH/.z;,
tauN=tauH;
];
Return[tauN];
];



sigmaFunc[tauNeu_, idMap_, erules_, crules_, pos1_ ,pos2_ ,pos3_]:=Module[{posTot, posRep, tauNN, tauNNN, range, rangeS, sigmaPos},
posTot=Union[pos1,pos2,pos3];
Print["posTot ", posTot];
posRep=replaceFunc[posTot,erules,crules];
Print["posRep ", posRep];
tauNN=tauNeu/.posRep;
Print["tauNN ", tauNN];
tauNNN=FixedPoint[simpFunc,tauNN,Length[tauNNN],SameTest->(Length[#1]==Length[#2]&)];
Print["tauNNN ", tauNNN];
range=BooleanVariables[tauNNN];
Print["Boolean Variables -  Numbers ", range];
rangeS=Map[Take[idMap[[All,1]],First[Position[idMap[[All,2]],#]]]&, BooleanVariables[tauNNN]];
Print["Boolean Variables - Sentences ", rangeS];
sigmaPos=SatisfiabilityCount[tauNNN,range];
Return[sigmaPos];
];


sigmaFunc[tauNeu_, idMap_, erules_, crules_, pos1_, pos2_]:=sigmaFunc[tauNeu, idMap, erules,crules, pos1,pos2,{}];
sigmaFunc[tauNeu_, idMap_, erules_, crules_, pos1_]:=sigmaFunc[tauNeu, idMap, erules,crules, pos1,{},{}];
sigmaFunc[tauNeu_, idMap_, erules_, crules_]:=sigmaFunc[tauNeu, idMap, erules,crules,{},{},{}];


dojFunc[sigmaZ_, sigmaN_]:=Module[{doj},
If[sigmaN==0, doj="DOJ ist nicht definiert! Division durch Null! ";, doj=N[sigmaZ/sigmaN];];
Return[{doj,sigmaZ,sigmaN}];
];


zFunc[ dojHEK_, dojH_]:=Module[
{z},
z=Which[StringQ[dojHEK], " z ist nicht definiert ",
Boole[dojHEK>=dojH && 0<dojH<1]==1,(dojHEK-dojH)/(1-dojH),
Boole[dojHEK==1 && dojH==1]==1, 1,
Boole[dojHEK==0 && dojH==0]==1, -1, 
Boole[dojHEK<dojH]==1,(dojHEK-dojH)/dojH];

Return[{z, dojHEK, dojH}];
];


lFunc[dojHEK_, dojEK_, dojH_]:=Module[
{dojEKH, dojNonH, dojNonHEK, dojEKNonH, L},

dojEKH=dojHEK*dojEK/dojH;
dojNonH=1-dojH;
dojNonHEK=1-dojHEK;
dojEKNonH=(dojNonHEK*dojEK)/dojNonH;

If[Boole[dojEKH+dojEKNonH==0]==1, L=" L ist nicht definiert ";, L=(dojEKH-dojEKNonH)/(dojEKH+dojEKNonH);];
Return[L];
];


fFunc[dojHEK_,dojEK_,dojH_]:=Module[
{f},
f=Which[StringQ[dojHEK], " f ist nicht definiert ",
Boole[dojHEK!=1 && dojHEK!=0]==1, lFunc[dojHEK, dojEK, dojH],
Boole[dojHEK==1 && dojEK!= 0]==1, 1,
Boole[dojHEK==0]==1, -1];

Return[{f,dojHEK,dojEK,dojH}];
];


End[] (* End Private Context *)

EndPackage[]
