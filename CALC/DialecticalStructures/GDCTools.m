(* ::Package:: *)

(* ::Chapter:: *)
(*Package GDCTools*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for analyzing POS*)


(* Wolfram Language Package *)
BeginPackage["DialecticalStructures`GDCTools`", { "DialecticalStructures`GDCAnalysis3`"}]


(* Exported symbols added here with SymbolName::usage *)  
timePosFunc::usage = "timePosFunc[PosFileIn_, idMap_, timeSteps_, modPersList_, revMod_]";
tauCHPoolFunc::usage = "tauCHPoolFunc[tauI_, posMod_, chP_, eC_,kC_,e_,k_]";
tauNeuFunc::usage = "tauNeuFunc[TauFile_,posFile_,  persMod_, persInd_]";
confToolsFunc::usage = "dojToolsFunc[tauFile_, sigFile_, posFile_, persInd_, modAll_]";
confCHPoolFunc::usage = "confCHPoolFunc[sigFI_,tauFI_,posFI_, chPL_, persL_, revM_, modAll_]";
boVaFunc::usage = "boVaFunc[set_, tauF_]";
minmaxFunc::usage = "minmaxFunc[conf_, chPI_]";
hFunc::usage = "hFunc[iL1_,bvL_]";
satIntFunc::usage = "satIntFunc[tauFile_, posFile_, persInd_, mod_,num_]";
diffFunc::usage = "diffFunc[tauF_,posF_,persInd1_, persInd2_, mod_]";
bvFunc::usage = "bvFunc[tauF_,posF_, persI_,posMod_]";


Begin["`Private`"] (* Begin Private Context *) 


timePosFunc[PosFileIn_, idMap_, timeSteps_, modPersList_, revMod_]:=Module[
{posAssList, chP, bkP, evP, h, p, PosFileInBR, posAssList1, posAssList2},

Which[revMod=="AR",
posAssList=Get[PosFileIn];
chP=posAssList[[modPersList[[1]]]][["CH"]]/.idMap;
bkP=posAssList[[modPersList[[2]]]][["BK"]]/.idMap;
evP=posAssList[[modPersList[[3]]]][["EV"]]/.idMap;,
revMod=="BR",
h=StringSplit[StringSplit[PosFileIn, "_",All][[-1]],"."][[1]];
p=Take[timeSteps,Flatten[Position[timeSteps,h]-1]][[1]];
PosFileInBR=StringReplace[PosFileIn, h->p];
Print["PosFileInBR ", PosFileInBR];
(*Position von Zeitschritt vor dem angegebenen Zeitschritt wird eingelesen*)
posAssList=Get[PosFileInBR];
chP=posAssList[[modPersList[[1]]]][["CH"]]/.idMap;
bkP=posAssList[[modPersList[[2]]]][["BK"]]/.idMap;
evP=posAssList[[modPersList[[3]]]][["EV"]]/.idMap;,
revMod=="ER",
h=StringSplit[StringSplit[PosFileIn, "_",All][[-1]],"."][[1]];
p=Take[timeSteps,Flatten[Position[timeSteps,h]-1]][[1]];
PosFileInBR=StringReplace[PosFileIn, h->p];
Print["PosFileInBR ", PosFileInBR];
(*Position von Zeitschritt vor dem angegebenen Zeitschritt wird eingelesen*)
posAssList1=Get[PosFileInBR];
posAssList2=Get[PosFileIn];
chP=posAssList1[[modPersList[[1]]]][["CH"]]/.idMap;
bkP=posAssList1[[modPersList[[2]]]][["BK"]]/.idMap;
evP=posAssList2[[modPersList[[3]]]][["EV"]]/.idMap;,
revMod=="EKR",
h=StringSplit[StringSplit[PosFileIn, "_",All][[-1]],"."][[1]];
p=Take[timeSteps,Flatten[Position[timeSteps,h]-1]][[1]];
PosFileInBR=StringReplace[PosFileIn, h->p];
Print["PosFileInBR ", PosFileInBR];
(*Position von Zeitschritt vor dem angegebenen Zeitschritt wird eingelesen*)
posAssList1=Get[PosFileInBR];
posAssList2=Get[PosFileIn];
chP=posAssList1[[modPersList[[1]]]][["CH"]]/.idMap;
bkP=posAssList2[[modPersList[[2]]]][["BK"]]/.idMap;
evP=posAssList2[[modPersList[[3]]]][["EV"]]/.idMap;
];

(*Print["ch ", chP];
Print["bk ", bkP];
Print["ev ", evP];*)

Return[{chP,evP,bkP}];
];



tauNeuFunc[TauFile_, PosFileIn_, modPersList_, revMod_, posMod_]:=Module[{
posAR, bkCON, evCON, timeSteps, posListe, ch, bk, ev, posTot, posNeu,posRep,tauN, tauNN},

posAR=Get[PosFileIn];
bkCON=posAR[[1]][["BK"]]/.TauFile[[2]];
evCON=posAR[[1]][["EV"]]/.TauFile[[2]];

timeSteps={"S0", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"};

posListe=timePosFunc[PosFileIn, TauFile[[2]], timeSteps, modPersList, revMod];

ch=posListe[[1]];
bk=posListe[[2]];
ev=posListe[[3]];

posTot=Which[posMod=="CH", Union[ch, evCON, bkCON],
posMod=="EV", Union[ev, evCON, bkCON],
posMod=="BK", Union[bk, evCON, bkCON],
posMod=="EVBK", Union[ev, bk, evCON, bkCON], 
posMod=="CHEVBK", Union[ch, ev, bk, evCON, bkCON]];

Print[" posTot ", posTot];
posNeu= (posTot/.TauFile[[3]])/.TauFile[[4]];
Print["posNeu ", posNeu];


posRep=Map[Function[x, If[IntegerQ[x],x->True, First[x]-> False]],posNeu];
tauN=TauFile[[1]]/.posRep;
Print["TAU vor Vereinfachung ", tauN];
tauNN=FixedPoint[simpFunc,tauN,Length[tauN],SameTest->(Length[#1]==Length[#2]&)];

Return[tauNN];];


tauCHPoolFunc[tauI_, posMod_, chP_, eC_,kC_,e_,k_]:=Module[
{posT, posN, posRep, tauN, tauNN},
posT=Which[posMod=="CHP", Union[chP, eC, kC],
posMod=="EVBK", Union[ e, k, eC, kC],
posMod=="CHPEVBK", Union[chP, e, k, eC, kC]];
(*Print[" posT ", posT];*)
posN= (posT/.tauI[[3]])/.tauI[[4]];
(*Print["posN ", posN];*)
posRep=Map[Function[x, If[IntegerQ[x],x->True, First[x]-> False]],posN];
tauN=tauI[[1]]/.posRep;
(*Print["TAU vor Vereinfachung ", tauN];*)
tauNN=FixedPoint[simpFunc,tauN,Length[tauN],SameTest->(Length[#1]==Length[#2]&)];
Return[tauNN];];


confToolsFunc[tauFile_, sigFile_, posFile_, modPersList_, revMod_, modAll_]:=Module[
{tauEK,tauHEK,sigEK,sigHEK,dojHEK, tauH, sigH, sig, dojH, zHEK, dojEK, fHEK, rListe},

(*tauNeuFunc[TauFile_, PosFileIn_, modPersList_, revMod_, posMod_]*)
tauEK=tauNeuFunc[tauFile, posFile, modPersList, revMod, "EVBK"];
Print["TAU nach Vereinfachung (EVBK) : ", tauEK];
tauHEK=tauNeuFunc[tauFile, posFile, modPersList, revMod, "CHEVBK"];
Print["TAU nach Vereinfachung (CHEVBK) : ", tauHEK];
sigEK=SatisfiabilityCount[tauEK,BooleanVariables[tauEK]];
sigHEK=SatisfiabilityCount[tauHEK,BooleanVariables[tauHEK]];
(*returns {doj,sigmaZ,sigmaN}*)
dojHEK=dojFunc[sigHEK, sigEK];

If[modAll!= "ALL", rListe=dojHEK;,
tauH=tauNeuFunc[tauFile, posFile, modPersList, revMod, "CH"];
Print["TAU nach Vereinfachung (CH) : ", tauH];
sigH=SatisfiabilityCount[tauH,BooleanVariables[tauH]];
Print["sigmaH ", sigH];
sig=sigFile;
Print["sigma ", sig];
dojH=dojFunc[sigH, sig];
Print[" dojH ", dojH];
zHEK=zFunc[First[dojHEK], First[dojH]];
Print[" z ", zHEK];
dojEK=dojFunc[sigEK, sig];
Print[" dojEK ", dojEK];
fHEK=fFunc[First[dojHEK], First[dojEK], First[dojH]];
Print[" f ", fHEK];
rListe={dojHEK,zHEK,fHEK};
];

Return[rListe];
];


confCHPoolFunc[sigFI_,tauFI_,posFI_, chPL_, persL_, revM_, modAll_]:=Module[
{pos,
backC,eviC,posL,back,evi, chP,
timeSteps,
tauCHP, tauEVBK,tauCHPEVBK,
sigEVBK,sigCHPEVBK,dojCHPEVBK,
sig,sigCHP,dojCHP,zCHPEVBK,
dojEVBK,fCHPEVBK,
rListe},

pos=Get[posFI];
backC=pos[[1]][["BK"]]/.tauFI[[2]];
eviC=pos[[1]][["EV"]]/.tauFI[[2]];

timeSteps={"S0", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"};

posL=timePosFunc[posFI, tauFI[[2]], timeSteps, persL, revM];
back=posL[[2]];
evi=posL[[3]];

chP=chPL/.tauFI[[2]];

tauEVBK=tauCHPoolFunc[tauFI,"EVBK",chP,eviC, backC,evi,back];
tauCHPEVBK=tauCHPoolFunc[tauFI,"CHPEVBK",chP,eviC, backC,evi,back];
sigEVBK=SatisfiabilityCount[tauEVBK,BooleanVariables[tauEVBK]];
(*Print["sigmaEVBK ", sigEVBK];*)
sigCHPEVBK=SatisfiabilityCount[tauCHPEVBK,BooleanVariables[tauCHPEVBK]];
(*returns {doj,sigmaZ,sigmaN}*)
dojCHPEVBK=dojFunc[sigCHPEVBK, sigEVBK];

If[modAll!= "ALL", rListe=dojCHPEVBK;,
tauCHP=tauCHPoolFunc[tauFI,"CHP",chP,eviC, backC,evi,back];
(*Print["TAU nach Vereinfachung (CH) : ", tauCHP];*)
sigCHP=SatisfiabilityCount[tauCHP,BooleanVariables[tauCHP]];
(*Print["sigmaCHP ", sigCHP];*)
sig=sigFI;
(*Print["sigma ", sig];*)
dojCHP=dojFunc[sigCHP, sig];
(*Print[" dojCHP ", dojCHP];*)
zCHPEVBK=zFunc[First[dojCHPEVBK], First[dojCHP]];
(*Print[" z ",zCHPEVBK];*)
dojEVBK=dojFunc[sigEVBK, sig];
(*Print[" dojEVBK ", dojEVBK];*)
fCHPEVBK=fFunc[First[dojCHPEVBK], First[dojEVBK], First[dojCHP]];
(*Print[" f ", fCHPEVBK];*)
rListe={dojCHPEVBK,zCHPEVBK,fCHPEVBK};
];

Return[rListe];
];


boVaFunc[set_, tauF_]:=Module[{l},
l=Map[Take[tauF[[2]][[All,1]],First[Position[tauF[[2]][[All,2]],#]]]&,set];
Return[Flatten[l]];
];


minmaxFunc[conf_, chPI_] := 
  Module[{min, max, posMin, posMax, minStrings, maxStrings},
   min = Min[conf];
   max = Max[conf];
   posMin = Position[conf, min];
   minStrings = Map[Take[chPI, #] &, posMin];
   posMax = Position[conf, max];
   maxStrings = Map[Take[chPI, #] &, posMax];
   Return[{<|"name" -> "MIN", "value" -> min, 
      "list" -> minStrings|>, <|"name" -> "MAX", "value" -> max, 
      "list" -> maxStrings|>, conf}];
   ];


hFunc[iL1_,bvL_]:=Module[{},
Map[If[iL1[[#]]==True,bvL[[#]],!bvL[[#]]]&,Range[Length[iL1]]]
];


satIntFunc[tau_,num_]:=Module[{intL,intLN,int,dWW},
intL=SatisfiabilityInstances[tau,BooleanVariables[tau],num];
intLN=Map[hFunc[#,BooleanVariables[tau]]&,intL];
Print["Erste Instanz : ", First[intLN]];
int=Intersection@@intLN;
(*Print["Schnittmenge aller Instanzen : ", int];*)
dWW=Map[Complement[#,int]&,intLN];
(*Print["Liste mit Komplementen : ", dWW];*)
Return[dWW];
];



bvFunc[tauF_,posF_, modPersList_, revMod_, posMod_]:=Module[{tau, sig,  idMap, compListe,bvListe,bvListeString, numBV, result},
idMap=tauF[[2]];
tau=tauNeuFunc[tauF, posF, modPersList, revMod, posMod];
Print["tau ", tau];
sig=SatisfiabilityCount[tau,BooleanVariables[tau]];
Print["Anzahl Instanzen ", sig];

result=Which[1<sig<50000,
compListe=satIntFunc[tau,sig];
bvListe=Cases[Union@Catenate@compListe, _Integer];
Print["Boolean Variables : ", bvListe];
bvListeString=Map[idMap[[#]][[1]]&,bvListe];
numBV=Length[bvListe];
{numBV,bvListeString},
sig==1,
Print["Es gibt nur eine Instanz, da TAU ", tau];
{},
sig>=50000,
Print["Anzahl der Instanzen > 50 000 ========  ABBRUCH"];
{}
];

Return[result];
];


diffFunc[tauF_,posF_,persInd1_, persInd2_, mod_]:=Module[
{tau, persAss1, persAss2, pos1, pos11, pos2, pos21, pos111, pos211, com1, com2,wwDiff},

tau=tauF[[1]];
persAss1=posF[[persInd1]];
persAss2=posF[[persInd2]];

Print["Name 1 ", persAss1["name"]/.tauF[[2]]];
Print["Name 2 ", persAss2["name"]/.tauF[[2]]];

pos1=Union[persAss1["BK"]/.tauF[[2]],persAss1["EV"]/.tauF[[2]]];
pos11= (pos1/.tauF[[3]])/.tauF[[4]];
Print["Position 1 ", pos11];

pos2=Union[persAss2["BK"]/.tauF[[2]],persAss2["EV"]/.tauF[[2]]];
pos21= (pos2/.tauF[[3]])/.tauF[[4]];
Print["Postion 2 ", pos21];

pos111=Map[Function[x, If[IntegerQ[x],x, First[x]]],pos11];
pos211=Map[Function[x, If[IntegerQ[x],x, First[x]]],pos21];

Which[mod=="WWD",
com1=Complement[Complement[pos11,pos21],Complement[pos111,pos211]];
Print["Aussagen, die jeweils einen anderen Wahrheitswert haben : ", com1];
com2=Map[Function[x, If[IntegerQ[x],x, First[x]]],com1];
wwDiff=boVaFunc[com2,tauF];,
mod=="SMD",
(*Aussagen, die jeweils einen anderen Wahrheitswert haben :*)
com1=Complement[pos111,pos211];
Print["Aussagen, die in Position 2 nicht enthalten sind : ", com1];
com2=Complement[pos211,pos111];
Print["Aussagen, die in Position 1 nicht enthalten sind : ", com2];
wwDiff={boVaFunc[com1,tauF],boVaFunc[com2,tauF]};
];

Return[wwDiff];
];






End[] (* End Private Context *)

EndPackage[]
