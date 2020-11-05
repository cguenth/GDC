(* ::Package:: *)

(* ::Chapter:: *)
(*Package GDCComparisons*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for Comparisons between ...*)


(* Wolfram Language Package *)
BeginPackage["DialecticalStructures`GDCComparisons`"]


(* Exported symbols added here with SymbolName::usage *) 
fileNameFunc::usage = "fileNameFunc[pers_,ts_]";
pointsSXWP::usage = "pointsSXWP[accData_, confVVData_, pers_, ts_]";
pointsWithoutPersons::usage = "pointsWithoutPersons[accData_, confVVData_, pers_]";
histoAccFunc::usage = "histoAccFunc[points_, accL_, accH_, ts_]";
numEVBKFunc::usage = "numEVBKFunc[posL_, pers_,ts_]";
tableFunc::usage = "tableFunc[data_, pers_]";
tableFunc2::usage = "tableFunc2[data_]";
maxValFunc::usage = "maxValFunc[tsAF_, tsBE_, dataAF_, dataBE_, pers_, conf_]"; 
vvCHMaxFunc::usage = "vvCHMaxFunc[tsAF_, tsBE_,dataAF_, dataBE_, pers_]";
vvHistPointsFunc::usage = "vvHistPointsFunc[dataIn_]";
histFunc::usage = "histFunc[inD_, revM_, ts_]";
compValCHFunc::usage = "compValCHFunc[maxCH_, tatCH_]";
compCHMaxFunc::usage = "compCHMaxFunc[persI_, ts_, chT_, chM_]";
compPosFunc::usage = "compPosFunc[persI1_, persI2_, ts_, ch_, compMod_, pMod_]";
vvPosFunc::usage = "vvPosFunc[persI_, ts_, ch_, compMod_, pMod_]";
arrayVVFunc::usage = "arrayVVFunc[persI_, numT_,ts_, ch_, compMod_, pMod]";
arrayFunc::usage = "arrayFunc[persI1_, persI2_,numT_,ts_, ch_, compMod_]";
list3DFunc::usage = "list3DFunc[dataIn_, tInd_, mod_]";


Begin["`Private`"] (* Begin Private Context *) 


fileNameFunc[pers_,ts_]:=Module[{tsL, mod,tsTat, fileN},
tsL={0,1,2,3,4,5,6,7,8};
If[MemberQ[tsL,ts],mod="AR";tsTat=ts+0.;,mod="BR";tsTat=ts+0.5;];
fileN="/home/carla/GDC/CONF/"<>pers<>"/GDC_Conf_"<>mod<>"_"<>pers<>"_ALL_Out/GDC_CONF_"<>mod<>"_"<>pers<>"_ALL_S"<>ToString[tsTat]<>"txt";
Return[fileN];
];


pointsSXWP[accData_, confVVData_, pers_, ts_]:=Module[{outData,tsRed,v,timL},
timL={0.,0.5,1.,1.5,2.,2.5,3.,3.5,4.,4.5,5.,5.5,6.,6.5,7.,7.5,8.};
Which[pers=="DLB",tsRed=Drop[timL,0];,
pers=="MUR",tsRed=Drop[timL,2];,
pers=="LYE", tsRed=Drop[timL,2];,
pers=="PHI",tsRed=Drop[timL,4];,
pers=="SED",tsRed=Drop[timL,6];,
pers=="AUS", tsRed=Drop[timL,12];
];
(*Print["pers ", pers];
Print["tsRed ",tsRed];
Print["ts ",ts];*)

If[MemberQ[tsRed,ts],
v=First@First@Position[tsRed,ts];
outData=Map[{accData[[pers]][[v,2]],#}&,
Cases[confVVData[[pers]], p_/;accData[[pers]][[v,1]]==First[p]][[All,2]]
];,
outData={};];

Return[outData];
];


pointsWithoutPersons[accData_, confVVData_, pers_]:=Module[{outData},
outData=Map[Function[v, 
Map[Function[u,{accData[[pers]][[v,2]],u}],
(*Cases[confVVData[[pers]], p_/;MemberQ[{accData[[pers]][[v,1]],(accData[[pers]][[v,1]]+0.5)},First[p]]][[All,2]]*)
Cases[confVVData[[pers]], p_/;accData[[pers]][[v,1]]==First[p]][[All,2]]
]],
Range[Length[accData[[pers]]]]
];
Return[outData]];


histoAccFunc[points_, accL_, accH_, ts_]:=Module[{pointsOut,plotLab, l, ant, prob, hist},
(* points {{acc,VV(CHMax)}} *)
If[accH=="NULL",
pointsOut=Map[Cases[#, p_/; accL== p[[1]]][[All,2]]&,points];
plotLab="ACC = "<>ToString[accL];,
pointsOut=Map[Cases[#, p_/; accL<p[[1]]<= accH][[All,2]]&,points];
plotLab=ToString[accL]<>"<=ACC<"<>ToString[accH];
];
If[Length[points]==3,
hist=Histogram[pointsOut, PlotLabel->"VV(CH=MAX),    "<> plotLab <>",    S"<>ToString[ts],ImageSize->400, ChartLegends->{"DOJ","Z","F"},ChartStyle->{Red,Blue,Yellow}];,
hist=Histogram[pointsOut, PlotLabel->"VV(CH=MAX),    "<> plotLab <>",    S"<>ToString[ts],ImageSize->400];];

Return[hist];
];


numEVBKFunc[posL_, pers_,ts_]:=Module[{list,middle},
list=Map[Length@Union[posL[[#]][[First@persMixFunc[pers]]][["BK"]],posL[[#]][[First@persMixFunc[pers]]][["EV"]]]&,ts];
middle=N[Total[list]/Length[list]];
Return[{middle,list}];
];


tableFunc[data_, pers_, mod_]:=Module[{g, persL,tableEnt, itemSL, persLTat},
persL={"DLB","MUR","LYE","PHI","SED","AUS"};
persLTat=Drop[persL, First[Position[persL,pers]]];
If[mod=="ALL",tableEnt={ToString[pers]<>" Compared With", "S1","S1.5","S2","S2.5","S3","S3.5","S4","S4.5","S5","S5.5","S6","S6.5","S7","S7.5","S8"};
itemSL={11,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7};,
tableEnt={ToString[pers]<>" Compared With", "S1","S2","S3","S4","S5","S6","S7","S8"};
itemSL={11,7,7,7,7,7,7,7,7}];
g=Grid[Prepend[Map[Prepend[data[[pers]][[#]],persLTat[[#]]]&,Range[5]], tableEnt],
Background->{None,{Lighter[Yellow,.9],{White,Lighter[Blend[{Blue,Green}],.8]}}},
Dividers->{{Darker[Gray,.6],{Lighter[Gray,.5]},Darker[Gray,.6]},{Darker[Gray,.6],Darker[Gray,.6],{False},Darker[Gray,.6]}},
Alignment->{{Left,Right,{Right}}},ItemSize->{itemSL},Frame->Darker[Gray,.6],ItemStyle->14,Spacings->{Automatic,.8}];
Return[g];];


tableFunc2[data_]:=Module[{g, persL},
persL={"DLB","MUR","LYE","PHI","SED","AUS"};
g=Grid[Prepend[Map[Prepend[data[[#]],persL[[#]]]&,Range[6]], {Null,"S0","S0.5","S1","S1.5","S2","S2.5","S3","S3.5","S4","S4.5","S5","S5.5","S6","S6.5","S7","S7.5","S8"}],
Background->{None,{Lighter[Yellow,.9],{White,Lighter[Blend[{Blue,Green}],.8]}}},
Dividers->{{Darker[Gray,.6],{Lighter[Gray,.5]},Darker[Gray,.6]},{Darker[Gray,.6],Darker[Gray,.6],{False},Darker[Gray,.6]}},
Alignment->{{Left,Right,{Right}}},ItemSize->{{7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7}},Frame->Darker[Gray,.6],ItemStyle->14,Spacings->{Automatic,.8}];
Return[g];];


maxValFunc[tsAF_, tsBE_, dataAF_, dataBE_, pers_, conf_]:=Module[{valAR, valBE, histoAF, histoBE},
valAR=Map[{tsAF[[#]],dataAF[[#,2]][["value"]]}&, Range[Length[dataAF]]];
valBE=Map[{tsBE[[#]],dataBE[[#,2]][["value"]]}&, Range[Length[dataBE]]];

histoAF=histFunc[dataAF,"AR",tsAF,pers, conf];
histoBE=histFunc[dataBE,"BR", tsBE,pers, conf];

Return[Union[valAR,valBE]];
];


vvCHMaxFunc[tsAF_, tsBE_,dataAF_, dataBE_, pers_]:=Module[{vvAR,vvBR},
vvAR=Map[{#,vvPosFunc[First[persMixFunc[pers]],# ,dataAF, "CH", "PEN"]}&,tsAF];
vvBR=Map[{#,vvPosFunc[First[persMixFunc[pers]],# ,dataBE, "CH", "PEN"]}&,tsBE];
Return[Union[vvAR,vvBR]];
];


vvHistPointsFunc[dataIn_]:=Module[{dataOut},
dataOut=Flatten[Map[
Function[x,Map[
Function[y,{x[[1]],y}],x[[2]]]
],dataIn],1];
Return[dataOut];];


histFunc[inD_, revM_, ts_, persName_, conf_]:=Module[{l,hD, hP,dirString},
l=Length[inD];
hD=Map[inD[[#,3]]&,Range[l]];
hP=Map[Histogram[hD[[#]],{Min[hD[[#]]],Max[hD[[#]]]+(Abs[Max[hD[[#]]]-Min[hD[[#]]]]/100),Abs[Max[hD[[#]]]-Min[hD[[#]]]]/100},"Probability",PlotTheme->"Scientific", PlotLabel->conf<>" "<>revM<>" S"<>ToString[ts[[#]]]]&,Range[l]];
dirString=FileNameJoin[{$HomeDirectory, "GDC", "CH_Pool",persName, "HIST", conf}];
SetDirectory[dirString];
Map[Export[persName<>"_HIST_"<>conf<> revM<>"_S"<>ToString[ts[[#]]]<>".jpeg", hP[[#]],ImageSize->850]&,Range[l]]
];


compValCHFunc[ch1_, ch2_]:=Module[{vv}, 
vv=N[Length[Intersection[ch2,ch1]]/Length[ch1]];
Return[vv];
];


compDistFunc[pos1_, pos2_, pMod_]:=Module[{z1,z2,z31,z32,z3,z,pMax,p}, 
(* z1 :  Menge der \[CapitalUDoubleDot]bereinstimmungen *)
z1=Intersection[pos1,pos2];
(* z2 :  Menge der Kontradiktionen *)
z2=Intersection[pos1,Map[Not[#]&,pos2]];
(* z31 : Menge der Aussagen, die nur in pos1 enthalten sind! 
Bsp mit pos1={1,2,3,4,!6,!7,!8}; pos2={0,2,!3,4,5,!6,7}; z31={1,!8}*)
z31=Complement[pos1,z1,z2];
(* z32 : Menge der Aussagen, die nur in pos2 enthalten sind!
Obiges Bsp. z32={0,5} *)
z32=Complement[pos2,z1,Map[Not[#]&,z2]];
(* z3 : Menge der Aussagen, die entweder nur in pos1 oder pos2 enthalten sind! *)
z3=Union[z31,z32];
(*Print["L\[ADoubleDot]nge z3 ", Length[z3]];*)
(*Print["z1 ", z1, " z2 ", z2, "  z3 ", z3, " z31 ", z31, " z32 ", z32];*)
If[pMod=="PEN",
(* PENALTY TAT=PEN/MAX=PEN
 TAT=PEN  :  0*|\[CapitalUDoubleDot]bereinstimmung| + (-3)*|Kontradiktionen|+ (-1)*|Ohne Partner|
 MAX=PEN : Alle geteilten Aussagen seien Kontradiktionen! : (-3)*(|Kontradiktionen|+|\[CapitalUDoubleDot]bereinstimmungen|)+ (-1)*|Ohne Partner|
 
 F\[UDoubleDot]r pos2 = DEVEVBK_DEDAB ist diese Option nicht sinnvoll, da |Ohne Partner| \[UDoubleDot]ber die Ma\[SZ]en gro\[SZ] ist, 
 da DEVEVBK_DEDAB zum Zeitpunkt S8 berechnet wird und \[UDoubleDot]berdies viel gr\[ODoubleDot]\[SZ]er weil DEDAB !!!! *)
 
z=((-3)*Length[z2])- Length[z3];
pMax=((-3)*(Length[z1]+Length[z2])) - Length[z3];,
(* ACC
|Kontradiktionen|/ |EVBK=PERS| *)
z=Length[z2];
pMax=Length[pos1];
];
(*Print["z ", z];
Print["pMax ", pMax];*)
p=N[z/pMax];
Return[1-p];
];


compCHMaxFunc[persI_, ts_, chT_, chM_]:=Module[{compL, t1, t2, pI, h},
t1={0,1,2,3,4,5,6,7,8};
t2={0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5};
pI=First@If[MemberQ[t1,ts],Flatten[Position[t1,ts]],Flatten[Position[t2,ts]]];
(*Print["pI ", pI];*)
Which[persI==19,
h=pI-6;,
persI==15,
h=pI-3;,
persI==11,
h=pI-2;,
persI==9,
h=pI-1;,
persI==7,
h=pI-1;,
persI==3,
h=pI;
];
compL=Map[compValCHFunc[#,chT[[pI]][[persI]][["CH"]]]&,Flatten[chM[[h,2]][["list"]],1]];
Return[compL];
];


compPosFunc[persI1_, persI2_, ts_, data_, compMod_, pMod_]:=Module[{compL, t1,t2, pI, pos1, pos2, conBK,conEV},
t1={0,1,2,3,4,5,6,7,8};
t2={0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5};
pI=First@If[MemberQ[t1,ts],Flatten[Position[t1,ts]],Flatten[Position[t2,ts]]];
(*Print["pI ", pI];*)
Which[compMod=="CH",
pos1=data[[pI]][[persI1]][["CH"]];
pos2=data[[pI]][[persI2]][["CH"]];
compL=compValCHFunc[pos1,pos2];,
compMod=="EV",
If[MemberQ[t2,ts],
conEV=data[[pI+1]][[1]][["EV"]];, 
conEV=data[[pI]][[1]][["EV"]];];
pos1=Union[data[[pI]][[persI1]][["EV"]],conEV];
pos2=Union[data[[pI]][[persI2]][["EV"]],conEV];
compL=compDistFunc[pos1,pos2, pMod];,
compMod=="BK",
If[MemberQ[t2,ts],
conBK=data[[pI+1]][[1]][["BK"]];, 
conBK=data[[pI]][[1]][["BK"]];];
pos1=Union[data[[pI]][[persI1]][["BK"]],conBK];
pos2=Union[data[[pI]][[persI2]][["BK"]],conBK];
compL=compDistFunc[pos1,pos2, pMod];,
compMod=="EVBK",
If[MemberQ[t2,ts],
conBK=data[[pI+1]][[1]][["BK"]];
conEV=data[[pI+1]][[1]][["EV"]];, 
conBK=data[[pI]][[1]][["BK"]];
conEV=data[[pI]][[1]][["EV"]];];
pos1=Union[data[[pI]][[persI1]][["EV"]],data[[pI]][[persI1]][["BK"]], conBK,conEV];
pos2=Union[data[[pI]][[persI2]][["EV"]],data[[pI]][[persI2]][["BK"]], conBK,conEV];
compL=compDistFunc[pos1,pos2, pMod];
];

Return[compL];
];


vvPosFunc[persI_, ts_, data_, compMod_, pMod_]:=Module[{compL, t1,t2, tInd, h, pos, conEV, conBK, chDEV, devFile, devEVBKTOT},
t1={0,1,2,3,4,5,6,7,8};
t2={0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5};
(* F\[UDoubleDot]r compMod=="EV","BK","EVBK" : data = pos-files *)
tInd=First@If[MemberQ[t1,ts],Flatten[Position[t1,ts]],Flatten[Position[t2,ts]]];
(*Print["tInd ", tInd];
Print["persI ", persI];*)

Which[persI==19,
h=tInd-6;,
persI==15,
h=tInd-3;,
persI==11,
h=tInd-2;,
persI==9,
h=tInd-1;,
persI==7,
h=tInd-1;,
persI==3,
h=tInd;
];
(*Print["h ", h];*)

(*Consens in S8*)
chDEV={!"No Main Culm as CM","No Main Culm as ML","No Main Culm as ORS","No Main Culm as SIL","No Main Culm as CAM",
"No BCL as CM",!"No BCL as ML","No BCL as ORS","No BCL as SIL","No BCL as CAM",
"No Non-Culm as CM","No Non-Culm as ML",!"No Non-Culm as ORS","No Non-Culm as SIL","No Non-Culm as CAM"};
devFile=FileNameJoin[{$HomeDirectory, "GDC", "DialecticalStructures", "gdc_DEV_EVBK_TOT.txt"}];
devEVBKTOT=Get[devFile];


        
Which[compMod=="CH",
If[KeyMemberQ[data[[h,2]], "list"],
 pos=Flatten[data[[h,2]][["list"]],1];
compL=Map[compValCHFunc[#,chDEV]&,pos]; ,
pos=data[[tInd]][[persI]][["CH"]];
compL=compValCHFunc[pos,chDEV];
 ];,
compMod=="EV",
pos=data[[tInd]][[persI]][["EV"]];
If[MemberQ[t2,ts],
conEV=data[[tInd+1]][[1]][["EV"]];, 
conEV=data[[tInd]][[1]][["EV"]];];
compL=compDistFunc[Union[pos,conEV],devEVBKTOT, pMod];,
compMod=="BK",
pos=data[[tInd]][[persI]][["BK"]];
If[MemberQ[t2,ts],
conBK=data[[tInd+1]][[1]][["BK"]];, 
conBK=data[[tInd]][[1]][["BK"]];];
compL=compDistFunc[Union[pos,conBK],devEVBKTOT, pMod];,
compMod=="EVBK",
pos=Union[data[[tInd]][[persI]][["BK"]],data[[tInd]][[persI]][["EV"]]];
If[MemberQ[t2,ts],
conBK=data[[tInd+1]][[1]][["BK"]];
conEV=data[[tInd+1]][[1]][["EV"]];, 
conBK=data[[tInd]][[1]][["BK"]];
conEV=data[[tInd]][[1]][["EV"]];];
(*Print["pos ", pos];
Print["CON ", Union[conBK,conEV]];*)
compL=compDistFunc[Union[pos,conBK,conEV],devEVBKTOT, pMod];
];

Return[compL];
];


arrayVVFunc[persI_, numT_,ts_, ch_, compMod_, pMod_]:=Module[{dataPre,dataOut},
dataPre=Map[vvPosFunc[persI, #, ch, compMod, pMod]&,Drop[ts,numT]];
(*Print[dataPre];*)
If[Length[dataPre]==Length[ts],
dataOut=dataPre;,
dataOut=Last[Map[PrependTo[dataPre,White]&,Range[numT]]];
];
Return[dataOut];
];


arrayFunc[persI1_, persI2_,numT_,ts_, ch_, compMod_, pMod_]:=Module[{dataPre,dataOut},
dataPre=Map[compPosFunc[persI1,persI2,#, ch, compMod, pMod]&,Drop[ts,numT]];
(*Print[dataPre];*)
If[Length[dataPre]==Length[ts],
dataOut=dataPre;,
dataOut=Last[Map[PrependTo[dataPre,White]&,Range[numT]]];
];
Return[dataOut];
];


list3DFunc[dataIn_, tInd_, mod_]:=Module[
{DLBdata, MURdata, LYEdata, PHIdata, SEDdata, AUSdata,
DLBMatrix, MURMatrix, LYEMatrix, PHIMatrix, SEDMatrix, AUSMatrix,
 dataOut},
DLBdata=Insert[dataIn[["DLB"]][[All,tInd]],White,1];
MURdata=Insert[dataIn[["MUR"]][[All,tInd]],White,2];
LYEdata=Insert[dataIn[["LYE"]][[All,tInd]],White,3];
PHIdata=Insert[dataIn[["PHI"]][[All,tInd]],White,4];
SEDdata=Insert[dataIn[["SED"]][[All,tInd]],White,5];
AUSdata=Insert[dataIn[["AUS"]][[All,tInd]],White,6];


If[mod=="MAT",
dataOut={DLBdata,MURdata,LYEdata,PHIdata,SEDdata,AUSdata};,
DLBMatrix=Union[Map[{1,#,DLBdata[[#]]}&,Range[6]],Map[{#,1,DLBdata[[#]]}&,Range[6]]];
MURMatrix=Union[Map[{2,#,MURdata[[#]]}&,Range[6]],Map[{#,2,MURdata[[#]]}&,Range[6]]];
LYEMatrix=Union[Map[{3,#,LYEdata[[#]]}&,Range[6]],Map[{#,3,LYEdata[[#]]}&,Range[6]]];
PHIMatrix=Union[Map[{4,#,PHIdata[[#]]}&,Range[6]],Map[{#,4,PHIdata[[#]]}&,Range[6]]];
SEDMatrix=Union[Map[{5,#,SEDdata[[#]]}&,Range[6]],Map[{#,5,SEDdata[[#]]}&,Range[6]]];
AUSMatrix=Union[Map[{6,#,AUSdata[[#]]}&,Range[6]],Map[{#,6,AUSdata[[#]]}&,Range[6]]];
dataOut=Union[DLBMatrix,MURMatrix,LYEMatrix,PHIMatrix,SEDMatrix,AUSMatrix];
];

Return[dataOut];
];




End[] (* End Private Context *)

EndPackage[]
