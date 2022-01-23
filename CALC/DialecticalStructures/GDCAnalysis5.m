(* ::Package:: *)

(* ::Chapter:: *)
(*Package GDCAnalysis5*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for analyzing POS*)


(* Wolfram Language Package *)
BeginPackage["DialecticalStructures`GDCAnalysis5`", { "DialecticalStructures`GDCAnalysis3`"}]


(* Exported symbols added here with SymbolName::usage *)  
compACCheFunc::usage = "compACCheFunc[nam_, accE_, accH_, circRes_]";
singlePlot::usage="singlePlot[nam_, dataIn_,colS_,fac1_, fac2_,type_]";
circlesDataFunc::usage="circlesDataFunc[name_,data_]";
sumUpFunc::usage="sumUpFunc[accE_, accH_,dataIn_,conf_,namL_, mod_]";
bowedArrowsData::usage="bowedArrowsData[data_]";
circTableDataFunc::usage="circTableDataFunc[dataIn_,conf_,namL_, mod_]";
circTableFunc::usage="circTableFunc[data_, mod_]";
tableOutFunc::usage="tableOutFunc[data_, tit_, con_]";


Begin["`Private`"] (* Begin Private Context *) 


compACCheFunc[nam_, accE_, accH_, circRes_]:=Module[{ts,tsRed,ind,inRed,datan,datax,dataxRed, data3},
ts={"0b","1a","1b","2a","2b","3a","3b","4a","4b","5a","5b","6a","6b","7a","7b","8a", "8b"};
ind=Range[Length[ts]];

Which[nam=="DLB",
	tsRed=ts;
inRed=ind;,
	nam=="MUR",
	tsRed=Drop[ts,2];
inRed=Drop[ind,2];,
	nam=="LYE",
	tsRed=Drop[ts,2];
inRed=Drop[ind,2];,
	nam=="PHI",
	tsRed=Drop[ts,4];
inRed=Drop[ind,4];,
	nam=="SED",
	tsRed=Drop[ts,6];
inRed=Drop[ind,6];,
	nam=="AUS",
	tsRed=Drop[ts,12];
inRed=Drop[ind,12];];


datax=Map[{accE[[nam]][[#,2]],accH[[nam]][[#,2]],tsRed[[#]]}&,Range[Length[accE[[nam]]]]];
datan=Map[{accE[[nam]][[#,2]],accH[[nam]][[#,2]],inRed[[#]]}&,Range[Length[accE[[nam]]]]];

If[Length[circRes]!=0,
dataxRed=Cases[datax,p_/; MemberQ[StringTake[circRes[[All,1]],{2,3}],ToString[p[[3]]]]];
data3=Map[{dataxRed[[#]][[1;;2]],circRes[[#,2]]}&,Range[Length[circRes]]];,
data3={}];
Return[{data3, datan,datax}];
];



singlePlot[nam_, dataIn_,colS_,fac1_, fac2_, type_]:=Module[{names,namInd,data,conf,mod,head,dataArrows,arrows,points,size, plot1,pointsG,plot2,dataInset,boxInset},
names={"DLB","MUR","LYE","PHI","SED","AUS"};
namInd=Position[names,nam][[1,1]];
(*Print[namInd];*)
(*data=dataIn[[3]][[namInd]];*)
data=dataIn[[3]][[namInd]];
(*Print["data ", data];*)
conf=dataIn[[1]];
(*Print["conf ,"conf];*)
mod=dataIn[[2]];
(*Print["mod ",mod];*)

head=Graphics[{Thickness[0.007],Line[{{{-1,1/2},{0,0},{-1,-1/2}}}]}];
(* ALTERNATIVE
head=Graphics[Triangle[{{{-1,1/2},{0,0},{-1,-1/2}}}]];*)
(*Background\[Rule]Lighter[Orange]*)
dataArrows=If[nam!="LYE",data[[2]],Drop[data[[2]],{-2}]];
arrows=bowedArrowsData[dataArrows];

dataInset={{"Circles", ToString[type]},{"CONF",ToString[conf]},{"Revisions",ToString[mod]},{"PERS",ToString[nam]}};
boxInset=Text@Grid[Prepend[dataInset,{"CON"," "}],Background->{None,{Lighter[Yellow,.9],{White,Lighter[Black,.9]}}},
Dividers->{{Darker[Gray,.6],{Lighter[Gray,.5]},Darker[Gray,.6]},{Darker[Gray,.6],Darker[Gray,.6],{False},Darker[Gray,.6]}},
Alignment->{{Left,Center}},ItemSize->{{6,4}},Frame->Darker[Gray,.6],ItemStyle->17,Spacings->{Automatic,.8}];


plot1=Graphics[{Opacity[0.4],colS[[#4]],Arrowheads[{{.012,1,{head,0.01}}}],Thickness[0.007],Arrow[BezierCurve[{#1,#2,#3}]]}&@@@arrows,
PlotRange->{{0.5,1.0},{0.5,1.0}},Frame->True,FrameLabel->{Style["\!\(\*SubscriptBox[\(CON\), \(e\)]\)",20],Style["\!\(\*SubscriptBox[\(CON\), \(h\)]\)",20]}, 
ImageSize->800, ImagePadding->120,Epilog->Inset[Style[boxInset,16],{0.61,0.97}]];
points=data[[1]][[All,1]];
(*Print["points ", points];*)
size=(Log[data[[1]][[All,2]]]*fac1)+fac2;
Print["size ", size];
pointsG=Map[Graphics[{Opacity[0.6],Hue[1,0.3],PointSize[size[[#]]],Point[points[[#]]]}]&, Range[Length[points]]];
plot2=Show[Catenate[{{plot1},pointsG}]];

Return[plot2];
];




circlesDataFunc[name_,data_]:=Module[{dataIn,dataEnlarged, points},
dataIn=data[name][[All,2]];
(*dataShrinked=Log[dataIn]*fac;
points=Map[{data[name][[All,1]][[#]],dataIn[[#]], dataShrinked[[#]]}&,Range[Length[dataShrinked]]];*)

points=Map[{data[name][[All,1]][[#]],dataIn[[#]]}&,Range[Length[dataIn]]];
Return[points];];




sumUpFunc[accE_, accH_,dataIn_,conf_,namL_, mod_]:=Module[{ind, confData, circles,circles2,dataConfCircles, plotsConfCircles},
Which[conf=="DOJ",ind=1;,
conf=="Z", ind=2;,
conf=="F", ind=3;];

confData=Association@Map[namL[[#]]->Values[Values[dataIn[[All,ind]]]][[All,2]][[#]]&,Range[Length[namL]]];
circles=Association[Map[namL[[#]]->circlesDataFunc[namL[[#]],confData]&,Range[Length[namL]]]];


Which[mod=="IND",
circles2=Association@Map[#->Cases[circles[#],p_/;StringTake[ToString[p[[1]]],-1]=="b"]&,namL],
mod=="CON",
circles2=Association@Map[#->Cases[circles[#],p_/;StringTake[ToString[p[[1]]],-1]=="a"]&,namL],
mod=="ALL",
circles2=circles;
];

dataConfCircles=Map[compACCheFunc[#, accE, accH, circles2[#]]&,namL];

Return[{conf,mod, dataConfCircles}];
];


bowedArrowsData[data_]:=Module[{head, plot, triple,tripleRed, ySame, ySameRed},
(*ALTERNATIVE:
triple=Map[{ex3[[#]][[1;;2]],{ex3[[#]][[1]],ex3[[#+1]][[2]]},ex3[[#+1]][[1;;2]],ex3[[#]][[3]]}&,Range[Length[ex3]-1]]*)
triple=Map[{data[[#]][[1;;2]],{data[[#]][[1]]+((data[[#+1]][[1]]-data[[#]][[1]])/2.),data[[#+1]][[2]]},data[[#+1]][[1;;2]],data[[#]][[3]]}&,Range[Length[data]-1]];
ySame=Cases[triple, p_/; p[[1,2]]==p[[2,2]]];

(*ALTERNATIVEN:
ySameRed=Map[{#[[1]],{#[[1,1]]+((#[[3,1]]-#[[1,1]])/2.),#[[1,2]]+(#[[3,1]]-#[[1,1]])},#[[3]],#[[4]]}&,ySame]
ySameRed=Map[{#[[1]],{#[[1,1]]+((#[[3,1]]-#[[1,1]])/2.),#[[1,2]]+(#[[1,2]]/(5+(#[[3,1]]-#[[1,1]])))},#[[3]],#[[4]]}&,ySame]
ySameRed=Map[{#[[1]],{#[[1,1]]+((#[[3,1]]-#[[1,1]])/2.),If[#[[3,1]]\[NotEqual]#[[1,1]],#[[1,2]]+((#[[3,1]]-#[[1,1]])/Abs[#[[3,1]]-#[[1,1]]])*(#[[1,2]]/(10+(#[[3,1]]-#[[1,1]]))),#[[1,2]]+(#[[1,2]]/(10+(#[[3,1]]-#[[1,1]])))]},#[[3]],#[[4]]}&,ySame];*)
ySameRed=Map[{#[[1]],{#[[1,1]]+((#[[3,1]]-#[[1,1]])/2.),If[#[[3,1]]!=#[[1,1]],#[[1,2]]+((#[[3,1]]-#[[1,1]])/Abs[#[[3,1]]-#[[1,1]]])*(#[[1,2]]/(10+(#[[3,1]]-#[[1,1]]))),#[[1,2]]+(#[[1,2]]/(10+(#[[3,1]]-#[[1,1]])))]},#[[3]],#[[4]]}&,ySame];
tripleRed=SortBy[Catenate[{DeleteCases[triple, p_/; p[[1,2]]==p[[2,2]]],ySameRed}],Last];

Return[tripleRed];
];


circTableDataFunc[dataIn_,conf_,namL_, mod_]:=Module[{ind, confData, circles,
ts={"0b","1a","1b","2a","2b","3a","3b","4a","4b","5a","5b","6a","6b","7a","7b","8a", "8b"},timesteps=Range[17],
col2,tsForm, dataSort,
dataOut,dataOut2, dataOut3},

Which[conf=="DOJ",ind=1;,
conf=="Z", ind=2;,
conf=="F", ind=3;];

confData=Association@Map[namL[[#]]->Values[Values[dataIn[[All,ind]]]][[All,2]][[#]]&,Range[Length[namL]]];
circles=Association[Map[namL[[#]]->circlesDataFunc[namL[[#]],confData]&,Range[Length[namL]]]];
(*Print["confData ", confData];
Print["circles ", circles];
*)

dataSort=Map[
Function[y,Map[
Function[x,{Keys[circles][[y]],Values[circles][[y]][[x]][[1]],Round[Values[circles][[y]][[x]][[2]],0.01]}],
Range[Length[Values[circles][[y]]]]]],
Range[Length[circles]]];
(*Print["dataSort ", dataSort];*)

col2=Map[Blend[{Blue, Yellow},#]&,Range[Length[timesteps]]/Length[timesteps]];
tsForm=Map[Style[ts[[#]],col2[[#]],20,Bold]&,Range[Length[ts]]];
dataOut=Association@Map[tsForm[[#]]->Drop[Cases[Flatten[dataSort,1],p_/;StringTake[ToString[p[[2]]],{2,3}]==ts[[#]]],None,{2}]&,Range[Length[ts]]];

Which[mod=="IND",
dataOut2=Last[Map[KeyDropFrom[dataOut,#]&,Cases[Keys[dataOut],p_/;StringTake[ToString[p],-1]=="a"]]];,
mod=="CON",
dataOut2=Last[Map[KeyDropFrom[dataOut,#]&,Cases[Keys[dataOut],p_/;StringTake[ToString[p],-1]=="b"]]];,
mod=="ALL",
dataOut2=dataOut;
];

dataOut3=Map[Function[y,Map[Function[x,StringReplace[ToString[x],{"{"->"(","}"->")"}]],y]],dataOut2];

Return[dataOut3];
];


circTableFunc[data_, mod_]:=Module[{nams, dataRed, tabs},
nams=Keys[data];
dataRed=Association[Map[#->Take[data[#],All,2]&, nams]];
(*Print["Data ", dataRed];*)
tabs=Map[Text@Grid[Prepend[Prepend[dataRed[[#]],{"Time",mod}],{#," "}],
Background->{None,{Lighter[Yellow,.9],Lighter[Yellow,.9],{White,Lighter[Black,.9]}}},
Dividers->{{Darker[Gray,.6],{Lighter[Gray,.5]},Darker[Gray,.6]},{Darker[Gray,.6],Lighter[Gray,.5],Darker[Gray,.6],{False},Darker[Gray,.6]}}, 
Alignment->{{Center,Center}},ItemSize->{{5,10}},Frame->Darker[Gray,.6],ItemStyle->14,Spacings->{Automatic,.8}]&,nams];
Return[tabs];
];



tableOutFunc[dataIn_, tit_]:=Module[{out},
out=Text@Grid[Prepend[Prepend[Map[Prepend[Values[dataIn][[#]],Keys[dataIn][[#]]]&, Range[Length[dataIn]]],{" ", "DOJ","Z","F"}],{tit}],
Background->{None,{Lighter[Yellow,.9],Lighter[Yellow,.9],{White,Lighter[Black,.9]}}},
Dividers->{{Darker[Gray,.6],{Lighter[Gray,.5]},Darker[Gray,.6]},{Darker[Gray,.6],Lighter[Gray,.5],Darker[Gray,.6],{False},Darker[Gray,.6]}}, 
Alignment->{{Center,Center}},ItemSize->{{3,12,12,12}},Frame->Darker[Gray,.6],ItemStyle->14,Spacings->{Automatic,.8}];
Return[out];];


End[] (* End Private Context *)

EndPackage[]
