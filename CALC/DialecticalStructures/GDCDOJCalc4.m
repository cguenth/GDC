(* ::Package:: *)

(* ::Chapter:: *)
(*Package GDCDOJCalc4*)


(* ::Text:: *)
(*Provides functions for*)


(* ::Item:: *)
(*merging equivalences*)


(* ::Item:: *)
(*rule out equivalences*)


(* ::Item:: *)
(*presenting tau without contradictions*)


(* Wolfram Language Package *)

BeginPackage["DialecticalStructures`GDCDOJCalc4`"]
(* Exported symbols added here with SymbolName::usage *)  

MergeEquivalences::usage = "MergeEquivalences[equivalences_]";
erulesContrasFunc::usage = "erulesContrasFunc[part_]";
RuleOutEquivalences::usage = "RuleOutEquivalences[conpairs_,senIDs_]";
TauWithoutContradictions::usage = "TauWithoutContradictions[er_, conps_ , tauc_]";
ContrariesNewForm::usage = "ContrariesNewForm[conpairs_]";

Begin["`Private`"] (* Begin Private Context *) 


MergeEquivalences[equivalences_]:=Module[{overlaps,s},
(*Liste der Partner eines geteilten Elements,die selber mehrfach auftauchen:*)
overlaps=Keys@Select[Counts@Catenate@equivalences,#>1&];
If[Length[overlaps]==0,
equivalences,
s=First[overlaps];
Join[DeleteCases[equivalences,p_/;MemberQ[p,s]], {Union@@Cases[equivalences,p_/;MemberQ[p,s]]}
]
]
];


erulesContrasFunc[part_]:=Module[{},
Union@@Map[
Function[eclass,
(#->First[eclass]&)/@eclass[[2;;]]
],
part
]
];


RuleOutEquivalences[conpairs_,senIDs_]:=Module[{equivalences,partition,erules},
equivalences=(Complement[Union@@#,Intersection@@#]&)/@DeleteCases[Subsets[conpairs,{2}],p_/; (!IntersectingQ@@p)];
Print["Equivalences ", equivalences];
partition=Sort/@FixedPoint[MergeEquivalences,equivalences,Length[senIDs],SameTest->(Length[#1]==Length[#2]&)];
Print["Partition ", partition];
(*Gleichsetzung der Instanzen ein und derselben Klasse via Liste von Zuweisungen*)
erules=erulesContrasFunc[partition];
Return[erules];
];


TauWithoutContradictions[er_, conps_ , tauc_]:=Module[{corecons, coreconsRev, crules,taunew},
corecons=Union@(conps/.er);
Print["Kontradiktorische Gegens\[ADoubleDot]tze ", conps];
Print["Reduzierte Kontradiktorische Gegens\[ADoubleDot]tze ", corecons];
(* Falls {13,67} und {67,13} beide enthalten, dann wird eins gel\[ODoubleDot]scht *)
coreconsRev=DeleteDuplicates@Map[Sort[#]&,corecons];
Print["Nochmals Reduzierte Kontradiktorische Gegens\[ADoubleDot]tze ", coreconsRev];
crules=Last[#]->!First[#]&/@coreconsRev;
(*z.B.21\[Rule]!34
d.h. falls 21 w, dann 34 f
		und falls 34 w, dann 21 f
Oder fehlt hier noch: crules2=!Last[#]\[Rule]First[#]&/@corecons;
Ist das obsolet?*)
taunew=(tauc/.er)/.crules;

Return[{taunew,crules}];
];



ContrariesNewForm[conpairs_]:=Module[{conpairsN},
conpairsN=And@@Map[Function[eclass,eclass[[1]]\[Implies]!eclass[[2]]],conpairs];
Return[conpairsN];
];


End[] (* End Private Context *)

EndPackage[]
