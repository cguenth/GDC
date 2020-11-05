(* ::Package:: *)

(* Wolfram Language Package *)

BeginPackage["DialecticalStructures`Argdown`"]
(* Exported symbols added here with SymbolName::usage *)  

DeductiveConstraints::usage = "DeductiveConstraints[json] extracts tau from imported argdown object json and returns it in DNF";
IdMap::usage = "IdMap[json] returns a list of rules that map every statement in imported argdown object json to unique integer ranging from 1 to number of statements n";
GetStatements::usage = "GetStatements[json] returns the list of statement titles in imported argdown object json.";
GetStatementDescription::usage = "GetStatementDescription[json] return list of rules that map every statement title to its corresponding (first) description; GetStatementDescription[json,title] does so for statement title.";
GetArguments::usage = "GetArguments[json] returns the list of argument titles in imported argdown object json.";
GetArgumentDescription::usage = "GetArgumentDescription[json] returns list of rules that map every argument title to its corresponding (first) description; GetArgumentDescription[json,title] does so for argument title.";
GetArgumentSection::usage = "GetArgumentSection[json] returns list of rules that map every argument title to the section of its corresponding (first) description; GetArgumentSection[json,title] does so for argument title.";
GetPCS::usage = "GetPCS[json,title] returns premiss conclusion structure {{p1,p2,...,pn},c} of argument title.";

Begin["`Private`"] (* Begin Private Context *) 


DeductiveConstraints[json_] := (GetArgumentConstraints[json] && GetSemanticConstraints[json]);

GetArgumentConstraints[json_] := (
	And @@
		Map[
			 If[
			   Length[Last[#]] > 0,
			   Implies[(And @@ First[#]), Last[Last[#]]],
			   True
			 ] &,
			 Transpose[{
			   Query["arguments", All, 2, "pcs", 
			     Select[Lookup[#, "role"] == "premise" &], "title"]@json,
			   Query["arguments", All, 2, "pcs", 
			     Select[Lookup[#, "role"] == "main-conclusion" &], "title"]@json
			 }]
		]
);

GetSemanticConstraints[json_] := (
	And @@
	Flatten[
		(Function[
     		{operator, type},
     		(Map[
		      operator[First[#], Last[#]] &,
		      Query["relations", 
		        Select[Lookup[#, "relationType"] == type &], {"from", "to"}]@json
		    ])
     	] @@ #) & /@ {{Implies, "entails"}, {Nand, "contrary"}, {Xor, "contradictory"}}
     ]
);


GetStatements[json_] := Query["statements", All, 1]@json;

GetArguments[json_] := Query["arguments", All, 1]@json;

IdMap[json_] := MapIndexed[#1->First[#2]&,Sort[GetStatements[json]]];

GetArgumentDescription[json_] := 	
	Map[
		First[#]->Last[#]&,
		Transpose[{
			Query["arguments", All, 1]@json,
			Query["arguments", All, 2, "descriptions", 1, "text"]@json
		}]
	];

GetArgumentDescription[json_, title_] := Query["arguments", title, "descriptions", 1, "text"]@json;

GetArgumentSection[json_] := 	
	Map[
		First[#]->Last[#]&,
		Transpose[{
			Query["arguments", All, 1]@json,
			Query["arguments", All, 2, "descriptions", 1, "section"]@json
		}]
	];

GetArgumentSection[json_, title_] := Query["arguments", title, "descriptions", 1, "section"]@json;

GetStatementDescription[json_] := 	
	Map[
		First[#]->Last[#]&,
		Transpose[{
			Query["statements", All, 1]@json,
			Query["statements", All, 2, "members", 1, "text"]@json
		}]
	];

GetStatementDescription[json_, title_] := Query["statements", title, "members", 1, "text"]@json;

GetPCS[json_,title_] := Module[{p,c},
	p = Query["arguments", title, "pcs", Select[Lookup[#, "role"] == "premise" &], "title"]@json;
	c = Query["arguments", title, "pcs", Select[Lookup[#, "role"] == "conclusion" &], "title"]@json;
	If[Length[c]>0,
		{p,Last[c]},
		{}
	]
];


End[] (* End Private Context *)

EndPackage[]
