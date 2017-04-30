(* :Title: rdml: RDML importer *) 
(* :Author: Ramiro Magno *)
(* :Email: ramiro.magno@gmail.com *)
(* :Affiliation: Theoretical Biology & Bioinformatics, Utrecht University, the
                 Netherlands; CBMR, University of Algarve, Portugal *)
(* :Summary: Package provides importing of RDML files (http://rdml.org/). *)
(* :Context: rdml` *)
(* :File: rdml.m *)
(* :Package version: 1.1 Mar 30 2016 *)
(* :Mathematica version: 11.1 for Linux x86 (64-bit) *)
(* :Depends:
      J/Link: XML Schema validation (validateXml) and 
              iso8601 date string conversion (toDate)                        *)
(*    RDML Schema 1.2: RDML_v1_2_REC.xsd (http://rdml.org/RDML_v1_2_REC.xsd) *)
(*    RDML documentation notebook: RDML_doc.nb                               *)
(*    undocumented function Internal`StringToDouble                          *)


BeginPackage["rdml`"]

Needs["JLink`"]
InstallJava[];

HelpPageRDML::usage=
"Launch the RDML documentation."

Begin["Private`"]

(* Supported RDML version *)
version = "1.2";

(* rdml package directory *)
dir = DirectoryName[$InputFileName];

(* xsd file absolute location *)
xsd = FileNameJoin[{dir, "RDML_v1_2_REC.xsd"}];

(* path of the documentation notebook about the RDML format *)
doc = FileNameJoin[{dir, "RDML_doc.nb"}];

(* import function default options *)
Options[import] = {

    "Compressed"         -> Automatic, (* Automatic, True or False *)
    "Dataset"            -> True,      (* True or False *)
    (*"DeleteMissing"      -> False,*)     (* True or False *)
    "ValidateAgainstXSD" -> False      (* True or False *)

};

(* import function body *)
import::xsderr = "Could not validate the file `1` against `2`: `3`."
import[filename_String, OptionsPattern[]] := 
    Module[{zipBool, xmlObject, xmlString, xsdString, validation, expression,
            deleteMissingBool, datasetBool},

        zipBool =
        Switch[ OptionValue["Compressed"], 
            Automatic, zipQ[filename],
            True,      True,
            False,     False,
            _,         True (* if no valid option passed, then assume True *)
        ];

        If[zipBool == $Failed, Return[$Failed]];
        If[zipBool,
            xmlObject = zip2XMLObject[filename],
            xmlObject = Import[filename, "XML"]
        ];

        If[xmlObject == $Failed, Return[$Failed]];

        (* if "ValidateAgainstXSD" is neither True neither False, then opt for
           True (default)                                                    *)
        If[!BooleanQ[validateBool = OptionValue["ValidateAgainstXSD"]],
            validateBool = True];

        If[validateBool,
            xmlString = xmlObject2String[xmlObject];
            xsdString = Import[xsd, "Text"];
            If[xsdString == $Failed, Return[$Failed]];
            validation = validateXml[xsdString, xmlString];
            If[!validation["valid"],
                Message[import::xsderr, filename, xsd, validation["error"]];
                Return[$Failed]];
        ];

        expression = xmlObjectToExpression[xmlObject[[2]]];

(*
        If[!BooleanQ[deleteMissingBool = OptionValue["DeleteMissing"]],
            deleteMissing = True];


        expression =
        If[deleteMissingBool,
            (* delete Missing *)
            DeleteCases[expression, _Missing, Infinity] //
            (* delete empty lists and/or empty associations *)
            Replace[#, x_List | x_Association :> DeleteCases[x, {} | <||>], 
                {0, Infinity}]& // DeleteCases[#, <||>]&,
            expression
        ];
*)

        If[!BooleanQ[datasetBool = OptionValue["Dataset"]],
            datasetBool = True];

        expression =
        If[ datasetBool, Dataset[expression], expression ];


    Return[expression];
    ]


importRDML[filename_String, opts___] := import[filename, opts]
importRDMLVersion[filename_String, opts___] :=
  "version" -> import[filename, opts]["version"]
importRDMLDateMade[filename_String, opts___] :=
  "dateMade" -> import[filename, opts]["dateMade"]
importRDMLDateUpdated[filename_String, opts___] := 
  "dateUpdated" -> import[filename, opts]["dateUpdated"]
importRDMLId[filename_String, opts___] :=
  "id" -> import[filename, opts]["id"]
importRDMLExperimenter[filename_String, opts___] :=
  "experimenter" -> import[filename, opts]["experimenter"]
importRDMLDocumentation[filename_String, opts___] :=
  "documentation" -> import[filename, opts]["documentation"]
importRDMLDye[filename_String, opts___] :=
  "dye" -> import[filename, opts]["dye"]
importRDMLSample[filename_String, opts___] :=
  "sample" -> import[filename, opts]["sample"]
importRDMLTarget[filename_String, opts___] :=
  "target" -> import[filename, opts]["target"]
importRDMLThermalCyclingConditions[filename_String, opts___] :=
  "thermalCyclingConditions" -> 
     import[filename, opts]["thermalCyclingConditions"]
importRDMLExperiment[filename_String, opts___] :=
  "experiment" -> import[filename, opts]["experiment"]

ImportExport`RegisterImport[
	"RDML",
	{
		"version" :> importRDMLVersion,
		"dateMade" :> importRDMLDateMade,
		"dateUpdated" :> importRDMLDateUpdated,
		"id" :> importRDMLId,
		"experimenter" :> importRDMLExperimenter,
		"documentation" :> importRDMLDocumentation,
		"dye" :> importRDMLDye,
		"sample" :> importRDMLSample,
		"target" :> importRDMLTarget,
		"thermalCyclingConditions" :> importRDMLThermalCyclingConditions,
		"experiment" :> importRDMLExperiment,
		importRDML (* default importer *)
	},
	{},
	"AvailableElements" -> {"version", "dateMade", "dateUpdated", "id",
							"experimenter", "documentation", "dye", "sample",
							"target", "thermalCyclingConditions", "experiment"}
]

(* Trick to make the Import recognize .rdml and .rdm extensions *)
(* http://mathematica.stackexchange.com/questions/65053/how-to-make-import\
-recognize-a-new-file-extension *)
Unprotect[Import];

Import[name_String, opts___?OptionQ] :=
    Import[name, "RDML", opts] /; FileExtension[name] === "rdml";

Import[name_String, opts___?OptionQ] :=
    Import[name, "RDML", opts] /; FileExtension[name] === "rdm";

Protect[Import];

(* Trick to make Information recognize RDML symbol  *)
(* and return its documentation page                *)
HelpPageRDML[] := Module[{}, NotebookOpen[doc];];

(* Function zipQ: tests if file is a zip file *)
(* http://stackoverflow.com/questions/1887041/what-is-a-good-way-to-test-a-\
file-to-see-if-its-a-zip-file *)

zipQ[filename_String] :=
    Module[{firstTwoBytes, asciiCode},

        firstTwoBytes = Import[filename, {"Byte", {1, 2}}];
        If[firstTwoBytes == $Failed, Return[$Failed]];
        asciiCode = FromCharacterCode[firstTwoBytes];

        asciiCode == "PK"
    ];

zip2XMLObject[zipFilename_String] := 
    Module[{xmlFilename},
        xmlFilename = Import[zipFilename, {"ZIP", 1}];
        If[xmlFilename == $Failed, Return[$Failed]];
        Import[zipFilename, {"ZIP", xmlFilename}]
    ];

xmlObject2String[xmlObject_] := ExportString[xmlObject, "XML"]

(*                *)
(* XSD validation *)
(*                *)
(* http://mathematica.stackexchange.com/questions/112200/how-to-validate-an-\
xml-file-against-xsd-in-wolfram-language *)


LoadJavaClass[
    "javax.xml.validation.SchemaFactory", 
    AllowShortContext -> False];
LoadJavaClass[
    "javax.xml.XMLConstants", 
    AllowShortContext -> False];

stringSource[s_String] := 
    JavaNew["javax.xml.transform.stream.StreamSource",
        JavaNew["java.io.StringReader", s]
    ];

validateXml[xsd_String, xml_String] := JavaBlock@
    Module[{factory, xsdSource, xmlSource, schema, validator, valid, error}, 

        xsdSource = stringSource[xsd]; 
        xmlSource = stringSource[xml]; 
        factory = javax`xml`validation`SchemaFactory`newInstance@
                  javax`xml`XMLConstants`W3CUXMLUSCHEMAUNSUURI;
        schema = factory@newSchema[xsdSource];
        validator = schema@newValidator[]; error = "";

         Block[
            {$JavaExceptionHandler = 
              (error = GetJavaException[]@getMessage[]) &},
            validator@validate[xmlSource];
            <|"valid" -> error === "", "error" -> error|>
         ]
    ];



(*            *)
(* XML Parser *)
(*            *)

na = Missing["NotAvailable"];
nv = Missing["NotValid"];
versionQ[version_, valid_] := version == valid;


a[] :=
  <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> 1, "e" -> Identity|>

a[id_, minOccurs_, maxOccurs_, e_:Identity] := 
  <|"id" -> id, "minOccurs" -> minOccurs, "maxOccurs" -> maxOccurs, "e" -> e|>

(* elements of idReferencesType are nicely handled by a[True, _, _] *)

h[{}, schema_] := na
h[{xml_String}, schema_] := (schema["e"][xml])
h[xml_List, schema_] := ((# -> g[xml, #, schema["e", #]]) & /@ 
    Keys[schema["e"]]) // Association


(* Walks on XMLObject tree (xml) in depth-first manner and reaps whatever
   patterns are matched according to the Schema defined in variable schema   *)

g[xml_List, tag_String, schema_] :=
 Module[{match, assoc = False, matchID = 0, minOccurs, maxOccurs, 
   idBoolean, complexTypeBoolean},
  
  minOccurs = "minOccurs" /. schema;
  maxOccurs = "maxOccurs" /. schema;
  idBoolean = "id" /. schema;
  complexTypeBoolean = MatchQ[schema["e"], _Association];
  
  If[Not[idBoolean],
   match = Cases[xml, XMLElement[tag, {}, c_] :> h[c, schema]],
   If[complexTypeBoolean,
     match = 
      Cases[xml, 
        XMLElement[tag, {"id" -> i_}, c_] -> 
         HoldPattern[i -> h[c, schema]]] // ReleaseHold; 
     assoc = True,
     (* idReferencesType *)
     match = Cases[xml, XMLElement[tag, {"id" -> i_}, c_] -> i]
     ];
   ];
  
  If[match == {}, Return[na]];
  
  Return[
   If[maxOccurs == 1, match[[1]],
    If[assoc, Association@match, match]]
   ];
 ]

(* schema construction *)
(* xsd schema 1.2      *)
(* root *)
schema = <||>;

(* dateMade, dateUpdated *)
AppendTo[schema, "dateMade" -> a[False, 0, 1, toDate]];
AppendTo[schema, "dateUpdated" -> a[False, 0, 1, toDate]];

(* id *)
AppendTo[schema, 
  "id" ->
   <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> "unbounded", "e" ->
     <|
      "publisher" -> 
        <|"id" -> False, "minOccurs" -> 1, "maxOccurs" -> 1, "e" -> Identity|>,
      "serialNumber" ->
        <|"id" -> False, "minOccurs" -> 1, "maxOccurs" -> 1, "e" -> Identity|>,
      "MD5Hash" ->
        <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> 1, "e" -> Identity|>
      |>
    |>
];

(* experimenter *)
AppendTo[schema,
  "experimenter" ->
   <|"id" -> True, "minOccurs" -> 0, "maxOccurs" -> "unbounded", "e" ->
     <|
      "firstName" ->
        <|"id" -> False, "minOccurs" -> 1, "maxOccurs" -> 1, "e" -> Identity|>,
      "lastName" ->
        <|"id" -> False, "minOccurs" -> 1, "maxOccurs" -> 1, "e" -> Identity|>,
      "email" ->
        <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> 1, "e" -> Identity|>,
      "labName" ->
        <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> 1, "e" -> Identity|>,
      "labAddress" ->
        <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> 1, "e" -> Identity|>
      |>
    |>
];

(* documentation *)
AppendTo[schema, 
  "documentation" ->
   <|"id" -> True, "minOccurs" -> 0, "maxOccurs" -> "unbounded", "e" ->
     <|
      "text" ->
        <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> 1, "e" -> Identity|>
      |>
    |>
];

(* dye *)
AppendTo[schema, 
  "dye" ->
   <|"id" -> True, "minOccurs" -> 0, "maxOccurs" -> "unbounded", "e" ->
     <|
      "description" ->
        <|"id" -> False, "minOccurs" -> 0, "maxOccurs" -> 1, "e" -> Identity|>
      |>
    |>
];


(* sample *)
Block[
{xRefElements, annotationElements, typeValidation,
interRunCalibratorValidation, quantityElements, quantityUnitValidation,
calibratorSampleValidation, cdnaSynthesisElements, primingMethodValidation,
dnaseTreatmentValidation, templateQuantityElements, nucleotideValidation},

 xRefElements = <|"name" -> a[], "id" -> a[]|>;

 annotationElements =
    <|"property" -> a[False, 1, 1], "value" -> a[False, 1, 1]|>;

 typeValidation =
  Function[u, 
   ToLowerCase[u] /. 
    {
    p : "unkn" | "ntc" | "nac" | "std" | "ntp" | "nrt" | "pos" | "opt" -> p,
    Null | "" -> "unkn", _ -> Missing["type value not valid", u]
    }
  ];

 interRunCalibratorValidation = 
  Function[u, 
   ToLowerCase[u] /. {p : "false" -> False, "true" -> True, 
     Null | "" -> False, _ -> 
      Missing["interRunCalibrator value not valid", u]}];

 quantityUnitValidation = 
  Function[u, 
   ToLowerCase[
     u] /. {p : "cop" | "fold" | "dil" | "ng" | "other" -> p, 
     "nmol" -> "nMol", 
     Null | "" -> na, _ -> 
      Missing["sample::quantity::unit value not valid", u]}];

 quantityElements = <|"value" -> a[False, 1, 1, toFloat], 
   "unit" -> a[False, 1, 1, quantityUnitValidation]|>;

 calibratorSampleValidation = 
  Function[u, 
   ToLowerCase[u] /. {p : "false" -> False, "true" -> True, 
     Null | "" -> False, _ -> 
      Missing["sample::calibratorSample value not valid", u]}];

 primingMethodValidation = 
  Function[u, 
   ToLowerCase[
     u] /. {p : 
       "oligo-dt" | "random" | "target-specific" | 
        "oligo-dt and random" | "other" -> p, 
     Null | "" -> na, _ -> 
      Missing["sample::cdnaSynthesisMethod::primingMethod value not valid", \
               u]}];

 dnaseTreatmentValidation = 
  Function[u, 
   ToLowerCase[u] /. {p : "false" -> False, "true" -> True, 
     Null | "" -> na, _ -> 
      Missing["sample::cdnaSynthesisMethod::dnaseTreatment value not valid", \
                u]}];

 cdnaSynthesisElements = <|
   "enzyme" -> a[],
   "primingMethod" -> a[False, 0, 1, primingMethodValidation],
   "dnaseTreatment" -> a[False, 0, 1, dnaseTreatmentValidation],
   "thermalCyclingConditions" -> a[True, 0, 1]
   |>;

 nucleotideValidation = 
  Function[u, 
   ToLowerCase[u] /. {"dna" -> "DNA", "genomic dna" -> "genomic DNA", 
     "cdna" -> "cDNA", "rna" -> "RNA", 
     Null | "" -> na, _ -> 
      Missing["sample::templateQuantity::nucleotide value not valid", 
       u]}];

 templateQuantityElements =
    <|"conc" -> a[False, 0, 1, toFloat], 
      "nucleotide" -> a[False, 0, 1, nucleotideValidation]|>;
 
 AppendTo[schema, 
  "sample" ->
   <|"id" -> True, "minOccurs" -> 0, 
    "maxOccurs" -> "unbounded", "e" ->
     <|
      "description" -> a[],
      "documentation" -> a[True, 0, "unbounded"],
      "xRef" -> a[False, 0, "unbounded", xRefElements],
      "annotation" -> 
       a[False, 0, "unbounded", annotationElements],
      "type" -> a[False, 1, 1, typeValidation],
      "interRunCalibrator" -> 
       a[False, 0, 1, interRunCalibratorValidation],
      "quantity" -> a[False, 0, 1, quantityElements],
      "calibratorSample" -> 
       a[False, 0, 1, calibratorSampleValidation],
      "cdnaSynthesisMethod" -> 
       a[False, 0, 1, cdnaSynthesisElements],
      "templateQuantity" -> 
       a[False, 0, 1, templateQuantityElements]
      |>
    |>];
 ]

(* target *)
Block[{oligo},
 oligo =
    <|"threePrimeTag" -> a[], "fivePrimeTag" -> a[],
      "sequence" -> a[False, 1, 1]|>;

 AppendTo[schema, 
  "target" ->
   <|"id" -> True, "minOccurs" -> 0, "maxOccurs" -> "unbounded", "e" ->
     <|
      "description" -> a[],
      "documentation" -> a[True, 0, "unbounded"],
      "xRef" -> 
       a[False, 0, 
        "unbounded", <|"name" -> a[], "id" -> a[]|>],
      "type" -> 
       a[False, 1, 1, 
        Function[u, 
         ToLowerCase[u] /. {p : "ref" | "toi" -> p, 
           Null | "" -> Missing["type value not available"], _ -> 
            Missing["type value not valid", u]}]],
      "amplificationEfficiencyMethod" -> a[],
      "amplificationEfficiency" -> a[False, 0, 1, toFloat],
      "amplificationEfficiencySE" -> a[False, 0, 1, toFloat],
      "detectionLimit" -> a[False, 0, 1, toFloat],
      "dyeId" -> a[True, 1, 1],
      "sequences" -> a[False, 0, 1,
        <|"forwardPrimer" -> a[False, 0, 1, oligo],
         "reversePrimer" -> a[False, 0, 1, oligo],
         "probe1" -> a[False, 0, 1, oligo],
         "probe2" -> a[False, 0, 1, oligo],
         "amplicon" -> a[False, 0, 1, oligo]|>],
      "commercialAssay" -> 
       a[False, 0, 1, 
        <|"company" -> a[False, 1, 1], "orderNumber" -> a[False, 1, 1]|>]
      |>
    |>];
]

(* thermalCyclingConditions *)
Block[{temperature, gradient, loop},
 temperature =
  <|"temperature" -> a[False, 1, 1, toFloat], "duration" -> a[False, 1, 1],
   "temperatureChange" -> a[False, 0, 1, toFloat], 
   "durationChange" -> a[False, 0, 1, toInt], "measure" -> 
    a[False, 0, 1, 
     Function[u, 
      ToLowerCase[u] /. {p : "real time" | "meltcurve" -> p, 
        Null | "" -> Missing["measure value not available"], _ -> 
         Missing["measure value not valid", u]}]], 
   "ramp" -> a[False, 0, 1, toFloat]|>;
 gradient = <|"highTemperature" -> a[False, 1, 1, toFloat], 
   "lowTemperature" -> a[False, 1, 1, toFloat], 
   "duration" -> a[False, 1, 1, toPosInt], 
   "temperatureChange" -> a[False, 0, 1, toFloat],
   "durationChange" -> a[False, 0, 1, toInt], 
   "measure" -> 
    a[False, 0, 1, 
     Function[u, 
      ToLowerCase[u] /. {p : "real time" | "melcurve" -> p, 
        Null | "" -> Missing["measure value not available"], _ -> 
         Missing["measure value not valid", u]}]], 
   "ramp" -> a[False, 0, 1, toFloat]|>;
 
 AppendTo[schema,
  "thermalCyclingConditions" ->
   <|"id" -> True, "minOccurs" -> 0, 
    "maxOccurs" -> "unbounded", "e" ->
     <|
      "description" -> a[],
      "documentation" -> a[True, 0, "unbounded"],
      "lidTemperature" -> a[False, 0, 1, toFloat],
      "experimenter" -> a[True, 0, "unbounded"],
      "step" -> a[False, 1, "unbounded",
        <|"nr" -> a[],
         "description" -> a[False, 0, 1],
         "temperature" -> a[False, 1, 1, temperature],
         "gradient" -> a[False, 1, 1, gradient],
         "loop" -> 
          a[False, 1, 
           1, <|"goto" -> a[False, 1, 1, toPosInt], 
            "repeat" -> a[False, 1, 1, toPosInt]|>],
         "pause" -> 
          a[False, 1, 1, <|"temperature" -> a[False, 1, 1, toFloat]|>],
         "lidOpen" -> a[False, 1, 1]
         |>]
      |>
    |>];
 ]

(* experiment *)
Block[{run, dataCollectionSoftware, 
  cqDetectionMethod, str1, str2, str3, str4, pcrFormat, label, react, 
  data, adp, mdp},
 cqDetectionMethod = Function[u,
   str1 = "automated threshold and baseline settings";
   str2 = "manual threshold and baseline settings";
   str3 = "second derivative maximum";
   str4 = "other"; 
   ToLowerCase[u] /. {p : (str1 | str2 | str3 | str4) -> p, 
     Null | "" -> 
      Missing["cqDetectionMethod value not available"], _ -> 
      Missing["cqDetectionMethod not valid", u]}];
 
 dataCollectionSoftware = <|"name" -> a[False, 1, 1], 
   "version" -> a[False, 1, 1]|>;
 
 label = Function[u, 
   ToLowerCase[u] /. {"abc" -> "ABC", "123" -> "123", 
     "a1a1" -> "A1a1", 
     Null | "" -> Missing["label not available"], _ -> 
      Missing["label not valid", u]}];
 pcrFormat = <|"rows" -> a[False, 1, 1, toInt], 
   "columns" -> a[False, 1, 1, toInt], 
   "rowLabel" -> a[False, 1, 1, label], 
   "columnLabel" -> a[False, 1, 1, label]|>;
 
 adp = <|"cyc" -> a[False, 1, 1, toFloat], "tmp" -> a[False, 0, 1, toFloat], 
   "fluor" -> a[False, 1, 1, toFloat]|>;
 mdp = <|"tmp" -> a[False, 1, 1, toFloat], 
   "fluor" -> a[False, 1, 1, toFloat]|>;
 data = <|"tar" -> a[True, 1, 1], "cq" -> a[False, 0, 1, toFloatCq], 
   "excl" -> a[], "adp" -> a[False, 0, "unbounded", adp], 
   "mdp" -> a[False, 0, "unbounded", mdp], "endPt" -> a[False, 0, 1, toFloat], 
   "bgFluor" -> a[False, 0, 1, toFloat], 
   "bgFluorSlp" -> a[False, 0, 1, toFloat], 
   "quantFluor" -> a[False, 0, 1, toFloat]|>;
 react = <|"sample" -> a[True, 1, 1], 
   "data" -> a[False, 1, "unbounded", data]|>;
 
 run = <|"description" -> a[], 
   "documentation" -> a[True, 0, "unbounded"], 
   "experimenter" -> a[True, 0, "unbounded"], 
   "instrument" -> a[], 
   "dataCollectionSoftware" -> 
    a[False, 0, 1, dataCollectionSoftware], 
   "backgroundDeterminationMethod" -> a[], 
   "cqDetectionMethod" -> a[False, 0, 1, cqDetectionMethod], 
   "thermalCyclingConditions" -> a[True, 0, 1], 
   "pcrFormat" -> a[False, 1, 1, pcrFormat], 
   "runDate" -> a[False, 0, 1, toDate], 
   "react" -> a[True, 0, "unbounded", react]|>;
 
 AppendTo[schema, 
  "experiment" ->
   <|"id" -> True, "minOccurs" -> 0, 
    "maxOccurs" -> "unbounded", "e" ->
     <|"description" -> a[], 
      "documentation" -> a[True, 0, "unbounded"], 
      "run" -> a[True, 0, "unbounded", run]|>
    |>];
]

xmlObjectToExpression::rdmlerr = "No rdml top-level tag found."
xmlObjectToExpression::version = "RDML file version is `1` but only version \
`2` is supported. The parsing will proceed presuming version `2`, hence some \
data might be not imported."

xmlObjectToExpression[xml_] := 
 Module[{myversion, root, dateMade, dateUpdated, id, experimenter, 
   documentation, dye, sample, target, thermalCyclingConditions, 
   experiment},

  If[xml[[1]] != "rdml", Message[xmlObjectToExpression::rdmlerr];
                         Return[$Failed]
  ];
  (*Print[xml[[2]]];*)
  myversion = "version" /. xml[[2]];
  If[! versionQ[myversion, version], 
                Message[xmlObjectToExpression::version, myversion, version]
  ];
  root = xml[[3]];

  dateMade = g[root, "dateMade", schema["dateMade"]];

  dateUpdated = g[root, "dateUpdated", schema["dateUpdated"]];

  id = g[root, "id", schema["id"]];

  experimenter = g[root, "experimenter", schema["experimenter"]];

  documentation = g[root, "documentation", schema["documentation"]];

  dye = g[root, "dye", schema["dye"]];

  sample = g[root, "sample", schema["sample"]];

  target = g[root, "target", schema["target"]];

  thermalCyclingConditions = g[root, "thermalCyclingConditions", 
    schema["thermalCyclingConditions"]];

  experiment = g[root, "experiment", schema["experiment"]];

  <|
   "version" -> myversion,
   "dateMade" -> dateMade,
   "dateUpdated" -> dateUpdated,
   "id" -> id,
   "experimenter" -> experimenter,
   "documentation" -> documentation,
   "dye" -> dye,
   "sample" -> sample,
   "target" -> target,
   "thermalCyclingConditions" -> thermalCyclingConditions,
   "experiment" -> experiment
  |>
 ]

(*               *)
(* leaf elements *)
(*               *)

(* date *)
toDate::notString = "`1` is not a string.";
toDate::nodate = "The string \"`1`\" is not a valid dateTime string.";
toDate[s_] := Message[toDate::notString, string]; s
(*
toDate[s_String] := Module[
{isDate, date},
	date = DateObject[s];
	isDate = DateObjectQ[date];

	If[Not[isDate],
		Message[toDate::nodate, s]; Return[s], 
		date
	]
]
*)

(*
http://mathematica.stackexchange.com/questions/15629/does-mathematica-support\
-importing-iso-8601-date-strings
*)

toDate[s_String] :=
JavaBlock[InstallJava[];
 LoadJavaClass["javax.xml.bind.DatatypeConverter", 
  StaticsVisible -> True];
 DateList[
  javax`xml`bind`DatatypeConverter`parseDateTime[s]@
   getTime[]@toString[]]] //DateObject;







(* boolean *)
toBool::notBoolean = "\"`1`\" is neither \"false\" nor \"true\".";
toBool::notString = "\"`1`\" is not a string.";
toBool[s_] := Message[toBool::notString, string]; s
toBool[s_String] := Module[{string},
  string = ToLowerCase[s];
  If[Not[MatchQ[string, "false" | "true"]], 
    Message[toBool::notBoolean, string]; Return[s], 
    Return[If[string == "true", True, False]]];
]

regexInt = "(\\+|-)?([0-9]+)" // RegularExpression;

(* int *)
toInt::notInt = "\"`1`\" is not an integer number. Leaving it as string..";
toInt[s_]:=s
toInt[s_String] := Module[{},

	match = StringMatchQ[s, regexInt];

	If[ Not[match], 
		Message[toInt::notInt, s]; Return[s] ];

	Return[Floor[Internal`StringToDouble[s]]]

]

(* positive integer *)
(* mapping to integer anyways *)
toPosInt[s_] := toInt[s]


(* http://www.w3.org/TR/xsd-precisionDecimal/#pD-lexical-mapping *)
regexFloat = 
"(\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|-)?[0-9]+)?|(\\+|-)?INF|NaN"\
// RegularExpression;

(* float *)
toFloat::notFloat = "\"`1`\" is not a floating point number. Leaving it as \
string..";
toFloat[""] := na
toFloat[s0_String] := Module[{s, match},

	s = StringReplace[s0, "," -> "."];

	match = StringMatchQ[s, regexFloat];

	If[ Not[match], 
		Message[toFloat::notFloat, s]; Return[s] ];

	If[ s == "INF",  Return[Infinity]];
	If[ s == "+INF", Return[Infinity]];
	If[ s == "-INF", Return[-Infinity]];

	If[ s == "NaN", Return[Indeterminate]];

	(*Return[Read[StringToStream[s], Number]]*)
	Return[Internal`StringToDouble[s]]

]

(* special function for cq element to deal with corner case cq = '-1'
   which should return Missing["NotAvailable"]                         *)
toFloatCq["-1"] := na
toFloatCq[s_]:= toFloat[s]


End[]

EndPackage[]
