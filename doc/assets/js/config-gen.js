function parseGMSIO() {
/*
credit: maloric (https://stackoverflow.com/questions/36127648/uploading-a-json-file-and-using-it)
*/
  $("#errMsg").innerHTML = "";
  var files = $("#selectFile")[0].files;
    if (files.length <= 0) {
      return false;
    }
    var fr = new FileReader();
    fr.onload = function(e) {
      try {
        var gmsData = JSON.parse(e.target.result);
        var gmsSymIn  = Object.keys(gmsData.gamsInputFiles);
        var gmsSymOut  = Object.keys(gmsData.gamsOutputFiles);
        var gmsSymHdrIn = new Object();
        var gmsSymNumHdrIn = new Object();
        var gmsSymHdrOut = new Object();
        var gmsSymNumHdrOut = new Object();
        var i = 0;
        var scalarSyms = [];
        var scalars = [];

        while (i < gmsSymIn.length) {
          if($.inArray("headers", Object.keys(gmsData.gamsInputFiles[gmsSymIn[i]])) === -1 || gmsSymIn[i] === "scalars"){
            if(gmsSymIn[i] === "scalars"){
              scalars.push("scalars");
              gmsSymHdrIn[gmsSymIn[i]] = Object.keys(gmsData.gamsInputFiles[gmsSymIn[i]].headers);
              gmsSymNumHdrIn[gmsSymIn[i]] = ["Value"];
              scalarSyms = scalarSyms.concat(gmsData.gamsInputFiles[gmsSymIn[i]].symnames);
            }else{
              scalarSyms.push(gmsSymIn[i]);
            }
            gmsSymIn.splice(i, 1);
          }else{
            i++;
          }
        }

        for (let i in gmsSymIn){
          gmsSymHdrIn[gmsSymIn[i]] = Object.keys(gmsData.gamsInputFiles[gmsSymIn[i]].headers);
          gmsSymNumHdrIn[gmsSymIn[i]] = [];
          for(let j in gmsSymHdrIn[gmsSymIn[i]]){
            if(gmsData.gamsInputFiles[gmsSymIn[i]].headers[gmsSymHdrIn[gmsSymIn[i]][j]].type === "parameter"){
              gmsSymNumHdrIn[gmsSymIn[i]].push(gmsSymHdrIn[gmsSymIn[i]][j]);
            }
          }
        }
        for (let i in gmsSymOut){
          if(gmsSymOut[i] === "scalars_out"){
            scalars.push("scalars_out");
            gmsSymNumHdrOut[gmsSymOut[i]] = ["Value"];
          }else{
            gmsSymNumHdrOut[gmsSymOut[i]] = [];
          }
          gmsSymHdrOut[gmsSymOut[i]] = Object.keys(gmsData.gamsOutputFiles[gmsSymOut[i]].headers);
          for(let j in gmsSymHdrOut[gmsSymOut[i]]){
            if(gmsData.gamsOutputFiles[gmsSymOut[i]].headers[gmsSymHdrOut[gmsSymOut[i]][j]].type === "parameter"){
              gmsSymNumHdrOut[gmsSymOut[i]].push(gmsSymHdrOut[gmsSymOut[i]][j]);
            }
          }
        }
        let iScalarOut = gmsSymOut.indexOf("scalars_out");
        if(iScalarOut > -1){
          gmsSymOut.splice(iScalarOut, 1);
        }
        $("#gmsioUpload").hide();
        $("#confGenContent").show();
        $("#selectConfigType").show();
        launchGenSelector(gmsSymIn, gmsSymOut, gmsSymHdrIn, gmsSymHdrOut, gmsSymNumHdrIn, gmsSymNumHdrOut, scalars, scalarSyms);
      }
      catch(err) {
          $("#errMsg").html("Problems parsing the JSON file. Please upload a valid GMSIO configutation file!");
          $("#errMsg").show();
          return;
      }
    }
  fr.readAsText(files.item(0));
};
function launchGenSelector(gmsSymIn, gmsSymOut, gmsSymHdrIn, gmsSymHdrOut, gmsSymNumHdrIn, gmsSymNumHdrOut, scalars, scalarSyms){
   $("#launchConfigGenBt").one("click", function(){
     $("#selectConfigType").hide();
     // Merge gmsSymHdrOut into gmsSymHdrIn
     $.extend(gmsSymHdrIn, gmsSymHdrOut);
     $.extend(gmsSymNumHdrIn, gmsSymNumHdrOut);
     launchConfigGen(gmsSymIn.concat(gmsSymOut, scalars), gmsSymHdrIn, gmsSymNumHdrIn, scalars);
   });
   $("#launchInputGenBt").one("click", function(){
     $("#selectConfigType").hide();
     launchInputGen(gmsSymIn, scalarSyms, gmsSymHdrIn);
   });
   $("#launchInputGenCBt").one("click", function(){
     $("#selectConfigType").hide();
     launchInputCGen(gmsSymIn, scalarSyms, gmsSymHdrIn);
   });
}
function launchConfigGen(gmsSym, gmsSymHdr, gmsSymNumHdr, scalars){
  Alpaca.defaultToolbarSticky = true;
    $("#form1").alpaca({
        "schema": {
           "$schema":"http://json-schema.org/draft-07/schema#",
           "title":"",
           "type":"object",
           "additionalProperties":false,
           "properties":{
              "language":{
                 "title":"Language for WebUI",
                 "type":"string",
                 "default":"en",
                 "enum":[
                    "de",
                    "en"
                 ],
                 "required":true
              },
              "pageSkin":{
                 "title":"Skin to use for shiny dashboard",
                 "type":"string",
                 "enum":[
                    "black",
                    "blue",
                    "purple",
                    "green",
                    "red",
                    "yellow"
                 ],
                 "default":"black",
                 "required":true
              },
              "excelIncludeMeta":{
                 "title":"Include a metadata sheet in the Excel file (export a scenario)?",
                 "type":"boolean",
                 "default":true,
                 "required":false
              },
              "excelIncludeEmptySheets":{
                 "title":"Include empty sheets in the Excel file?",
                 "type":"boolean",
                 "default":true,
                 "required":false
              },
              "UILogo":{
                 "title":"Name of the logo to use. Must be located in the static folder of current model directory.",
                 "type":"string",
                 "minLength":1,
                 "default":"gams_logo.png",
                 "required":false
              },
              "autoGenInputGraphs":{
                 "title":"Generate graphs for each input sheet automatically?",
                 "type":"boolean",
                 "default":true,
                 "required":false
              },
              "defCompMode":{
                 "title":"Default scenario comparison mode (the one that is loaded on startup).",
                 "type":"string",
                 "enum":[
                    "split",
                    "tab"
                 ],
                 "default":"split",
                 "required":true
              },
              "activateModules":{
                 "title":"Activate/deactivate certain modules",
                 "type":"object",
                 "additionalProperties":false,
                 "properties":{
                    "scenario":{
                       "title":"Activate scenario functionality?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "strictmode":{
                       "title":"Launch App in strict mode? This results in throwing error messages instead of accepting possibly faulty user entries.",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "loadLocal":{
                       "title":"Activate local data upload module?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "sharedScenarios":{
                       "title":"Enable scenario sharing between different users?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "batchMode":{
                       "title":"Solve scenarios in batch mode?",
                       "type":"boolean",
                       "default":false,
                       "required":false
                    },
                    "logFile:":{
                       "title":"Show log file in UI?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "lstFile:":{
                       "title":"Show lst file in UI?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    }
                 }
              },
              "aggregateWidgetsTmp":{
                 "title":"Aggregate all input widgets on a single tab?",
                 "type":"boolean",
                 "required":false,
                 "default":false
              },
              "aggregateWidgetsTitle":{
                "title":"Title of the tab where input widgets are aggregated",
                "type":"string",
                "minLength":"1",
                "required":true
              },
              "scalarAliases":{
                 "title":"Specify the aliases for the input and output scalar tables",
                 "type":"object",
                 "additionalProperties":false,
                 "properties":{
                    "inputScalars":{
                       "title":"Alias for the input scalar table",
                       "type":"string",
                       "minLength":"1",
                       "required":false
                    },
                    "outputScalars":{
                       "title":"Alias for the output scalar table",
                       "type":"string",
                       "minLength":"1",
                       "required":false
                    }
                 }
              },
              "saveTraceFile":{
                 "title":"Save trace files?",
                 "type":"boolean",
                 "default":false,
                 "required":false
              },
              "dataRendering":{
                 "title":"Format in which input or output data is to be displayed in the WebUI.",
                 "type":"object",
                 "additionalProperties":false,
                 "properties":{
                    "Format: datatable":{
                       "title":"datatable",
                       "type":"array",
                       "required":false,
                       "items":{
                          "type":"object",
                          "properties":{
                             "Parameter":{
                                "title":"Name of the Parameter to display:",
                                "type":"string",
                                "enum":gmsSym,
                                "default":gmsSym[0],
                                "required":true
                             },
                             "height":{
                                "title":"Height of the output element. (optional)",
                                "type":"integer",
                                "minimum":0,
                                "default":null
                             }
                          }
                       }
                    },
                    "Format: pivottable":{
                       "title":"pivottable",
                       "type":"array",
                       "required":false,
                       "items":{
                          "type":"object",
                          "properties":{
                             "Parameter":{
                                "title":"Name of the Parameter to display:",
                                "type":"string",
                                "enum":gmsSym,
                                "default":gmsSym[0],
                                "required":true
                             },
                             "height":{
                                "title":"Height of the output element. (optional)",
                                "type":"integer",
                                "minimum":0,
                                "default":null
                             },
                             "pivottable":{
                                "type":"object",
                                "properties":{
                                   "rows":{
                                      "title":"Prepopulate rows of the pivot table (optional). Column to use for rows of pivottable:",
                                      "type":"array",
                                      "uniqueItems":true,
                                      "items":{
                                         "type":"string",
                                         "enum":gmsSymHdr[gmsSym[0]],
                                         "default":gmsSymHdr[gmsSym[0]][0],
                                         "required": true,
                                         "minLength":1
                                      },
                                      "default":null
                                   },
                                   "cols":{
                                      "title":"Prepopulate columns of the pivot table (optional). Column to use for columns of pivottable:",
                                      "type":"array",
                                      "uniqueItems":true,
                                      "items":{
                                         "type":"string",
                                         "enum":gmsSymHdr[gmsSym[0]],
                                         "default":gmsSymHdr[gmsSym[0]][0],
                                         "minLength":1
                                      },
                                      "default":null
                                   },
                                   "aggregatorName":{
                                      "title":"Aggregator to use",
                                      "type":"string",
                                      "enum":[
                                         "Count",
                                         "Count Unique Values",
                                         "List Unique Values",
                                         "Sum",
                                         "Integer Sum",
                                         "Average",
                                         "Median",
                                         "Sample Variance",
                                         "Sample Standard Deviation",
                                         "Minimum",
                                         "Maximum",
                                         "First",
                                         "Last",
                                         "Sum over Sum",
                                         "80% Upper Bound",
                                         "80% Lower Bound",
                                         "Sum as Fraction of Total",
                                         "Sum as Fraction of Total",
                                         "Sum as Fraction of Rows",
                                         "Sum as Fraction of Columns",
                                         "Count as Fraction of Total",
                                         "Count as Fraction of Rows",
                                         "Count as Fraction of Columns"
                                      ],
                                      "default":"Count",
                                      "required":true
                                   },
                                   "vals":{
                                      "title":"String name of the column in the dataset to use with aggregatorName. Must be additive (i.e a number).",
                                      "type":"string",
                                      "enum":gmsSymNumHdr[gmsSym[0]],
                                      "required":false,
                                      "minLength":1,
                                      "default":null
                                   },
                                   "rendererName":{
                                      "title":"Name of the renderer selected, e.g. Table, Heatmap, Treemap etc",
                                      "type":"string",
                                      "enum":[
                                         "Table",
                                         "Table Barchart",
                                         "Heatmap",
                                         "Row Heatmap",
                                         "Col Heatmap",
                                         "Treemap",
                                         "Horizontal Bar Chart",
                                         "Horizontal Stacked Bar Chart",
                                         "Bar Chart",
                                         "Stacked Bar Chart",
                                         "Line Chart",
                                         "Area Chart",
                                         "Scatter Chart"
                                      ],
                                      "default":"Table",
                                      "required":true
                                   },
                                   "locale":{
                                      "title":"Localization options",
                                      "type":"string",
                                      "enum":[
                                         "cs",
                                         "da",
                                         "de",
                                         "en",
                                         "es",
                                         "fr",
                                         "it",
                                         "nl",
                                         "pl",
                                         "pt",
                                         "ru",
                                         "sq",
                                         "tr",
                                         "zh"
                                      ],
                                      "default":"en",
                                      "required":true
                                   },
                                   "subtotals":{
                                      "title":"Should subtotal plugin be used (for examples, see: <a href='http://nagarajanchinnasamy.com/subtotal/' target='_blank'>http://nagarajanchinnasamy.com/subtotal/</a>)",
                                      "type":"boolean",
                                      "default":false
                                   },
                                   "width":{
                                      "title":"width of pivottable (optional)",
                                      "type":"integer",
                                      "minimum":1,
                                      "default":null
                                   },
                                   "height":{
                                      "title":"height of pivottable (optional)",
                                      "type":"integer",
                                      "minimum":1,
                                      "default":null
                                   }
                                }
                             }
                          },
                          "additionalProperties":false,
                          "required":[
                             "outType"
                          ]
                       }
                    },
                    "Format: graph":{
                       "title":"graph",
                       "type":"array",
                       "required":false,
                       "items":{
                          "type":"object",
                          "properties":{
                             "Parameter":{
                                "title":"Name of the Parameter to display:",
                                "type":"string",
                                "enum":gmsSym,
                                "default":gmsSym[0],
                                "required":true
                             },
                             "graphtitle":{
                                "title":"Title of the graphic",
                                "type":"string",
                                "minLength":1,
                                "default":null
                             },
                             "height":{
                                "title":"Height of the output element. (optional)",
                                "type":"integer",
                                "minimum":0,
                                "default":null
                             },
                             "outType":{
                                "title":"Show graph in seperate view or in split view with underlying data table?",
                                "type":"string",
                                "enum":[
                                   "single view",
                                   "split view"
                                ],
                                "default":"single view",
                                "required":true
                             },
                             "graph":{
                                "type":"object",
                                "title":"Graph display options",
                                      "properties": {
                                          "choice":{
                                            "title": "Choose tool for graph rendering",
                                            "type": "string",
                                            "enum": ["UsePlotly", "UseDygraph"],
                                            "required": true
                                         },
                                         "plotly":{
                                            "title":"Use Plotly. Note: Only basic configuration is pre-implemented here. For more sophisticated graphic options please refer to <a href='https://plot.ly/r/' target='_blank'>the plotly documentation</a> and configure the config.json file manually.",
                                            "type":"object",
                                            "properties":{
                                               "graphtype":{
                                                  "title":"graph type:",
                                                  "type":"object",
                                                  "properties":{
                                                     "choice":{
                                                        "title": "Choose tool for graph rendering",
                                                        "type": "string",
                                                        "enum": ["Pie", "Chart", "Histogram"],
                                                        "required": true
                                                     },
                                                     "pie":{
                                                        "title":"Use Pie",
                                                        "type":"object",
                                                        "properties":{
                                                           "labels":{
                                                              "title":"symbol labels",
                                                              "type":"string",
                                                              "enum":gmsSymHdr[gmsSym[0]],
                                                              "default":gmsSymHdr[gmsSym[0]][0],
                                                              "required": true
                                                           },
                                                           "values":{
                                                              "title":"symbol values",
                                                              "type":"string",
                                                              "enum":gmsSymHdr[gmsSym[0]],
                                                              "default":gmsSymHdr[gmsSym[0]][0],
                                                              "required": true
                                                           },
                                                           "xaxis":{
                                                              "title":"Options: x axis",
                                                              "type":"object",
                                                              "properties":{
                                                                 "showgrid":{
                                                                    "title":"Show grid?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "zeroline":{
                                                                    "title":"Show zeroline?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "showticklabels":{
                                                                    "title":"Show ticklabels?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 }
                                                              }
                                                           },
                                                           "yaxis":{
                                                              "title":"Options: y axis",
                                                              "type":"object",
                                                              "properties":{
                                                                 "showgrid":{
                                                                    "title":"Show grid?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "zeroline":{
                                                                    "title":"Show zeroline?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "showticklabels":{
                                                                    "title":"Show ticklabels?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 }
                                                              }
                                                           }
                                                        }
                                                     },
                                                     "chart":{
                                                        "title":"Use Chart",
                                                        "type":"object",
                                                        "properties":{
                                                           "type":{
                                                              "title":"chart type",
                                                              "type":"string",
                                                              "enum":[
                                                                 "scatter",
                                                                 "line",
                                                                 "bar"
                                                              ],
                                                              "default":"scatter",
                                                              "required":true
                                                           },
                                                           "color":{
                                                              "title":"Symbol that is used to select different colors.",
                                                              "type":"string",
                                                              "enum":gmsSymHdr[gmsSym[0]],
                                                              "default":gmsSymHdr[gmsSym[0]][0],
                                                              "required": true
                                                           },
                                                           "margins":{
                                                              "title":"Margins to use",
                                                              "type":"object",
                                                              "properties":{
                                                                 "b":{
                                                                    "title":"bottom",
                                                                    "type":"integer",
                                                                    "minimum":0,
                                                                    "maximum":1000,
                                                                    "default":0,
                                                                    "required":false
                                                                 },
                                                                 "t":{
                                                                    "title":"top",
                                                                    "type":"integer",
                                                                    "minimum":0,
                                                                    "maximum":1000,
                                                                    "default":0,
                                                                    "required":false
                                                                 },
                                                                 "l":{
                                                                    "title":"left",
                                                                    "type":"integer",
                                                                    "minimum":0,
                                                                    "maximum":1000,
                                                                    "default":0,
                                                                    "required":false
                                                                 },
                                                                 "r":{
                                                                    "title":"right",
                                                                    "type":"integer",
                                                                    "minimum":0,
                                                                    "maximum":1000,
                                                                    "default":0,
                                                                    "required":false
                                                                 },
                                                                 "pad":{
                                                                    "title":"padding",
                                                                    "type":"integer",
                                                                    "minimum":0,
                                                                    "maximum":250,
                                                                    "default":0,
                                                                    "required":false
                                                                 }
                                                              }
                                                           },
                                                           "xdata":{
                                                              "title":"Data to use for x axis",
                                                              "type":"string",
                                                              "enum":gmsSymHdr[gmsSym[0]],
                                                              "default":gmsSymHdr[gmsSym[0]][0],
                                                              "required":true
                                                           },
                                                           "ydata":{
                                                              "title":"Data to use for y axis",
                                                              "type":"object",
                                                              "properties":{
                                                                 "dataname":{
                                                                    "title":"data column.",
                                                                    "type":"string",
                                                                    "enum":gmsSymHdr[gmsSym[0]],
                                                                    "default":gmsSymHdr[gmsSym[0]][0],
                                                                    "required":true
                                                                 },
                                                                 "label":{
                                                                    "title":"labels to use.",
                                                                    "type":"string",
                                                                    "enum":gmsSymHdr[gmsSym[0]],
                                                                    "default":gmsSymHdr[gmsSym[0]][0],
                                                                    "required": true
                                                                 },
                                                                 "mode":{
                                                                    "title":"Mode of plot data",
                                                                    "type":"string",
                                                                    "enum":[
                                                                       "lines",
                                                                       "markers"
                                                                    ],
                                                                    "default":"lines",
                                                                    "required":true
                                                                 }
                                                              }
                                                           },
                                                           "xaxis":{
                                                              "title":"Options: x axis",
                                                              "type":"object",
                                                              "properties":{
                                                                 "dataname":{
                                                                    "title":"title",
                                                                    "type":"string",
                                                                    "default":""
                                                                 },
                                                                 "showgrid":{
                                                                    "title":"Show grid?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "zeroline":{
                                                                    "title":"Show zeroline?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "showticklabels":{
                                                                    "title":"Show ticklabels?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 }
                                                              }
                                                           },
                                                           "yaxis":{
                                                              "title":"Options: y axis",
                                                              "type":"object",
                                                              "properties":{
                                                                 "dataname":{
                                                                    "title":"title",
                                                                    "type":"string",
                                                                    "default":""
                                                                 },
                                                                 "showgrid":{
                                                                    "title":"Show grid?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "zeroline":{
                                                                    "title":"Show zeroline?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 },
                                                                 "showticklabels":{
                                                                    "title":"Show ticklabels?",
                                                                    "type":"boolean",
                                                                    "default":false,
                                                                    "required":false
                                                                 }
                                                              }
                                                           }
                                                        }
                                                     },
                                                     "histogram":{
                                                        "title":"Use Histogram",
                                                        "type":"object",
                                                        "properties":{
                                                           "histnorm":{
                                                              "title":"histnorm",
                                                              "type":"string",
                                                              "enum":[
                                                                 "percent",
                                                                 "probability",
                                                                 "density",
                                                                 "probability density"
                                                              ],
                                                              "default":"percent",
                                                              "required":true
                                                           },
                                                           "nbins":{
                                                              "title":"number of bins",
                                                              "type":"integer",
                                                              "minimum":0,
                                                              "default":20,
                                                              "required":true
                                                           },
                                                           "barmode":{
                                                              "title":"barmode",
                                                              "type":"string",
                                                              "enum":[
                                                                 "overlay",
                                                                 "stack",
                                                                 "group",
                                                                 "relative"
                                                              ],
                                                              "default":"overlay",
                                                              "required":true
                                                           },
                                                           "alpha":{
                                                              "title":"transparency",
                                                              "type":"number",
                                                              "minimum":0,
                                                              "maximum":1,
                                                              "default":0.6,
                                                              "required":true
                                                           },
                                                           "xdata":{
                                                              "title":"Data to use for x axis",
                                                              "type":"array",
                                                              "required":true,
                                                              "items":{
                                                                 "type":"object",
                                                                 "properties":{
                                                                    "dataname":{
                                                                       "title":"data column (use the explanatory text of GAMS symbol).",
                                                                       "type":"string",
                                                                       "enum":gmsSymHdr[gmsSym[0]],
                                                                       "default":gmsSymHdr[gmsSym[0]][0],
                                                                       "required":true
                                                                    },
                                                                    "label":{
                                                                       "title":"label for x axis.",
                                                                       "type":"string",
                                                                       "minLength":1,
                                                                       "default":null
                                                                    },
                                                                    "color":{
                                                                       "title":"color of histogram data",
                                                                       "type":"string",
                                                                       "minLength":1,
                                                                       "default":"darkgreen"
                                                                    },
                                                                    "alpha":{
                                                                       "title":"transparency",
                                                                       "type":"number",
                                                                       "minimum":0,
                                                                       "maximum":1,
                                                                       "default":0.6,
                                                                       "required":true
                                                                    }
                                                                 }
                                                              }
                                                           },
                                                           "xaxis":{
                                                              "title":"Options: x axis",
                                                              "type":"object",
                                                              "properties":{
                                                                 "dataname":{
                                                                    "title":"title",
                                                                    "type":"string",
                                                                    "default":""
                                                                 }
                                                              }
                                                           },
                                                           "yaxis":{
                                                              "title":"Options: y axis",
                                                              "type":"object",
                                                              "properties":{
                                                                 "dataname":{
                                                                    "title":"title",
                                                                    "type":"string",
                                                                    "default":""
                                                                 }
                                                              }
                                                           }
                                                        }
                                                     }
                                                  },
                                                  "dependencies": {
                                                      "pie": ["choice"],
                                                      "chart": ["choice"],
                                                      "histogram": ["choice"]
                                                  }
                                               }
                                            }
                                         },
                                         "dygraph":{
                                            "title":"Use Dygraph. Note: Only basic configuration is pre-implemented. For more sophisticated graphic options please refer to https://rstudio.github.io/dygraphs/ and configure the config.json file manually.",
                                            "type": "object",
                                            "required":false,
                                            "properties":{
                                               "dyOptions":{
                                                  "title":"dygraph options",
                                                  "type": "object",
                                                  "required":false,
                                                  "properties":{
                                                     "includeZero":{
                                                         "title":"include zero value of axes in graphic?",
                                                         "type":"boolean",
                                                         "default":false,
                                                         "required":false
                                                      },
                                                      "logscale":{
                                                         "title":"use a logarythmic scale for axes?",
                                                         "type":"boolean",
                                                         "default":false,
                                                         "required":false
                                                      },
                                                      "drawGrid":{
                                                         "title":"draw grid?",
                                                         "type":"boolean",
                                                         "default":true,
                                                         "required":false
                                                      },
                                                      "drawPoints":{
                                                         "title":"draw data points?",
                                                         "type":"boolean",
                                                         "default":false,
                                                         "required":false
                                                      },
                                                      "pointSize":{
                                                         "title":"point size (refers to previous option)",
                                                         "type":"integer",
                                                         "minimum":0,
                                                         "default":2,
                                                         "required":false
                                                      } ,
                                                      "fillGraph":{
                                                         "title":"fill graph?",
                                                         "type":"boolean",
                                                         "default":false,
                                                         "required":false
                                                      },
                                                      "fillAlpha":{
                                                         "title":"transperancy",
                                                         "type":"number",
                                                         "minimum":0,
                                                         "maximum":1,
                                                         "default":0.15,
                                                         "required":true
                                                      }
                                                  }
                                               },
                                               "color":{
                                                  "title":"Symbol that is used to select different colors.",
                                                  "type":"string",
                                                  "enum":gmsSymHdr[gmsSym[0]],
                                                  "default":gmsSymHdr[gmsSym[0]][0]
                                               },
                                               "dyHighlight":{
                                                  "title":"highlight options",
                                                  "type": "object",
                                                  "required":false,
                                                  "properties":{
                                                     "highlightSeriesOpts":{
                                                         "type":"object",
                                                         "properties":{
                                                            "strokeWidth":{
                                                                "title":"stroke width (px)",
                                                                "type":"integer",
                                                                "minimum":0,
                                                                "default":3,
                                                                "required":false
                                                             },
                                                             "strokeBorderWidth":{
                                                                "title":"stroke border width (px)",
                                                                "type":"integer",
                                                                "minimum":0,
                                                                "default":1,
                                                                "required":false
                                                             },
                                                             "highlightCircleSize":{
                                                                "title":"highlight circle size (px)",
                                                                "type":"integer",
                                                                "minimum":0,
                                                                "default":5,
                                                                "required":false
                                                             }
                                                         }
                                                      },
                                                      "hideOnMouseOut":{
                                                         "title":"only highlight data when hovering with cursor?",
                                                         "type":"boolean",
                                                         "default":true,
                                                         "required":false
                                                      }
                                                  }
                                               },
                                               "dylegend":{
                                                  "title":"legend options",
                                                  "type": "object",
                                                  "required":false,
                                                  "properties":{
                                                      "width":{
                                                         "title":"legend width (px)",
                                                         "type":"integer",
                                                         "minimum":0,
                                                         "required":false
                                                      },
                                                      "show":{
                                                         "title":"show legend",
                                                         "type":"string",
                                                         "enum":[
                                                            "always",
                                                            "follow cursor"
                                                         ],
                                                         "default":"always",
                                                         "required":true
                                                      },
                                                      "hideOnMouseOut":{
                                                         "title":"only show legend when hovering graphic with cursor?",
                                                         "type":"boolean",
                                                         "default":true,
                                                         "required":false
                                                      }
                                                  }
                                               },
                                               "dyRangeSelector":{
                                                  "title":"Range selector options",
                                                  "type": "object",
                                                  "required":false,
                                                  "properties":{
                                                      "height":{
                                                         "title":"height of range selector (in px)",
                                                         "type":"integer",
                                                         "minimum":0,
                                                         "default":20,
                                                         "required":false
                                                      },
                                                      "strokeColor":{
                                                         "title":"stroke color",
                                                         "type":"string",
                                                         "default":"",
                                                         "required":false
                                                      }
                                                  }
                                               },
                                               "dyEvent":{
                                                  "title":"event options. Event data is highlighted in a graphic, for example, a key date marked as such.",
                                                  "type": "array",
                                                  "required":false,
                                                  "items":{
                                                     "type":"object",
                                                     "properties":{
                                                        "eventdata":{
                                                           "title":"GAMS data symbol for event",
                                                           "type":"string",
                                                           "required":false
                                                        },
                                                        "label":{
                                                           "title":"event label",
                                                           "type":"string",
                                                           "default":"",
                                                           "required":false
                                                        },
                                                        "labelLoc":{
                                                           "title":"event label location in graphic",
                                                           "type":"string",
                                                           "enum":[
                                                              "bottom",
                                                              "top"
                                                           ],
                                                           "default":"bottom",
                                                           "required":true
                                                        },
                                                        "color":{
                                                           "title":"color",
                                                           "type":"string",
                                                           "default":"red",
                                                           "required":false
                                                        }
                                                     }
                                                  }
                                               },
                                               "xdata":{
                                                  "title":"data for x axis (explanatory text of GAMS symbol)",
                                                  "type": "string",
                                                  "enum":gmsSymHdr[gmsSym[0]],
                                                  "default":gmsSymHdr[gmsSym[0]][0],
                                                  "required":true
                                               },
                                               "ydata":{
                                                  "title":"y data options (at least one data set required)",
                                                  "type": "array",
                                                  "required":true,
                                                  "items":{
                                                     "type":"object",
                                                     "properties":{
                                                        "dataname":{
                                                           "title":"data set  (use the explanatory text of GAMS symbol)",
                                                           "enum":gmsSymHdr[gmsSym[0]],
                                                           "default":gmsSymHdr[gmsSym[0]][0],
                                                           "required":true
                                                        },
                                                        "label":{
                                                           "title":"label",
                                                           "type":"string",
                                                           "required":false
                                                        },
                                                        "stepPlot":{
                                                           "title":"stepPlot",
                                                           "type":"boolean",
                                                           "default":false,
                                                           "required":false
                                                        },
                                                        "stemPlot":{
                                                           "title":"stemPlot",
                                                           "type":"boolean",
                                                           "default":false,
                                                           "required":false
                                                        },
                                                        "fillGraph":{
                                                           "title":"fill graph?",
                                                           "type":"boolean",
                                                           "default":false,
                                                           "required":false
                                                        },
                                                        "drawpoints":{
                                                           "title":"draw data points?",
                                                           "type":"boolean",
                                                           "default":true,
                                                           "required":false
                                                        }
                                                     }
                                                  }
                                               },
                                               "xaxis":{
                                                  "title":"xaxis options",
                                                  "type": "object",
                                                  "required":false,
                                                  "properties":{
                                                      "xtitle":{
                                                         "title":"title",
                                                         "type":"string",
                                                         "default":"",
                                                         "required":false
                                                      }
                                                  }
                                               },
                                               "yaxis":{
                                                  "title":"yaxis options",
                                                  "type": "object",
                                                  "required":false,
                                                  "properties":{
                                                      "ytitle":{
                                                         "title":"title",
                                                         "type":"string",
                                                         "default":"",
                                                         "required":false
                                                      }
                                                  }
                                               }
                                            }
                                         }
                                      },
                                      "dependencies": {
                                          "plotly": ["choice"],
                                          "dygraph": ["choice"]

                                }
                             }
                          }
                       }
                    },
                    "Format: custom":{
                       "title":"custom graphic",
                       "type":"array",
                       "required":false,
                       "items":{
                          "type":"object",
                          "properties":{
                             "Parameter":{
                                "title":"GAMS Symbol name",
                                "type":"string",
                                "enum":gmsSym,
                                "default":gmsSym[0],
                                "required":true
                             },
                             "outType":{
                                "title":"Name of custom renderer",
                                "type":"string",
                                "minLength":2,
                                "required":true,
                                "not":{
                                   "enum":[
                                      "valueBox",
                                      "graph",
                                      "dtGraph",
                                      "datatable",
                                      "pivot"
                                   ]
                                }
                             },
                             "height":{
                                "title":"Height of the output element (px)",
                                "type":"integer",
                                "minimum":0,
                                "default":null,
                                "required":false
                             },
                             "packages":{
                                "title":"Packages that need to be installed",
                                "type":"array",
                                "uniqueItems":true,
                                "items":{
                                   "type":"string",
                                   "minLength":2
                                }
                             },
                             "optionsTmp":{
                                "title":"Additional options",
                                "type":"array",
                                "items":{
                                   "type":"object",
                                   "properties":{
                                      "key":{
                                         "title":"option key",
                                         "type":"string",
                                         "minLength":1
                                      },
                                      "value":{
                                         "title":"option value",
                                         "type":"string",
                                         "minLength":1
                                      }
                                   }
                                }
                             }
                          },
                          "additionalProperties":false
                       }
                    },
                    "Format: valuebox":{
                       "title":"valuebox (only for scalar values)",
                       "type":"array",
                       "required":false,
                       "maxItems":1,
                       "items":{
                          "type":"object",
                          "properties":{
                             "scalar_type":{
                                "title":"Which scalar type shall be displayed as valuebox?",
                                "type":"string",
                                "enum":["scalars_out"],
                                "default":"scalars_out",
                                "required":true
                             },
                             "options":{
                                "type":"object",
                                "additionalProperties":false,
                                "properties":{
                                   "width":{
                                      "title":"Width of the output element.",
                                      "type":"integer",
                                      "minimum":0,
                                      "maximum":12,
                                      "default":4
                                   },
                                   "color":{
                                      "type":"string",
                                      "title":"Background color of value box",
                                      "enum":[
                                         "red",
                                         "yellow",
                                         "aqua",
                                         "blue",
                                         "light-blue",
                                         "green",
                                         "navy",
                                         "teal",
                                         "olive",
                                         "lime",
                                         "orange",
                                         "fuchsia",
                                         "purple",
                                         "maroon",
                                         "black"
                                      ],
                                      "default":"aqua",
                                      "required":"true"
                                   },
                                   "icon":{
                                      "title":"icon for value box (optional)",
                                      "type":"object",
                                      "properties":{
                                         "name":{
                                            "type":"string",
                                            "title":"Name of icon. Icons are drawn from the Font Awesome and Glyphicons libraries.",
                                            "minLength":0,
                                            "default":null
                                         },
                                         "lib":{
                                            "type":"string",
                                            "title":"Name of library where icon is from.",
                                            "enum":[
                                               "glyphicon",
                                               "font-awesome"
                                            ],
                                            "default":"font-awesome",
                                            "required":true
                                         }
                                      }
                                   }
                                }
                             }
                          },
                          "additionalProperties":false,
                          "required":[
                             "outType"
                          ]
                       }
                    }
                 }
              },
              "roundingDecimals":{
                 "title":"Number of decimal places used for rounding output values (max. 6).",
                 "type":"integer",
                 "minimum":0,
                 "maximum":6,
                 "default":2,
                 "required":false
              },
              "handsontable":{
                 "title":"Parameters used to customize handsontables (spreadsheet like js-editor used to modify input data).",
                 "type":"object",
                 "required":false,
                 "properties":{
                    "height":{
                       "title":"Height of handsontable (in px).",
                       "type":"integer",
                       "exclusiveMinimum":0,
                       "default":700,
                       "required":false
                    },
                    "width":{
                       "title":"Width of handsontable (in px). If not set, optimum value is determined",
                       "type":"integer",
                       "exclusiveMinimum":0,
                       "default":null,
                       "required":false
                    },
                    "readonly":{
                       "title":"Should handsontables be read-only?",
                       "type":"boolean",
                       "default":false,
                       "required":false
                    },
                    "search":{
                       "title":"Enable browser search for handsontables?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "highlightCol":{
                       "title":"Hoghlight column of current active cell?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "highlightRow":{
                       "title":"Highlight row of current active cell?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "rowHeaderWidth":{
                       "title":"Width of the row number/row header column (in px).",
                       "type":"integer",
                       "exclusiveMinimum":0,
                       "default":null,
                       "required":false
                    },
                    "enableComments":{
                       "title":"Enable comments in table? Comments will be ignored when solving the model.",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "stretchH":{
                       "title":"Strech table to full box width?",
                       "type":"string",
                       "enum":[
                          "none",
                          "last",
                          "all"
                       ],
                       "default":"all",
                       "required":true
                    },
                    "columnSorting":{
                       "title":"Enable column sorting option (in ascending/descending order)?",
                       "type":"boolean",
                       "default":true,
                       "required":false
                    },
                    "manualColumnMove":{
                       "title":"Enable manual column moving (change position)?",
                       "type":"boolean",
                       "default":false,
                       "required":false
                    },
                    "manualColumnResize":{
                       "title":"Enable manuall column resizing?",
                       "type":"boolean",
                       "default":false,
                       "required":false
                    },
                    "colWidths":{
                       "title":"Width of columns (not required).",
                       "type":"integer",
                       "exclusiveMinimum":0,
                       "default":null,
                       "required":false
                    },
                    "fixedColumnsLeft":{
                       "title":"Number of columns fixed on the left (not required).",
                       "type":"integer",
                       "exclusiveMinimum":0,
                       "default":null,
                       "required":false
                    },
                    "contextMenu":{
                       "title":"Options for customizing the handsontable context menu (accessible via right mouse click)",
                       "type":"object",
                       "additionalProperties":false,
                       "required":"false",
                       "properties":{
                          "enabled":{
                             "title":"Enable context menu (using right mouse click)?",
                             "type":"boolean",
                             "default":true,
                             "required":false
                          },
                          "allowRowEdit":{
                             "title":"logical enabling row editing",
                             "type":"boolean",
                             "default":true
                          },
                          "allowColEdit":{
                             "title":"logical enabling column editing. Note that Handsontable does not support column add/remove when column types are defined (i.e. useTypes == TRUE in rhandsontable).",
                             "type":"boolean",
                             "default":true
                          },
                          "allowReadOnly":{
                             "title":"logical enabling read-only toggle",
                             "type":"boolean",
                             "default":false
                          },
                          "allowComments":{
                             "title":"Enable comments in table. Will be ignored when solving the model.",
                             "type":"boolean",
                             "default":true
                          }
                       }
                    }
                 }
              },
              "pivottable":{
                 "title":"Parameters used to customize pivottable (used to render output data).",
                 "type":"object",
                 "additionalProperties":false,
                 "properties":{
                    "bgColor":{
                       "title":"Background color (of row and column headers)",
                       "type":"string",
                       "required":false,
                       "default":"#ffffff"
                    }
                 }
              },
              "datatable":{
                 "title":"Parameters used to customize datatable (used to render output data).",
                 "type":"object",
                 "additionalProperties":false,
                 "properties":{
                    "class":{
                       "title":"css classes used for styling datatable (see <a href='https://datatables.net/manual/styling/classes' target='_blank'>here</a>)",
                       "type":"array",
                       "minLength":1,
                       "items":{
                          "type":"string",
                          "enum":[
                             "display",
                             "cell-border",
                             "compact",
                             "hover",
                             "nowrap",
                             "order-column",
                             "row-border",
                             "stripe",
                          ],
                          "default":"display",
                          "required":true,
                       },
                       "required":false
                    },
                    "filter":{
                       "title":"Include column filters?",
                       "type":"string",
                       "enum":[
                          "none",
                          "bottom",
                          "top"
                       ],
                       "default":"bottom",
                       "required":true
                    },
                    "rownames":{
                       "title":"Show row names",
                       "type":"boolean",
                       "required":false,
                       "default":false
                    },
                    "extensions":{
                       "title":"Extensions: For a list of supported extensions see <a href='https://rstudio.github.io/DT/extensions.html' target='_blank'>here</a>",
                       "type":"array",
                       "uniqueItems":true,
                       "required":false,
                       "items":{
                          "type":"string",
                          "enum":[
                             "AutoFill",
                             "Buttons",
                             "ColReorder",
                             "ColVis",
                             "FixedColumns",
                             "FixedHeader",
                             "KeyTable",
                             "Responsive",
                             "RowReorder",
                             "Scroller",
                             "Select"
                          ],
                          "required":true,
                       }
                    },
                    "options":{
                       "title":"Additional options (e.g. column specific).",
                       "type":"object",
                       "required":false,
                       "properties":{
                          "dom":{
                             "title":"Appearing (position) and order of table control elements",
                             "type":"string",
                             "minLength":1,
                             "required":false
                          },
                          "buttons":{
                             "title":"Buttons or groups of buttons to insert",
                             "type":"object",
                             "required":false,
                             "default":null
                          },
                          "pageLength":{
                             "title":"Number of items to display per page",
                             "type":"integer",
                             "minimum":1,
                             "default":15,
                             "required":true
                          },
                          "columnDefs":{
                             "title":"Column specific options.",
                             "type":"array",
                             "items":{
                                "type":"object",
                                "properties":{
                                   "choice":{
                                      "title": "Column numbers to refer to:",
                                      "type": "string",
                                      "enum": ["select manually", "select all"],
                                      "required": true
                                   },
                                   "manually":{
                                      "title":"Column numbers to refer to.",
                                         "type":"array",
                                         "minItems":1,
                                         "items":{
                                            "type":"integer",
                                            "minimum":1
                                         }
                                   },
                                   "className":{
                                      "title":"column(s) class name",
                                      "required":false,
                                      "type":"string",
                                      "minLength":1
                                   },
                                   "visible":{
                                      "title":"visible",
                                      "required":false,
                                      "type":"boolean"
                                   },
                                   "searchable":{
                                      "title":"searchable",
                                      "required":false,
                                      "type":"boolean"
                                   }
                                },
                                "dependencies": {
                                    "manually": ["choice"]
                                }
                             }
                          }
                       }
                    }
                 }
              }
           },
           "dependencies": {
             "aggregateWidgetsTitle": "aggregateWidgetsTmp"
           }
        },
        "options":{
           "fields":{
             "pivottable":{
               "fields":{
                 "bgColor":{
                   "type":"color"
                 }
               }
             },
              "dataRendering":{
                 "fields":{
                    "Format: graph":{
                       "fields":{
                          "item":{
                             "fields":{
                                "graph":{
                                   "fields":{

                                            "plotly":{
                                               "fields":{
                                                  "graphtype":{
                                                     "fields":{
                                                        "pie":{
                                                           "dependencies":{
                                                              "choice": "Pie"
                                                           }
                                                        },
                                                        "chart":{
                                                           "dependencies":{
                                                              "choice": "Chart"
                                                           }
                                                        },
                                                        "histogram":{
                                                           "dependencies":{
                                                              "choice": "Histogram"
                                                           },
                                                           "fields":{
                                                             "xdata":{
                                                               "fields":{
                                                                 "item":{
                                                                   "fields":{
                                                                     "color":{
                                                                       "type":"color"
                                                                     }
                                                                   }
                                                                 }
                                                               }
                                                             }
                                                           }
                                                        }
                                                     }
                                                  }
                                               },
                                               "dependencies":{
                                                  "choice": "UsePlotly"
                                               }
                                            },
                                            "dygraph":{
                                                "dependencies":{
                                                    "choice": "UseDygraph"
                                                }
                                            }

                                    }
                                }
                             }
                          }
                       }
                    }
                 }
              },
              "datatable":{
                 "fields":{
                    "options":{
                       "fields":{
                          "columnDefs":{
                             "fields":{
                                "item":{
                                   "fields":{
                                      "manually":{
                                         "dependencies":{
                                            "choice": "select manually"
                                         }
                                      }
                                   }
                                }
                             }
                          }
                       }
                    }
                 }
              }
           }
        },
        "view": {
            "wizard": {
                "bindings": {
                    "language": 1,
                    "pageSkin": 1,
                    "excelIncludeMeta": 1,
                    "excelIncludeEmptySheets": 1,
                    "UILogo": 1,
                    "autoGenInputGraphs":1,
                    "defCompMode": 1,
                    "activateModules": 1,
                    "aggregateWidgetsTmp": 1,
                    "aggregateWidgetsTitle": 1,
                    "scalarAliases":1,
                    "saveTraceFile": 1,
                    "roundingDecimals":1,
                    "dataRendering": 2,
                    "handsontable": 3,
                    "pivottable": 3,
                    "datatable": 3
                },
                "steps": [{
                    "title": "General settings",
                    "description": "Basic Information"
                }, {
                    "title": "Graphs",
                    "description": "Configure graphs"
                }, {
                    "title": "Tables",
                    "description": "Customize the table layout"
                }],
                "showProgressBar": false,
                "markAllStepsVisited":true,
                "buttons":{
                 "submit":{
                    "title":"Download JSON",
                    "click":function(){
                       co = this.getValue();
                       function makeKey(oldKey, newKey, outType = null){
                         for (var i = 0; i < co.dataRendering[oldKey].length; i++) {
                            var keyName = co.dataRendering[oldKey][i][newKey];
                            co.dataRendering[keyName] = co.dataRendering[oldKey][i];
                            if(outType !== null){
                              co.dataRendering[keyName].outType = outType;
                            }
                            delete co.dataRendering[keyName][newKey];
                         }
                         delete co.dataRendering[oldKey];
                       }
                       // change object "tabtitle" to "title" (to match schema)
                       if(co.aggregateWidgetsTmp){
                          co.aggregateWidgets = new Object();
                          co.aggregateWidgets.title = co.aggregateWidgetsTitle;
                          delete co.aggregateWidgetsTitle;
                       }
                       delete co.aggregateWidgetsTmp
                       //restructuring of "dataRendering" for all output elements
                       makeKey("Format: datatable", "Parameter", "datatable");
                       makeKey("Format: valuebox", "scalar_type", "valueBox");
                       makeKey("Format: pivottable", "Parameter", "pivot");

                       for (let i = 0; i < co.dataRendering["Format: custom"].length; i++) {
                          let cuname = co.dataRendering["Format: custom"][i].Parameter;
                          let cuTmp = co.dataRendering["Format: custom"][i];
                          var cuOpt = new Object();
                          for(let j = 0; j < cuTmp.optionsTmp.length; j++) {
                            cuOpt[cuTmp.optionsTmp[j].key] = cuTmp.optionsTmp[j].value;
                          }
                          cuTmp.options = cuOpt;
                          delete cuTmp.optionsTmp;
                          co.dataRendering[cuname] = cuTmp;
                          delete co.dataRendering[cuname].Parameter;
                       }
                       delete co.dataRendering["Format: custom"];
                       if(co.dataRendering["Format: graph"].length){
                          for (var i = 0; i < co.dataRendering["Format: graph"].length; i++) {

                             var grname = co.dataRendering["Format: graph"][i].Parameter;
                             co.dataRendering[grname] = co.dataRendering["Format: graph"][i];
                             delete co.dataRendering[grname].Parameter;
                             //definition of graph / dtgraph
                             if(co.dataRendering[grname].outType === "single view"){
                                   co.dataRendering[grname].outType = "graph";
                             }
                             if(co.dataRendering[grname].outType === "split view"){
                                co.dataRendering[grname].outType = "dtgraph";
                             }
                             //move object graph title to match the json schema
                             if(co.dataRendering[grname].hasOwnProperty("graphtitle")){
                                co.dataRendering[grname].graph.title = co.dataRendering[grname].graphtitle;
                                delete co.dataRendering[grname].graphtitle;
                             }
                             //dyGraph
                             if(co.dataRendering[grname].graph.choice === "UseDygraph"){
                                co.dataRendering[grname].graph.tool = "dyGraph";
                                $.extend(co.dataRendering[grname].graph,co.dataRendering[grname].graph.dygraph);
                                delete co.dataRendering[grname].graph.dygraph;
                                //dyEvent object needs a name of GAMS Symbol ("eventdata")
                                for (var k = 0; k < co.dataRendering[grname].graph.dyEvent.length; k++) {
                                   var eventname = co.dataRendering[grname].graph.dyEvent[k].eventdata;
                                   delete co.dataRendering[grname].graph.dyEvent[k].eventdata;
                                   co.dataRendering[grname].graph.dyEvent[eventname] = co.dataRendering[grname].graph.dyEvent[k];
                                   delete co.dataRendering[grname].graph.dyEvent[k];
                                }
                                //ydata object needs name of GAMS Symbol ("dataname")
                                for (var k = 0; k < co.dataRendering[grname].graph.ydata.length; k++) {
                                   var yname = co.dataRendering[grname].graph.ydata[k].dataname;
                                   delete co.dataRendering[grname].graph.ydata[k].dataname;
                                   co.dataRendering[grname].graph.ydata[yname] = co.dataRendering[grname].graph.ydata[k];
                                   delete co.dataRendering[grname].graph.ydata[k];
                                }

                             }
                             else if(co.dataRendering[grname].graph.choice === "UsePlotly"){
                                co.dataRendering[grname].graph.tool = "plotly";
                                // set plotly graph type to chosen element
                                if(co.dataRendering[grname].graph.plotly.graphtype.choice === "Histogram"){
                                   co.dataRendering[grname].graph.type = "hist";
                                }
                                else if(co.dataRendering[grname].graph.plotly.graphtype.choice === "Chart"){
                                   co.dataRendering[grname].graph.type = co.dataRendering[grname].graph.plotly.graphtype.chart.type;
                                }
                                else if(co.dataRendering[grname].graph.plotly.graphtype.choice === "Pie"){
                                   co.dataRendering[grname].graph.type = "pie";
                                }
                                //restructuring (alpaca-object "choice" and the underlying structure has to be adapted in order to be compatible with schema)
                                $.extend(co.dataRendering[grname].graph,co.dataRendering[grname].graph.plotly);
                                var chosen = co.dataRendering[grname].graph.plotly.graphtype.choice;
                                $.extend(co.dataRendering[grname].graph,co.dataRendering[grname].graph.plotly.graphtype[chosen.toLowerCase()]);
                                delete co.dataRendering[grname].graph.plotly;
                                delete co.dataRendering[grname].graph.graphtype;

                                //in a histogram the data is defined for the x axis, not for the y axis
                                if(co.dataRendering[grname].graph.type === "hist"){
                                     for (var k = 0; k < co.dataRendering[grname].graph.xdata.length; k++) {
                                        var xname = co.dataRendering[grname].graph.xdata[k].dataname;
                                        delete co.dataRendering[grname].graph.xdata[k].dataname;
                                        co.dataRendering[grname].graph.xdata[xname] = co.dataRendering[grname].graph.xdata[k];
                                        delete co.dataRendering[grname].graph.xdata[k];
                                     }
                                }

                             }
                             delete co.dataRendering[grname].graph.choice;

                             //ydata manipulation for plotly and dygraph: change the name of ydata objects to the "dataname" value
                             if((co.dataRendering["Format: graph"][i].graph.choice === "UseDygraph") || ((co.dataRendering["Format: graph"][i].graph.choice === "UsePlotly") && (co.dataRendering[grname].graph.plotly.graphtype.choice !== "Histogram"))){
                                for (var j = 0; j < co.dataRendering[grname].graph.ydata.length; j++) {
                                      var yname = co.dataRendering[grname].graph.ydata[j].dataname;
                                      delete co.dataRendering[grname].graph.ydata[j].dataname;
                                      co.dataRendering[grname].graph.ydatatmp[yname] = co.dataRendering[grname].graph.ydata[j];
                                   }
                                   //delete co.dataRendering[grname].graph.ydata;
                                   co.dataRendering[grname].graph.ydata = co.dataRendering[grname].graph.ydatatmp;
                                   delete co.dataRendering[grname].graph.ydatatmp;
                             }
                          }
                       }
                       delete co.dataRendering["Format: graph"];
                       let data = "text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(co,null,"  "));
                       $("<a href=\"data:" + data + "\" download=\"config.json\" id=\"jsonDlLink\">Download</a>").appendTo("body");
                       $("#jsonDlLink")[0].click();
                       $("#jsonDlLink").remove();
                    }
                 }
              }
            }
        },
        "postRender": function(control) {
            var pivot = control.getControlByPath("dataRendering/Format: pivottable");
            pivot.on("change", function() {
                for(let i = 0; i < pivot.children.length; i++) {
                  let gmsSymName = pivot.children[i].childrenByPropertyId["Parameter"].data[0].text;
                  let pivottable = pivot.children[i].childrenByPropertyId["pivottable"]
                  let rows = pivottable.childrenByPropertyId["rows"];
                  rows.schema.items.enum = gmsSymHdr[gmsSymName];
                  rows.schema.items.default = gmsSymHdr[gmsSymName][0];
                  rows.refresh();
                  for(let j = 0; j < rows.children.length; j++) {
                    rows.children[j].schema.enum = rows.children[j].options.optionLabels = gmsSymHdr[gmsSymName];
                    rows.children[j].schema.default = gmsSymHdr[gmsSymName][0];
                    rows.children[j].refresh();
                  }
                  let cols = pivottable.childrenByPropertyId["cols"];
                  cols.schema.items.enum = gmsSymHdr[gmsSymName];
                  cols.schema.items.default = gmsSymHdr[gmsSymName][0];
                  cols.refresh();
                  for(let j = 0; j < cols.children.length; j++) {
                    cols.children[j].schema.enum = cols.children[j].options.optionLabels = gmsSymHdr[gmsSymName];
                    cols.children[j].schema.default = gmsSymHdr[gmsSymName][0];
                    cols.children[j].refresh();
                  }
                  let vals = pivottable.childrenByPropertyId["vals"];
                  vals.schema.enum = vals.options.optionLabels = gmsSymNumHdr[gmsSymName];
                  vals.refresh();
                }
            });
            let graph = control.getControlByPath("dataRendering/Format: graph");
            graph.on("change", function() {
                for(let i = 0; i < graph.children.length; i++) {
                  let gmsSymName = graph.children[i].childrenByPropertyId["Parameter"].data[0].text;
                  let plotly = graph.children[i].childrenByPropertyId["graph"].childrenByPropertyId["plotly"].childrenByPropertyId["graphtype"];
                  let dataReqSym = plotly.childrenByPropertyId["pie"].childrenByPropertyId["labels"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  dataReqSym = plotly.childrenByPropertyId["pie"].childrenByPropertyId["values"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  dataReqSym = plotly.childrenByPropertyId["chart"].childrenByPropertyId["color"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  dataReqSym = plotly.childrenByPropertyId["chart"].childrenByPropertyId["xdata"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  let ydata = plotly.childrenByPropertyId["chart"].childrenByPropertyId["ydata"];
                  dataReqSym = ydata.childrenByPropertyId["dataname"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  dataReqSym = ydata.childrenByPropertyId["label"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  dataReqSym = plotly.childrenByPropertyId["histogram"].childrenByPropertyId["xdata"];
                  dataReqSym.schema.items.properties.dataname.enum = gmsSymHdrIn[gmsSymName];
                  dataReqSym.schema.items.properties.dataname.default = gmsSymHdrIn[gmsSymName][0];
                  for(let j = 0; j < dataReqSym.children.length; j++) {
                    dataReqSym.children[j].childrenByPropertyId["dataname"].schema.enum = dataReqSym.children[j].childrenByPropertyId["dataname"].options.optionLabels = gmsSymHdr[gmsSymName];
                    dataReqSym.children[j].childrenByPropertyId["dataname"].schema.default = gmsSymHdr[gmsSymName][0];
                    dataReqSym.children[j].refresh();
                  }
                  let dygraph = graph.children[i].childrenByPropertyId["graph"].childrenByPropertyId["dygraph"];
                  dataReqSym = dygraph.childrenByPropertyId["color"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  dataReqSym = dygraph.childrenByPropertyId["xdata"];
                  dataReqSym.schema.enum = dataReqSym.options.optionLabels = gmsSymHdr[gmsSymName];
                  dataReqSym.schema.default = gmsSymHdr[gmsSymName][0];
                  dataReqSym.refresh();
                  dataReqSym = dygraph.childrenByPropertyId["ydata"];
                  dataReqSym.schema.items.properties.dataname.enum = gmsSymHdrIn[gmsSymName];
                  dataReqSym.schema.items.properties.dataname.default = gmsSymHdrIn[gmsSymName][0];
                  for(let j = 0; j < dataReqSym.children.length; j++) {
                    dataReqSym.children[j].childrenByPropertyId["dataname"].schema.enum = dataReqSym.children[j].childrenByPropertyId["dataname"].options.optionLabels = gmsSymHdr[gmsSymName];
                    dataReqSym.children[j].childrenByPropertyId["dataname"].schema.default = gmsSymHdr[gmsSymName][0];
                    dataReqSym.children[j].refresh();
                  }
                }
            });

            $("[data-alpaca-field-name='aggregateWidgetsTmp']").addClass("alpaca-popup-image");
            $("[data-alpaca-field-name='aggregateWidgetsTmp']>.control-label").append(" <i class='fas fa-info-circle'/><img src='./assets/images/generator_img/gen_aggregate_both.PNG' style = 'max-height:1000px; max-Width:800px;'/>");

            $("[data-alpaca-field-name='activateModules_logFile:']").addClass("alpaca-popup-image");
            $("[data-alpaca-field-name='activateModules_logFile:']>.control-label").append(" <i class='fas fa-info-circle'/><img src='./assets/images/generator_img/gen_log.PNG' style = 'max-height:600px; max-Width:1000px;'/>");

            $("[data-alpaca-field-name='activateModules_lstFile:']").addClass("alpaca-popup-image");
            $("[data-alpaca-field-name='activateModules_lstFile:']>.control-label").append(" <i class='fas fa-info-circle'/><img src='./assets/images/generator_img/gen_lst.PNG' style = 'max-height:600px; max-Width:1000px;'/>");

            $("[data-alpaca-field-name='dataRendering_Format: graph']").addClass("alpaca-popup-image");
            $("[data-alpaca-field-name='dataRendering_Format: graph']>.alpaca-container-label").append(" <i class='fas fa-info-circle'/><img src='./assets/images/generator_img/gen_graph.PNG' style = 'max-height:600px; max-Width:1000px;'/>");

            $("[data-alpaca-field-name='dataRendering_Format: valuebox']").addClass("alpaca-popup-image");
            $("[data-alpaca-field-name='dataRendering_Format: valuebox']>.alpaca-container-label").append(" <i class='fas fa-info-circle'/><img src='./assets/images/generator_img/gen_scalar_widget.PNG' style = 'max-height:300px; max-Width:500px;'/>");

            $("[data-alpaca-field-name='dataRendering_Format: valuebox_0_options_icon']").addClass("alpaca-popup-image");
            $("[data-alpaca-field-name='dataRendering_Format: valuebox_0_options_icon']>.alpaca-container-label").append(" <i class='fas fa-info-circle'/><img src='./assets/images/generator_img/gen_scalar_widget_icon.PNG' style = 'max-height:300px; max-Width:500px;'/>");

            $("[data-alpaca-field-name='dataRendering_Format: pivottable']").addClass("alpaca-popup-image");
            $("[data-alpaca-field-name='dataRendering_Format: pivottable']>.alpaca-container-label").append(" <i class='fas fa-info-circle'/><img src='./assets/images/generator_img/gen_pivot_table.PNG' style = 'max-height:600px; max-Width:1000px;'/>");

        }
    });
}
function launchInputGen(gmsSymIn, scalarSyms, gmsSymHdrIn){
  let gmsSymHeaders = [];
  for(let i=0;i<gmsSymIn.length;i++){
    if(typeof gmsSymHdrIn[gmsSymIn[i]] !== "undefined"){
      gmsSymHeaders = gmsSymHeaders.concat(gmsSymHdrIn[gmsSymIn[i]]);
    }
  }
  Alpaca.defaultToolbarSticky = true;
    $("#form1").alpaca({
      "schema": {
        "$schema":"http://json-schema.org/draft-07/schema#",
        "title":"",
        "type":"object",
        "additionalProperties":false,
        "properties":{
              "gmsParam": {
                "title":"Which parameter would you like to customize?",
                "type":"string",
                "enum":gmsSymIn.concat(scalarSyms),
                "default":gmsSymIn.concat(scalarSyms)[0],
                "required":true
              },
              "widgetType":{
                "title":"What type of input widget do you want?",
                "type":"string",
                "enum":["slider","dropdown","date selector","date range selector","checkbox"],
                "default":"slider",
                "required":true
              },
              "readOnlyCols":{
                "title":"Select columns that are to be read-only",
                "type":"array",
                "items":{
                  "type":"object",
                  "properties":{
                    "colName":{
                      "title":"Column name",
                      "type":"string",
                      "enum":gmsSymHdrIn[gmsSymIn[0]],
                      "default":gmsSymHdrIn[gmsSymIn[0]][0],
                      "required":true
                    }
                  }
                }
              },
              "slider":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for slider",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "slidertype":{
                    "title":"Do you want a standard slider (single value) or a slider range (min and max values)?",
                    "type":"string",
                    "enum":["standard", "slider range"],
                    "default":"standard",
                    "required":true
                  },
                  "minDepSel":{
                    "title":"Do you want the minimum slider value to be dependent on another dataset?",
                    "type":"string",
                    "enum":["dependent","static"],
                    "default":"static",
                    "required":true
                  },
                  "min":{
                    "title":"Minimum value",
                    "type":"number",
                    "required":true
                  },
                  "minDep":{
                    "type":"object",
                    "additionalProperties":false,
                    "properties":{
                      "minop":{
                        "title":"Operator to use in order to summarize set elements to single value",
                        "type":"string",
                        "enum":["minimum","maximum","variance","standard deviation","median","mean","count"],
                        "default":["minimum"],
                        "required":true
                      },
                      "minpar":{
                        "title":"Parameter to depend on",
                        "type":"string",
                        "enum":gmsSymIn.concat("None"),
                        "default":gmsSymIn[0],
                        "required":true
                      },
                      "minhdr":{
                        "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                        "type":"string",
                        "enum":gmsSymHdrIn[gmsSymIn[0]],
                        "default":gmsSymHdrIn[gmsSymIn[0]][0],
                        "required":true
                      }
                    }
                  },
                  "maxDepSel":{
                    "title":"Do you want the maximum slider value to be dependent on another dataset?",
                    "type":"string",
                    "enum":["dependent","static"],
                    "default":"static",
                    "required":true
                  },
                  "max":{
                    "title":"Maximum value",
                    "type":"number",
                    "required":true
                  },
                  "maxDep":{
                    "type":"object",
                    "additionalProperties":false,
                    "properties":{
                      "maxop":{
                        "title":"Operator to use in order to summarize set elements to single value",
                        "type":"string",
                        "enum":["minimum","maximum","variance","standard deviation","median","mean","count"],
                        "default":["minimum"],
                        "required":true
                      },
                      "maxpar":{
                        "title":"Parameter to depend on",
                        "type":"string",
                        "enum":gmsSymIn.concat("None"),
                        "default":gmsSymIn[0],
                        "required":true
                      },
                      "maxhdr":{
                        "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                        "type":"string",
                        "enum":gmsSymHdrIn[gmsSymIn[0]],
                        "default":gmsSymHdrIn[gmsSymIn[0]][0],
                        "required":true
                      }
                    }
                  },
                  "defDepSel":{
                    "title":"Do you want the default slider value to be dependent on another dataset?",
                    "type":"string",
                    "enum":["dependent","static"],
                    "default":"static",
                    "required":true
                  },
                  "defaultmin":{
                    "title":"Default/starting value (lower end of range)",
                    "type": "number",
                    "required":true
                  },
                  "defaultmax":{
                    "title":"Default/starting value (upper end of range)",
                    "type": "number",
                    "required":true
                  },
                  "default":{
                    "title":"Default/starting value",
                    "type": "number",
                    "required":true
                  },
                  "defDep":{
                    "type":"object",
                    "additionalProperties":false,
                    "properties":{
                      "defop":{
                        "title":"Operator to use in order to summarize set elements to single value",
                        "type":"string",
                        "enum":["minimum","maximum","variance","standard deviation","median","mean","count"],
                        "default":["minimum"],
                        "required":true
                      },
                      "defpar":{
                        "title":"Parameter to depend on",
                        "type":"string",
                        "enum":gmsSymIn.concat("None"),
                        "default":gmsSymIn[0],
                        "required":true
                      },
                      "defhdr":{
                        "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                        "type":"string",
                        "enum":gmsSymHdrIn[gmsSymIn[0]],
                        "default":gmsSymHdrIn[gmsSymIn[0]][0],
                        "required":true
                      }
                    }
                  },
                  "step":{
                    "title":"Specify the interval between each selectable value",
                    "type":"number",
                    "required":true
                  },
                  "width":{
                    "title":"Width of the slider (optional)",
                    "type":"string",
                    "required":false
                  },
                  "ticks":{
                    "title":"Do you want to show tick marks on the slider?",
                    "type":"boolean",
                    "default":true,
                    "required":false
                  }
                },
                "dependencies":{
                  "min":["minDepSel"],
                  "minDep":["minDepSel"],
                  "max":["maxDepSel"],
                  "maxDep":["maxDepSel"],
                  "defDepSel":["slidertype"],
                  "default":["defDepSel"],
                  "defDep":["defDepSel"],
                  "defaultmin":["slidertype"],
                  "defaultmax":["slidertype"],
                }
              },
              "dropdown":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for dropdown menu",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "choices":{
                    "title":"List of values to select from. This is the text that will be passed on to GAMS.",
                    "type":"array",
                    "minLength":1,
                    "minItems":1,
                    "required":true,
                    "items":{
                      "type":"object",
                      "additionalProperties":false,
                      "properties":{
                        "choiceDepSel":{
                          "title":"Should item be dependent on another dataset or static?",
                          "type": "string",
                          "enum":["dependent","static"],
                          "default":"static",
                          "required":true
                        },
                        "choice":{
                          "type":"string",
                          "title":"Enter new choice",
                          "required": true,
                          "minLength":1
                        },
                        "choiceDep":{
                          "type":"object",
                          "additionalProperties":false,
                          "properties":{
                            "choiceDepType":{
                              "title":"Do you want to filter the dependent table based on the selection you made, fill the dropdown menu based on the elements in the dependent table or both?",
                              "type": "string",
                              "enum":["filter table","fill with values","both"],
                              "default":"both",
                              "required":true
                            },
                            "choicepar":{
                              "title":"Parameter to depend on",
                              "type":"string",
                              "enum":gmsSymIn.concat("None"),
                              "default":gmsSymIn[0],
                              "required":true
                            },
                            "choicehdr":{
                              "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                              "type":"string",
                              "enum":gmsSymHdrIn[gmsSymIn[0]],
                              "default":gmsSymHdrIn[gmsSymIn[0]][0],
                              "required":true
                            }
                          }
                        }
                      },
                      "dependencies":{
                        "choice":["choiceDepSel"],
                        "choiceDep":["choiceDepSel"]
                      }
                    }
                  },
                  "aliases":{
                    "type":"array",
                    "title":"Aliases for the elements (length must match the number of choices you entered or left empty). This is the text the user will see in the UI.  In case no aliases are defined, the text displayed in the UI is the same as the one for choices.",
                    "required":false,
                    "items":{
                      "type":"object",
                      "additionalProperties":false,
                      "properties":{
                        "aliasDepSel":{
                          "title":"Should item be dependent on another dataset or static?",
                          "type": "string",
                          "enum":["dependent","static"],
                          "default":"static",
                          "required":true
                        },
                        "alias":{
                          "type":"string",
                          "title":"Enter the element name",
                          "required":true,
                          "minLength":1
                        },
                        "aliasDep":{
                          "type":"object",
                          "additionalProperties":false,
                          "properties":{
                            "aliasDepType":{
                              "title":"Do you want to filter the dependent table based on the selection you made, fill the dropdown menu based on the elements in the dependent table or both?",
                              "type": "string",
                              "enum":["filter table","fill with values","both"],
                              "default":"both",
                              "required":true
                            },
                            "aliaspar":{
                              "title":"Parameter to depend on",
                              "type":"string",
                              "enum":gmsSymIn.concat("None"),
                              "default":gmsSymIn[0],
                              "required":true
                            },
                            "aliashdr":{
                              "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                              "type":"string",
                              "enum":gmsSymHdrIn[gmsSymIn[0]],
                              "default":gmsSymHdrIn[gmsSymIn[0]][0],
                              "required":true
                            }
                          }
                        }
                      },
                      "dependencies":{
                        "alias":["aliasDepSel"],
                        "aliasDep":["aliasDepSel"]
                      }
                    }
                  },
                  "multiple":{
                    "title":"Are multiple choices allowed to be selected?",
                    "type":"boolean",
                    "required":false
                  },
                  "choiceMandatory":{
                    "title":"Is it mandatory to select an option in the dropdown menu?",
                    "type":"boolean",
                    "required":false
                  },
                  "selected":{
                    "title":"The initially selected value",
                    "type":"string",
                    "required":false
                  }
                },
                "dependencies":{
                  "choiceMandatory":["multiple"]
                }
              },
              "date":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for date selector",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "value":{
                    "title":"initial start date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "min":{
                    "title":"minimum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "max":{
                    "title":"maximum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "format":{
                    "title":"format of the date to display in the browser",
                    "type":"string",
                    "minLength":1,
                    "maxLength":10,
                    "default":"yyyy-mm-dd",
                    "required":false
                  },
                  "startview":{
                    "title":"date range shown when the input object is first clicked",
                    "type":"string",
                    "enum":[
                      "month",
                      "year",
                      "decade"
                    ],
                    "default":"month",
                    "required":true
                  },
                  "weekstart":{
                    "title":"Which day is displayed to be the first of the week (0=Sunday, 6=Saturday)?",
                    "type":"integer",
                    "minimum":0,
                    "maximum":6,
                    "required":false
                  },
                  "width":{
                    "title":"width of the input",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  }
                }
              },
              "daterange":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for date range selector",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "start":{
                    "title":"initial start date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "end":{
                    "title":"initial end date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "min":{
                    "title":"minimum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "max":{
                    "title":"maximum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "format":{
                    "title":"format of the date to display in the browser.",
                    "type":"string",
                    "minLength":1,
                    "maxLength":10,
                    "default":"yyyy-mm-dd",
                    "required":false
                  },
                  "startview":{
                    "title":"date range shown when the input object is first clicked",
                    "type":"string",
                    "enum":[
                      "month",
                      "year",
                      "decade"
                    ],
                    "default":"month",
                    "required":true
                  },
                  "weekstart":{
                    "title":"Which day is displayed to be the first of the week (0=Sunday, 6=Saturday)?",
                    "type":"integer",
                    "minimum":0,
                    "maximum":6,
                    "default":0,
                    "required":false
                  },
                  "separator":{
                    "title":"String to display between the start and end input boxes",
                    "type":"string",
                    "minLength":1,
                    "default":" to ",
                    "required":false
                  },
                  "width":{
                    "title":"width of the input",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "autoclose":{
                    "title":"Should the datepicker be closed automatically once a date is selected?",
                    "type":"boolean",
                    "required":false
                  }
                }
              },
              "checkbox":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for checkbox",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "value":{
                    "title":"initial value",
                    "type":"boolean",
                    "required":false
                  },
                  "class":{
                    "title":"css class",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "width":{
                    "title":"width of the input",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  }
                }
              },
              "noImport":{
                "title":"Should data NOT be imported from an external source (e.g. spreadsheet)?",
                "type":"boolean",
                "required":false
              },
              "readonly":{
                "title":"Should the entire table be readonly?",
                "type":"boolean"
              },
              /*"sharedData":{
                "title":"Is data for this widget stored in a shared database table and thus loaded at startup?",
                "type":"boolean",
                "required":false
              },*/
              "colSubset":{
                "title":"Which columns should be imported to the UI (names of columns)?",
                "type":"array",
                "uniqueItems":true,
                "minItems":1,
                "items":{
                  "title":"Name of column",
                  "type":"string",
                  "minLength":1,
                  "required":true
                },
                "required":false
              },
              "noBatch":{
                "title":"Should element be excluded from batch mode (only relevant for batch module)?",
                "type":"boolean",
                "required":false
              }
            },
            "dependencies":{
              "widgetType":["gmsParam"],
              "headers":["gmsParam"],
              "dropdown":["widgetType"],
              "slider":["widgetType"],
              "date":["widgetType"],
              "daterange":["widgetType"],
              "checkbox":["widgetType"],
              "noImport":["gmsParam"],
              "readonly":["gmsParam"],
              "readOnlyCols":["gmsParam"],
              "sharedData":["gmsParam"],
              "colSubset":["sharedData"],
              "noBatch":["widgetType"]
            }
          },
          "options":{
             "fields":{
               "widgetType":{
                 "dependencies":{
                    "gmsParam": scalarSyms
                 }
               },
                "headers":{
                  "dependencies":{
                     "gmsParam": gmsSymIn
                  }
                },
                "noImport":{
                  "dependencies":{
                     "gmsParam": gmsSymIn
                  }
                },
                "readonly":{
                  "dependencies":{
                     "gmsParam": gmsSymIn
                  }
                },
                "readOnlyCols":{
                  "dependencies":{
                     "gmsParam": gmsSymIn
                  }
                },
                "sharedData":{
                  "dependencies":{
                     "gmsParam": gmsSymIn
                  }
                },
                "dropdown":{
                  "dependencies":{
                     "widgetType": "dropdown"
                  },
                  "fields":{
                    "choiceMandatory":{
                      "dependencies":{
                        "multiple": false
                      }
                    },
                    "choices":{
                      "fields":{
                        "item":{
                          "fields":{
                            "choice":{
                              "dependencies":{
                                "choiceDepSel": "static"
                              }
                            },
                            "choiceDep":{
                              "dependencies":{
                                "choiceDepSel":"dependent"
                              }
                            }
                          }
                        }
                      }
                    },
                    "aliases":{
                      "fields":{
                        "item":{
                          "fields":{
                            "alias":{
                              "dependencies":{
                                "aliasDepSel": "static"
                              }
                            },
                            "aliasDep":{
                              "dependencies":{
                                "aliasDepSel": "dependent"
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                },
                "slider":{
                  "dependencies":{
                     "widgetType": "slider"
                  },
                  "fields":{
                    "min":{
                      "dependencies":{
                        "minDepSel": "static"
                      }
                    },
                    "minDep":{
                      "dependencies":{
                        "minDepSel":"dependent"
                      }
                    },
                    "max":{
                      "dependencies":{
                        "maxDepSel": "static"
                      }
                    },
                    "maxDep":{
                      "dependencies":{
                        "maxDepSel": "dependent"
                      }
                    },
                    "defDepSel":{
                      "dependencies":{
                        "slidertype": "standard"
                      }
                    },
                    "defaultmin":{
                      "dependencies":{
                        "slidertype": "slider range"
                      }
                    },
                    "defaultmax":{
                      "dependencies":{
                        "slidertype": "slider range"
                      }
                    },
                    "default":{
                      "dependencies":{
                        "defDepSel": "static"
                      }
                    },
                    "defDep":{
                      "dependencies":{
                        "defDepSel": "dependent"
                      }
                    }
                  }
                },
                "date":{
                  "dependencies":{
                     "widgetType": "date selector"
                  },
                  "fields":{
                    "value":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "min":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "max":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    }
                  }
                },
                "daterange":{
                  "dependencies":{
                     "widgetType": "date range selector"
                  },
                  "fields":{
                    "start":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "end":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "min":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "max":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    }
                  }
                },
                "checkbox":{
                  "dependencies":{
                     "widgetType": "checkbox"
                  }
                },
                "noBatch":{
                  "dependencies":{
                     "widgetType": ["checkbox","slider","dropdown","date selector","date range selector"]
                  }
                }
              },
              "form":{
                "buttons":{
                   "submit":{
                      "title":"Copy JSON to clipboard",
                      "click":function(){
                        co = this.getValue();
                        switch(co.widgetType) {
                          case "slider":
                            function addOp(opName, sliderVal){
                              let sliderValOp;
                              switch(opName){
                                case "count":
                                  sliderValOp = "card(" + sliderVal + ")";
                                break;
                                case "maximum":
                                  sliderValOp = "max(" + sliderVal + ")";
                                break;
                                case "mean":
                                  sliderValOp = "mean(" + sliderVal + ")";
                                break;
                                case "median":
                                  sliderValOp = "median(" + sliderVal + ")";
                                break;
                                case "minimum":
                                  sliderValOp = "min(" + sliderVal + ")";
                                break;
                                case "standard deviation":
                                  sliderValOp = "sd(" + sliderVal + ")";
                                break;
                                case "variance":
                                  sliderValOp = "var(" + sliderVal + ")";
                                break;
                              }
                              return sliderValOp;
                            }
                            if(co.slider.minDepSel === "dependent"){
                              let sliderVal = "";
                              if($.inArray("minpar", Object.keys(co.slider.minDep)) !== -1){
                                sliderVal = co.slider.minDep.minpar + "$";
                              }

                              sliderVal = sliderVal + co.slider.minDep.minhdr;
                              sliderVal = addOp(co.slider.minDep.minop, sliderVal);
                              co.slider.min = sliderVal;
                              delete co.slider.minDep;
                            }
                            delete co.slider.minDepSel;

                            if(co.slider.maxDepSel === "dependent"){
                              let sliderVal = "";
                              if($.inArray("maxpar", Object.keys(co.slider.maxDep)) !== -1){
                                sliderVal = co.slider.maxDep.maxpar + "$";
                              }
                              sliderVal = sliderVal + co.slider.maxDep.maxhdr;
                              sliderVal = addOp(co.slider.maxDep.maxop, sliderVal);
                              co.slider.max = sliderVal;
                              delete co.slider.maxDep;
                            }
                            delete co.slider.maxDepSel;

                            if(co.slider.defDepSel === "dependent"){
                              let sliderVal = "";
                              if($.inArray("defpar", Object.keys(co.slider.defDep)) !== -1){
                                sliderVal = co.slider.defDep.defpar + "$";
                              }
                              sliderVal = sliderVal + co.slider.defDep.defhdr;
                              sliderVal = addOp(co.slider.defDep.defop, sliderVal);
                              co.slider.default = sliderVal;
                              delete co.slider.defDep;
                            }
                            delete co.slider.defDepSel;
                            if(co.slider.slidertype === "slider range"){
                              co.slider.default = [co.slider.defaultmin, co.slider.defaultmax];
                              delete co.slider.defaultmin;
                              delete co.slider.defaultmax;
                            }
                            delete co.slider.slidertype;
                            // get rid of default values as this makes JSON string shorter
                            if(co.slider.ticks){
                              delete co.slider.ticks;
                            }
                          break;
                          case "dropdown":
                            if(co.dropdown.aliases.length !== 0 && co.dropdown.choices.length !== co.dropdown.aliases.length){
                              alert("The number of choices does not match the number of aliases! Please make sure you enter as many aliases as you entered choices.");
                              return;
                            }
                            let choicesR = co.dropdown.choices;
                            let choices = [];
                            for(let i=0;i<choicesR.length;i++){
                              if(choicesR[i].choiceDepSel === "dependent"){
                                let choice = "";
                                if($.inArray("choicepar", Object.keys(choicesR[i].choiceDep)) !== -1){
                                  choice = choicesR[i].choiceDep.choicepar + "$";
                                }
                                choice = choice + choicesR[i].choiceDep.choicehdr;
                                switch(choicesR[i].choiceDep.choiceDepType){
                                  case "fill with values":
                                    choice = "$" + choice;
                                  break;
                                  case "filter table":
                                    choice = choice + "$";
                                  break;
                                  case "both":
                                    choice = "$" + choice + "$";
                                  break;
                                }
                                choices.push(choice);
                              }else{
                                choices.push(choicesR[i].choice);
                              }
                            }
                            if(!co.dropdown.choiceMandatory){
                              // push underscore _ as its a special symbol indicating that nothing was selected in dropdown
                              choices.push("_");
                            }
                            co.dropdown.choices = choices;
                            choicesR = co.dropdown.aliases;
                            let aliases = [];
                            for(let i=0;i<choicesR.length;i++){
                              if(choicesR[i].aliasDepSel === "dependent"){
                                let alias = "";
                                if($.inArray("aliaspar", Object.keys(choicesR[i].aliasDep)) !== -1){
                                  alias = choicesR[i].aliasDep.aliaspar + "$";
                                }
                                alias = alias + choicesR[i].aliasDep.aliashdr;
                                switch(choicesR[i].aliasDep.aliasDepType){
                                  case "fill with values":
                                    alias = "$" + alias;
                                  break;
                                  case "filter table":
                                    alias = alias + "$";
                                  break;
                                  case "both":
                                    alias = "$" + alias + "$";
                                  break;
                                }
                                aliases.push(alias);
                              }else{
                                aliases.push(choicesR[i].alias);
                              }
                            }
                            if(aliases.length){
                              if(!co.dropdown.choiceMandatory){
                                aliases.push("_");
                              }
                              co.dropdown.aliases = aliases;
                            }else{
                              delete co.dropdown.aliases;
                            }
                            delete co.dropdown.choiceMandatory
                          break;
                          case "date selector":
                            // get rid of default values as this makes JSON string shorter
                            if(co.date.format === "yyyy-mm-dd"){
                              delete co.date.format;
                            }
                            if(co.date.startview === "month"){
                              delete co.date.startview;
                            }
                            if($.isEmptyObject(co.date)){
                              delete co.date;
                            }
                          break;
                          case "date range selector":
                            // get rid of default values as this makes JSON string shorter
                            if(co.daterange.format === "yyyy-mm-dd"){
                              delete co.daterange.format;
                            }
                            if(co.daterange.startview === "month"){
                              delete co.daterange.startview;
                            }
                            if(co.daterange.weekstart === 0){
                              delete co.daterange.weekstart;
                            }
                            if(co.daterange.separator === " to "){
                              delete co.daterange.separator;
                            }
                            if(co.daterange.autoclose === false){
                              delete co.daterange.autoclose;
                            }
                            if($.isEmptyObject(co.daterange)){
                              delete co.daterange;
                            }
                          break;
                          case "checkbox":
                            if(co.checkbox.value){
                              co.checkbox.value = 1;
                            }else{
                              co.checkbox.value = 0;
                            }
                          break;
                          default:
                            if(co.readOnlyCols.length){
                              let roCols = new Object();
                              for(let i=0;i<co.readOnlyCols.length;i++){
                                roCols[co.readOnlyCols[i].colName] = new Object();
                                roCols[co.readOnlyCols[i].colName].readonly = true;
                              }
                              co.headers = roCols;
                            }
                            delete co.readOnlyCols;
                            // get rid of default values as this makes JSON string shorter
                            if(!co.noImport){
                              delete co.noImport;
                            }
                            if(!co.readonly){
                              delete co.readonly;
                            }
                            if(!co.sharedData){
                              delete co.sharedData;
                            }
                        }
                        if(!co.noBatch){
                          delete co.noBatch;
                        }
                        delete co.gmsParam;
                        delete co.widgetType;
                        if($.isEmptyObject(co)){
                          alert("No customization was found (you probably selected only defult values). You won't have to add any JSON to your GAMS prameter!");
                          return;
                        }
                        // copy and paste idea from: https://stackoverflow.com/questions/400212/how-do-i-copy-to-the-clipboard-in-javascript
                        $("#cpTA-wrapper").show();
                        let cpTA = $("#cpTA");
                        cpTA.val(" ### " + JSON.stringify(co,null,"  ").replace(/\s/g,""));
                        cpTA.focus();
                        cpTA.select();
                        let err = false;
                        try {
                          let successful = document.execCommand("copy");
                          if(!successful){
                            err = true;
                          }
                        } catch (err) {
                          err = true;
                        }
                        if(err){
                          alert("Copying the text to clipboard was not successful. Please copy manually.")
                          // don't remove textarea if copying failed
                          return;
                        }else{
                          $("#cpTA-wrapper").hide();
                          alert("Your cutomization options were copied to your clipboard. Please paste them at the end of the explanatory text of the parameter you just customized.");
                        }

                        console.log(JSON.stringify(co,null,"  "));
                      }
                   }
                }
              }
            },
            "postRender": function(control) {
                let gmsParam = control.getControlByPath("gmsParam");
                gmsParam.on("change", function() {
                  let roCols = control.getControlByPath("readOnlyCols");
                  let gmsSymName = gmsParam.data[0].text;
                  roCols.schema.items.properties.colName.enum = gmsSymHdrIn[gmsSymName];
                  roCols.schema.items.properties.colName.default = gmsSymHdrIn[gmsSymName][0];
                  for(var i = 0; i < roCols.children.length; i++) {
                    roCols.children[i].childrenByPropertyId["colName"].schema.enum=
                    roCols.children[i].childrenByPropertyId["colName"].options.optionLabels = gmsSymHdrIn[gmsSymName];
                    roCols.children[i].childrenByPropertyId["colName"].schema.default=gmsSymHdrIn[gmsSymName][0];
                    roCols.children[i].childrenByPropertyId["colName"].refresh();
                  }
                });
                let minPar = control.getControlByPath("slider/minDep/minpar");
                minPar.on("change", function() {
                  let minHdr = control.getControlByPath("slider/minDep/minhdr");
                  let gmsSymName = minPar.data[0].text;

                  if(gmsSymName === "None"){
                    minHdr.schema.enum=
                    minHdr.options.optionLabels = gmsSymHeaders;
                    minHdr.schema.default=gmsSymHeaders[0];
                    minHdr.refresh();
                  }else{
                    minHdr.schema.default = gmsSymHdrIn[gmsSymName][0];
                    minHdr.schema.enum=
                    minHdr.options.optionLabels = gmsSymHdrIn[gmsSymName];
                    minHdr.refresh();
                  }
                });
                let maxPar = control.getControlByPath("slider/maxDep/maxpar");
                maxPar.on("change", function() {
                  let maxHdr = control.getControlByPath("slider/maxDep/maxhdr");
                  let gmsSymName = maxPar.data[0].text;

                  if(gmsSymName === "None"){
                    maxHdr.schema.enum=
                    maxHdr.options.optionLabels = gmsSymHeaders;
                    maxHdr.schema.default=gmsSymHeaders[0];
                    maxHdr.refresh();
                  }else{
                    maxHdr.schema.default = gmsSymHdrIn[gmsSymName][0];
                    minHdr.schema.enum=
                    minHdr.options.optionLabels = gmsSymHdrIn[gmsSymName];
                    minHdr.refresh();
                  }
                });
                let defPar = control.getControlByPath("slider/defDep/defpar");
                defPar.on("change", function() {
                  let defHdr = control.getControlByPath("slider/defDep/defhdr");
                  let gmsSymName = defPar.data[0].text;

                  if(gmsSymName === "None"){
                    defHdr.schema.enum=
                    defHdr.options.optionLabels = gmsSymHeaders;
                    defHdr.schema.default=gmsSymHeaders[0];
                    defHdr.refresh();
                  }else{
                    defHdr.schema.default = gmsSymHdrIn[gmsSymName][0];
                    minHdr.schema.enum=
                    minHdr.options.optionLabels = gmsSymHdrIn[gmsSymName];
                    minHdr.refresh();
                  }
                });
                let choicePar = control.getControlByPath("dropdown/choices");
                choicePar.on("change", function() {
                  for(let i = 0; i < choicePar.children.length; i++) {
                    let gmsSymName = choicePar.children[i].childrenByPropertyId["choiceDep"].childrenByPropertyId["choicepar"].data[0].text;
                    let hdr = choicePar.children[i].childrenByPropertyId["choiceDep"].childrenByPropertyId["choicehdr"];
                    if(gmsSymName === "None"){
                      hdr.schema.enum =
                      hdr.options.optionLabels = gmsSymHeaders;
                      hdr.schema.default =gmsSymHeaders[0];
                      hdr.refresh();
                    }else{
                      hdr.schema.default = gmsSymHdrIn[gmsSymName][0];
                      hdr.schema.enum=
                      hdr.options.optionLabels = gmsSymHdrIn[gmsSymName];
                      hdr.refresh();
                    }
                  }
                });
                let aliasPar = control.getControlByPath("dropdown/aliases");
                aliasPar.on("change", function() {
                  for(let i = 0; i < choicePar.children.length; i++) {
                    let gmsSymName = aliasPar.children[i].childrenByPropertyId["aliasDep"].childrenByPropertyId["aliaspar"].data[0].text;
                    let hdr = aliasPar.children[i].childrenByPropertyId["aliasDep"].childrenByPropertyId["aliashdr"];

                    if(gmsSymName === "None"){
                      hdr.schema.enum =
                      hdr.options.optionLabels = gmsSymHeaders;
                      hdr.schema.default =gmsSymHeaders[0];
                      hdr.refresh();
                    }else{
                      hdr.schema.default = gmsSymHdrIn[gmsSymName][0];
                      hdr.schema.enum=
                      hdr.options.optionLabels = gmsSymHdrIn[gmsSymName];
                      hdr.refresh();
                    }
                  }
                });
              }
    });
}
function launchInputCGen(gmsSymIn, scalarSyms, gmsSymHdrIn){
  Alpaca.defaultToolbarSticky = true;
    $("#form1").alpaca({
      "schema": {
        "$schema":"http://json-schema.org/draft-07/schema#",
        "title":"",
        "type":"array",
        "minItems":1,
        "items":{
          "type":"object",
          "title":"Add new command line parameter",
          "additionalProperties":false,
          "properties":{
              "gmsParam": {
                "title":"Which parameter would you like to customize?",
                "type":"string",
                "minLength":1,
                "required":true
              },
              "paramType": {
                "title":"What type of command line parameter is it?",
                "type":"string",
                "enum":["Double dash parameter", "Gams option"],
                "default": "Double dash parameter",
                "required":true
              },
              "widgetType":{
                "title":"What type of input widget do you want?",
                "type":"string",
                "enum":["slider","dropdown","date selector","date range selector","checkbox"],
                "default":"slider",
                "required":true
              },
              "slider":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for slider",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "slidertype":{
                    "title":"Do you want a standard slider (single value) or a slider range (min and max values)?",
                    "type":"string",
                    "enum":["standard", "slider range"],
                    "default":"standard",
                    "required":true
                  },
                  "minDepSel":{
                    "title":"Do you want the minimum slider value to be dependent on another dataset?",
                    "type":"string",
                    "enum":["dependent","static"],
                    "default":"static",
                    "required":true
                  },
                  "min":{
                    "title":"Minimum value",
                    "type":"number",
                    "required":true
                  },
                  "minDep":{
                    "type":"object",
                    "additionalProperties":false,
                    "properties":{
                      "minop":{
                        "title":"Operator to use in order to summarize set elements to single value",
                        "type":"string",
                        "enum":["minimum","maximum","variance","standard deviation","median","mean","count"],
                        "default":["minimum"],
                        "required":true
                      },
                      "minpar":{
                        "title":"Parameter to depend on",
                        "type":"string",
                        "minLength":1,
                        "required":true
                      },
                      "minhdr":{
                        "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                        "type":"string",
                        "minLength":1,
                        "required":true
                      }
                    }
                  },
                  "maxDepSel":{
                    "title":"Do you want the maximum slider value to be dependent on another dataset?",
                    "type":"string",
                    "enum":["dependent","static"],
                    "default":"static",
                    "required":true
                  },
                  "max":{
                    "title":"Maximum value",
                    "type":"number",
                    "required":true
                  },
                  "maxDep":{
                    "type":"object",
                    "additionalProperties":false,
                    "properties":{
                      "maxop":{
                        "title":"Operator to use in order to summarize set elements to single value",
                        "type":"string",
                        "enum":["minimum","maximum","variance","standard deviation","median","mean","count"],
                        "default":["minimum"],
                        "required":true
                      },
                      "maxpar":{
                        "title":"Parameter to depend on",
                        "type":"string",
                        "minLength":1,
                        "required":true
                      },
                      "maxhdr":{
                        "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                        "type":"string",
                        "minLength":1,
                        "required":true
                      }
                    }
                  },
                  "defDepSel":{
                    "title":"Do you want the default slider value to be dependent on another dataset?",
                    "type":"string",
                    "enum":["dependent","static"],
                    "default":"static",
                    "required":true
                  },
                  "defaultmin":{
                    "title":"Default/starting value (lower end of range)",
                    "type": "number",
                    "required":true
                  },
                  "defaultmax":{
                    "title":"Default/starting value (upper end of range)",
                    "type": "number",
                    "required":true
                  },
                  "default":{
                    "title":"Default/starting value",
                    "type": "number",
                    "required":true
                  },
                  "defDep":{
                    "type":"object",
                    "additionalProperties":false,
                    "properties":{
                      "defop":{
                        "title":"Operator to use in order to summarize set elements to single value",
                        "type":"string",
                        "enum":["minimum","maximum","variance","standard deviation","median","mean","count"],
                        "default":["minimum"],
                        "required":true
                      },
                      "defpar":{
                        "title":"Parameter to depend on",
                        "type":"string",
                        "minLength":1,
                        "required":true
                      },
                      "defhdr":{
                        "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                        "type":"string",
                        "minLength":1,
                        "required":true
                      }
                    }
                  },
                  "step":{
                    "title":"Specify the interval between each selectable value",
                    "type":"number",
                    "required":true
                  },
                  "width":{
                    "title":"Width of the slider (optional)",
                    "type":"string",
                    "required":false
                  },
                  "ticks":{
                    "title":"Do you want to show tick marks on the slider?",
                    "type":"boolean",
                    "default":true,
                    "required":false
                  }
                },
                "dependencies":{
                  "min":["minDepSel"],
                  "minDep":["minDepSel"],
                  "max":["maxDepSel"],
                  "maxDep":["maxDepSel"],
                  "defDepSel":["slidertype"],
                  "default":["defDepSel"],
                  "defDep":["defDepSel"],
                  "defaultmin":["slidertype"],
                  "defaultmax":["slidertype"],
                }
              },
              "dropdown":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for dropdown menu",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "choices":{
                    "title":"List of values to select from. This is the text that will be passed on to GAMS.",
                    "type":"array",
                    "minLength":1,
                    "minItems":1,
                    "required":true,
                    "items":{
                      "type":"object",
                      "additionalProperties":false,
                      "properties":{
                        "choiceDepSel":{
                          "title":"Should item be dependent on another dataset or static?",
                          "type": "string",
                          "enum":["dependent","static"],
                          "default":"static",
                          "required":true
                        },
                        "choice":{
                          "type":"string",
                          "title":"Enter new choice",
                          "required": true,
                          "minLength":1
                        },
                        "choiceDep":{
                          "type":"object",
                          "additionalProperties":false,
                          "properties":{
                            "choiceDepType":{
                              "title":"Do you want to filter the dependent table based on the selection you made, fill the dropdown menu based on the elements in the dependent table or both?",
                              "type": "string",
                              "enum":["filter table","fill with values","both"],
                              "default":"both",
                              "required":true
                            },
                            "choicepar":{
                              "title":"Parameter to depend on",
                              "type":"string",
                              "minLength":1,
                              "required":true
                            },
                            "choicehdr":{
                              "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                              "type":"string",
                              "minLength":1,
                              "required":true
                            }
                          }
                        }
                      },
                      "dependencies":{
                        "choice":["choiceDepSel"],
                        "choiceDep":["choiceDepSel"]
                      }
                    }
                  },
                  "aliases":{
                    "type":"array",
                    "title":"Aliases for the elements (length must match the number of choices you entered or left empty). This is the text the user will see in the UI.  In case no aliases are defined, the text displayed in the UI is the same as the one for choices.",
                    "required":false,
                    "items":{
                      "type":"object",
                      "additionalProperties":false,
                      "properties":{
                        "aliasDepSel":{
                          "title":"Should item be dependent on another dataset or static?",
                          "type": "string",
                          "enum":["dependent","static"],
                          "default":"static",
                          "required":true
                        },
                        "alias":{
                          "type":"string",
                          "title":"Enter the element name",
                          "required":true,
                          "minLength":1
                        },
                        "aliasDep":{
                          "type":"object",
                          "additionalProperties":false,
                          "properties":{
                            "aliasDepType":{
                              "title":"Do you want to filter the dependent table based on the selection you made, fill the dropdown menu based on the elements in the dependent table or both?",
                              "type": "string",
                              "enum":["filter table","fill with values","both"],
                              "default":"both",
                              "required":true
                            },
                            "aliaspar":{
                              "title":"Parameter to depend on",
                              "type":"string",
                              "minLength":1,
                              "required":true
                            },
                            "aliashdr":{
                              "title":"Set to depend on (in case Parameter is not 'None', only set elements from this parameter are used)",
                              "type":"string",
                              "minLength":1,
                              "required":true
                            }
                          }
                        }
                      },
                      "dependencies":{
                        "alias":["aliasDepSel"],
                        "aliasDep":["aliasDepSel"]
                      }
                    }
                  },
                  "multiple":{
                    "title":"Are multiple choices allowed to be selected?",
                    "type":"boolean",
                    "required":false
                  },
                  "choiceMandatory":{
                    "title":"Is it mandatory to select an option in the dropdown menu?",
                    "type":"boolean",
                    "required":false
                  },
                  "selected":{
                    "title":"The initially selected value",
                    "type":"string",
                    "required":false
                  }
                },
                "dependencies":{
                  "choiceMandatory":["multiple"]
                }
              },
              "date":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for date selector",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "value":{
                    "title":"initial start date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "min":{
                    "title":"minimum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "max":{
                    "title":"maximum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "format":{
                    "title":"format of the date to display in the browser",
                    "type":"string",
                    "minLength":1,
                    "maxLength":10,
                    "default":"yyyy-mm-dd",
                    "required":false
                  },
                  "startview":{
                    "title":"date range shown when the input object is first clicked",
                    "type":"string",
                    "enum":[
                      "month",
                      "year",
                      "decade"
                    ],
                    "default":"month",
                    "required":true
                  },
                  "weekstart":{
                    "title":"Which day is displayed to be the first of the week (0=Sunday, 6=Saturday)?",
                    "type":"integer",
                    "minimum":0,
                    "maximum":6,
                    "required":false
                  },
                  "width":{
                    "title":"width of the input",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  }
                }
              },
              "daterange":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for date range selector",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "start":{
                    "title":"initial start date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "end":{
                    "title":"initial end date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "min":{
                    "title":"minimum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "max":{
                    "title":"maximum allowed date",
                    "type":"string",
                    "minLength":10,
                    "maxLength":10,
                    "required":false
                  },
                  "format":{
                    "title":"format of the date to display in the browser.",
                    "type":"string",
                    "minLength":1,
                    "maxLength":10,
                    "default":"yyyy-mm-dd",
                    "required":false
                  },
                  "startview":{
                    "title":"date range shown when the input object is first clicked",
                    "type":"string",
                    "enum":[
                      "month",
                      "year",
                      "decade"
                    ],
                    "default":"month",
                    "required":true
                  },
                  "weekstart":{
                    "title":"Which day is displayed to be the first of the week (0=Sunday, 6=Saturday)?",
                    "type":"integer",
                    "minimum":0,
                    "maximum":6,
                    "default":0,
                    "required":false
                  },
                  "separator":{
                    "title":"String to display between the start and end input boxes",
                    "type":"string",
                    "minLength":1,
                    "default":" to ",
                    "required":false
                  },
                  "width":{
                    "title":"width of the input",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "autoclose":{
                    "title":"Should the datepicker be closed automatically once a date is selected?",
                    "type":"boolean",
                    "required":false
                  }
                }
              },
              "checkbox":{
                "type":"object",
                "additionalProperties":false,
                "properties":{
                  "label":{
                    "title":"Descriptive text for checkbox",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "value":{
                    "title":"initial value",
                    "type":"boolean",
                    "required":false
                  },
                  "class":{
                    "title":"css class",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  },
                  "width":{
                    "title":"width of the input",
                    "type":"string",
                    "minLength":1,
                    "required":false
                  }
                }
              },
              "noBatch":{
                "title":"Should element be excluded from batch mode (only relevant for batch module)?",
                "type":"boolean",
                "required":false
              }
            },
            "dependencies":{
              "dropdown":["widgetType"],
              "slider":["widgetType"],
              "date":["widgetType"],
              "daterange":["widgetType"],
              "checkbox":["widgetType"],
              "noBatch":["widgetType"]
            }
          }},
          "options":{
             "fields":{
               "item":{
                 "fields":{
                "dropdown":{
                  "dependencies":{
                     "widgetType": "dropdown"
                  },
                  "fields":{
                    "choiceMandatory":{
                      "dependencies":{
                        "multiple": false
                      }
                    },
                    "choices":{
                      "fields":{
                        "item":{
                          "fields":{
                            "choice":{
                              "dependencies":{
                                "choiceDepSel": "static"
                              }
                            },
                            "choiceDep":{
                              "dependencies":{
                                "choiceDepSel":"dependent"
                              }
                            }
                          }
                        }
                      }
                    },
                    "aliases":{
                      "fields":{
                        "item":{
                          "fields":{
                            "alias":{
                              "dependencies":{
                                "aliasDepSel": "static"
                              }
                            },
                            "aliasDep":{
                              "dependencies":{
                                "aliasDepSel": "dependent"
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                },
                "slider":{
                  "dependencies":{
                     "widgetType": "slider"
                  },
                  "fields":{
                    "min":{
                      "dependencies":{
                        "minDepSel": "static"
                      }
                    },
                    "minDep":{
                      "dependencies":{
                        "minDepSel":"dependent"
                      }
                    },
                    "max":{
                      "dependencies":{
                        "maxDepSel": "static"
                      }
                    },
                    "maxDep":{
                      "dependencies":{
                        "maxDepSel": "dependent"
                      }
                    },
                    "defDepSel":{
                      "dependencies":{
                        "slidertype": "standard"
                      }
                    },
                    "defaultmin":{
                      "dependencies":{
                        "slidertype": "slider range"
                      }
                    },
                    "defaultmax":{
                      "dependencies":{
                        "slidertype": "slider range"
                      }
                    },
                    "default":{
                      "dependencies":{
                        "defDepSel": "static"
                      }
                    },
                    "defDep":{
                      "dependencies":{
                        "defDepSel": "dependent"
                      }
                    }
                  }
                },
                "date":{
                  "dependencies":{
                     "widgetType": "date selector"
                  },
                  "fields":{
                    "value":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "min":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "max":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    }
                  }
                },
                "daterange":{
                  "dependencies":{
                     "widgetType": "date range selector"
                  },
                  "fields":{
                    "start":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "end":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "min":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    },
                    "max":{
                      "type":"date",
                      "dateFormat": "YYYY-MM-DD",
                      "manualEntry": true
                    }
                  }
                },
                "checkbox":{
                  "dependencies":{
                     "widgetType": "checkbox"
                  }
                },
                "noBatch":{
                  "dependencies":{
                     "widgetType": ["checkbox","slider","dropdown","date selector","date range selector"]
                  }
                }
              }}},
              "form":{
                "buttons":{
                   "submit":{
                      "title":"Copy JSON to clipboard",
                      "click":function(){
                        let coTmp = this.getValue();
                        let coFull = new Object();
                        for(let objIdx in coTmp){
                          let co = coTmp[objIdx];
                          switch(co.widgetType) {
                            case "slider":
                              function addOp(opName, sliderVal){
                                let sliderValOp;
                                switch(opName){
                                  case "count":
                                    sliderValOp = "card(" + sliderVal + ")";
                                  break;
                                  case "maximum":
                                    sliderValOp = "max(" + sliderVal + ")";
                                  break;
                                  case "mean":
                                    sliderValOp = "mean(" + sliderVal + ")";
                                  break;
                                  case "median":
                                    sliderValOp = "median(" + sliderVal + ")";
                                  break;
                                  case "minimum":
                                    sliderValOp = "min(" + sliderVal + ")";
                                  break;
                                  case "standard deviation":
                                    sliderValOp = "sd(" + sliderVal + ")";
                                  break;
                                  case "variance":
                                    sliderValOp = "var(" + sliderVal + ")";
                                  break;
                                }
                                return sliderValOp;
                              }
                              if(co.slider.minDepSel === "dependent"){
                                let sliderVal = "";
                                if($.inArray("minpar", Object.keys(co.slider.minDep)) !== -1){
                                  sliderVal = co.slider.minDep.minpar + "$";
                                }

                                sliderVal = sliderVal + co.slider.minDep.minhdr;
                                sliderVal = addOp(co.slider.minDep.minop, sliderVal);
                                co.slider.min = sliderVal;
                                delete co.slider.minDep;
                              }
                              delete co.slider.minDepSel;

                              if(co.slider.maxDepSel === "dependent"){
                                let sliderVal = "";
                                if($.inArray("maxpar", Object.keys(co.slider.maxDep)) !== -1){
                                  sliderVal = co.slider.maxDep.maxpar + "$";
                                }
                                sliderVal = sliderVal + co.slider.maxDep.maxhdr;
                                sliderVal = addOp(co.slider.maxDep.maxop, sliderVal);
                                co.slider.max = sliderVal;
                                delete co.slider.maxDep;
                              }
                              delete co.slider.maxDepSel;

                              if(co.slider.defDepSel === "dependent"){
                                let sliderVal = "";
                                if($.inArray("defpar", Object.keys(co.slider.defDep)) !== -1){
                                  sliderVal = co.slider.defDep.defpar + "$";
                                }
                                sliderVal = sliderVal + co.slider.defDep.defhdr;
                                sliderVal = addOp(co.slider.defDep.defop, sliderVal);
                                co.slider.default = sliderVal;
                                delete co.slider.defDep;
                              }
                              delete co.slider.defDepSel;
                              if(co.slider.slidertype === "slider range"){
                                co.slider.default = [co.slider.defaultmin, co.slider.defaultmax];
                                delete co.slider.defaultmin;
                                delete co.slider.defaultmax;
                              }
                              delete co.slider.slidertype;
                              // get rid of default values as this makes JSON string shorter
                              if(co.slider.ticks){
                                delete co.slider.ticks;
                              }
                            break;
                            case "dropdown":
                              if(co.dropdown.aliases.length !== 0 && co.dropdown.choices.length !== co.dropdown.aliases.length){
                                alert("The number of choices does not match the number of aliases! Please make sure you enter as many aliases as you entered choices.");
                                return;
                              }
                              let choicesR = co.dropdown.choices;
                              let choices = [];
                              for(let i=0;i<choicesR.length;i++){
                                if(choicesR[i].choiceDepSel === "dependent"){
                                  let choice = "";
                                  if($.inArray("choicepar", Object.keys(choicesR[i].choiceDep)) !== -1){
                                    choice = choicesR[i].choiceDep.choicepar + "$";
                                  }
                                  choice = choice + choicesR[i].choiceDep.choicehdr;
                                  switch(choicesR[i].choiceDep.choiceDepType){
                                    case "fill with values":
                                      choice = "$" + choice;
                                    break;
                                    case "filter table":
                                      choice = choice + "$";
                                    break;
                                    case "both":
                                      choice = "$" + choice + "$";
                                    break;
                                  }
                                  choices.push(choice);
                                }else{
                                  choices.push(choicesR[i].choice);
                                }
                              }
                              if(!co.dropdown.choiceMandatory){
                                // push underscore _ as its a special symbol indicating that nothing was selected in dropdown
                                choices.push("_");
                              }
                              co.dropdown.choices = choices;
                              choicesR = co.dropdown.aliases;
                              let aliases = [];
                              for(let i=0;i<choicesR.length;i++){
                                if(choicesR[i].aliasDepSel === "dependent"){
                                  let alias = "";
                                  if($.inArray("aliaspar", Object.keys(choicesR[i].aliasDep)) !== -1){
                                    alias = choicesR[i].aliasDep.aliaspar + "$";
                                  }
                                  alias = alias + choicesR[i].aliasDep.aliashdr;
                                  switch(choicesR[i].aliasDep.aliasDepType){
                                    case "fill with values":
                                      alias = "$" + alias;
                                    break;
                                    case "filter table":
                                      alias = alias + "$";
                                    break;
                                    case "both":
                                      alias = "$" + alias + "$";
                                    break;
                                  }
                                  aliases.push(alias);
                                }else{
                                  aliases.push(choicesR[i].alias);
                                }
                              }
                              if(aliases.length){
                                if(!co.dropdown.choiceMandatory){
                                  aliases.push("_");
                                }
                                co.dropdown.aliases = aliases;
                              }else{
                                delete co.dropdown.aliases;
                              }
                              delete co.dropdown.choiceMandatory
                            break;
                            case "date selector":
                              // get rid of default values as this makes JSON string shorter
                              if(co.date.format === "yyyy-mm-dd"){
                                delete co.date.format;
                              }
                              if(co.date.startview === "month"){
                                delete co.date.startview;
                              }
                              if($.isEmptyObject(co.date)){
                                delete co.date;
                              }
                            break;
                            case "date range selector":
                              // get rid of default values as this makes JSON string shorter
                              if(co.daterange.format === "yyyy-mm-dd"){
                                delete co.daterange.format;
                              }
                              if(co.daterange.startview === "month"){
                                delete co.daterange.startview;
                              }
                              if(co.daterange.weekstart === 0){
                                delete co.daterange.weekstart;
                              }
                              if(co.daterange.separator === " to "){
                                delete co.daterange.separator;
                              }
                              if(co.daterange.autoclose === false){
                                delete co.daterange.autoclose;
                              }
                              if($.isEmptyObject(co.daterange)){
                                delete co.daterange;
                              }
                            break;
                            case "checkbox":
                              if(co.checkbox.value){
                                co.checkbox.value = 1;
                              }else{
                                co.checkbox.value = 0;
                              }
                            break;
                            default:
                              if(co.readOnlyCols.length){
                                let roCols = new Object();
                                for(let i=0;i<co.readOnlyCols.length;i++){
                                  roCols[co.readOnlyCols[i].colName] = new Object();
                                  roCols[co.readOnlyCols[i].colName].readonly = true;
                                }
                                co.headers = roCols;
                              }
                              delete co.readOnlyCols;
                              // get rid of default values as this makes JSON string shorter
                              if(!co.noImport){
                                delete co.noImport;
                              }
                              if(!co.readonly){
                                delete co.readonly;
                              }
                              if(!co.sharedData){
                                delete co.sharedData;
                              }
                          }
                          if(!co.noBatch){
                            delete co.noBatch;
                          }
                          delete co.widgetType;
                          let coGmsParam = "";
                          if(co.paramType === "Gams option"){
                            coGmsParam = "GMSOPT_" + co.gmsParam;
                          }else{
                            coGmsParam = "GMSPAR_" + co.gmsParam;
                          }
                          delete co.paramType;
                          delete co.gmsParam;
                          if($.isEmptyObject(co)){
                            continue
                          }
                          coFull[coGmsParam] = co;
                        } // END for loop

                        // copy and paste idea from: https://stackoverflow.com/questions/400212/how-do-i-copy-to-the-clipboard-in-javascript
                        $("#cpTA-wrapper").show();
                        let cpTA = $("#cpTA");
                        cpTA.val("$onecho > webuiconf.json\n" + JSON.stringify(coFull,null,"  ") + "\n$offecho");
                        cpTA.focus();
                        cpTA.select();
                        let err = false;
                        try {
                          let successful = document.execCommand("copy");
                          if(!successful){
                            err = true;
                          }
                        } catch (err) {
                          err = true;
                        }
                        if(err){
                          alert("Copying the text to clipboard was not successful. Please copy manually.")
                          // don't remove textarea if copying failed
                          return;
                        }else{
                          $("#cpTA-wrapper").hide();
                          alert("Your cutomization options were copied to your clipboard. Please paste them at the end of the explanatory text of the parameter you just customized.");
                        }
                      }
                   }
                }
              }
            }
    });
}
