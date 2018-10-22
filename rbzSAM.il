/*============================================================*
*	SKILL Application Manager (SAM)
*
*	Purpose:	The SKILL Application Manager (SAM), is a new
*				tool for Cadence Virtuoso that assists SKILL
*				programmers in software development. This tool
*				allows a very user-friendly organization and
*				execution of source code.
*				To start the SAM after loading this code file
*				to the Virtuoso Command Interpreter Window (CIW)
*				you can use the command (rbzSAMcore )
*				or the user customization in the .saminit file.
*
*	Author:		Florian Leber, Tobias Wolfer
*
*	Revision:	1.0		(26.04.2017)
*
*	Copyright 2017 Robert-Bosch-Center Reutlingen (Germany)
*	
*	Permission is hereby granted, free of charge, to any person obtaining a copy of this software
*	and associated documentation files (the "Software"), to deal in the Software without restriction,
*	including without limitation the rights to use, copy, modify, merge, publish, distribute,
*	sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
*	furnished to do so, subject to the following conditions:
*	
*	The above copyright notice and this permission notice shall be included in all copies or substantial
*	portions of the Software.
*	
*	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
*	NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
*	IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
*	WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
*	SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*=============================================================*/

/*============================================================*
Description:
	This function returns the string AboutrbzSAM, witch is
	accessible in the SAM Help menu "About rbz Skill App Manager.
*=============================================================*/
(defun rbzSAMStringAboutrbzSAM ()
	(strcat	"The SKILL Application Manager (SAM), is a new tool for Cadence Virtuoso "
			"that assists SKILL programmers in software development. This tool allows "
			"a very user-friendly organization and execution of source code. Using SAM "
			"the manual effort is reduced and many errors are avoided. Additionally, "
			"its powerful GUI allows a much more comfortable way of developing userware "
			"than the usage of command lines. We successfully applied SAM on our practical "
			"programming courses at Reutlingen University.")
)
/*============================================================*
Description:
	This function creates in the DPL data (here SAM->data) some
	symbols (also DPLs). The data structure gets created and
	initialized.
*=============================================================*/
(defun rbzSAMinitializeData (data)						; data is a DPL
	(let (rbzsaconfigFile)
		(eval data)->config = (list 'nil)
		(eval data)->config->workspacePath = (simplifyFilename "./skill")	; here: (simplifyFilename "./skill") => "/home/.../skill"
		(eval data)->config->import = "gtar -xzkf"							
		(eval data)->config->export = "gtar -czf"							
		(eval data)->config->editor = "gedit"
		(eval data)->config->filebrowser = "thunar"								
		(eval data)->config->editfile = ""
		(eval data)->config->notAskForWorkspace = t							
		
		(eval data)->currentApp = 'nil										
		(eval data)->selectedRun = 'nil										
		(eval data)->appTable = (makeTable "app" 'nil)
	)
)
/*============================================================*
Description:
	This function creates (here in SAM->data->appTable) a
	association table, ther elements / key are the names
	of the relevant project directorys in the workspace. The
	values of these keys are the DPLs, read from the
	corresponding .sap-File.
*=============================================================*/
( defun rbzSAMupdateAppDataList (data)
	(let (path nameList)
		path = (eval data)->config->workspacePath	; SAM->data->config->workspacePath looks like "/home/..lalala../skill")
		nameList = (rbzSAMreadWorkspace path)		; rbzSAMreadWorkspace creates a list of the relevant project directorys in the workspace
		(eval data)->currentApp = 'nil				; nil means, no project is selected
   		(eval data)->selectedRun = 'nil				; nil means, no run is selected
    	(eval data)->appTable = (makeTable "app" 'nil)	; overwrite appTable (AT with one element/key: app = nil)
		(foreach name nameList						; for each element (relevant project directorys) do...
		    (eval data)->appTable[name] = (rbzSAMreadSa name path) )
		(if (eval data)->currentApp == 'nil 		
			(eval data)->currentApp = (car nameList) )	
		(if (eval data)->selectedRun == 'nil
			(eval data)->selectedRun = (eval data)->appTable[(car nameList)]->runDefault  )	
	)
)
/*============================================================*
Description:
	This function creates a list of the relevant project
	directorys in the workspace (relevant = ther is a .sap-file).
*=============================================================*/
( defun rbzSAMreadWorkspace (sPathToWorkspace)	;this function gets SAM->data->config->workspacePath
	(let (sPath aFiles) 
		sPath = (simplifyFilename sPathToWorkspace)
		(if (isDir sPath)
			then
			aFiles = ( getDirFiles sPath )											; read files and directorys from path
			aFiles = ( setof fl aFiles fl!="." && fl!=".." )						; "." and ".." become sorted out
			aFiles = ( setof fl aFiles (isDir (strcat sPath "/" fl) ) )				; sort out, what is no directory
			aFiles = ( setof fl aFiles (isFile (strcat sPath "/" fl "/.sap" ) ) )	; sort out, what contains no .sap-file
		) 
	) 
)
/*============================================================*
Description:
	The .sap-file in workspace/project_directory/ gets read. The
	return value is a DPL with:
		name 		= name of the project directory (as string)
		path 		= workspace/project_directory/ (as string)
		runTable	= Association Table (Keys are the DPL elements out of the .sap-File)
							(here: 'description, 'runList und 'runDefault)
		ilFileList	= list of all .il-files as string: "path/FileName.il"
		ilsFileList	= list of all .ils-files as string: "path/FileName.ils"
*=============================================================*/
( defun rbzSAMreadSa (name path)	; (project_directory workspace)
	(let (returnVar SpFolder)
		SpFolder = (strcat path "/" name)
		returnVar = (rbzSAMevalFile (strcat SpFolder "/.sap") )
			;	returnVar	=>	(dpl:	description "string"
			;							runList (dpl:	"string" (dpl:	preEval "string" postEval "string" eval "string")
			;											"string" (dpl:	preEval "string" postEval "string" eval "string")
			;											...)
			;							runDefault "string")
		returnVar->runTable = (listToTable 'runTable returnVar->runList)
		returnVar->name = name		
		returnVar->path = SpFolder	
		returnVar->ilFileList = (rbzSAMgetIlFiles SpFolder)		; list of all .il-Files as string like "path/FileName.il"
		returnVar->ilsFileList = (rbzSAMgetIlsFiles SpFolder)	; list of all .il-Files as string like "path/FileName.il"
		returnVar->libraryList = (rbzSAMgetLibrarys SpFolder)	; list of all Librarys as string like "path/LibName"
		returnVar ) )
/*============================================================*
Description:
	This function reads the .samconfig-file in the workspace
	and returns its contents as DPL.
*=============================================================*/
( defun rbzSAMreadSaConfig (path)	; (workspace)
	(let (sFileContent oPort sNextline)
		sFileContent = ""
		oPort = infile( (strcat path "/.samconfig") )
		(if (eq oPort nil) then
			sFileContent = "nil"
		else
			( while
				(gets sNextline oPort)
				sFileContent = ( strcat sFileContent sNextline )
			)
		)
		(evalstring sFileContent)
	) 
)
/*============================================================*
Description:
	This function reads the configuration data from the
	.samconfig-file and applys these settings.
*=============================================================*/
( defun rbzSAMupdateconfig (window data)
	(let (rbzsaconfigFile)
		rbzsaconfigFile = (rbzSAMreadSaConfig ".")
		(if (neq rbzsaconfigFile->editor nil) then
			(eval data)->config->editor = rbzsaconfigFile->editor
		)
		(if (neq rbzsaconfigFile->filebrowser nil) then
			(eval data)->config->filebrowser = rbzsaconfigFile->filebrowser
		)
		(if (neq rbzsaconfigFile->workspacePath nil) then
			(eval data)->config->workspacePath = rbzsaconfigFile->workspacePath
		)
		(eval data)->config->notAskForWorkspace = rbzsaconfigFile->notAskForWorkspace
	)
)
/*============================================================*
Description:
	This function opens a port and reads the file in the given
	path as a string. Return value is the file content.
*=============================================================*/
( defun rbzSAMevalFile (sFilePath)
	( let (oPort sNextline sFileContent)
		sFileContent = ""
		oPort = infile( sFilePath )
		( while
    		(gets sNextline oPort)
			sFileContent = ( strcat sFileContent sNextline ) )
		(evalstring sFileContent)
	)
)
/*============================================================*
Description:
	This function returns a list of all .il-files in the given
	path. The elements look like "path/FileName.il".
	Example:	"/home/..lalala../skill/ProjektA/MyCode.il"
*=============================================================*/
( defun rbzSAMgetIlFiles (sPath)
	(let (aFiles aIlFiles /*aIlsFiles*/)
		aFiles = ( getDirFiles sPath )
		aFiles = ( setof fl aFiles (isFile strcat(sPath "/" fl)))			; sort out all directorys
		aIlFiles = ( setof fl aFiles (rexMatchp "^[a-zA-Z].*\\.il$" fl)) 	; sort out everything, that is no .il-file
		aIlFiles = ( sort aIlFiles nil )									; alphabetical sort
		( mapcar (lambda (x) (strcat sPath "/" x)  ) aIlFiles )				; add the path to the names of the .il-files
	)
)
/*============================================================*
Description:
	This function returns a list of all .ils-files in the given
	path. The elements look like "path/FileName.ils".
	Example:	"/home/..lalala../skill/ProjektA/MyCode.ils"
*=============================================================*/
( defun rbzSAMgetIlsFiles (sPath)
	(let (aFiles /*aIlFiles*/ aIlsFiles)
		aFiles = ( getDirFiles sPath )
		aFiles = ( setof fl aFiles (isFile strcat(sPath "/" fl)))			; sort out all directorys
		aIlsFiles = ( setof fl aFiles (rexMatchp "^[a-zA-Z].*\\.ils$" fl))	; sort out everything, that is no .ils-file
		aIlsFiles = ( sort aIlsFiles nil )									; alphabetical sort
		( mapcar (lambda (x) (strcat sPath "/" x)  ) aIlsFiles )			; add the path to the names of the .ils-files
	)
)
/*============================================================*
Description:
	This function returns a list of all Librarys in the given
	path. The elements look like "path/LibraryName".
	Example:	"/home/..lalala../skill/ProjektA/myLib"
*=============================================================*/
( defun rbzSAMgetLibrarys (sPath)
	(let (aFiles Librarys)
		aFiles = ( getDirFiles sPath )
		aFiles = ( setof fl aFiles (isDir (strcat sPath "/" fl) ) )					; sort out, what is no directory
		Librarys = ( setof fl aFiles (isFile (strcat sPath "/" fl "/.oalib" ) ) )	; sort out, what contains no .oalib-file
		Librarys = ( sort Librarys nil )											; alphabetical sort
		(mapcar (lambda (x) (strcat sPath "/" x)  ) Librarys )						; add the path to the names of the .ils-files
	)
)
/*============================================================*
Description:
	This function loads all .il-files in the given project
	name with the command (load ...) in the CIW.
*=============================================================*/
( defun rbzSAMloadIlFiles (data name)
	( mapcar 'load (eval data)->appTable[name]->ilFileList)
)
/*============================================================*
Description:
	This function loads all .ils-files in the given project
	name with the command (load ...) in the CIW.
*=============================================================*/
( defun rbzSAMloadIlsFiles (data name)
	( mapcar 'load (eval data)->appTable[name]->ilsFileList)
)
/*============================================================*
Description:
	This function evaluates the strings in preEval, eval and
	postEval in this order in the CIW with (stringeval ...)
	The return value is a list, containing the return values of 
	the three evaluations.
*=============================================================*/
( defun rbzSAMrunApp (data name runName)	; data = SAM->data	; name = name of project directory (= key in AT appTable)	; runName = name of the selected run
	(let ( theRun preRet ret postRet)
		theRun = (eval data)->appTable[name]->runTable[runName]
		preRet = (evalstring theRun->preEval)				
		ret = (evalstring theRun->eval)		
		postRet = (evalstring theRun->postEval) 
		(list preRet ret postRet)
	)
)	
		
/*============================================================*
Description:
	This function evaluates the .il-files and .ils-files in the
	selected project directory in the CIW. Then it evaluates
	the strings in preEval, eval and postEval of the selected
	run in this order in the CIW.
	All returns and results become printed in the CIW.
*=============================================================*/
( defun rbzSAMloadAndRunCurrentApp (data)
	(let (name runName returnVar)
		name = (eval data)->currentApp
		(if name then
			runName = (eval data)->selectedRun

			(printf "\n***** rbzSAM - Load - Name:\"%s\" Run:\"%s\"\n" name (rbzSAMnilToString runName) )
			(rbzSAMloadIlFiles data name)
			(rbzSAMloadIlsFiles data name)

			(if runName then
				(printf "***** rbzSAM - Run - Name:\"%s\" Run:\"%s\"\n" name runName )
				returnVar = (rbzSAMrunApp data name runName)
				(printf "***** rbzSAM - Return (preRun Run postRun) : \"%L\"" returnVar )
			else
				(printf "***** rbzSAM - Run not possible, no run data available!")
			)
		else
			(printf "No current application selected!")
		)
			
	) 
)
/*============================================================*
Description:
	This function writes the selected Value of the Combo-Box
	"Load and Run" to the data structure data->selectedRun.
*=============================================================*/
( defun rbzSAMupdateSelectedRunFromRunCombo (window data)
	( let (tempValue)
		tempValue = (eval window)->vToolbar->xRunCombo->value
		(if tempValue == "nil" then
		    (eval data)->selectedRun = nil
		else
			(eval data)->selectedRun = tempValue
		);if
	)
	
)
/*============================================================*
Description:
	This function updates the elements of the Combo-Box "Load
	and Run" (xRunCombo) depending on, witch project is
	selected (here SAM->data->selectetApp).
*=============================================================*/
( defun rbzSAMupdateRunComboFromCurrentApp (window data)
	( let (name tempItems tempValue)
		name = (eval data)->currentApp	
		tempItems = (reverse (eval data)->appTable[name]->runTable->?)
		(eval window)->vToolbar->xRunCombo->items = (sort tempItems nil)

		tempValue = (eval data)->appTable[name]->runDefault
		(eval window)->vToolbar->xRunCombo->value = (rbzSAMnilToString tempValue)
		;(printf "Debug: rbzSAMupdateRunComboFromCurrentApp Name: %A runDefault: %A\n" name (rbzSAMnilToString tempValue))
		;(printf "DEBUG: combo items = %L" (eval window)->vToolbar->xRunCombo->items)
	)
)
/*============================================================*
Description:
	This function writes the selected project (the name of the
	project directory) to the input box  "Current Application: "
	(in the argument value). The input box  "Current Application: "
	(xDisplayApp) is displayed in the toolbar "Display"
	(xDisplayApp).
*=============================================================*/
( defun rbzSAMupdateToolbarDisplayApp (window data)
	(if (eval data)->currentApp then
		(eval window)->vToolbar->xDisplayApp->value = (eval data)->currentApp
	else
		(eval window)->vToolbar->xDisplayApp->value = ""
	)
)
/*============================================================*
Description:
	This function updates the selection "Current Application"
	(currentApp) and also the GUI.
*=============================================================*/
(defun rbzSAMupdateCurrentApp (window data name)
    (eval data)->currentApp = name
    (rbzSAMupdateToolbarDisplayApp window data)
    (rbzSAMupdateRunComboFromCurrentApp window data)
)
/*============================================================*
Description:
	This function creates a callback function for double clicking
	to a element in the tree form.
*=============================================================*/
(defun rbzSAMgenerateTreeDoubleClickCallback (window data) 
	( let (functionSymbol functionAsString)
		functionSymbol = (gensym 'rbzSAMtreeDoubleClickCallback)
		functionAsString = ( strcat 
			"(defun " functionSymbol " (formOrRegion field item) "
				"(if (or (rexMatchp \"^[a-zA-Z].*\\\\.il$\" (car (eval item)->_description)) "
				"(rexMatchp \"^[a-zA-Z].*\\\\.ils$\" (car (eval item)->_description)) ) "
				"then "
					"(eval '" data ")->config->editfile = (nth 2 (eval item)->_description) "
					"(rbzSAMopenFileInEditor '" window " '" data ") "
				"else "
					"(if (eq nil (isFile (strcat (nth 2 (eval item)->_description) \"/.oalib\" ) ) ) then "
						"(rbzSAMupdateCurrentApp '" window " '" data " (car (eval item)->_description)) "
					")"
				") "
			") "
		)
		(evalstring functionAsString)
		functionSymbol
	)
)
/*============================================================*
Description:
	This function opens the file in editfile with the shell
	command in editor for editing in an external editor.
*=============================================================*/
( defun rbzSAMopenFileInEditor (window data)
	(system (strcat (eval data)->config->editor " " (eval data)->config->editfile " &"))
)
/*============================================================*
Description:
	This function opens the file in editfile with the shell
	command in editor for editing in an external editor.
*=============================================================*/
( defun rbzSAMopenFileBrowser (window data path)
	(system (strcat (eval data)->config->filebrowser " " path " &"))
)
/*============================================================*
Description:
	This function calls all update functions to update the SAM.
*=============================================================*/
( defun rbzSAMrefreshAllApp (window data) 
	( rbzSAMupdateAppDataList data )					; Funktion erstellt (hier in SAM->data->appTable) eine AT, die als Keys die Namen der Proj.verz. hat (Value zu den Keys sind aus .sap-File)
	( rbzSAMupdateFromAppTree window data )				; @Tobias ...komentieren!
	( rbzSAMupdateToolbarDisplayApp window data )		; Aktualisieren des Eingabefelds "Current Application: " (xDisplayApp) fr die neue selektedApp
	( rbzSAMupdateRunComboFromCurrentApp window data )	; Aktualisieren der Combo-Box "Load and Run" (xRunCombo) fr die neue selektedApp
	;(rbzSAMupdateconfig window data)
)

/*============================================================*
Description:
	This function updates the form app-tree (oAppTree) by
	removing and recreating its items. The items become created
	out of the association table SAM->data->appTable.
*=============================================================*/
( defun rbzSAMupdateFromAppTree (window data) 
	( hiTreeRemoveAllItems (eval window)->vForm->oAppTree )
	(let (ocurrentTreeItem)
		(foreach oneItem (eval data)->appTable->?
			; Create main tree item
			ocurrentTreeItem =	(hiCreateTreeItem
									( gensym 'rbzSAMtreeItem )
									(list oneItem (eval data)->appTable[oneItem]->description (eval data)->appTable[oneItem]->path)
								)
			; Add sub tree to main tree item
			(hiItemInsertTree
				ocurrentTreeItem
				(hiCreateTree	'itemroot)
			)
			; Add sub tree items to sub tree	=> il-Files
			(foreach subItem (eval data)->appTable[oneItem]->ilFileList
				(hiTreeAppendItem
					itemroot
					(hiCreateTreeItem
						( gensym 'rbzSAMtreeItem )
						(list (substring (rindex subItem "/") 2) "SKILL Code File" subItem)	; Items of the Tree Table are Name, Discription, Path
					)
				)
			)
			; Add sub tree items to sub tree	=> ils-Files
			(foreach subItem (eval data)->appTable[oneItem]->ilsFileList
				(hiTreeAppendItem
					itemroot
					(hiCreateTreeItem
						( gensym 'rbzSAMtreeItem )
						(list (substring (rindex subItem "/") 2) "SKILL Code File" subItem)	; Items of the Tree Table are Name, Discription, Path
					)
				)
			)
			; Add sub tree items to sub tree	=> Librarys
			(foreach subItem (eval data)->appTable[oneItem]->libraryList
				(hiTreeAppendItem
					itemroot
					(hiCreateTreeItem
						( gensym 'rbzSAMtreeItem )
						(list (substring (rindex subItem "/") 2) "Library" subItem)	; Items of the Tree Table are Name, Discription, Path
					)
				)
			)
			; Add main tree item to main tree
			(hiTreeAppendItem 	
				(eval window)->vForm->oAppTree	
				ocurrentTreeItem
			)
		)
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMexportApp ( data name ) 
	( let (command)
		command = ( strcat (eval data)->config->export " " name ".sam -C " (eval data)->config->workspacePath " " name "/ " )
		;( printf "\n***** rbzSAM - Export %s \nDebug: %s\n" name command )
		( shell command )
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMimportApp	( data file )
	( let (command)
		command = ( strcat (eval data)->config->import " " file " -C " (eval data)->config->workspacePath )
		;( printf "\n***** rbzSAM - Import %s \nDebug: %s\n" file command )
		( shell command )
	)
)
;===============================================================================================================;
;===============================================================================================================;

/*============================================================*
Description:
	This function is the main function of the Skill Application
	Manager. Calling this function in the CIW starts the SAM.
*=============================================================*/
( defun rbzSAMcore ()
	( let ( ( SAM ( list nil ) ) name )	
		SAM->window = (gensym 'rbzSAMwindow)
		(set SAM->window (list 'nil))

		SAM->data = (gensym 'rbzSAMdata)
		(set SAM->data (list 'nil))

		(rbzSAMinitializeData SAM->data)
		(rbzSAMupdateconfig SAM->window SAM->data)
		(rbzSAMupdateAppDataList SAM->data)
		
		rbzSAMinitializeWindow( SAM->window SAM->data )
		
		rbzSAMaskForSwitchWorkspace( SAM->window SAM->data )
		
		(rbzSAMupdateRunComboFromCurrentApp SAM->window SAM->data)
		(rbzSAMupdateToolbarDisplayApp SAM->window SAM->data )
		;(rbzSAMceckLibIntegrationToVirtuoso window data)
		(list SAM->window SAM->data)
	)
)

;===============================================================================================================;
;===============================================================================================================;

/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMinitializeWindow (window data)
	( let ()
		;=======================================================
		;Generate GUI
		;=======================================================
		(eval window)->oSessionWindow = ( hiCreateWindow nil "session" "rbz Skill App Manager" )
		
		;=======================================================
		;Action
		;=======================================================
		/*	Die Actions fr die Icons in den verschiedenen
			Toolbars werden erzeugt.
		*/
		(eval window)->vAction = (list nil)
		(eval window)->vAction->newApp = ( hiCreateAction	; Button "New"
				?name ( gensym 'rbzSAMaction )
				?iconText "New App"
				?callback ( strcat "(rbzSAMCreateNewAppForm '" window " '" data  ")")
		)
		(eval window)->vAction->importApp = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )
				?iconText "Import"
				?callback ( strcat "(rbzSAMimportForm '" window " '" data  ")" )
		)
		(eval window)->vAction->exportApp = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )
				?iconText "Export"
				?callback ( strcat "(rbzSAMexportForm '" window " '" data  ")" )
		)
		(eval window)->vAction->refreshAllApp = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )
				?iconText "Refresh All"
				?callback ( strcat "(rbzSAMrefreshAllApp '" window " '" data  ")" )
		)
		(eval window)->vAction->loadAndRun = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )		
				?iconText "Load and Run"
				?callback (strcat "(rbzSAMloadAndRunCurrentApp '" data " )")
				;?enabled nil 
		)
		(eval window)->vAction->runConfigurations = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )	
				?iconText "Edit Run"	
				?callback ( strcat "(rbzSAMRunConfigurationForm '" window " '" data  ")" )
		)

		;=======================================================
		;Toolbar
		;=======================================================
		/*	Create the different toolbars (and add the created actions
			to them) and put the toolbars in the session window.
			Beim erstellen der Toobars werden folgende Name verwendet:
				xXY...	= Handle variable of the elements of a toobar
				aXY...	= List of the actions/elements of a toolbar
				oXY...	= Handle variable of a toolbar
		*/
		(eval window)->vToolbar = (list nil)
		;====================================
		(eval window)->vToolbar->aFileItem = ( list	
			(eval window)->vAction->newApp	
			hiCreateToolbarSeparator(?name ( gensym 'rbzSAMtoolbarSeparator ))
			(eval window)->vAction->importApp
			(eval window)->vAction->exportApp
		)
		(eval window)->vToolbar->oFile = ( hiCreateToolbar
			?name ( gensym 'rbzSAMtoolbar )	
			?title "File"
			?items (eval window)->vToolbar->aFileItem
		)
		
		(eval window)->vAction->displayFileToolbar = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )
				?iconText "File"
				?callback (strcat "hiShowToolbar(" window "->vToolbar->oFile)")
		)
		( hiPlaceToolbar (eval window)->oSessionWindow (eval window)->vToolbar->oFile)
		;====================================
		(eval window)->vToolbar->aAppItem = ( list
			(eval window)->vAction->refreshAllApp
		)
		(eval window)->vToolbar->oApp = ( hiCreateToolbar
			?name ( gensym 'rbzSAMtoolbar )
			?title "App"
			?items (eval window)->vToolbar->aAppItem
		)
		(eval window)->vAction->displayAppToolbar = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )
				?iconText "App"
				?callback (strcat "hiShowToolbar(" window "->vToolbar->oApp)")
		)
		( hiPlaceToolbar (eval window)->oSessionWindow (eval window)->vToolbar->oApp)
		;====================================
		(eval window)->vToolbar->xRunCombo = ( hiCreateToolbarComboBox
			?name 'comboItem
			?value "nil"
			?items (list "nil")
			?editable nil
			?width 120
			?callback (strcat "( rbzSAMupdateSelectedRunFromRunCombo '" window " '" data " )")
		)
		(eval window)->vToolbar->aRunItem = ( list
			(eval window)->vAction->loadAndRun
			(eval window)->vToolbar->xRunCombo
			(eval window)->vAction->runConfigurations
		)
		(eval window)->vToolbar->oRun = ( hiCreateToolbar
			?name ( gensym 'rbzSAMtoolbar )
			?title "Run"
			?items (eval window)->vToolbar->aRunItem
		)
		(eval window)->vAction->displayRunToolbar = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )
				?iconText "Run"
				?callback (strcat "hiShowToolbar(" window "->vToolbar->oRun)")
		)
		( hiPlaceToolbar (eval window)->oSessionWindow (eval window)->vToolbar->oRun)
		;====================================
		(eval window)->vToolbar->xDisplayApp = ( hiCreateToolbarTypein
			?name 'typein 
			?prompt "Current Application: "
			?value ""
			?editable nil
			?enabled nil
			?width 220
		)
		(eval window)->vToolbar->aDisplayItem = ( list
			(eval window)->vToolbar->xDisplayApp
		)				
		(eval window)->vToolbar->oDisplay = ( hiCreateToolbar
			?name ( gensym 'rbzSAMtoolbar )
			?title "Display"
			?items (eval window)->vToolbar->aDisplayItem
		)
		(eval window)->vAction->displayDisplayToolbar = ( hiCreateAction
				?name ( gensym 'rbzSAMaction )
				?iconText "Display"
				?callback (strcat "hiShowToolbar(" window "->vToolbar->oDisplay)")
		)
		( hiPlaceToolbar (eval window)->oSessionWindow (eval window)->vToolbar->oDisplay)

		;=======================================================
		;Menu: File
		;=======================================================
		/*	Create the Banner-Menu "File":
				File	>	New		>	New App
									>	New File
						>	Import
						>	Export
						>	Switch Workspace
						>	Exit
		*/
		(eval window)->vMenu = (list nil)
		; Create the items for the pulldown menu for the slider menu "New" im Pulldown-Menu "File":
		(eval window)->vMenu->aFileNewItem = (list
			(eval window)->vAction->newApp
			( hiCreateMenuItem
				?name ( gensym 'rbzSAMmenuItem )
				?itemText "New File"
				?callback ( strcat "(rbzSAMCreateNewFileForm '" window " '" data  ")" )	;"(printf \"Create new Code-File\" )"
			)
		)
		; Create the pulldown menu for the slider menu "New" in the pulldown menu "File":
		(eval window)->vMenu->aFileNew = (hiCreatePulldownMenu
			( gensym 'rbzSAMmenuItem )
			""
			(eval window)->vMenu->aFileNewItem
		)
		; Create the slider menues in the pulldown menue "File":
		(eval window)->vMenu->aFileItem = ( list
			(hiCreateSliderMenuItem
				?name ( gensym 'rbzSAMmenuItem )
				?itemText "New"
				?subMenu (eval window)->vMenu->aFileNew
			)
			( hiCreateSeparatorMenuItem	
				?name ( gensym 'rbzSAMmenuItem )
			)
			(eval window)->vAction->importApp	
			(eval window)->vAction->exportApp	
			( hiCreateSeparatorMenuItem			
				?name ( gensym 'rbzSAMmenuItem ) 
			)
			( hiCreateMenuItem					
				?name ( gensym 'rbzSAMmenuItem )
				?itemText "Switch &Workspace"
				?callback ( strcat "(rbzSAMSwitchWorkspaceForm '" window " '" data  ")" )
			)
			( hiCreateSeparatorMenuItem		
				?name ( gensym 'rbzSAMmenuItem )
			)
			( hiCreateMenuItem	
				?name ( gensym 'rbzSAMmenuItem )
				?itemText "&Exit"
				?callback (strcat "hiCloseWindow( " window "->oContentWindow )" )
			)
		)
		; Create the pulldown menu "file":
		(eval window)->vMenu->oFile = ( hiCreatePulldownMenu
			( gensym 'rbzSAMmenu )
			"&File"		
			(eval window)->vMenu->aFileItem
		)
		( hiInsertBannerMenu (eval window)->oSessionWindow (eval window)->vMenu->oFile 10)

		;=======================================================
		;Menu: App
		;=======================================================
		/*	Create the Banner-Menu "App":
				App		>	Refresh All
		*/
		(eval window)->vMenu->aAppItem = ( list	
			(eval window)->vAction->refreshAllApp
		)
		(eval window)->vMenu->oApp = ( hiCreatePulldownMenu	
			( gensym 'rbzSAMmenu )
			"&Apps"
			(eval window)->vMenu->aAppItem
		)
		( hiInsertBannerMenu (eval window)->oSessionWindow (eval window)->vMenu->oApp 10)

		;=======================================================
		;Menu: Run
		;=======================================================
		/*	Create the Banner-Menu "Run":
				Run		>	Load and Run
						>	Run Configurations...
		*/
		(eval window)->vMenu->aRunItem = ( list		
			(eval window)->vAction->loadAndRun		
			(eval window)->vAction->runConfigurations
		)
		(eval window)->vMenu->oRun = ( hiCreatePulldownMenu
			( gensym 'rbzSAMmenu )
			"&Run"
			(eval window)->vMenu->aRunItem
		)
		( hiInsertBannerMenu (eval window)->oSessionWindow (eval window)->vMenu->oRun 10)

		;=======================================================
		;Menu: Window
		;=======================================================
		/*	Create the Banner-Menu "Window":
				Window	>	Assistants	>	DockXY
						>	Toolbars	>	File
										>	App
										>	Run
		*/
		; Create the pulldown menu for the slider menu "Assistans" in the pulldown menu "Window":
		(eval window)->vMenu->aWindowAssistansItem = ( list
			( hiCreateMenuItem						
				?name ( gensym 'rbzSAMmenuItem )	
				?itemText "DockXY"					
				?callback "(println)( \"DockXY\" )" )
		)
		(eval window)->vMenu->aWindowAssistans =  ( hiCreatePulldownMenu
			( gensym 'rbzSAMassistansMenu )
			""
			(eval window)->vMenu->aWindowAssistansItem
		)
		; Create the pulldown menu for the slider menu "Toolbars" in the pulldown menu "Window":
		(eval window)->vMenu->aWindowToolbarsItem = ( list
			(eval window)->vAction->displayFileToolbar	
			(eval window)->vAction->displayAppToolbar	
			(eval window)->vAction->displayRunToolbar	
			(eval window)->vAction->displayDisplayToolbar
		)
		(eval window)->vMenu->aWindowToolbars =  ( hiCreatePulldownMenu 
			( gensym 'rbzSAMtoolbarsMenu )			
			""										
			(eval window)->vMenu->aWindowToolbarsItem
		)
		; Create the slider menus "Toolbars" and "Assistans" in the pulldown menu "Window":
		(eval window)->vMenu->aWindowItem = ( list				
			( hiCreateSliderMenuItem							
				?name ( gensym 'rbzSAMmenuItem )				
				?itemText "&Assistans"							
				?subMenu (eval window)->vMenu->aWindowAssistans )
			( hiCreateSliderMenuItem							
				?name ( gensym 'rbzSAMmenuItem )				
				?itemText "&Toolbars"							
				?subMenu (eval window)->vMenu->aWindowToolbars )
		)
		; Create the pulldown menu "Window":
		(eval window)->vMenu->oWindow = ( hiCreatePulldownMenu 	
			( gensym 'rbzSAMmenu )								
			"&Window"											
			(eval window)->vMenu->aWindowItem					
		)
		( hiInsertBannerMenu (eval window)->oSessionWindow (eval window)->vMenu->oWindow 10)	; Das Banner-Menu wird in (oWindow) "Window" wird im Session-Window platziert.
		;=======================================================
		;Menu: Options
		;=======================================================
		/*	Create the Banner-Menu "Options":
				Options	>	Editor Browser Config
		*/
		(eval window)->vMenu->aOptionsItem = ( list		
			( hiCreateMenuItem	
				?name ( gensym 'rbzSAMmenuItem )	
				?itemText "&Editor+Browser Config"		
				?callback (strcat "(rbzSAMEditorBrowserConfigForm '" window " '" data ")") )
			( hiCreateMenuItem
				?name ( gensym 'rbzSAMmenuItem )	
				?itemText "&Check Library Integration"	
				?callback (strcat "(rbzSAMceckLibIntegrationToVirtuoso '" window " '" data ")") )
	;		( hiCreateMenuItem	
	;			?name ( gensym 'rbzSAMmenuItem )		
	;			?itemText "&Other Option"			
	;			?callback "(printf \"Other Option\" )" )	
		)
		(eval window)->vMenu->oOptions = ( hiCreatePulldownMenu
			( gensym 'rbzSAMmenu )	
			"&Options"
			(eval window)->vMenu->aOptionsItem
		)
		( hiInsertBannerMenu (eval window)->oSessionWindow (eval window)->vMenu->oOptions 10)
		
		;=======================================================
		;Menu: Help
		;=======================================================
		/*	Ceate the Banner-Menu "Help":
				Help	>	User Guide
						>	About rbz Skill App Manager
		*/
		(eval window)->vMenu->aHelpItem = ( list	
			( hiCreateMenuItem					
				?name ( gensym 'rbzSAMmenuItem )	
				?itemText "&User Guide"		
				?callback "println( \"One\" )" )
			( hiCreateMenuItem										
				?name ( gensym 'rbzSAMmenuItem )							
				?itemText "&About rbz Skill App Manager"					
				?callback (strcat "(printf \"" (rbzSAMStringAboutrbzSAM ) "\" )") )	
			( hiCreateMenuItem										
				?name ( gensym 'rbzSAMmenuItem )
				?itemText "&About rbz"
				?callback "(rbzSAMopenrbzHomepage )" )
			( hiCreateSeparatorMenuItem ?name ( gensym 'rbzSAMmenuItem ) )
			; Help-Menu of CIW (nth 3 ...)						; (nth 9 (hiGetMenuItems...)) => Cadence Online Support
			(eval (nth 3 (hiGetBannerMenus hiGetCIWindow())))->dfiiOnline;(eval (nth 9 (hiGetMenuItems (eval (nth 3 (hiGetBannerMenus hiGetCIWindow()))))))
			; Help-Menu of CIW (nth 3 ...)						; (nth 11 (hiGetMenuItems...)) => Cadence Community
			(eval (nth 3 (hiGetBannerMenus hiGetCIWindow())))->dfiiCommunity;(eval (nth 11 (hiGetMenuItems (eval (nth 3 (hiGetBannerMenus hiGetCIWindow()))))))
			; Help-Menu of CIW (nth 3 ...)						; (nth 12 (hiGetMenuItems...)) => Cadence OS Platform Support
			(eval (nth 3 (hiGetBannerMenus hiGetCIWindow())))->dfiiPlatform;(eval (nth 12 (hiGetMenuItems (eval (nth 3 (hiGetBannerMenus hiGetCIWindow()))))))
			; Help-Menu of CIW (nth 3 ...)						; (nth 14 (hiGetMenuItems...)) => Cadence Home
			(eval (nth 3 (hiGetBannerMenus hiGetCIWindow())))->dfiiCadence;(eval (nth 14 (hiGetMenuItems (eval (nth 3 (hiGetBannerMenus hiGetCIWindow()))))))
			; Help-Menu of CIW (nth 3 ...)						; (nth 15 (hiGetMenuItems...)) => About Virtuoso
			(eval (nth 3 (hiGetBannerMenus hiGetCIWindow())))->dfiiAbout
		)
		(eval window)->vMenu->oHelp = ( hiCreatePulldownMenu
			( gensym 'rbzSAMmenu )
			"&Help"
			(eval window)->vMenu->aHelpItem
		)
		( hiInsertBannerMenu (eval window)->oSessionWindow (eval window)->vMenu->oHelp 10)

		;=======================================================
		;Form: App Tree
		;=======================================================
		(eval window)->vForm = (list nil)
		; Create main tree
		(eval window)->vForm->oAppTree = ( hiCreateTree 'root)
		(let (ocurrentTreeItem)
			(foreach oneItem (eval data)->appTable->?
				; Create main tree item
				ocurrentTreeItem =	(hiCreateTreeItem
										( gensym 'rbzSAMtreeItem )
										(list oneItem (eval data)->appTable[oneItem]->description (eval data)->appTable[oneItem]->path window 'app oneItem)
									)
				; Add sub tree to main tree item
				(hiItemInsertTree
					ocurrentTreeItem
					(hiCreateTree	'itemroot)
				)
				; Add sub tree items to sub tree	=> il-Files
				(foreach subItem (eval data)->appTable[oneItem]->ilFileList
					(hiTreeAppendItem
						itemroot
						(hiCreateTreeItem
							( gensym 'rbzSAMtreeItem )
							(list (substring (rindex subItem "/") 2) "SKILL Code File" subItem window 'file oneItem)	; Items der Tree Table sind Name, Discription, Path
						)
					)
				)
				; Add sub tree items to sub tree	=> ils-Files
				(foreach subItem (eval data)->appTable[oneItem]->ilsFileList
					(hiTreeAppendItem
						itemroot
						(hiCreateTreeItem
							( gensym 'rbzSAMtreeItem )
							(list (substring (rindex subItem "/") 2) "SKILL Code File" subItem window 'file oneItem)	; Items der Tree Table sind Name, Discription, Path
						)
					)
				)
				; Add sub tree items to sub tree	=> Librarys
				(foreach subItem (eval data)->appTable[oneItem]->libraryList
					(hiTreeAppendItem
						itemroot
						(hiCreateTreeItem
							( gensym 'rbzSAMtreeItem )
							(list (substring (rindex subItem "/") 2) "Library" subItem window 'lib oneItem)	; Items der Tree Table sind Name, Discription, Path
						)
					)
				)
				; Add main tree item to main tree
				(hiTreeAppendItem 	; This function adds a tree item to a tree
					(eval window)->vForm->oAppTree 
					ocurrentTreeItem
				)
			)
		)
		(eval window)->vForm->oAppTreeTable = ( hiCreateTreeTable
			?name 'treeField2
			?selectMode 'single
			?titleAlignment 'center
			?headers '(("Name" 200 left) ("Description" 300 left) ("Path" 330 left))
			?choice (eval window)->vForm->oAppTree 
		)
		(eval window)->vForm->oAppTreeDoubleClickCallback = ( rbzSAMgenerateTreeDoubleClickCallback window data)
		(eval window)->vForm->oAppTreeTable->hiDoubleClickCallback = (eval window)->vForm->oAppTreeDoubleClickCallback

		(eval window)->vForm->oAppTreeContextMenuItem = ( list		
			( hiCreateMenuItem	
				?name ( gensym 'rbzSAMmenuItem )	
				?itemText "Set Current Application"		
				?callback (strcat "(rbzSAMappTreeContextMenuCallbackSetCurrentApp '" window " '" data ")") )
			( hiCreateMenuItem	
				?name ( gensym 'rbzSAMmenuItem )	
				?itemText "Open Folder in File Browser"		
				?callback (strcat "(rbzSAMappTreeContextMenuCallbackOpenFolder '" window " '" data ")") )
			( hiCreateMenuItem
				?name ( gensym 'rbzSAMmenuItem )	
				?itemText "Open File in Editor"
				?callback (strcat "(rbzSAMappTreeContextMenuCallbackOpenFile '" window " '" data ")") )
		)
		(eval window)->vForm->oAppTreeContextMenu = ( hiCreateMenu
			( gensym 'rbzSAMmenu )
			"Example Menu"
			(eval window)->vForm->oAppTreeContextMenuItem
		)
		(eval window)->vForm->oAppTreeTable->hiContextMenu = (eval window)->vForm->oAppTreeContextMenu

		;Callback which invoked just before the context menu is displayed
		(eval window)->vForm->oAppTreeTable->hiShowContextMenuCallback='rbzSAMappTreeContextMenuCallbackShow

		(eval window)->oForm = ( hiCreateAppForm
			?name ( gensym 'rbzSAMtreeForm )
			?fields list(
				list((eval window)->vForm->oAppTreeTable  0:0 150:300 0 )
			)
			?initialSize (list 150 300 )
			?attachmentList (list 
				hicTopPositionSet|hicLeftPositionSet|hicRightPercentSet|hicBottomPercentSet 
			)
		)
		
		(eval window)->oContentWindow = (hiCreateWindow
			(list 100:100 950:450)
			"form"
			"rbz Skill Application Manager (SAM)"
			"noHelp" nil
			(eval window)->oForm
		)
		
		( hiDisplayWindow 
			(eval window)->oContentWindow
			(eval window)->oSessionWindow
		)
		(hiSetWindowIcon
			(eval window)->oContentWindow
			(hiLoadIconData (strcat rbzSAMilPath "rbzIcon.png") "png" )
		)
	)
)

/*============================================================*
Description:
	Callback function for Context Menu in oAppTree. Function
	set the actual selected row to the current application.
*=============================================================*/
(defun rbzSAMappTreeContextMenuCallbackSetCurrentApp (window data)
	(let (description)
		description = (eval window)->vForm->oAppTreeContextMenuCurrentDescription
		;(printf "Debug: SetCurrentApp: %A" (nth 5 description))
		(rbzSAMupdateCurrentApp window data (nth 5 description))
	);let    
);defun

/*============================================================*
Description:
	Callback function for Context Menu in oAppTree. Function
	opens the Folder of the current application in a file browser.
*=============================================================*/
(defun rbzSAMappTreeContextMenuCallbackOpenFolder (window data)
	(let (description app)
		description = (eval window)->vForm->oAppTreeContextMenuCurrentDescription
		app = (nth 5 description)
		;(printf "Debug: OpenFolder: %A" (eval data)->appTable[app]->path)
		(rbzSAMopenFileBrowser window data	(eval data)->appTable[app]->path)
	);let    
);defun

/*============================================================*
Description:
	Callback function for Context Menu in oAppTree. Function
	opens the current selected file in a editor.
*=============================================================*/
(defun rbzSAMappTreeContextMenuCallbackOpenFile (window data)
	(let (description)
		description = (eval window)->vForm->oAppTreeContextMenuCurrentDescription
		(if (equal (nth 4 description) 'file) then
			(eval data)->config->editfile = (nth 2 description)
			(rbzSAMopenFileInEditor window data)
		else
			printf("No file.")
		);if
	);let    
);defun

/*============================================================*
Description:
	Callback Function for the oAppTree which invoked just 
	before the context menu is displayed (right click).
*=============================================================*/
(defun rbzSAMappTreeContextMenuCallbackShow (menu form field item)
	(let (window)
		;(printf "rbzSAMappTreeContextMenuCallbackShow: %A" (eval item)->_description)
		window = (nth 3 (eval item)->_description)
		(eval window)->vForm->oAppTreeContextMenuCurrentDescription = (eval item)->_description
	);let    
);defun

/*============================================================*
Description:
	This function creates and opens a window, when rbzSAMcore
	is called. It asks for changing the workspace.
	Button:	- Yes	=>	Open "Switch Workspace"-Window
			- No	=>	Close Window
			- Do not ask again	=> Auswahl wird in
						data->config->NotAskForWorkspace
						vermerkt und im .samconfig gespeichert.
*=============================================================*/
(defun rbzSAMaskForSwitchWorkspace ( window data )
	(if (neq t (eval data)->config->notAskForWorkspace) then
		(eval window)->askForSwitchWorkspace = (list nil)
		
		(eval window)->askForSwitchWorkspace->filePathField = ( hiCreateStringField
			?name ( gensym 'rbzSAMaskForSwitchWorkspace )
			?prompt (strcat "Selected workspace directory: " (eval data)->config->workspacePath)
			?editable nil
		)
		(eval window)->askForSwitchWorkspace->questionField = ( hiCreateStringField
			?name ( gensym 'rbzSAMaskForSwitchWorkspace )
			?prompt "Do you want to change workspace directory?"
			?editable nil
		)
		(eval window)->askForSwitchWorkspace->yesButton = ( hiCreateFormButton
			?name ( gensym 'rbzSAMaskForSwitchWorkspace )
			?callback ( strcat	"(rbzSAMSwitchWorkspaceForm '" window " '" data ") "
								"(hiFormDone (eval window)->askForSwitchWorkspace->appForm)" )
			?buttonText "Yes"
		)
		(eval window)->askForSwitchWorkspace->noButton = ( hiCreateFormButton
			?name ( gensym 'rbzSAMaskForSwitchWorkspace )
			?callback "(hiFormDone (eval window)->askForSwitchWorkspace->appForm)"
			?buttonText "No"
		)
		(eval window)->askForSwitchWorkspace->DoNotAskAgainButton = ( hiCreateFormButton
			?name ( gensym 'rbzSAMaskForSwitchWorkspace )
			?callback ( strcat	"(eval data)->config->notAskForWorkspace = t "
								"(rbzSAMsaveSaConfigData window data) "
								"(hiFormDone (eval window)->askForSwitchWorkspace->appForm) " )
			?buttonText "Do not ask again"
		)
		(eval window)->askForSwitchWorkspace->appForm = ( hiCreateAppForm 
			?name  ( gensym 'rbzSAMaskForSwitchWorkspace )
			?formTitle "SAM: Change Workspace?"
			?dialogStyle 'modal
			?buttonLayout 'Empty
			?callback ""
			?fields (list
				;x_xpos : x_ypos x_width : x_height [x_promptBoxWidthOrTitleHeight]
				(list (eval window)->askForSwitchWorkspace->questionField 0:0 450:30 450)
				(list (eval window)->askForSwitchWorkspace->filePathField 0:40 450:30 450)
				(list (eval window)->askForSwitchWorkspace->noButton 370:80  80:22)
				(list (eval window)->askForSwitchWorkspace->yesButton 280:80  80:22)
				(list (eval window)->askForSwitchWorkspace->DoNotAskAgainButton 0:80  150:22)
			)
		)
		(hiDisplayForm (eval window)->askForSwitchWorkspace->appForm)
	)
)
/*============================================================*
Description:
	This function is called, when the user clicks on the banner
	menu Options->Editor Browser Config. It opens a window, where the 
	user can edit the terminal command for opening files in an
	external editor.
*=============================================================*/
( defun rbzSAMEditorBrowserConfigForm ( window data )
	(eval window)->EditorBrowserConfig = (list nil)
	(eval window)->EditorBrowserConfig->editorPathField = ( hiCreateStringField	
		?name ( gensym 'rbzSAMEditorBrowserConfigFormItem )
		?prompt "Select Editor (Terminal Command): "
		?defValue (eval data)->config->editor ; = "gedit"
		?editable t
	)
	(eval window)->EditorBrowserConfig->browserPathField = ( hiCreateStringField	
		?name ( gensym 'rbzSAMEditorBrowserConfigFormItem )
		?prompt "Select File Browser (Terminal Command): "
		?defValue (eval data)->config->filebrowser 
		?editable t
	)
	(eval window)->EditorBrowserConfig->appForm = ( hiCreateAppForm 
		?name  ( gensym 'rbzSAMEditorBrowserConfigFormItem )
		?formTitle "SAM: Editor+Browser Config"
		?dialogStyle 'modal
		?buttonLayout 'OKCancel
		?callback ( strcat "(rbzSAMEditorBrowserConfigFormCallback '" window " '" data  ")" )
		?fields (list ;x_xpos : x_ypos x_width : x_height [x_promptBoxWidthOrTitleHeight]
			(list (eval window)->EditorBrowserConfig->editorPathField 0:0 450:30 220 )
			(list (eval window)->EditorBrowserConfig->browserPathField 0:40 450:30 220 )
		)	
	)
	(hiDisplayForm (eval window)->EditorBrowserConfig->appForm)
)
/*============================================================*
Description:
	This is the callback function of rbzSAMEditorBrowserConfigForm,
	that is called by clicking the OK-Button.
*=============================================================*/
( defun rbzSAMEditorBrowserConfigFormCallback (window data) 
	( let (selectedEditor selectedBrowser)
		selectedEditor = (eval window)->EditorBrowserConfig->editorPathField->value
		selectedBrowser = (eval window)->EditorBrowserConfig->browserPathField->value
		(eval data)->config->editor = selectedEditor
		(eval data)->config->filebrowser = selectedBrowser
		(rbzSAMsaveSaConfigData window data)	; Save new config-data in .samconfig-File
	)
)

/*============================================================*
Description:
	This function creates the app form Switch Workspace with
	a string field and the Button (...) for opening a file
	dialog form.
*=============================================================*/
( defun rbzSAMSwitchWorkspaceForm ( window data )
	(eval window)->SwitchWorkspace = (list nil)
	(eval window)->SwitchWorkspace->filePathField = ( hiCreateStringField
		?name ( gensym 'rbzSAMSwitchWorkspaceFormItem )
		?prompt "Selected workspace directory:"
		?defValue ""
		?editable t
	)
	(eval window)->SwitchWorkspace->openFileDialog = ( hiCreateFormButton
		?name ( gensym 'rbzSAMSwitchWorkspaceFormItem )
		?callback ( strcat "(rbzSAMopenSwitchWorkspaceFileDialog '" window " '" data ")" )
		?buttonText "..."
	)
	(eval window)->SwitchWorkspace->appForm = ( hiCreateAppForm 
		?name  ( gensym 'rbzSAMSwitchWorkspaceFormItem )
		?formTitle "SAM: Switch Workspace"
		?dialogStyle 'modal
		?buttonLayout 'OKCancel
		?callback ( strcat "(rbzSAMSwitchWorkspaceFormCallback '" window " '" data  ")" )
		?fields (list
			(list (eval window)->SwitchWorkspace->filePathField 0:0 420:30 170)
			(list (eval window)->SwitchWorkspace->openFileDialog 425:5  80:22) )
	)
	(hiDisplayForm (eval window)->SwitchWorkspace->appForm)
)
/*============================================================*
Description:
	This function creates the form file dialog to select the
	new workspace directory. It is called as the callback
	function, when the button (...) in the app form switch
	workspace become clicked.
*=============================================================*/
( defun rbzSAMopenSwitchWorkspaceFileDialog (window data)
	( let (SwitchWorkspaceCallback selectedWorkspaceDir)
		SwitchWorkspaceCallback =  (rbzSAMgenerateSwitchWorkspaceFileDialogCallback window data )
		selectedWorkspaceDir = ( hiDisplayFileDialog
	        ?dialogName ( gensym 'rbzSAMSwitchWorkspaceFileDialog)
	        ?callback SwitchWorkspaceCallback
	        ?caption "Open Workspace Directory"
	        ?workingDir "."
	        ?filter ""
	        ?mode 'directoryOnly
	        ?modal t
	        ?userData 'userData
	    )
    )
)
/*============================================================*
Description:
	This is the callback functon for the "Choose" button in the
	file dialog to select the workspace directory. The function
	creates and evaluates a callback function as a string.
*=============================================================*/
(defun rbzSAMgenerateSwitchWorkspaceFileDialogCallback (window data) 
	( let (functionSymbol functionAsString) 
		functionSymbol = (gensym 'rbzSAMSwitchWorkspaceFileDialogCallback)
		functionAsString = ( strcat
			"(defun " functionSymbol " (dialog event userData \"&ss\") "
				"(let (sFolder sPathToFile) "
					"(when ( eq event 'done ) "
					"sFolder = ( simplifyFilename ( car ( hiFileDialogSelection dialog ) ) ) "
					"sPathToFile = ( strcat sFolder ( cadr ( hiFileDialogSelection dialog ) ) ) "
					"(get (eval '" window " )->SwitchWorkspace->appForm (eval '" window " )->SwitchWorkspace->filePathField->hiFieldSym )->value = sPathToFile "
					") "
				") "
			")"
		)
		( evalstring functionAsString )
		( symbolToString functionSymbol )
	)
)
/*============================================================*
Description:
	This function is called by clicking the button OK in the
	app form Switch workspace. 
*=============================================================*/
( defun rbzSAMSwitchWorkspaceFormCallback (window data) 
	( let (pathToWorkspaceDir rbzsaconfigFile)
		pathToWorkspaceDir = (eval window)->SwitchWorkspace->filePathField->value
		(printf "Switch Workspace Directory to: %s \n" pathToWorkspaceDir)
		(eval data)->config->workspacePath = pathToWorkspaceDir
		(rbzSAMsaveSaConfigData window data)	; Save new config-data in .samconfig-File
		(rbzSAMrefreshAllApp window data)
	)
)
/*============================================================*
Description:
	This function creates the app form "Create New File" with
	a string field "Directory", the string field "File Name" and
	the button (...) to open a file dialog.
*=============================================================*/
( defun rbzSAMCreateNewFileForm ( window data )
	(if (eval data)->currentApp then
	    (eval window)->CreateNewFile = (list nil)
		(eval window)->CreateNewFile->templateList = (list "-" "(defun..." "defun Hello World Example")
		(eval window)->CreateNewFile->descriptionFieldApp = ( hiCreateOutputStringField
			?name ( gensym 'rbzSAMCreateNewFileFormItem )
			?value "Add new file to App:"
		)
		(eval window)->CreateNewFile->comboFieldApp = ( hiCreateComboField
			?name ( gensym 'rbzSAMCreateNewFileFormItem )
			?items (eval data)->appTable->?
			?defValue (eval data)->currentApp
			?editable nil
		)
		(eval window)->CreateNewFile->fileName = ( hiCreateStringField
			?name ( gensym 'rbzSAMCreateNewFileFormItem )
			?prompt "File Name"
			?defValue "new.il"
			?editable t
		)
		(eval window)->CreateNewFile->descriptionFieldTemplate = ( hiCreateOutputStringField
			?name ( gensym 'rbzSAMCreateNewFileFormItem )
			?value "Templates:"
		)
		(eval window)->CreateNewFile->comboFieldTemplate = ( hiCreateComboField
			?name ( gensym 'rbzSAMCreateNewFileFormItem )
			?items (eval window)->CreateNewFile->templateList
			?defValue (car (eval window)->CreateNewFile->templateList)
			?callback (strcat "(rbzSAMCreateNewFileTemplateCallback '" window " '" data  ")")
			?editable nil
		)
		(eval window)->CreateNewFile->defunName = ( hiCreateStringField
			?name ( gensym 'rbzSAMCreateNewFileFormItem )
			?prompt "Function Name"	
			?defValue "myAppFunction"
			?editable t
		)
		(eval window)->CreateNewFile->appForm = ( hiCreateAppForm 
			?name  ( gensym 'rbzSAMCreateNewFileFormItem )
			?formTitle "SAM: Create New File"
			?dialogStyle 'modal
			?buttonLayout 'OKCancel
			?callback ( strcat "(rbzSAMCreateNewFileFormCallback '" window " '" data  ")" )
			?fields (list
				(list (eval window)->CreateNewFile->descriptionFieldApp			0:0		160:30)
				(list (eval window)->CreateNewFile->comboFieldApp				170:0	250:30)
				(list (eval window)->CreateNewFile->fileName					0:40	420:30 170)
				(list (eval window)->CreateNewFile->descriptionFieldTemplate	0:80	160:30)
				(list (eval window)->CreateNewFile->comboFieldTemplate			170:80	250:30)
			)
		)
		(hiDisplayForm (eval window)->CreateNewFile->appForm)
	else
		(printf "No current application selected!")
	);if
	
)
/*============================================================*
Description:
	This function is called by clicking the button OK in the 
	app form "Switch Workspace".
*=============================================================*/
( defun rbzSAMCreateNewFileFormCallback (window data) 
	( let (pathToNewFileDir fileName appName templateName defunName)
		fileName = (eval window)->CreateNewFile->fileName->value
		appName = (eval window)->CreateNewFile->comboFieldApp->value
		templateName = (eval window)->CreateNewFile->comboFieldTemplate->value
		defunName = (eval window)->CreateNewFile->defunName->value
		
		pathToNewFileDir	= (strcat (eval data)->config->workspacePath "/" appName )
		(printf "\nCreate New File in Directory: %s" pathToNewFileDir)
		(printf "\nSet the Name of to: %s" fileName)
		;(printf "\nDEBUG: (system %s) = %N"	; return = 0 => OK, 0 Errors.
		;	(strcat "touch " pathToNewFileDir "/" fileName " &")
		;	(system (strcat "touch " pathToNewFileDir "/" fileName " &"))			; create File
		;)
		(system (strcat "touch " pathToNewFileDir "/" fileName " &"))
		(system (strcat "touch " pathToNewFileDir "/" fileName " &"))
		; Add Template Data:
		(if (eq "(defun..." templateName) then
			(rbzSAMwriteTemplateDefunToFile (strcat pathToNewFileDir "/" fileName) defunName )
			(eval data)->appTable[appName]->runTable[defunName] = (list nil
				'preEval	""
				'postEval	""
				'eval		(strcat "(" defunName " )")
			)
			(eval data)->currentApp = appName
			(eval data)->appTable[(eval data)->currentApp]->runDefault = defunName
			(eval data)->selectedRun = defunName
			(eval window)->vToolbar->xRunCombo->value = defunName

			(rbzSAMupdateRunComboFromCurrentApp window data)
			(rbzSAMsaveAppData window data)	; update .sap-File from appTable
		)
		(if (eq "defun Hello World Example" templateName) then
			(rbzSAMwriteTemplateExampleToFile (strcat pathToNewFileDir "/" fileName))
			(eval data)->appTable[appName]->runTable["helloworldexample"] = (list nil
				'preEval	""
				'postEval	""
				'eval		"(helloworldexample )"
			)
			(eval data)->currentApp = appName
			(rbzSAMupdateRunComboFromCurrentApp window data)
			(rbzSAMsaveAppData window data)	; update .sap-File from appTable
		)
		(eval data)->config->editfile = (strcat pathToNewFileDir "/" fileName)	; create path to open in Editor
		(rbzSAMopenFileInEditor window data)									; open editfile in Editor
		(rbzSAMrefreshAllApp window data)
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMCreateNewFileTemplateCallback ( window data )
	;(printf "Debug: Select template\n")
	(if (eq "-" (eval window)->CreateNewFile->comboFieldTemplate->value ) then
		(hiSetFieldEnabled
			(eval window)->CreateNewFile->defunName
			nil
		)
	)
	(if (eq "(defun..." (eval window)->CreateNewFile->comboFieldTemplate->value ) then
		;(printf "AddField...: %N"
			(hiAddField
				(eval window)->CreateNewFile->appForm
				(list (eval window)->CreateNewFile->defunName	0:120	420:30 170)
			)
			(hiSetFieldEnabled
				(eval window)->CreateNewFile->defunName
				t
			)
		;)
	)
	(if (eq "defun Hello World Example" (eval window)->CreateNewFile->comboFieldTemplate->value ) then
		(hiSetFieldEnabled
			(eval window)->CreateNewFile->defunName
			nil
		)
	)
	
)
/*============================================================*
Description:
	This function creates the app form "Create New App" with
	the string field "Directory / Name", the string field
	"Discription" and the Button (...) to open a file dialog.
*=============================================================*/
( defun rbzSAMCreateNewAppForm ( window data )
	(eval window)->CreateNewApp = (list nil)
	(eval window)->CreateNewApp->projectPathField = ( hiCreateStringField
		?name ( gensym 'rbzSAMCreateNewAppFormItem )
		?prompt "Application Directory / Name:"
		?defValue "newApplication"
		?editable t
	)
	(eval window)->CreateNewApp->projectDiscription = ( hiCreateStringField
		?name ( gensym 'rbzSAMCreateNewFileFormItem )
		?prompt "Application Discription:"
		?defValue "Textual description"
		?editable t
	)
	(eval window)->CreateNewApp->appForm = ( hiCreateAppForm 
		?name  ( gensym 'rbzSAMCreateNewAppFormItem )
		?formTitle "SAM: Create New Application"
		?dialogStyle 'modal
		?buttonLayout 'OKCancel
		?callback ( strcat "(rbzSAMCreateNewAppFormCallback '" window " '" data  ")" )
		?fields (list
			(list (eval window)->CreateNewApp->projectPathField 0:0 420:30 175)
			(list (eval window)->CreateNewApp->projectDiscription 0:40 420:30 175)
		)
	)
	(hiDisplayForm (eval window)->CreateNewApp->appForm)
)
/*============================================================*
Description:
	This function is called by clicking the button "OK" in the
	app form "Create New App".
*=============================================================*/
( defun rbzSAMCreateNewAppFormCallback (window data) 
	( let (pathToNewProjectDir projectDiscription)
		pathToNewProjectDir	= (strcat (eval data)->config->workspacePath "/" (eval window)->CreateNewApp->projectPathField->value)
		projectDiscription	= (eval window)->CreateNewApp->projectDiscription->value
		;(printf "Create New App in Directory: %N" pathToNewProjectDir)
		;(printf "Set Project discription to: %N" projectDiscription)
		(system (strcat "mkdir " pathToNewProjectDir))			; create Directory
		(rbzSAMcreateStdSa pathToNewProjectDir projectDiscription)
		(rbzSAMrefreshAllApp window data)
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMrunConfigurationsForm (window data) 
	( let ()
		(printf "Open Run Configurations Form")
	)
)
/*============================================================*
Description:
	This function creates and opens a form, witch shows the Runs
	of the Current Application in a combobox. The Runs can
	become edited ("Edit"), deleted ("Delete") or supplemented
	("Add").
*=============================================================*/
( defun rbzSAMRunConfigurationForm ( window data )
	(if (eval data)->currentApp then
		;(if (eval data)->selectedRun then
			(eval window)->RunConfiguration = (list nil)
			(eval window)->RunConfiguration->descriptionField = ( hiCreateOutputStringField
				?name ( gensym 'rbzSAMRunConfigurationFormItem )
				?value (strcat "Run definitions of \"" (eval data)->currentApp "\":")
			)
			
			(eval window)->RunConfiguration->comboField = ( hiCreateComboField
				?name ( gensym 'rbzSAMRunConfigurationFormItem )
				?items (sort (eval data)->appTable[(eval data)->currentApp]->runTable->? nil)
				;?defValue "nil"
				;?defValue (rbzSAMnilToString (car (eval data)->appTable[(eval data)->currentApp]->runTable->?))
				?defValue (eval window)->vToolbar->xRunCombo->value
				?callback ""
				?editable nil
			)
			(eval window)->RunConfiguration->comboField->insertPolicy = 'InsertAtTop
		
			(eval window)->RunConfiguration->ButtonEdit = ( hiCreateFormButton
				?name ( gensym 'rbzSAMRunConfigurationFormItem )
				?callback ( strcat "(rbzSAMopenEditRunConfigurationForm '" window " '" data ")" )
				?buttonText "Edit"
				?enabled nil
			)
			(eval window)->RunConfiguration->ButtonDelete = ( hiCreateFormButton
				?name ( gensym 'rbzSAMRunConfigurationFormItem )
				?callback ( strcat "(rbzSAMDeleteRunConfigurationCallback '" window " '" data ")" )
				?buttonText "Delete"
				?enabled nil
			)
			(eval window)->RunConfiguration->ButtonAdd = ( hiCreateFormButton
				?name ( gensym 'rbzSAMRunConfigurationFormItem )
				?callback ( strcat "(rbzSAMopenAddRunConfigurationForm '" window " '" data ")" )
				?buttonText "Add"
			)
			(eval window)->RunConfiguration->appForm = ( hiCreateAppForm 
				?name ( gensym 'rbzSAMRunConfigurationFormItem )
				?formTitle "SAM: Edit Run"
				?dialogStyle 'modal
				?buttonLayout 'OKCancel
				?callback ( strcat "(rbzSAMRunConfigurationFormCallback '" window " '" data  ")" )
				?fields (list
					(list (eval window)->RunConfiguration->descriptionField 0:5  400:30)	;x_xpos : x_ypos x_width : x_height
					(list (eval window)->RunConfiguration->comboField	20:50	230:30)
					(list (eval window)->RunConfiguration->ButtonEdit	350:40 	80:20)
					(list (eval window)->RunConfiguration->ButtonDelete	350:65	80:20)
					(list (eval window)->RunConfiguration->ButtonAdd	350:90	80:20)
					)
			)
			(if (eval data)->appTable[(eval data)->currentApp]->runTable->? then
			    (eval window)->RunConfiguration->ButtonEdit->enabled = t
				(eval window)->RunConfiguration->ButtonDelete->enabled = t
			;else
			;	(printf "Debug Edit Run: runTable contains NO elements")
			);if
			
			(hiDisplayForm (eval window)->RunConfiguration->appForm)
			;(printf "Debug Edit Run: %A")
		;else
		;	(printf "No run data available!")
		;)
	else
		(printf "No current application selected!")
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMRunConfigurationFormCallback (window data) 
	( let (selectedRun)
		(printf "Save Run Configuration")
		;selectedRun = (eval window)->RunConfiguration->comboField->value
		(eval window)->vToolbar->xRunCombo->items = (sort (eval data)->appTable[(eval data)->currentApp]->runTable->? nil)
		;(eval window)->vToolbar->xRunCombo->value = (rbzSAMnilToString (car (eval data)->appTable[(eval data)->currentApp]->runTable->?))
		(rbzSAMsaveAppData window data)	; update .sap-File from appTable
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMopenEditRunConfigurationForm (window data) 
	(let (selectedRun currentRunTable)
		selectedRun = (eval window)->RunConfiguration->comboField->value;
		currentRunTable = (eval data)->appTable[(eval data)->currentApp]->runTable->?

		(if (member selectedRun currentRunTable) then
		    ;printf("Debug Edit Run Config: SelectedRun is part of current runTable")
		    (eval window)->EditRunConfiguration = (list nil)
			(eval window)->EditRunConfiguration->descriptionField = ( hiCreateOutputStringField
				?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
				?value (strcat "Edit Run \"" selectedRun "\" of App " (eval data)->currentApp ":")
			)
			(eval window)->EditRunConfiguration->RunNameField = ( hiCreateStringField
				?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
				?prompt "Run Name:"
				?defValue selectedRun
				?editable nil
			)
			(eval window)->EditRunConfiguration->PreEvalField = ( hiCreateStringField
				?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
				?prompt "Pre Eval:"
				?defValue (eval data)->appTable[(eval data)->currentApp]->runTable[selectedRun]->preEval
				?editable t
			)
			(eval window)->EditRunConfiguration->PostEvalField = ( hiCreateStringField
				?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
				?prompt "Post Eval:"
				?defValue (eval data)->appTable[(eval data)->currentApp]->runTable[selectedRun]->postEval
				?editable t
			)
			(eval window)->EditRunConfiguration->EvalField = ( hiCreateStringField
				?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
				?prompt "Eval:"
				?defValue (eval data)->appTable[(eval data)->currentApp]->runTable[selectedRun]->eval
				?editable t
			)
			(eval window)->EditRunConfiguration->appForm = ( hiCreateAppForm 
				?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
				?formTitle "SAM: Edit Run"
				?dialogStyle 'modal
				?buttonLayout 'OKCancel
				?callback ( strcat "(rbzSAMopenEditRunConfigurationFormCallback '" window " '" data  ")" )
				?fields (list
					(list (eval window)->EditRunConfiguration->descriptionField 0:5		400:30)
					(list (eval window)->EditRunConfiguration->RunNameField		20:40	400:25	60)
					(list (eval window)->EditRunConfiguration->PreEvalField		20:70	400:25	60)
					(list (eval window)->EditRunConfiguration->PostEvalField	20:100	400:25	60)
					(list (eval window)->EditRunConfiguration->EvalField		20:130	400:25	60)
					)
			)
			(hiDisplayForm (eval window)->EditRunConfiguration->appForm)
		else
			;printf("Debug Edit Run Config: SelectedRun is NOT part of current runTable")
			(printf "SelectedRun is NOT part of current runTable!")
		);if


	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMopenEditRunConfigurationFormCallback (window data)
	(let (selectedRun)
		selectedRun = (eval window)->RunConfiguration->comboField->value
		(eval data)->appTable[(eval data)->currentApp]->runTable[selectedRun]->preEval	= (eval window)->EditRunConfiguration->PreEvalField->value
		(eval data)->appTable[(eval data)->currentApp]->runTable[selectedRun]->postEval	= (eval window)->EditRunConfiguration->PostEvalField->value
		(eval data)->appTable[(eval data)->currentApp]->runTable[selectedRun]->eval	= (eval window)->EditRunConfiguration->EvalField->value
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMDeleteRunConfigurationCallback (window data) 
	(let (selectedRun currentRunTable)
		selectedRun = (eval window)->RunConfiguration->comboField->value;
		currentRunTable = (eval data)->appTable[(eval data)->currentApp]->runTable->?
		(if (member selectedRun currentRunTable) then
			(remove selectedRun (eval data)->appTable[(eval data)->currentApp]->runTable)

			(if selectedRun == (eval data)->appTable[(eval data)->currentApp]->runDefault then
				(eval data)->appTable[(eval data)->currentApp]->runDefault = (car (eval data)->appTable[(eval data)->currentApp]->runTable->?)
			);if

			(if selectedRun == (eval data)->selectedRun then
				(eval data)->selectedRun = (eval data)->appTable[(eval data)->currentApp]->runDefault
				(eval window)->vToolbar->xRunCombo->value = (rbzSAMnilToString (eval data)->selectedRun)
			)

			;(eval window)->RunConfiguration->comboField->items = (rbzSAMnilToString (reverse (eval data)->appTable[(eval data)->currentApp]->runTable->?))
			;(eval window)->RunConfiguration->comboField->value = (rbzSAMnilToString (car (eval data)->appTable[(eval data)->currentApp]->runTable->?))
			; Close and Open the Form to update the items of the Combobox
			(hiFormClose (eval window)->RunConfiguration->appForm)
			(rbzSAMRunConfigurationForm window	data)
		else
			;printf("Debug Edit Run Config: SelectedRun is NOT part of current runTable")
			(printf "SelectedRun is NOT part of current runTable!")
		);if
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMopenAddRunConfigurationForm (window data)
	(let ()
		(eval window)->AddRunConfiguration = (list nil)
		(eval window)->AddRunConfiguration->descriptionField = ( hiCreateOutputStringField
			?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
			?value (strcat "Add Run to App " (eval data)->currentApp ":")
		)
		(eval window)->AddRunConfiguration->RunNameField = ( hiCreateStringField
			?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
			?prompt "Run Name:"
			?defValue ""
			?editable t
		)
		(eval window)->AddRunConfiguration->PreEvalField = ( hiCreateStringField
			?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
			?prompt "Pre Eval:"
			?defValue ""
			?editable t
		)
		(eval window)->AddRunConfiguration->PostEvalField = ( hiCreateStringField
			?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
			?prompt "Post Eval:"
			?defValue ""
			?editable t
		)
		(eval window)->AddRunConfiguration->EvalField = ( hiCreateStringField
			?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
			?prompt "Eval:"
			?defValue ""
			?editable t
		)
		(eval window)->AddRunConfiguration->appForm = ( hiCreateAppForm 
			?name ( gensym 'rbzSAMEditRunConfigurationFormItem )
			?formTitle "SAM: Add Run"
			?dialogStyle 'modal
			?buttonLayout 'OKCancel
			?callback ( strcat "(rbzSAMopenAddRunConfigurationFormCallback '" window " '" data  ")" )
			?fields (list
				(list (eval window)->AddRunConfiguration->descriptionField 0:5  400:30)	;x_xpos : x_ypos x_width : x_height
				(list (eval window)->AddRunConfiguration->RunNameField	20:40	400:25	60)
				(list (eval window)->AddRunConfiguration->PreEvalField	20:70	400:25	60)
				(list (eval window)->AddRunConfiguration->PostEvalField	20:100	400:25	60)
				(list (eval window)->AddRunConfiguration->EvalField		20:130	400:25	60)
				)
		)
		(hiDisplayForm (eval window)->AddRunConfiguration->appForm)
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMopenAddRunConfigurationFormCallback (window data)
	(let (RunName)
		RunName = (eval window)->AddRunConfiguration->RunNameField->value;
		(if (rexMatchp "[-_A-Za-z0-9]+" RunName)  then
			(eval data)->appTable[(eval data)->currentApp]->runTable[RunName] = (list nil
				'preEval	(eval window)->AddRunConfiguration->PreEvalField->value
				'postEval	(eval window)->AddRunConfiguration->PostEvalField->value
				'eval		(eval window)->AddRunConfiguration->EvalField->value
			)
			(eval data)->appTable[(eval data)->currentApp]->runDefault = RunName
			(eval data)->selectedRun = RunName
			;(eval window)->RunConfiguration->comboField->value = RunName
			(eval window)->vToolbar->xRunCombo->value = RunName

		else
			(printf "\n*ERROR* Run Name is not valid.")
		)
		; Close and Open the Form to update the items of the Combobox
		(hiFormClose (eval window)->RunConfiguration->appForm)
		(rbzSAMRunConfigurationForm window	data)
	)
)
/*============================================================*
Description:
	This function creates a window, that gets opened by clicking
	"Export" in the menu "File".
*=============================================================*/
( defun rbzSAMexportForm ( window data )
	(if (eval data)->currentApp then
		(eval window)->export = (list nil)
		(eval window)->export->descriptionField = ( hiCreateOutputStringField
			?name ( gensym 'rbzSAMexportFormItem )
			?value "Select the App for export:"
		)

		(eval window)->export->comboField = ( hiCreateComboField
			?name ( gensym 'rbzSAMexportFormItem )
			?items (eval data)->appTable->?
			?defValue (eval data)->currentApp
			?editable nil
		)
		(eval window)->export->appForm = ( hiCreateAppForm 
			?name 'ExportForm
			?formTitle "SAM: Export App"
			?dialogStyle 'modal
			?buttonLayout 'OKCancel
			?callback ( strcat "(rbzSAMexportFormCallback '" window " '" data  ")" )
			?fields (list
				(list (eval window)->export->descriptionField 5:10  200:30)
				(list (eval window)->export->comboField 210:6  300:30) )
		)
		(hiDisplayForm (eval window)->export->appForm)
	else
		(printf "No current application selected!")
	)
)

/*============================================================*
Description:
	This is the callback function for clicking the callback
*=============================================================*/
( defun rbzSAMexportFormCallback (window data) 
	( let (name)
		name = (eval window)->export->comboField->value
		(when name != ""
			( rbzSAMexportApp data name ) 
		)
	)
)

/*============================================================*
Description:
	This function opens the import form to import a .sam-
	container file.
*=============================================================*/
( defun rbzSAMimportForm ( window data )
	(eval window)->import = (list nil)
	(eval window)->import->filePathField = ( hiCreateStringField
		?name ( gensym 'rbzSAMimportFormItem )
		?prompt "Selected SAM-File:"
		?defValue ""
		?editable t
	)
	(eval window)->import->openFileDialog = ( hiCreateFormButton
		?name ( gensym 'rbzSAMimportFormItem )
		?callback ( strcat "(rbzSAMopenImportFormFileDialog '" window " '" data ")" )
		?buttonText "..."
	)
	(eval window)->import->appForm = ( hiCreateAppForm 
		?name  ( gensym 'rbzSAMimportForm )
		?formTitle "SAM: Import App"
		?dialogStyle 'modal
		?buttonLayout 'OKCancel
		?callback ( strcat "(rbzSAMimportFormCallback '" window " '" data  ")" )
		?fields (list
			(list (eval window)->import->filePathField 0:0 420:30 120)
			(list (eval window)->import->openFileDialog 425:5  80:22) )
	)
	(hiDisplayForm (eval window)->import->appForm)
)

/*============================================================*
Description:
	This function opens the file dialog to select the .sam-file
	to import in the SAM.
*=============================================================*/
( defun rbzSAMopenImportFormFileDialog (window data)
	( let (callback)
		callback = ( rbzSAMgenerateImportFormFileDialogCallback window data )

		( hiDisplayFileDialog
	        ?dialogName (gensym 'rbzSAMimportFormFileDialog)
	        ?callback callback
	        ?caption "Open Existing File"
	        ?workingDir "."
	        ?filter "SAM-File (*.sam)"
	        ?mode 'existingFile
	        ?modal t
	        ?userData ( gensym 'rbzSAMimportFormFileDialogData )
	    )
    )
)

/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMimportFormCallback (window data) 
	( let (pathToFile)
		pathToFile = (eval window)->import->filePathField->value
		( rbzSAMimportApp data pathToFile )
		(rbzSAMrefreshAllApp window data)
		(rbzSAMceckLibIntegrationToVirtuoso window data)
	)
)

/*============================================================*
Description:
	--description--
*=============================================================*/
(defun rbzSAMgenerateImportFormFileDialogCallback (window data) 
	( let (functionSymbol functionAsString) 
		functionSymbol = (gensym 'rbzSAMimportFormFileDialogCallback)
		functionAsString = ( strcat
			"( defun " functionSymbol " (dialog event userData \"&ss\") "
				"( let (sFolder sPathToFile) "
					"( when ( eq event 'done ) "
					"sFolder = ( simplifyFilename ( car ( hiFileDialogSelection dialog ) ) ) "
					;"( printf \"\nDEBUG: %N\" sFolder) "
					"( if (neq 0 (strcmp \"/\" (substring sFolder (length sFolder)))) then "	; (if < last character in the string != "/"> then
					"	 sFolder = ( strcat sFolder \"/\" ) "						;		ergnzen von "/"
					") "
					"sPathToFile = ( strcat sFolder ( cadr ( hiFileDialogSelection dialog ) ) ) "
					;"( printf \"\nDEBUG: %N\" sPathToFile) "
					"(get (eval '" window " )->import->appForm (eval '" window " )->import->filePathField->hiFieldSym )->value = sPathToFile "
					") "
				") "
			")"
		)
		( evalstring functionAsString )
		( symbolToString functionSymbol )
	)
)
/*============================================================*
Description:
	This function creates a .samconfig-file with the current
	configuration data.
*=============================================================*/
( defun rbzSAMsaveSaConfigData (window data)
	(let (oPort path)
		path = (strcat (simplifyFilename ".") "/.samconfig")
		;printf("DEBUG: Save config data to %s" path )
		oPort = ( outfile path )
		(fprintf oPort "list( nil \n")
		(fprintf oPort "    'editor \"%s\" \n" (eval data)->config->editor)
		(fprintf oPort "    'filebrowser \"%s\" \n" (eval data)->config->filebrowser)
		(fprintf oPort "    'workspacePath \"%s\" \n" (eval data)->config->workspacePath)
		(fprintf oPort "    'notAskForWorkspace %N \n" (eval data)->config->notAskForWorkspace)
		(fprintf oPort ") \n")
		( close oPort )
	)
)
/*============================================================*
Description:
	This function overwrites the .sap-file of the currentApp to
	save the changed runTable and selected Run.
*=============================================================*/
( defun rbzSAMsaveAppData (window data)
	(let (oPort path)
		name = (eval data)->currentApp
		path = (eval data)->config->workspacePath
		path = (strcat path "/" name "/.sap")
		;printf("DEBUG: Save data to %s" path )
		oPort = ( outfile path )
		(fprintf oPort "list( nil \n")
		(fprintf oPort "    'description \"%s\" \n" (eval data)->appTable[name]->description)
		(fprintf oPort "\n")
		(fprintf oPort "    'runList list( \n")
		( foreach theRun ( reverse (eval data)->appTable[name]->runTable->? )
		    (fprintf oPort "        list( \"%s\" list( nil 'preEval %N 'postEval %N 'eval %N) ) \n" theRun (eval data)->appTable[name]->runTable[theRun]->preEval (eval data)->appTable[name]->runTable[theRun]->postEval (eval data)->appTable[name]->runTable[theRun]->eval )
			; Attention:	The runTable[...]->... contains strings, but here they become formated with %N (= any)
			;				while writing them to .sap-file. The reason is, that %N adds "" to the string automatically
			;				and existing "" become escaped with \"\" (and \"\" become escaped with \\\"\\\" etc.).
			;				This allows the file to be read back correctly.
		)
		(fprintf oPort "    ) \n")
		(fprintf oPort "\n")
		(fprintf oPort "    'runDefault \"%s\" \n" (rbzSAMnilToString (eval data)->selectedRun));(eval data)->appTable[name]->runDefault)
		(fprintf oPort ") \n")
		( close oPort )
	)
)
/*============================================================*
Description:
	This function creates a .sap-file with initial entries.
	The function gets the project directory name and the
	workspace directory path.
*=============================================================*/
( defun rbzSAMcreateStdSa (path discription)
	(let (oPort)
		oPort = (outfile	; Open Port
			(strcat path "/" "/.sap")
			"w"
		)
		(fprintf oPort "list( nil \n")
		(fprintf oPort (strcat "    'description \"" discription "\" \n"))
		(fprintf oPort "\n")
		(fprintf oPort "    'runList list( \n")
		;(fprintf oPort "        list( \"non\" list( nil 'preEval \"\" 'postEval \"\" 'eval \"\" ) ) \n")
		(fprintf oPort "    ) \n")
		(fprintf oPort "\n")
		;(fprintf oPort "    'runDefault \"non\" \n")
		(fprintf oPort "    'runDefault nil \n")
		(fprintf oPort ") \n")
		( close oPort )		; Close Port
	)
)
/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMtoggleAssistant (theNameSymbol)
	(let (theDockWindow theWindowState theReturnVar)
		theReturnVar = 'nil
		theDockWindow = ( dwindow theNameSymbol )
		theWindowState = (car (hiGetWindowDisplayState theDockWindow))
		(if (theWindowState == 'mapped)
			(hiUnmapWindow theDockWindow) 
			theReturnVar = 't)
		(if (theWindowState == 'unmapped)
			(hiMapWindow theDockWindow) 
			theReturnVar = 't) 
		theReturnVar ) )

/*============================================================*
Description:
	This function opens the Robert Bosch Center (rbz) website by
	executing a terminal command.
*=============================================================*/	
(defun rbzSAMopenrbzHomepage ()
	(system "xdg-open http://www.rbzentrum.de/home/?L=1 &")
)
/*============================================================*
Description:
	This function gets called by clicking the "Check Library
	Integration". It checks, wheter all Librarys in the workspace
	out of (eval data)->appTable[...]->libraryList are in the
	cds.lib file. Missing librarys in the cds.lib will become
	added in the end of the file.
*=============================================================*/
( defun rbzSAMceckLibIntegrationToVirtuoso (window data)
	(let (cdslibFile oPort sNextline cdsDefineList cdslibaryList notintegratedLibraryList)
		cdslibFile = ""
		cdsDefineList = (list )
		cdslibaryList = (list )
		notintegratedLibraryList = (list )
		oPort = (infile "./cds.lib")
		(if (neq oPort nil) then							; if cds.lib-File exists
			( while
				(gets sNextline oPort)
				cdslibFile = (strcat cdslibFile sNextline )	
				(if (eq 0 (strcmp "DEFINE" (substring sNextline 1 6))) then
					cdsDefineList = (append cdsDefineList (list (substring sNextline 1 (sub1 (length sNextline)))) )
				)
			)
			; Erstellen einer Liste der Librarys aus cds.lib-File die mit ...->libraryList verglichen werden kann
			(foreach definition cdsDefineList
				cdslibaryList = (append	; cdslibraryList contains: ("/home/.../libA" "/home/.../TestLIB" ...)
					cdslibaryList 
					(list (substring (substring definition 8) (add1 (nindex (substring definition 8) " "))))
					)
			)
			; Erzeugen einer Liste der Librarys die noch nicht im cds.lib-File stehen
			(foreach appName (eval data)->appTable->?
				(foreach libInApp (eval data)->appTable[appName]->libraryList
					(if (eq nil (lindex cdslibaryList libInApp)) then
						notintegratedLibraryList =  (append notintegratedLibraryList (list libInApp))
					)
				)
			)
			( close oPort )
			oPort = (outfile "./cds.lib")
			(fprintf oPort cdslibFile)
			(foreach libInApp notintegratedLibraryList
				(fprintf oPort (strcat "DEFINE " (substring (rindex libInApp "/") 2) " " libInApp "\n"))
			)
			( close oPort )
			(if (neq notintegratedLibraryList nil) then
				(printf "\n***** rbzSAM - Added the following Librarys to Virtuoso: \n")
				(foreach lib notintegratedLibraryList
					(printf "%s \n" lib)
				)
			else
				(printf "\n***** rbzSAM - All Librarys are integrated. \n")
			)
		)
		(rbzSAMceckLibIntegrationNameConflict )
	)	
)
/*============================================================*
Description:
	Due to adding Librarys to the project directory directly or
	by importing them it is possible, that name conflicts in the
	cds.lib could arise. In the case of multible entries for the
	same library name the Library Manager will take the path
	of the last entrie of the library name. Because of that it
	is possible, that the user will edit mistakenly the wrong
	library via the Library Manager. This function checks the
	cds.lib for name conflicts and prints a WARNING to the CIW.
*=============================================================*/
( defun rbzSAMceckLibIntegrationNameConflict ()
	(let (cdslibFile oPort sNextline cdsDefineList TEMPcdslibaryNameList TEMPcdslibaryName nameConflictList)
		cdslibFile = ""
		cdslibaryList = (list )
		cdslibaryNameList = (list )
		nameConflictList = (list )
		oPort = (infile "./cds.lib")
		(if (neq oPort nil) then	; if cds.lib-File exists
			( while
				(gets sNextline oPort)	; reads one line via the input port
				cdslibFile = (strcat cdslibFile sNextline )
				(if (eq 0 (strcmp "DEFINE" (substring sNextline 1 6))) then
				;	cdsDefineList contains a list of all DEFINE-lines in the cds.lib-File
					cdsDefineList = (append cdsDefineList (list (substring sNextline 1 (sub1 (length sNextline)))) )
				)
			)
			; Create a list of the librarys in the cds.lib-file, witch can become compared with the ...->libraryList
			(foreach definition cdsDefineList
				cdslibaryNameList = (append	; cdslibraryNameList contains: ("TestLIB" "OtherLib" ...)
					cdslibaryNameList 
					(list (substring (substring definition 8) 1 (sub1 (nindex (substring definition 8) " "))))
				)
			)
			TEMPcdslibaryNameList = cdslibaryNameList
			(foreach libraryName cdslibaryNameList
				TEMPcdslibaryName = (car TEMPcdslibaryNameList)	;...first element
				TEMPcdslibaryNameList = (cdr TEMPcdslibaryNameList)	;...the rest
				(if (neq nil (lindex TEMPcdslibaryNameList TEMPcdslibaryName)) then
					nameConflictList = (append
						nameConflictList
						(list TEMPcdslibaryName)
					)
				)
			)
			(if (neq nameConflictList nil) then
				(printf "\n***** rbzSAM - WARNING: \n")
				(printf "The following library names are used multible in the cds.lib-file. Virtuoso will use the path of the last definition ")
				(printf "in the cds.lib-file. Please change the library names and the entries in the cds.lib to avoid ")
				(printf "mistakes while editing the librarys with Virtuoso.\n\tLibrarys:\n")
				(foreach nameConflict nameConflictList
					(printf "\t\t%s\n" nameConflict)
				)
				(printf "***** \n")
			)
		)
	)
)

/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMwriteTemplateDefunToFile (filePath defunName)
	(let (codeFile oPort sNextline)
		codeFile = ""
		oPort = (infile filePath)
		(if (neq oPort nil) then	; if cds.lib-File exists
			( while
				(gets sNextline oPort)
				codeFile = (strcat codeFile sNextline )
			)
			( close oPort )
			oPort = (outfile filePath)
			(fprintf oPort codeFile)
			(fprintf oPort (strcat "(defun " defunName " () \n\t(let ()\n\t\tnil\n\t)\n)" ) )
			( close oPort )
		else
			(printf "\nFile does not exist." )
		)
	)
)



/*============================================================*
Description:
	--description--
*=============================================================*/
( defun rbzSAMwriteTemplateExampleToFile (filePath)
	(let (codeFile oPort sNextline)
		codeFile = ""
		oPort = (infile filePath)
		(if (neq oPort nil) then	; if cds.lib-File exists
			( while
				(gets sNextline oPort)
				codeFile = (strcat codeFile sNextline )	
			)
			oPort = (outfile filePath)
			(fprintf oPort codeFile)
			(fprintf oPort (strcat "(defun helloworldexample () \n\t(let ()\n\t\t(printf \"Hello World\\n\" )\n\t)\n)" ) )
			( close oPort )
		else
			(printf "\nFile does not exist." )
		)
	)	
)

/*============================================================*
Description:
	This function converts a 'nil to a "nil". If the value is 
	not 'nil, the value is not changed. 
*=============================================================*/
(defun rbzSAMnilToString (value)
    (let (returnVar)
    	(if value == nil then
    	    returnVar = "nil"
    	else
    		returnVar = value
    	);if
        returnVar
    );let
);defun