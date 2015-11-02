



#plot message

plot_message = function(m){
	frame()
	text(.5,.5,m)
}


#tex

getAbsPath = function(path, base = ''){ #get absolute path, give currect WD
	if(base == '') base = getwd()
	if(substr(path,1,1) != '/'){ #abosulte if starts with a slash
		paste(base,path, sep='/')
	}else{
		path
	}
}

makeTex = function(filename, tex = '', moveToDestination = '') #runs tools::texi2dvi in the right directory
{
	#PARSE FOR PATH
	fileSplit = strsplit(filename, '/')[[1]]
	
	name = tail(fileSplit, 1)
	
	if(length(fileSplit) > 1){
		path = fileSplit[1:(length(fileSplit) -1)]
	}else{
		path = ''
	}
	
	targetPath = getAbsPath(paste(path, collapse='/'))

	origPath = getwd()
	
	setwd(targetPath)
	
	if (tex!=''){cat(tex, file=name)}	
	
	try( #try, so we can switch back to our original path in case of failure	
		tools::texi2dvi(name,pdf=T, clean=T)
	)
	
	if (moveToDestination != ''){
		outputPDF = name
		substr(outputPDF, nchar(name)-2, nchar(name)) <- 'pdf'
		try(file.rename(outputPDF, paste(getAbsPath(moveToDestination, origPath),outputPDF, sep='/')))
	}
	
	setwd(origPath)
}











#Automatic regression tables

sigStars = function(x,s){ ifelse (abs(x/s) > qnorm(.995), '**', ifelse(abs(x/s) > qnorm(.975), '*\\phantom{*}','\\phantom{*}\\phantom{*}')) }

sigStars2 = function(x,s){ ifelse (abs(x/s) > qnorm(.995), '**', ifelse(abs(x/s) > qnorm(.975), '*\\phantom{*}',ifelse(abs(x/s) > qnorm(.95), "\\textasciicircum\\phantom{*}",'\\phantom{*}\\phantom{*}'))) }

phantomPlus = function (x) ifelse(as.numeric(x)>=0, paste('\\phantom{$+$}',x,sep=''), paste('$-$',sub('-','',x),sep='') )

phantomPlusPadded = function (x) ifelse(as.numeric(x)>=0, paste('\\phantom{$+$}',padValue(x),sep=''), paste('$-$',padValue(-1*x),sep='') )

padValue = function(x, nLeft=1,nRight=2) sprintf(paste('%0',(nLeft+1+nRight),'.',nRight,'f', sep=''), x)


formatDatum = function(b, se){
	paste(
		phantomPlusPadded(b),
		'(',
		padValue(se),
		')',
		sigStars2(b,se),
		sep=''
	)
}


stringReplaceDictionary = list(
		age3 = 'Age(years)',
		condbasic = 'Proximity Condition',
		condglow = 'Glow Condition', 
		condeyes = 'Eyes Condition',
		agency = 'Agency Cues Linearised',
		control = 'Control',
		basic = 'Proximity',
		glow = 'Glow',
		eyes = 'Eyes',
		maleTRUE = 'Male?',
		brazilTRUE = 'Brazilian?', 
		pointedToTriangleAsPennyTRUE = 'Pointed to triangle?',
		':' = ' * '
	)

stringReplaceFunction = function(x, dict){ #for auto-prettier table row headings
	for (i in 1:length(dict))
	{
		x <- gsub( names(dict)[i],dict[i],x,fixed=T)
	}
	return(x)
}

latexRegressionTables = function(listOfModels, rounding=2,ivs=c(),standAlone=F,compile=F, compileCommand='pdflatex', filename='',booktabs=T,Ns=T,heading='',caption='',captionFirst=F,tableHeader = '', label='', previewPage=T, printSigStars = T, sigStarsFunction = sigStars, sigStarsCaption ='\\newline \\newline **: $p<.01$, *: $p<.05$',tableEnvironmnet='table',centering=F,strRep=stringReplaceFunction, strDict=list('old_name'='New Name'), columnAlign = 'c', shortcaption=''){
	
	
	#coerce models into consistent format (summary object, since we can't go back the other way), or throw an error if we aren't dealing with lm or glm
	
	if(class(listOfModels) != 'list') {stop('First argument must be an R list')}
	
	listOfModels = lapply(listOfModels, 
		function(x){ 
			if(class(x)[1] %in% c('lm','glm')) 
			{
				summary(x)
			} 
			else 
			{
				if(!class(x)[1] %in% c('summary.lm','summary.glm')) 
					{
						stop(paste('Unknown model object type:',class(x)))
					} else {x}    
			}
		}   
	)
		
	mm = lapply(listOfModels, coef)
	
	#OLD ROUNDING METHOD, now i just adjust the strings not the actual numbers
	#if(!is.na(rounding) & rounding >=0) {
	#	mm = mapply(function(x)round(x,rounding),mm,SIMPLIFY=F)
	#}
	
	dvs = unlist(lapply(listOfModels, function(x) {terms(x)[[2]]} ))
	if(length(ivs)==0){
		ivs = unique(unlist(lapply(mm, function(x) {rownames(x)})))		
		}
	#ivs_clean = gsub( '_',' ', ivs, fixed=T)
	
	if (length(unique(dvs))>1){printDVs = T}else{printDVs = F}
	
	 
	 
	 #START WRITING TEX
	 
	 
	#PREMABLE
	
	if(standAlone){preambleCode = paste('\\documentclass[]{article}
	',ifelse(previewPage,'\\usepackage[active,tightpage,pdftex,floats]{preview}
	',''),ifelse(booktabs,'\\usepackage{booktabs}
	',''),ifelse(caption!='' | printSigStars ,'\\usepackage{caption}
	',''),'\\begin{document}
	') }else preambleCode = ''
	
	
	
	#TABULAR
	
	
	
	if(previewPage){
		#First we draw the table and save it in a box, so we know the width
		#tex magic to make sure the caption is centered even if we have a super wide table
		randomLetters = paste(sample(letters, 5), collapse = '')
		boxName = paste('\\tabularBox',randomLetters,sep='')
		boxlength = paste('\\len',randomLetters,sep='')
		tabularCode = paste('
		\\newsavebox{',boxName,'}   
		\\sbox{',boxName,'}{
		\\begin{tabular}{l',paste(rep(columnAlign,length(mm)), collapse=''),'}\n\t& ',sep='')
	}else{
		tabularCode = paste('\\begin{tabular}{l',paste(rep('c',length(mm)), collapse=''),'}\n\t& ',sep='')
	}
	
	modelNames = gsub( '_',' ', names(listOfModels), fixed=T)
	
	if (tableHeader != '') {tabularCode = paste(tabularCode,tableHeader, sep='') } #add header
	else{ #autogenerate header }
		for (i in 1:length(mm))
		{
			if (is.null( modelNames )){
				tabularCode = paste(tabularCode,'\\textbf{(',i,')}', sep='')
			}else{
				tabularCode = paste(tabularCode,'\\textbf{',strRep(modelNames[i], strDict),'}', sep='')
			}
			if (i != length(mm)) {tabularCode = paste(tabularCode,'&',sep='')} else {tabularCode = paste(tabularCode,'\\\\ ',sep='')}
		}
	}
	if(booktabs){tabularCode = paste(tabularCode,'\\toprule \n', sep='')}else{paste(tabularCode,'\\hline \n', sep='')}
	
	for (i in 1:length(ivs))
	{
		tabularCode = paste(tabularCode,'\t',strRep(ivs[i], strDict),' & ',sep='')
		
			for (j in 1:length(mm))
			{
				if (ivs[i] %in% rownames(mm[[j]]))
				{
					x = mm[[j]][ivs[i],1]
					s = mm[[j]][ivs[i],2]
					
					if(!is.na(rounding) & rounding >=0) { #rounding
						xx = sprintf(paste("%.",rounding,"f",sep=''), x)
						ss = sprintf(paste("%.",rounding,"f",sep=''), s)
					}else{xx=x;ss=s}
					
					#for visual balance, we add phantom plus signs in from of positive numbers
					
					
					tabularCode = paste(tabularCode,phantomPlus(xx),' (',ss,')',sep='')
					if(printSigStars){tabularCode = paste(tabularCode,sigStarsFunction(x,s),' ',sep='')}
				}
				if (j != length(mm)) {tabularCode = paste(tabularCode,'&',sep='')} else {tabularCode = paste(tabularCode,'\\\\ \n',sep='')}
			}
	}
	
	if(Ns){
		if(booktabs){tabularCode = paste(tabularCode,'\\midrule \n', sep='')}else{tabularCodepaste(tabularCode,'\\hline \n', sep='')}
		tabularCode = paste(tabularCode,'\t \\textbf{N} &')
		
		nn = unlist(lapply(listOfModels, function(x) sum(x[['df']][c(1,2)]) ))
		
		for (i in 1:length(mm))
		{
			tabularCode = paste(tabularCode,nn[i], sep='')
			if (i != length(mm)) {tabularCode = paste(tabularCode,' & ')} else {tabularCode = paste(tabularCode,'\\\\ \n',sep='')}
		}
	}else {tabularCode = paste(tabularCode,' \n')}
	
		
	if(previewPage){
	
		tabularCode = paste(tabularCode,'\\end{tabular}
		} %end hbox and savebox 
		\\newlength{',boxlength,'}
		\\settowidth{',boxlength,'}{\\usebox{',boxName,'}}
		',sep='')
	
	}else{tabularCode = paste(tabularCode,'\\end{tabular}')}
	
	
	
	
	
	
	#Wrap the whole thing in a float with a caption
	
	if(tableEnvironmnet != '' || standAlone){
		tableCode = paste('\\begin{',tableEnvironmnet,'}\n',sep='')
		
		if(centering){tableCode =  paste(tableCode,'\\centering\n')}
		if(previewPage){tableCode = paste(tableCode,'\\begin{minipage}[h]{',boxlength,'}')}
		
		if(heading !='') {		tableCode = paste(tableCode,'{\\large ',heading,'}\n',sep='')	}
		
		if(printSigStars) {caption = paste(caption,sigStarsCaption)}
		
		if( caption != ''){
			if(shortcaption !=''){
				captionCode = paste('\\caption[',shortcaption,']{ ',caption,'}\n', sep='')
			}else{
				captionCode =  paste('\\caption{ ',caption,'}\n', sep='')
			}
		}
		
		
		if(caption !='' & captionFirst) tableCode = paste(tableCode,captionCode,sep='')
		
		if(previewPage){ tableCode = paste(tableCode, '\\usebox{',boxName,'}\n') }else{ tableCode = paste(tableCode, '\n\n TABULAR_GOES_HERE \n\n')}
		
		if(caption !='' & !captionFirst) tableCode = paste(tableCode,captionCode,sep='')
		
		if(label !='') {		tableCode = paste(tableCode,'\\label{',label,'}\n',sep='')	}

		if(previewPage){tableCode = paste(tableCode,'\\end{minipage}')}
		tableCode = paste(tableCode,'\\end{',tableEnvironmnet,'}\n ',sep='')
	
	
	
	#COMBINE
	
	if(previewPage){
		tex = paste(preambleCode,tabularCode,tableCode,sep='\n')
		}
	else{
		tex = paste(preambleCode,sub('TABULAR_GOES_HERE',tabularCode,tableCode, fixed=T),sep='\n')
	}

	} else {
		tex = tabularCode
	}
	
	#COMPILE
	
	
	if(standAlone){tex =  paste(tex,'\\end{document}\n',sep='')}
	if(standAlone&compile&filename != '')
		{
			cat(tex, file=filename)
			system(paste(compileCommand,filename), wait=F)
		}else{
			if(filename != ''){
				message('Table written to: ',filename);cat(tex, file=filename)
			}else{tex}
		}
		
		
		
		
		

}







#DESIGNED TO WORK WITH COEFTEST RESULT!
latexRegressionTables_beta = function(listOfModels, rounding=2,ivs=c(),standAlone=F,compile=F, compileCommand='pdflatex', filename='',booktabs=T,heading='',caption='',captionFirst=F,tableHeader = '', label='', previewPage=T, printSigStars = T, sigStarsFunction = sigStars2, sigStarsCaption ='\\newline \\newline **: $p<.01$, *: $p<.05$',tableEnvironmnet='table',centering=F,strRep=stringReplaceFunction){
	
	
	#coerce models into consistent format (summary object, since we can't go back the other way), or throw an error if we aren't dealing with lm or glm
	
	if(class(listOfModels) != 'list') {stop('First argument must be an R list')}
	
	lapply(listOfModels, 
		function(x){ 
			if(!class(x)[1] %in% c('coeftest')) 
			{
				stop(paste('Unknown model object type, "coeftest" required:',class(x)))
			}
		}
	)
		
	mm = listOfModels
	
	#OLD ROUNDING METHOD, now i just adjust the strings not the actual numbers
	#if(!is.na(rounding) & rounding >=0) {
	#	mm = mapply(function(x)round(x,rounding),mm,SIMPLIFY=F)
	#}
	
	if(length(ivs)==0){ivs = unique(unlist(lapply(listOfModels, function(x) {rownames(x)})))}
	

	
	##NEW: SEPERATE OUT THE PRECISION MODEL
	
	phi_indexes = substring(ivs, 1,5) == '(phi)'
	phi_ivs = ivs[phi_indexes] 
	mu_ivs = ivs[!phi_indexes]

	all_ivs = list(mu_ivs, phi_ivs)
	iv_names = c('$\\mu$','$\\phi$')
	
	

	#START WRITING TEX
	 
	 
	#PREMABLE
	
	if(standAlone){preambleCode = paste('\\documentclass[]{article} \\usepackage{multirow,bigdelim}
	',ifelse(previewPage,'\\usepackage[active,tightpage,pdftex,floats]{preview}
	',''),ifelse(booktabs,'\\usepackage{booktabs}
	',''),ifelse(caption!='' | printSigStars ,'\\usepackage{caption}
	',''),'\\begin{document}
	') }else preambleCode = ''
	
	
	
	#TABULAR
	
	ncols = length(mm)
	colType = ifelse(printSigStars, 'cl','c')
	
	if(previewPage){
		#First we draw the table and save it in a box, so we know the width
		#tex magic to make sure the caption is centered even if we have a super wide table
		randomLetters = paste(sample(letters, 5), collapse = '')
		boxName = paste('\\tabularBox',randomLetters,sep='')
		boxlength = paste('\\len',randomLetters,sep='')
		tabularCode = paste('
		\\newsavebox{',boxName,'}   
		\\sbox{',boxName,'}{
		\\begin{tabular}{ll',paste(rep(colType,ncols), collapse=''),'}\n\t&& ',sep='')
	}else{
		tabularCode = paste('\\begin{tabular}{ll',paste(rep(colType,ncols), collapse=''),'}\n\t && ',sep='')  
	}
	
	modelNames = gsub( '_',' ', names(listOfModels), fixed=T)
	
	if (tableHeader != '') {tabularCode = paste(tabularCode,tableHeader, sep='') } #add header
	else{ #autogenerate header }
		for (i in 1:length(mm))
		{
			if (is.null( modelNames )){
				tabularCode = paste(tabularCode,'\\textbf{(',i,')}', sep='')
			}else{
				tabularCode = paste(tabularCode,'\\textbf{',strRep(modelNames[i]),'}', sep='')
			}
			
			if (i != length(mm)) {tabularCode = paste(tabularCode,ifelse(printSigStars, '&&','&'),sep='')} else {tabularCode = paste(tabularCode,'\\\\ ',sep='')}
		}
	}
	if(booktabs){tabularCode = paste(tabularCode,'\\toprule \n', sep='')}else{paste(tabularCode,'\\hline \n', sep='')}
	
	for (iv_type in 1:2)
	{
		ivs = all_ivs[[iv_type]]
		iv_name = iv_names[iv_type]

		for (i in 1:length(ivs))
		{
		
			#IF IV = 1  \multirow{1}{0.5cm}{$ \left.  \right\} $}& 	\multirow{1}{.1\textwidth}{Body}
			if(i == 1) tabularCode= paste(tabularCode, '
				\\ldelim\\{{',length(ivs),'}{3mm}[',iv_name,'] &' )else tabularCode= paste(tabularCode, '&') 
			
			iv_print = if(iv_type==1) ivs else substring(ivs, 7) 
			tabularCode = paste(tabularCode,'\t',strRep(   iv_print[i]   ),' & ',sep='')
			
				for (j in 1:length(mm))
				{
					if (ivs[i] %in% rownames(mm[[j]]))
					{
						x = mm[[j]][ivs[i],1]
						s = mm[[j]][ivs[i],2]
						
						if(!is.na(rounding) & rounding >=0) { #rounding
							xx = sprintf(paste("%.",rounding,"f",sep=''), x)
							ss = sprintf(paste("%.",rounding,"f",sep=''), s)
						}else{xx=x;ss=s}

						#for visual balance, we add phantom plus signs in from of positive numbers
						
						
						tabularCode = paste(tabularCode,phantomPlus(xx),' (',ss,')',sep='')
						if(printSigStars){tabularCode = paste(tabularCode,'&',sigStarsFunction(x,s),' ',sep='')}
					}else{
						if(printSigStars){tabularCode = paste(tabularCode,'&')}
					}
					if (j != length(mm)) {tabularCode = paste(tabularCode,'&',sep='')} else {tabularCode = paste(tabularCode,'\\\\ \n',sep='')}
				}
		}
		
		#leave a break
		
		tabularCode = paste(tabularCode,'\\midrule \\\\',sep='')
	}
	print(1)
	
	tabularCode = paste(tabularCode,' \n')
	
		
	if(previewPage){
	
		tabularCode = paste(tabularCode,'\\end{tabular}
		} %end hbox and savebox 
		\\newlength{',boxlength,'}
		\\settowidth{',boxlength,'}{\\usebox{',boxName,'}}
		',sep='')
	
	}else{tabularCode = paste(tabularCode,'\\end{tabular}')}
	
	
	
	
	
	
	#Wrap the whole thing in a float with a caption
	
	
	tableCode = paste('\\begin{',tableEnvironmnet,'}\n',sep='')
	
	if(centering){tableCode =  paste(tableCode,'\\centering\n')}
	if(previewPage){tableCode = paste(tableCode,'\\begin{minipage}[h]{',boxlength,'}')}
	
	if(heading !='') {		tableCode = paste(tableCode,'{\\large ',heading,'}\n',sep='')	}
	
	if(printSigStars) {caption = paste(caption,sigStarsCaption)}
	
	if(caption !='' & captionFirst) tableCode = paste(tableCode,'\\caption{ ',caption,'}\n',sep='')
	
	if(previewPage){ tableCode = paste(tableCode, '\\usebox{',boxName,'}\n') }else{ tableCode = paste(tableCode, '\n\n TABULAR_GOES_HERE \n\n')}
	
	if(caption !='' & !captionFirst) tableCode = paste(tableCode,'\\caption{ ',caption,'}\n',sep='')
	
	if(label !='') {		tableCode = paste(tableCode,'\\label{',label,'}\n',sep='')	}

	if(previewPage){tableCode = paste(tableCode,'\\end{minipage}')}
	tableCode = paste(tableCode,'\\end{',tableEnvironmnet,'}\n ',sep='')
	
	
	
	#COMBINE
	
	if(previewPage){
		tex = paste(preambleCode,tabularCode,tableCode,sep='\n')
		}
	else{
		tex = paste(preambleCode,sub('TABULAR_GOES_HERE',tabularCode,tableCode, fixed=T),sep='\n')
	}

	
	
	#COMPILE
	
	
	if(standAlone){tex =  paste(tex,'\\end{document}\n',sep='')}
	if(standAlone&compile&filename != '')
		{
			makeTex(filename, tex)
			#cat(tex, file=filename)
			#system(paste(compileCommand,filename), wait=F)
		}else{
			if(filename != ''){
				message('Table written to: ',filename);cat(tex, file=filename)
			}else{tex}
		}
}






wrapTabular = function(tabularCode)
{
		#First we draw the table and save it in a box, so we know the width
		#tex magic to make sure the caption is centered even if we have a super wide table
		randomLetters = paste(sample(letters, 5), collapse = '')
		boxName = paste('\\tabularBox',randomLetters,sep='')
		boxlength = paste('\\len',randomLetters,sep='')
		tex = paste('
		\\newsavebox{',boxName,'}   
		\\sbox{',boxName,'}{
		',tabularCode,'
		} %end hbox and savebox 
		\\newlength{',boxlength,'}
		\\settowidth{',boxlength,'}{\\usebox{',boxName,'}}',sep='')
				
		list(code=tex, box=paste('\\begin{minipage}[h]{',boxlength,'}\\usebox{',boxName,'}\n'))
}



wrapTabularInTable = function(tabularCode, caption='', label='', sideways=F)
{
	if(sideways) tEnv = 'sidewaystable' else tEnv = 'table'
	paste('
	\\begin{',tEnv,'}
	',tabularCode,'
	\\caption{',caption,'}
	\\label{',label,'}
	\\end{',tEnv,'}',sep='')
}

basicTab = function(tabularCode, ncols, caption='', label='')
{
	paste('
	\\begin{table} \\centering
	\\begin{tabular}{',paste(rep('c',ncols),collapse=''),'}
	',tabularCode,'
	\\bottomrule
	\\end{tabular}
	\\caption{',caption,'}
	\\label{',label,'}
	\\end{table}',sep='')
}

basicTabular = function(tabularCode, ncols, caption='', label='')
{
	tex = paste('\\begin{tabular}{',paste(rep('c',ncols),collapse=''),'}
	',tabularCode,'
	\\bottomrule
	\\end{tabular}',sep='')
	ww = wrapTabular(tex)
	
	paste(ww[1], '
	\\begin{table}
	', ww[2],"
	\\caption{",caption,"}
	\\label{",label,"}
	\\end{minipage}\\end{table}

	",sep='')

}

basicWarpedTable = function(tabularCode, caption)
{
	ww = wrapTabular(tabularCode)
	
	paste(ww[1], '
	\\begin{table} \\centering
	', ww[2],"
	\\caption{",caption,"}
	\\end{minipage}\\end{table}

	",sep='')
}


getLine = function(y) paste(y, collapse='&')


df2Tex = function(x, rowNames=F, filename='', preview=F, tabEnv=F, caption='', label='', midRulelines = c())
{

	
	if(rowNames){
		tex = paste(getLine(c(' ',colnames(x))),'\\\\ \\toprule \n',
			paste(
				sapply(1:nrow(x), 
					function(i) 
						paste( 
							ifelse( (i-1) %in% midRulelines, '\\midrule',''),
							getLine(c(rownames(x)[i],x[i,]))
						)
				)
			, collapse='\\\\
		'), '\\\\ \n')
	}else{
		tex = paste(getLine(colnames(x)),'\\\\ \\toprule',
		paste(sapply(1:nrow(x), function(i) paste(ifelse( (i-1) %in% midRulelines, '\\midrule',''), getLine(x[i,]) )  ), collapse='\\\\
		'), '\\\\')
	}
	if(preview){ tex = basicTabular(tex, ifelse(rowNames,ncol(x)+1,ncol(x)), caption=caption,label=label) }
	if(tabEnv){ tex = basicTab(tex, ifelse(rowNames,ncol(x)+1,ncol(x)), caption=caption,label=label) }
	
	if (filename != '') cat(tex, file=filename, append=F)
	else return(tex)
}


strReplace = function(x,dict = list(':' = ' * '	)){ #for auto-prettier table row headings
	for (i in 1:length(dict))
	{
		x <- gsub( names(dict)[i],dict[i],x,fixed=T)
	}
	return(x)
}




standaloneHeader = function(preview =T, booktabs=T, multirow=T,caption=T){

	paste('\\documentclass[]{article}
	',ifelse(preview, '\\usepackage[active,tightpage,pdftex,floats]{preview}',''),'
	',ifelse(booktabs, '\\usepackage{booktabs}',''),'
	',ifelse(multirow, '\\usepackage{multirow}',''),'
	',ifelse(caption, '\\usepackage{caption}',''),'
	\\begin{document}')
}

standaloneTailer = function() '\\end{document}'

generateStandalonePreviewedLatex = function(tabularCode,caption='',filename=NA){
	paste(
		standaloneHeader(),
		basicWarpedTable(tabularCode,caption),
		standaloneTailer()
		)
}









makeLatexTableArray = function(arrayOfTables, outHeaderX = 'Inner (X)', outHeaderY = 'Outer (Y)', innerHeaderX = 'Inner (X)',  innerHeaderY = 'Inner (Y)', legend='Legend Goes Here'){
	
	nX = 3
	nY = 3
	
	tabularRowSpec = paste('LL', paste(rep('F',nX), collapse=''), sep='')

	tabularContent = ''

	for (y in 1:nY){
		
		
		figures = ''
		
		for(x in 1:nX){
			figure = 'temp.jpg'
			figures = paste(figures, ' & \\includegraphics[width=\\figWidth]{',figure,'}', sep='')
		}
		
		innerLabel = ifelse(y != nY, '\\rotatebox{90}{ \\qquad }', paste('\\rotatebox{90}{ \\textbf{',innerHeaderY,'}}'))

		tabularContent =  paste(tabularContent,  ifelse(y%%2 ==0, '\\colorOne', '\\colorTwo'), y, '&', innerLabel, figures, '\\\\')
	}

	outerLabels = ''
	for(x in 1:nX){
		outerLabels = paste(outerLabels, ifelse(x>1,' & ',''),ifelse(x%%2 ==0, '\\colorOne', '\\colorTwo'), x,sep='')
	}

	latexCode = paste('
	\\documentclass[varwidth]{standalone}

	\\usepackage{graphicx}
	\\usepackage[table]{xcolor}
	\\usepackage{array}


	\\newlength{\\figWidth}
	\\setlength{\\figWidth}{',.6/nX,'\\textwidth}

	\\newcommand{\\fig}{\\includegraphics[width=\\figWidth]{temp.jpg} } 

	\\newcommand{\\colorOne}{\\cellcolor[gray]{0.8} } 
	\\newcommand{\\colorTwo}{\\cellcolor[gray]{0.7} } 

	\\newcolumntype{F}{ >{\\centering\\arraybackslash} m{\\figWidth} }
	\\newcolumntype{L}{ >{\\centering\\arraybackslash} m{.2em} }


	\\begin{document}
	\\begin{tabular}{>{\\centering\\arraybackslash} m{1ex}c}

	\\rotatebox{90}{ \\large \\textbf{ \\qquad ',outHeaderY,'} } &
		\\begin{tabular}{',tabularRowSpec,'}
			',tabularContent,'
			&& \\textbf{',innerHeaderX,'} \\\\
			&\\vspace*{1ex} \\quad & ',outerLabels,' \\\\	
		\\end{tabular} \\\\
		\\vspace*{1ex} \\quad & \\large \\textbf{',outHeaderX,'} \\\\
	\\end{tabular}
	\\end{document}
	', sep='')
	
	return(latexCode)
}


