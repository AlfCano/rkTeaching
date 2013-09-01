// author: Alfredo Sánchez Alberca (asalber@ceu.es)

// globals
var variable;

function preprocess(){
	// add requirements etc. here
	echo('require(rk.Teaching)\n');
}

function calculate(){
	variable = getString("variable");
	data = variable.split('[[')[0];
	variablename = getString("variable.shortname");
	// Filter
	echo(getString("filter_embed.code.calculate"));
	// Calculate frequencies
	echo('result <- frequencyTableIntervals(' + data + ', ' + quote(variablename) + getString("cells.code.calculate")); 
    // Grouped mode
	if (getBoolean("grouped")) {
		groups = getList("groups");
		groupsname = getList("groups.shortname");
		echo(', groups=c(' + groupsname.map(quote) + ')');
	}
	echo(')\n');
}

function printout(){
	// printout the results
	echo('rk.header("Tabla de frecuencias (datos agrupados)", ');
	echo('parameters=list("Variable" = rk.get.description(' + variable +  ')' + getString("filter_embed.code.printout") + getString("cells.code.printout"));  
	if (getBoolean("grouped")) {
		echo(', "Variable(s) de agrupaci&oacute;n" = rk.get.description(' + groups + ',paste.sep=", ")');
	}
	echo ('))\n');
	if (getBoolean("grouped")){
		echo('for (i in 1:length(result)){\n');
		echo('\t rk.header(names(result)[i],level=3)\n');
		echo('\t\t rk.results(result[[i]])\n');
		echo('}\n');
	}
	else {
		echo('rk.results(result)\n');
	}
}

