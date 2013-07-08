// author: Alfredo Sánchez Alberca (asalber@ceu.es)

// globals
var options;

include ('plot_dist_common.js');

function getParameters () {
	options['df'] = getString("df");
	getContRangeParameters ();

	if (options['is_density']) {
		options['fun'] = "dt";
	} else {
		options['fun'] = "pt";
	}
}

function doHeader () {
	echo ('rk.header ("Funci&oacute;n de ' + options['label'] + ' T de student", list ("Grados de libertad"= "' + options['df'] + '"))\n');
}

function doFunCall () {
	echo (options['fun'] + '(x, df=' + options['df'] + ')');
}

