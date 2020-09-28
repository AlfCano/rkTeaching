// author: Alfredo Sánchez Alberca (asalber@ceu.es)

var lambda,
	fun,
	min,
	max,
	n;

include('plot_dist_common.js');

function getParameters() {
	lambda = getString("lambda");
	setDistParameters()
	if (density) {
		fun = "dpois";
	} else {
		fun = "ppois";
	}
	min = 0;
	max = parseInt(lambda) + Math.round(4 * Math.sqrt(parseFloat(lambda)));
	n = max - min + 1;
}

function doHeader() {
	header = new Header(i18n("Poisson %1 function P(%2)", label, lambda));
  header.add(i18n("Mean"), lambda);
  header.print();
}

function doFunCall() {
	echo('ylim <- max(' + fun + '(seq(' + min + ',' + max + '), lambda=' + lambda + '))\n');
	echo('p <- qplot(c(' + min + ',' + max + '), geom="blank") + stat_function(fun=' + fun + ', colour="#FF5555", n=' + n + ', geom="point", size=I(3), args=list(lambda=' + lambda + ')) + ylim(0,ylim)');
}