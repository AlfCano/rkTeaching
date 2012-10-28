// author: Alfredo Sánchez Alberca (asalber@ceu.es)

// globals
var q;

function calculate () {
	q = "c (" + getValue ("q").replace (/[, ]+/g, ", ") + ")";

	echo ('result <- (pf(q = ' + q + ', df1 = ' + getValue ("df1") + ', df2 = ' + getValue ("df2") + ', ' + getValue ("tail") + '))\n');
}

function printout () {
	echo ('rk.header ("Probabilidad acumulada F de Fisher", list ("Grados de libertad del numerador" = "' + getValue ("df1") + '", "Grados de libertad del denominador" = "' + getValue ("df2") + '", "Cola de acumulaci&oacute;n" = ');
	if (getValue ("tail")=="lower.tail=TRUE" )
		echo('"Izquierda"));\n');
	else
		echo('"Derecha"));\n');
	echo ('rk.results (list("Valores" = ' + q + ', "Probabilidades acumuladas" = result))\n');
}
