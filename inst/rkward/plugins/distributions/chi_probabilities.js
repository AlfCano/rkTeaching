// author: Alfredo Sánchez Alberca (asalber@ceu.es)

var q, df, tail, max;

function calculate () {
	df = getString("df");
	q = 'c(' + getString("q").replace (/[, ]+/g, ", ") + ')';
	tail = getString("tail");
	echo ('result <- (pchisq(q = ' + q + ', df = ' + df + ', ' + tail + '))\n');
	max = parseFloat(df)+4*Math.sqrt(2*parseFloat(df));
}

function printout () {
	echo ('rk.header ("Probabilidad acumulada Chi-cuadrado &chi;(' + df + ')", list ("Degrees of freedom" = "' + getString("df") + '", "Cola de acumulaci&oacute;n" = ');
	if (tail=="lower.tail=TRUE" )
		echo('"Izquierda (&lt;)"));\n');
	else
		echo('"Derecha (&gt;)"));\n');
	echo ('rk.results (list("Valores" = ' + q + ', "Probabilidades acumuladas" = result))\n');
	// Plot
	if (getBoolean("plot")){
		if (tail=="lower.tail=TRUE" ){
			echo('x <- seq(0,' + q + '[1], length.out= 100)\n');
		} else {
			echo('x <- seq(' + q + '[1],' + max + ', length.out= 100)\n');
		}
		echo('y <- dchisq(x,' + df + ')\n');
		echo('rk.graph.on()\n');
		echo('try ({\n');
		echo('p <- qplot(x=c(0, ' + max + '), geom="blank") + geom_area(aes(x=c(x[1],x,x[100]), c(0,y,0)), fill=I("#FF9999"), alpha=0.5) + stat_function(fun=dchisq, colour="#FF5555", args=list(df=' + df + ')) + xlab(expression(italic("X"))) + ylab(expression(paste("Densidad ",italic(f(x))))) + scale_x_continuous(breaks=c(' + q + '[1]))'); 
		//echo('	curve(dnorm(x, mean=' + mean + ', sd=' + sd + '), from=' + mean + '-3*' + sd + ', to=' + mean + '+3*' + sd + ', n= 100 ,  , )');
		if (tail=="lower.tail=TRUE" ){
			echo(' + labs(title=paste("P(X<",' + q + '[1], ")=", round(result[1],4)))\n');
		} else {
			echo(' + labs(title=paste("P(X>",' + q + '[1], ")=", round(result[1],4)))\n');
		}
		//echo('p + axis(side=1,' + q + '[1])\n');
		echo('print(p)\n');
		//echo('	lines (x, y, type="l")\n');
		//echo(' 	abline(h=0, col="gray")\n');
		echo ('})\n');
		echo('rk.graph.off()\n');
	}
}
