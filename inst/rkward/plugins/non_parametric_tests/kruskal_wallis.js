// author: Alfredo Sánchez Alberca (asalber@ceu.es)

include("../common/common_functions.js")
include("../common/filter.js")

// globals
var variable,
	variableName,
	factor,
	factorName,
	grouped,
	groups,
	groupsName,
	pairwise;

function setGlobalVars() {
	variable = getString("variable");
	variableName = getString("variable.shortname");
	dataframe = getDataframe(variable);
	factor = getString("factor");
	factorName = getString("factor.shortname");
	groupsName = getList("groups.shortname");
	grouped = getBoolean("grouped");
	groups = getList("groups");
	pairwise = getBoolean("pairwise");
}

function preprocess() {
	setGlobalVars();
	echo('library(plyr)\n')
	echo('library(rkTeaching)\n');
}

function calculate() {
	// Filter
	filter();
	// Grouped mode
	if (grouped) {
		echo(dataframe + ' <- transform(' + dataframe + ', .groups=interaction(' + dataframe + '[,c(' + groupsName.map(quote) + ')]))\n');
		echo('result <- dlply(' + dataframe + ', ".groups", function(df) kruskal.test(df[[' + quote(variableName) + ']], df[[' + quote(factorName) + ']]))\n');
		if (pairwise) {
			echo('pairs <- dlply(' + dataframe + ', ".groups", function(df) kruskalMultipleComparison(df[[' + quote(variableName) + ']], df[[' + quote(factorName) + ']]))\n');
		}
	} else {
		// Non-grouped mode
		echo('result <- kruskal.test(' + variable + ', ' + factor + ')\n');
		if (pairwise) {
			echo('pairs <- kruskalMultipleComparison(' + variable + ', ' + factor + ')\n');
			echo('pairs[["dif.com"]][["difference"]] <- replace(pairs[["dif.com"]][["difference"]],pairs[["dif.com"]][["difference"]]==TRUE,' + i18n("YES") + ')\n');
			echo('pairs[["dif.com"]][["difference"]] <- replace(pairs[["dif.com"]][["difference"]],pairs[["dif.com"]][["difference"]]==FALSE,' + i18n("NO") + ')\n');
		}
	}
}

function printout() {
	// Header
	header = new Header(i18n("Kruskal-Wallis test for comparing the distribution of %1 according to %2", variableName, factorName));
	header.add(i18n("Data frame"), dataframe);
	header.add(i18n("Comparison of"), i18n("%1 according to %2", variableName, factorName));
	header.add(i18n("Null hypothesis"), i18n("There is no significant difference between the populations"));
	header.add(i18n("Alternative hypothesis"), i18n("There are significant difference between at least two populations"));
	if (grouped) {
		header.add(i18n("Grouping variable(s)"), groupsName.join(", "));
	}
	if (filtered) {
		header.addFromUI("condition");
	}
	header.print();

	// Grouped mode
	if (grouped) {
		echo('for (i in 1:length(result)){\n');
		echo('\trk.header(paste(' + i18n("Group %1 =", groupsName.join('.')) + ', names(result)[i]), level=3)\n');
		echo('\trk.results (list(');
		echo(i18n("Variable") + ' = ' + quote(variableName) + ', ');
		echo(i18n("Populations defined by") + ' = ' + quote(factorName) + ', ');
		echo(i18n("Chi statistic") + ' = result$statistic, ');
		echo(i18n("p-value") + ' = result[[i]]$p.value))\n');
		if (pairwise) {
			echo('\trk.header(' + i18n("Pairwise comparison") + ', level=3)\n');
			echo('\trk.results(list(');
			echo(i18n("Pairs") + ' = rownames(pairs[[i]][["dif.com"]]), ');
			echo(i18n("Observed difference") + ' = pairs[[i]][["dif.com"]][["obs.dif"]], ');
			echo(i18n("Critical difference") + ' = pairs[[i]][["dif.com"]][["critical.dif"]], ');
			echo(i18n("Significant difference") + ' = pairs[[i]][["dif.com"]][["difference"]]');
			echo('))\n');
		}
		echo('}\n');
	} else {
		// Non-grouped mode
		echo('rk.results (list(');
		echo(i18n("Variable") + ' = ' + quote(variableName) + ', ');
		echo(i18n("Populations defined by") + ' = ' + quote(factorName) + ', ');
		echo(i18n("Chi statistic") + ' = result$statistic, ');
		echo(i18n("p-value") + ' = result$p.value');
		echo('))\n');
		if (pairwise) {
			echo('rk.header(' + i18n("Pairwise comparison") + ', level=3)\n');
			echo('rk.results(list(');
			echo(i18n("Pairs") + ' = rownames(pairs[["dif.com"]]), ');
			echo(i18n("Observed difference") + ' = pairs[["dif.com"]][["obs.dif"]], ');
			echo(i18n("Critical difference") + ' = pairs[["dif.com"]][["critical.dif"]], ');
			echo(i18n("Significant difference") + ' = pairs[["dif.com"]][["difference"]]');
			echo('))\n');
		}
	}
}
