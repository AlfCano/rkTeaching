// author: Alfredo Sánchez Alberca (asalber@ceu.es)

var objects, 
	numObjects,
	listObjects,
	numDraws,
	numTimes,
	replace,
	ordered,
	freq,
	dataframe;

function preprocess() {
	echo('library(probs)\n');
}

function setGlobals() {
	objects = getString("objects");
	numObjects = getString("numObjects");
	listObjects = getList("listObjects");
	numDraws = getString("numDraws");
	numTimes = getString("numTimes");
	replace = getString("replace");
	ordered = getString("ordered");
	freq = getBoolean("freq");
	dataframe = getString("save");
}

function calculate() {
	setGlobals();	
	if (objects === "num") {
		echo('s <- urnsamples(1:' + numObjects + ', size=' + numDraws +
			', replace=' + replace + ', ordered=' + ordered + ')\n');
	} else {
		echo('s <- urnsamples(c("' + listObjects.replace(/,/g, '","') +
			'"), size=' + numDraws + ', replace=' + replace +
			', ordered=' + ordered + ')\n');
	}
	echo('s <- probspace(s)\n');
	echo('result <- sim(s, ntrials = ' + numTimes + ')\n');
	if (getBoolean("freq")) {
		echo('result <- empirical(result)\n');
		echo('names(result)[ncol(result)]=' + i18n("frequency") + '\n');
	}
	echo('assign("' + dataframe + '", result, .GlobalEnv)\n');
}

function printout() {
	header = new Header(i18n("Random experiment of drawing objects from an urn"));
	if (objects === "num") {
		header.add(i18n("Number of objects in the urn"), numObjects);
	} else {
		header.add(i18n("Objects in the urn"), listObjects);
	}
	header.add(i18n("Number of draws"), numDraws);
	header.add(i18n("Number of times"), numTimes);
	if (getBoolean("replace.state")) {
		header.add(i18n("With replacement"), i18n("Yes"));
	} else {
		header.add(i18n("With replacement"), i18n("No"));
	}
	if (getBoolean("ordered.state")) {
		header.add(i18n("With order"), i18n("Yes"));
	} else {
		header.add(i18n("With order"), i18n("No"));
	}
	header.add(i18n("Data frame"), dataframe);
	header.print();
}
