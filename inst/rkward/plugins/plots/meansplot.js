// author: Alfredo Sánchez Alberca (asalber@ceu.es)

include("../common/common_functions.js")
include("../common/filter.js")

// globals
var dataframe,
  variables,
  variablesName,
  groups,
  groupsName,
  getPoints,
  points,
  getConfInt,
  confInt,
  meanColor,
  intervalColor,
  facet;

function setGlobalVars() {
  variables = getList("variables");
  variablesName = getList("variables.shortname");
  dataframe = getDataframe(variables);
  grouped = getBoolean("grouped");
  groups = getList("groups");
  groupsName = getList("groups.shortname");
  getPoints = getBoolean("points");
  getConfInt = getBoolean("frameConfInt.checked");
}

function preprocess() {
  setGlobalVars();
  echo('require(ggplot2)\n');
}

function calculate() {
  // Filter
  filter();
  // Prepare data frame
  echo('.df <- data.frame(y=c(' + variables.join() + '), x=factor(rep(c(' + variablesName.map(quote) + '), each=nrow(' + dataframe + ')))');
  if (grouped) {
    echo(', ' + groupsName.join(".") + '=rep(interaction(' + groups + '),' + variables.length + '))\n');
    echo('.df <- .df[!is.na(.df[["' + groupsName.join(".") + '"]]),]\n');
  } else {
    echo(')\n');
  }
  // Set mean and interval color
  // Set grouped mode
  facet = '';
  if (grouped) {
    meanColor = ', colour=' + groupsName.join(".");
    intervalColor = '';
  } else {
    meanColor = '';
    intervalColor = ', colour="#FF5555"';
  }
  // Set confidence intervals
  confInt = '';
  if (getConfInt) {
    confInt = ' + stat_summary(fun.data=function(x) mean_cl_normal(x, conf.int=' + getString("conflevel") + '), geom="pointrange"' + intervalColor + ', position=position_dodge(width=0.25))';
  }
  // Set points
  points = '';
  if (getPoints) {
    points = ' + geom_point(position=position_dodge(width=0.25))';
  }
}

function printout() {
  doPrintout(true);
}

function preview() {
  preprocess();
  calculate();
  doPrintout(false);
}

function doPrintout(full) {
  // Print header
  if (full) {
    header = new Header(i18n("Means plot of %1", variablesName.join(", ")));
    header.add(i18n("Data frame"), dataframe);
    header.add(i18n("Variables"), variablesName.join(", "));
    if (grouped) {
      header.add(i18n("Grouping variable(s)"), groupsName.join(", "));
    }
    if (filtered) {
      header.addFromUI("condition");
    }
    header.print();
    echo('rk.graph.on()\n');
    }
  // Plot
  echo('try ({\n');
  echo('.meansplot <- ggplot(data=.df, aes(x=x,y=y' + meanColor + ')) + stat_summary(fun.y="mean", size=3,  geom="point", position=position_dodge(width=0.25)' + intervalColor + ')' + points + confInt + ' + xlab("") + ylab("")' + facet + getString("plotOptions.code.calculate") + '\n');
  echo('print(.meansplot)\n');
  echo('})\n');
  if (full) {
    echo('rk.graph.off ()\n');
  }
}
