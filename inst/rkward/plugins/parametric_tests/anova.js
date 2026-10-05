// author: Alfredo Sánchez Alberca (asalber@ceu.es)

include("../common/common_functions.js")
include("../common/filter.js")

// globals
var dataframe,
design,
variable,
variableName,
within,
withinName,
between,
betweenName,
caseId,
caseIdName,
observed,
observedName,
showSumSq,
sumSqType,
heterocedasticity,
pairwiseMeans,
pairwisePlot;

function setGlobalVars() {
  variable = getString("variable");
  variableName = getString("variable.shortname");
  dataframe = getDataframe(variable);
  design = getString("design");
  within = getList("within");
  withinName = getList("within.shortname");
  between = getList("between");
  betweenName = getList("between.shortname");
  caseId = getString("caseId");
  caseIdName = getString("caseId.shortname");
  observed = getList("observed");
  observedName = getList("observed.shortname");
  showSumSq = getBoolean("showSumSq");
  sumSqType = getString("sumSqType");
  heterocedasticity = getString("heterocedasticity");
  pairwiseMeans = getBoolean("pairwiseMeans");
  pairwisePlot = getBoolean("pairwisePlot");
}

function preprocess() {
  setGlobalVars();
  echo('library(tidyverse)\n');
  echo('library(broom)\n');
  echo('library(knitr)\n');
  echo('library(kableExtra)\n');
  echo('library(ez)\n');
  echo('library(nlme)\n');
  echo('library(multcomp)\n');
}

function calculate() {
  // Filter
  filter();
  // ANOVA settings
  if (sumSqType == 3) {
    echo('# Set type III of sum of squares\n');
    echo('options(contrasts=c("contr.sum","contr.poly"))\n');
  }
  if (caseId == '' & design == 'between') {
    echo('# ezANOVA requires a case identifier variable\n');
    echo(dataframe + ' <- cbind(' + dataframe + ', subjectid=factor(1:nrow(' + dataframe + ')))\n');
  }
  echo('anova.results <- ezANOVA(data=' + dataframe + '[!is.na(' + dataframe + '$' + variableName + '),], dv=.(' + variableName + ')');
  if (caseId) {
    echo(', wid=' + caseIdName);
  } else if (design == 'between') {
    echo(', wid=subjectid');
  }
  if (within != '' & design != 'between') {
    echo(', within=.(' + withinName.join(", ") + ')');
  }
  if (between != '' & design != 'within') {
    echo(', between=.(' + betweenName.join(", ") + ')');
  }
  if (observed != '') {
    echo(', observed=.(' + observedName.join(", ") + ')');
  }
  if (sumSqType != 2) {
    echo(', type=' + sumSqType);
  }
  if (heterocedasticity != "false") {
    echo(', white.adjust=' + quote(heterocedasticity));
  }
  if (showSumSq) {
    echo(', detailed=TRUE');
  }
  echo(', return_aov=TRUE)\n');
  // Pairwise means coparison
  if (pairwiseMeans | pairwisePlot) {
    if (design == 'between') {
      echo('pairs <- tidy(TukeyHSD(anova.results[["aov"]])) |>\n');
      echo('\tmutate(conf.int = paste0("(", round(conf.low, 6), " , ", round(conf.high, 6), ")"))\n');
    }
    // FIX 1: Removed 'tidy()' wrapper. We need the raw glht object for summary, confint and plot to work later.
    if (design == 'within') {
      echo('pairs <- glht(lme(' + variableName + '~' + withinName.join("*") + ', data = ' + dataframe + '[!is.na(' + dataframe + '$' + variableName + '),], random = ~1|' + caseIdName + '), linfct = mcp(' + withinName.join("=\"Tukey\",") + '= "Tukey"))\n');
    }
    if (design == 'mixed') {
      echo('pairs <- glht(lme(' + variableName + '~' + betweenName.join("*") + '*' + withinName.join("*") + ', data = ' + dataframe + '[!is.na(' + dataframe + '$' + variableName + '),], random = ~1|' + caseIdName + '), linfct = mcp(' + betweenName.join("=\"Tukey\",") + '= "Tukey", ' + withinName.join("=\"Tukey\",") + '= "Tukey"))\n');
    }
  }
  echo('assign("anova.results", anova.results, envir = .GlobalEnv)\n');
}

function printout() {
  // Header
  header = new Header(i18n("ANOVA of %1", variableName));
  header.add(i18n("Data frame"), dataframe);
  header.add(i18n("Dependent variable"), variableName);
  if (between != '' & design != "within") {
    header.add(i18n("Between subjects factors"), betweenName.join(", "));
  }
  if (within != '' & design != "between") {
    header.add(i18n("Within subjects factors"), withinName.join(", "));
  }
  if (observed != '') {
    header.add(i18n("Observed (non-manipulated) factors"), observedName.join(", "));
  }
  header.add(i18n("Sums of squares"), 'Type' + sumSqType);
  if (filtered) {
    header.addFromUI("condition");
  }
  header.print();
  // ANOVA results
  echo('rk.print.literal(tibble(');
  echo(i18n("Source of variation") + ' = anova.results[["ANOVA"]][["Effect"]], ');
  echo(i18n("Num DF") + ' = anova.results[["ANOVA"]][["DFn"]], ');
  echo(i18n("Den DF") + ' = anova.results[["ANOVA"]][["DFd"]], ');
  if (showSumSq) {
    echo(i18n("Num sum of squares") + ' = anova.results[["ANOVA"]][["SSn"]], ');
    echo(i18n("Den sum of squares") + ' = anova.results[["ANOVA"]][["SSd"]], ');
  }
  echo(i18n("F statistic") + ' = anova.results[["ANOVA"]][["F"]], ');
  echo(i18n("p-value") + ' = anova.results[["ANOVA"]][["p"]]');
  echo(') |>\n');
  echo('\tkable("html", align = "c", escape = FALSE) |>\n');
  echo('\tkable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)\n');
  echo(')\n');
  // Mauchly's sphericity test (for repeated measures)
  echo("if(\"Mauchly\'s Test for Sphericity\" %in% names(anova.results)){\n");
  echo('\trk.header(' + i18n("Mauchly\'s test for sphericity") + ', level=3)\n');
  echo('\trk.print.literal(tibble(');
  echo(i18n("Source of variation") + ' = anova.results[["Mauchly\'s Test for Sphericity"]][["Effect"]], ');
  echo(i18n("W statistic") + ' = anova.results[["Mauchly\'s Test for Sphericity"]][["W"]], ');
  echo(i18n("p-value") + ' = anova.results[["Mauchly\'s Test for Sphericity"]][["p"]]');
  echo(') |>\n');
  echo('\tkable("html", align = "c", escape = FALSE) |>\n');
  echo('\tkable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)\n');
  echo('\t)\n');
  echo('}\n');
  // Sphericity correction
  echo('if("Sphericity Corrections" %in% names(anova.results)){\n');
  echo('\trk.header(' + i18n("Sphericity correction") + ', level=3)\n');
  echo('\trk.print.literal(tibble(');
  echo(i18n("Source of variation") + ' = anova.results[["Sphericity Corrections"]][["Effect"]], ');
  echo(i18n("Epsilon Greenhouse-Geisser") + ' = anova.results[["Sphericity Corrections"]][["GGe"]], ');
  echo(i18n("G-G corrected p-value") + ' = anova.results[["Sphericity Corrections"]][["p[GG]"]], ');
  echo(i18n("Epsilon Huynh-Feldt") + ' = anova.results[["Sphericity Corrections"]][["HFe"]], ');
  echo(i18n("H-F corrected p-value") + ' = anova.results[["Sphericity Corrections"]][["p[HF]"]]');
  echo(') |>\n');
  echo('\tkable("html", align = "c", escape = FALSE) |>\n');
  echo('\tkable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)\n');
  echo('\t)\n');
  echo('}\n');
  // Levene's test for homogeneity of variance
  echo('if("Levene\'s Test for Homogeneity of Variance" %in% names(anova.results)){\n');
  echo('\trk.header("Levene\'s test for comparing variances", level=3)\n');
  echo('\trk.print.literal(tibble(');
  echo(i18n("Num DF") + ' = anova.results[["Levene\'s Test for Homogeneity of Variance"]][["DFn"]], ');
  echo(i18n("Den DF") + ' = anova.results[["Levene\'s Test for Homogeneity of Variance"]][["DFd"]], ');
  echo(i18n("Num sum of squares") + ' = anova.results[["Levene\'s Test for Homogeneity of Variance"]][["SSn"]], ');
  echo(i18n("Den sum of squares") + ' = anova.results[["Levene\'s Test for Homogeneity of Variance"]][["SSd"]], ');
  echo(i18n("F statistics") + ' = anova.results[["Levene\'s Test for Homogeneity of Variance"]][["F"]], ');
  echo(i18n("p-value") + ' = anova.results[["Levene\'s Test for Homogeneity of Variance"]][["p"]]');
  echo(') |>\n');
  echo('\tkable("html", align = "c", escape = FALSE) |>\n');
  echo('\tkable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)\n');
  echo('\t)\n');
  echo('}\n');
  // Pairwise means comparison
  if (pairwiseMeans) {
    echo('rk.header(' + i18n("Pairwise comparison of means") + ', level=3)\n');
    if (design == 'between') {
      echo('rk.header(' + i18n("Confidence intervals for the difference between the means") + ', level=4)\n');
      echo('rk.print.literal(tibble(');
      echo(i18n("Pairs") + ' = pairs$contrast, ');
      echo(i18n("Difference between means") + ' = pairs$estimate, ');
      echo(i18n("Confidence interval<br>difference between means") + ' = pairs$conf.int, ');
      echo(i18n("p-value") + ' = pairs$adj.p.value');
      echo(') |>\n');
      echo('\tkable("html", align = "c", escape = FALSE) |>\n');
      echo('\tkable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)\n');
      echo(')\n');
    } else {
      echo('rk.header(' + i18n("Tukey\'s test for pairwise comparison of means") + ', level=4)\n');
      echo('rk.print.literal(tibble(');
      echo(i18n("Pairs") + ' = names(summary(pairs)$test$coefficients), ');
      echo(i18n("Estimate") + ' = summary(pairs)$test$coefficients, ');
      echo(i18n("Standard error") + ' = summary(pairs)$test$sigma, ');
      echo(i18n("t statistic") + ' = summary(pairs)$test$tstat, ');
      echo(i18n("p-value") + ' = summary(pairs)$test$pvalues');
      echo(') |>\n');
      echo('\tkable("html", align = "c", escape = FALSE) |>\n');
      echo('\tkable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)\n');
      echo('\t)\n');

      // FIX 2: Removed "echo('}\n');" here. This was inserting a stray "}" in the R code.

      echo('rk.header(' + i18n("Confidence intervals for the difference of means") + ', level=4)\n');
      echo('rk.print.literal(tibble(');
      echo(i18n("Pairs") + ' = rownames(confint(pairs)$confint), ');
      echo(i18n("Estimate") + ' = confint(pairs)$confint[,1], ');
      // FIX 3: Removed trailing comma below and fixed object access (confint(pairs)$confint)
      echo(i18n("Confidence interval<br>difference between means") + ' = paste0("(", round(confint(pairs)$confint[,2], 6), " , ", round(confint(pairs)$confint[,3], 6), ")")');
      echo(') |>\n');
      echo('\tkable("html", align = "c", escape = FALSE) |>\n');
      echo('\tkable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)\n');
      echo('\t)\n');

      // FIX 4: Removed "echo('}\n');" here. Another stray brace.
    }
  }
  // Pairwise means plot
  if (pairwisePlot) {
    echo('rk.header(' + i18n("Pairwise comparison of means plot") + ', level=4)\n');
    echo('rk.graph.on()\n');

    if (design == 'between') {
      // Si es 'between', 'pairs' es un data.frame. Usamos ggplot2.
      echo('require(ggplot2)\n');
      echo('p <- ggplot(pairs, aes(x = estimate, y = contrast)) +\n');
      echo('  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +\n');
      echo('  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkblue", linewidth = 1) +\n');
      echo('  geom_point(size = 3, color = "darkblue") +\n');
      echo('  labs(title = "Comparación de Medias por Pares (Tukey HSD)", subtitle = "Intervalos de confianza al 95%", x = "Diferencia de Medias", y = "Pares Comparados") +\n');
      echo('  theme_minimal(base_size = 14) +\n');
      echo('  theme(panel.grid.minor = element_blank())\n');
      echo('print(p)\n');
    } else {
      // Si es 'within' o 'mixed', 'pairs' es un modelo glht. Usamos R base.
      echo('par(mar=c(4,10,4,2))\n');
      echo('plot(pairs)\n');
    }

    echo('rk.graph.off()\n');
  }
}

