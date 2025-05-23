// author: Alfredo Sánchez Alberca (asalber@ceu.es)

include("../common/common_functions.js")
include("../common/filter.js")

// globals
var dataframe,
    x,
    y,
    xName,
    yName,
    grouped,
    groups,
    groupsName,
    pointColor,
    pointSymbol,
    pointSize,
    confidenceStrip,
    stripColor,
    facet,
    smooth,
    smoothColor,
    legend,
    regression,
    model,
    formula,
    linear,
    quadratic,
    cubic,
    potential,
    exponential,
    logarithmic,
    inverse,
    sigmoid;

function setGlobalVars() {
    x = getString("x");
    y = getString("y");
    dataframe = getDataframe(x);
    xName = getString("x.shortname");
    yName = getString("y.shortname");
    grouped = getBoolean("grouped");
    groups = getList("groups");
    groupsName = getList("groups.shortname");
    pointSize = getString("pointSize");
    confidenceStrip = getBoolean("confidenceStrip");
    linear = getBoolean("linear");
    quadratic = getBoolean("quadratic");
    cubic = getBoolean("cubic");
    potential = getBoolean("potential");
    exponential = getBoolean("exponential");
    logarithmic = getBoolean("logarithmic");
    inverse = getBoolean("inverse");
    sigmoid = getBoolean("sigmoid");
}

function preprocess() {
    setGlobalVars();
    echo('library(tidyverse)\n');
}

function calculate() {
    // Filter
    filter();
    // Set point color
    pointColor = getString("pointColor.code.printout");
    if (pointColor != '') {
        pointColor = '), colour=' + pointColor;
    } else {
        pointColor = '), colour="#FF9999"'; // Default bar color
    }
    // Set point symbol
    pointSymbol = getString("pointSymbol.code.printout");
    if (pointSymbol != '') {
        pointSymbol = ', shape=' + pointSymbol;
    }
    // Set point size
    pointSize = ', size=' + pointSize;
    // Set grouped mode
    facet = '';
    smoothColor = '';
    stripColor = ')';
    legend = '';
    if (grouped) {
        echo(dataframe + ' <- transform(' + dataframe + ', .groups=interaction(' + dataframe + '[,c(' + groupsName.map(quote) + ')]))\n');
        echo(dataframe + ' <- ' + dataframe + '[!is.na(' + dataframe + '[[".groups"]]),]\n');
        pointColor = ', colour=.groups';
        pointSymbol = ', shape=.groups)';
        smoothColor = ', colour=.groups';
        stripColor = ', fill=.groups)';
        legend = ' + scale_colour_discrete(name=' + quote(groupsName.join('.')) + ') + scale_shape_discrete(name=' + quote(groupsName.join('.')) + ')';
        if (confidenceStrip){
            legend += ' + scale_fill_discrete(name=' + quote(groupsName.join('.')) + ')';
        }
        if (getString("position") === 'faceted') {
            facet = ' + facet_grid(.~.groups)';
        }
    }
    regression = linear | quadratic | cubic | potential | exponential | logarithmic | inverse | sigmoid;
    smooth = '';
    model = [];
    formula = '';
    if (regression) {
        echo('.df <- data.frame(x=' + x + ', y=' + y);
        if (grouped) {
            echo(', .groups=' + dataframe + '[[".groups"]]');
        }
        echo(')\n');
        echo('.df <- .df[complete.cases(.df),]\n');
        smooth = ' +\n\t# Legend\n\tscale_linetype("Regression model")';
    }
    if (linear) {
        if (grouped) {
            echo('.df.linear <- ddply(.df, ".groups", function(.df) predictions(lm(y~x, data=.df), seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence"))\n');
        } else {
            echo('.df.linear <- predictions(lm(y~x, data=.df), seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence")\n');
        }
        smooth += ' +\n\t# Linear model\n\tgeom_line(data=.df.linear, aes(x=x, y=pred.y' + smoothColor + ', linetype="Linear"))';
        model.push('Linear (' + yName + ' = a+b*' + xName + ')');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.linear, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
    }
    if (quadratic) {
        if (grouped) {
            echo('.df.quadratic <- ddply(.df,".groups",function(.df) predictions(lm(y~x+I(x^2),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence"))\n');
        } else {
            echo('.df.quadratic <- predictions(lm(y~x+I(x^2),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence")\n');
        }
        smooth += ' +\n\t# Cuadratic model\n\tgeom_line(data=.df.quadratic, aes(x=x, y=pred.y' + smoothColor + ',  linetype="Cuadratic"))';
        model.push('Quadratic (' + yName + ' = a+b*' + xName + '+c*' + xName + '<sup>2</sup>)');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.quadratic, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
    }
    if (cubic) {
        if (grouped) {
            echo('.df.cubic <- ddply(.df,".groups",function(.df) predictions(lm(y~x+I(x^2)+I(x^3),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence"))\n');
        } else {
            echo('.df.cubic <- predictions(lm(y~x+I(x^2)+I(x^3),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence")\n');
        }
        smooth += ' +\n\t# Cubic model\n\tgeom_line(data=.df.cubic, aes(x=x, y=pred.y' + smoothColor + ',  linetype="Cubic"))';
        model.push('Cubic (' + yName + ' = a+b*' + xName + '+c*' + xName + '<sup>2</sup>+d*' + xName + '<sup>3</sup>)');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.cubic, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
    }
    if (potential) {
        if (grouped) {
            echo('.df.potential <- ddply(.df,".groups",function(.df) predictions(lm(log(y)~log(x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence"))\n');
            echo('.df.potential[,-2:-1]=exp(.df.potential[,-2:-1])\n');
            echo('names(.df.potential)[3]="pred.y"\n');
        } else {
            echo('.df.potential <- predictions(lm(log(y)~log(x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence")\n');
            echo('.df.potential[,-1]=exp(.df.potential[,-1])\n');
            echo('names(.df.potential)[2]="pred.y"\n');
        }
        smooth += ' +\n\t# Potential model\n\tgeom_line(data=.df.potential, aes(x=x, y=pred.y' + smoothColor + ',  linetype="Potential"))';
        model.push('Potential (' + yName + ' = a*' + xName + '^b)');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.potential, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
    }
    if (exponential) {
        if (grouped) {
            echo('.df.exponential <- ddply(.df,".groups",function(.df) predictions(lm(log(y)~x,data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence"))\n');
            echo('.df.exponential[,-2:-1]=exp(.df.exponential[,-2:-1])\n');
            echo('names(.df.exponential)[3]="pred.y"\n');
        } else {
            echo('.df.exponential <- predictions(lm(log(y)~x,data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence")\n');
            echo('.df.exponential[,-1]=exp(.df.exponential[,-1])\n');
            echo('names(.df.exponential)[2]="pred.y"\n');
        }
        smooth += ' +\n\t# Exponential model\n\tgeom_line(data=.df.exponential, aes(x=x, y=pred.y' + smoothColor + ',  linetype="Exponential"))';
        model.push('Exponential (' + yName + ' = exp(a+b*' + xName + '))');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.exponential, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
    }
    if (logarithmic) {
        if (grouped) {
            echo('.df.logarithmic <- ddply(.df,".groups",function(.df) predictions(lm(y~log(x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence"))\n');
        } else {
            echo('.df.logarithmic <- predictions(lm(y~log(x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100), interval="confidence")\n');
        }
        smooth += ' +\n\t# Logarithmic model\n\tgeom_line(data=.df.logarithmic, aes(x=x, y=pred.y' + smoothColor + ',  linetype="Logarithmic"))';
        model.push('Logarithmic (' + yName + ' = a+b*log(' + xName + '))');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.logarithmic, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
    }
    if (inverse) {
        if (grouped) {
            echo('.df.inverse <- ddply(.df,".groups",function(.df) predictions(lm(y~I(1/x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100),interval="confidence"))\n');
        } else {
            echo('.df.inverse <- predictions(lm(y~I(1/x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100),interval="confidence")\n');
        }
        smooth += ' +\n\t# Inverse model\n\tgeom_line(data=.df.inverse, aes(x=x, y=pred.y' + smoothColor + ',  linetype="Inverse"))';
        model.push('Inverse (' + yName + ' = a+b/' + xName + ')');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.inverse, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
    }
    if (sigmoid) {
        if (grouped) {
            echo('.df.sigmoidal <- ddply(.df,".groups",function(.df) predictions(lm(log(y)~I(1/x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100),interval="confidence"))\n');
            echo('.df.sigmoidal[,-2:-1]=exp(.df.sigmoidal[,-2:-1])\n');
            echo('names(.df.sigmoidal)[3]="pred.y"\n');
        } else {
            echo('.df.sigmoidal <- predictions(lm(log(y)~I(1/x),data=.df),seq(min(.df[["x"]]), max(.df[["x"]]), length.out=100),interval="confidence")\n');
            echo('.df.sigmoidal[,-1]=exp(.df.sigmoidal[,-1])\n');
            echo('names(.df.sigmoidal)[2]="pred.y"\n');
        }
        smooth += ' +\n\t# Sigmoidal model\n\tgeom_line(data=.df.sigmoidal, aes(x=x, y=pred.y' + smoothColor + ',  linetype="Sigmoidal"))';
        model.push('Sigmoidal (' + yName + ' = exp(a+b/' + xName + '))');
        if (confidenceStrip) {
            smooth += ' +\n\tgeom_ribbon(data=.df.sigmoidal, aes(x=x, ymin=lwr.conf.int, ymax=upr.conf.int' + stripColor + ', alpha=0.3)';
        }
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
        header = new Header(i18n("Scatter plot of %1 on %2", yName, xName));
        header.add(i18n("Data frame"), dataframe);
        header.add(i18n("X variable"), xName);
        header.add(i18n("Y variable"), yName);
        if (regression) {
            header.add(i18n("Regression model(s)"), model.join(", "));
        }
        if (grouped) {
            header.add(i18n("Grouping variable(s)"), groupsName.join(", "));
        }
        if (filtered) {
            header.addFromUI("condition");
        }
        header.print();
        echo('rk.graph.on()\n');
    }
    echo('p <- ggplot(data = ' + dataframe + ') + geom_point(' + 'aes(x=' + xName + ', y=' + yName + pointColor + pointSymbol + pointSize + ')' + smooth + legend + facet + getString("plotOptions.code.calculate") + '\n');
    echo('print(p)\n');
    if (full) {
        echo('rk.graph.off()\n');
    }
}