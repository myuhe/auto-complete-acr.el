;;; auto-complete-acr.el --- auto-complete-mode extension for GNU R

;; Filename: auto-complete-acr.el
;; Description: auto-complete-mode extension for GNU R
;; Author: myuhe
;; Maintainer: myuhe
;; Copyright (C)  2009, myuhe  , all rights reserved.
;; Created: 2009-04-13 
;; Version: 0.3.2

;; URL: http://github.com/myuhe/auto-complete-acr.el
;; Keywords: auto-complete
;; Compatibility: GNU Emacs 23.1.1
;;
;; Features that might be required by this library:
;;
;; `auto-complete'
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Installation:
;;
;; Put auto-complete-acr.el to your load-path.
;; The load-path is usually ~/.emacs.d/.
;; It's set in your ~/.emacs like this:

;; (require 'auto-complete)
;; (require 'auto-complete-acr)

;;; Change log:
;;
;; 2009/04/13
;;      First released.
;; 2009/10/18
;;     use ess-get-object-list function
;; 2010/08/25
;;     add omni completion
;; 2010/08/27
;;     refactoring

;;; Acknowledgements:
;; I refered and hacked auto-complete-octave.el(http://www.emacswiki.org/emacs/auto-complete-octave.el)
;; thanks Yen-Chin,Lee!!

;; Require
(require 'auto-complete)

;;; Code:
(defconst acr-keywords
  (sort (list

;;;NAMESPACE:base
         "Bessel" "Defunct" "Deprecated" "LAPACK" "New-Internal" "Scripts" "TAOCP" "all.equal"
         "allnames" "aperm" "append" "apply" "array" "as" "assign" "attach" "attr" "autoload" "backquote" "backsolve" "bindenv" "builtins"
         "by" "callCC" "cat" "character" "chol" "colSums" "conditions" "conflicts"
         "connections" "constants" "contributors" "converters" "cut" "data.matrix" "dataframe" "dates" "datetime" "dcf" "debug" "delay" "det" "diag" 
         "diff" "dput" "dump" "duplicated" "dynload" "eapply" "eigen" "environment"
         "eval" "eval~" "exists" "expand.grid" "factor" "files" "findInt" "formals" "format" "frametools" "funprog" "get" "getenv"
         "gl" "grep" "identical" "ifelse" "interaction" "is" "jitter"
         "kappa" "kronecker" "labels" "lapply" "lazyload" "library" "license" "load" "locales"
         "lower.tri" "mapply" "match" "match.fun" "matrix" "max.col" "mean" "merge" "message"
         "methodsSupport" "mode" "namespace" "notyet" "options" "outer" "pairlist" "parse" "paste" "pmax"
         "pretty" "print" "qr" "quit" "range" "rank" "raw" "rep" "replace"
         "replicate" "rev" "rle" "rm" "rowsum" "sample" "sapply" "scale" "scan" "seq" "serialize"
         "sets" "sink" "solve" "sort" "source" "split" "srcfile" "stop" "structure" "strwrap"
         "summary" "svd" "sweep" "switch" "sys" "table" "tabulate" "tapply" "taskCallback" "temp"
         "time" "toString" "traceback" "unix" "unlist" "unname" "upper.tri"
         "userhooks" "utilities" "utils" "vector" "version" "warnings" "which" "windows"
         "withVisible" "write" "xor" "zapsmall" "zdatetime" "zdynvars" "zzz" "data.frame"

;;;NAMESPACE:other all
         "class.ind" "multinom" "nnet" "nnetHess" "which.is.max" "nnet.default" "nnet.formula" 
         "anova.trls" "anovalist.trls" "correlogram" "expcov" "gaucov" "Kaver" "Kenvl" "Kfn" 
         "plot.trls" "ppgetregion" "ppinit" "pplik" "ppregion" "predict.trls" "prmat" "Psim" 
         "semat" "sphercov" "SSI" "Strauss" "surf.gls" "surf.ls" "trls.influence" "trmat" 
         "variogram" "meanvar" "na.rpart" "path.rpart" "plotcp" "post" "printcp" "prune" 
         "prune.rpart" "rpart" "rpart.control" "rpconvert" "rsq.rpart" "snip.rpart" 
         "xpred.rpart" "abc.ci" "boot" "boot.array" "boot.ci" "censboot" "control" "corr" 
         "cum3" "cv.glm" "EEF.profile" "EL.profile" "empinf" "envelope" "exp.tilt" "freq.array" 
         "glm.diag" "glm.diag.plots" "imp.moments" "imp.prob" "imp.quantile" "imp.weights" 
         "inv.logit" "jack.after.boot" "k3.linear" "lik.CI" "linear.approx" "logit" "nested.corr" 
         "norm.ci" "saddle" "saddle.distn" "simplex" "smooth.f" "tilt.boot" "tsboot" "var.linear" 
         "histogram" "densityplot" "qqmath" "qq" "bwplot" "dotplot" "barchart" 
         "stripplot" "xyplot" "contourplot" "levelplot" "cloud" "wireframe" "splom" "parallel" 
         "tmd" "rfs" "oneway" "panel.histogram" "panel.densityplot" "panel.qqmath" "panel.qq" 
         "panel.bwplot" "panel.dotplot" "panel.barchart" "panel.stripplot" "panel.xyplot" 
         "panel.contourplot" "panel.levelplot" "panel.cloud" "panel.3dscatter" "panel.wireframe" 
         "panel.3dwire" "panel.pairs" "panel.splom" "diag.panel.splom" "panel.parallel" 
         "panel.tmd.default" "panel.tmd.qqmath" "panel.error" "panel.superpose" "panel.superpose.2" 
         "panel.violin" "panel.rug" "panel.average" "panel.linejoin" "panel.lmline" "panel.loess" 
         "panel.qqmathline" "panel.abline" "panel.refline" "panel.curve" "panel.fill" "panel.grid" 
         "panel.mathdensity" "panel.axis" "current.row" "current.column" "panel.number" 
         "packet.number" "which.packet" "trellis.currentLayout" "lplot.xy" "llines" "panel.lines" 
         "lpoints" "panel.points" "lsegments" "panel.segments" "ltext" "panel.arrows" "lrect" 
         "panel.text" "larrows" "panel.rect" "lpolygon" "panel.polygon" "prepanel.tmd.default" 
         "prepanel.tmd.qqmath" "prepanel.lmline" "prepanel.loess" "prepanel.qqmathline" 
         "prepanel.default.bwplot" "prepanel.default.histogram" "prepanel.default.qq" 
         "prepanel.default.xyplot" "prepanel.default.cloud" "prepanel.default.levelplot" 
         "prepanel.default.qqmath" "prepanel.default.densityplot" "prepanel.default.parallel" 
         "prepanel.default.splom" "shingle" "is.shingle" "as.shingle" "as.factorOrShingle" 
         "equal.count" "draw.colorkey" "draw.key" "simpleKey" "latticeParseFormula" 
         "ltransform3dMatrix" "ltransform3dto3d" "strip.default" "strip.custom" 
         "packet.panel.default" "axis.default" "xscale.components.default" "yscale.components.default" 
         "do.breaks" "make.groups" "banking" "Rows" "level.colors" "show.settings" "trellis.device" 
         "trellis.par.get" "trellis.par.set" "lattice.getOption" "lattice.options" "canonical.theme" 
         "standard.theme" "col.whitebg" "simpleTheme" "panel.identify" "panel.identify.qqmath" 
         "panel.identify.cloud" "panel.link.splom" "panel.brush.splom" "trellis.focus" 
         "trellis.unfocus" "trellis.switchFocus" "trellis.panelArgs" "trellis.vpname" 
         "trellis.grobname" "current.panel.limits" "trellis.last.object" "data.restore" "lookup.xport" 
         "read.arff" "read.dbf" "read.dta" "read.epiinfo" "read.mtp" "read.octave" "read.S" "read.spss" 
         "read.ssd" "read.systat" "read.xport" "write.arff" "write.dbf" "write.dta" "write.foreign" 
         "ACF" "allCoef" "asOneFormula" "asTable" "augPred" "balancedGrouped" "BIC" "coef<-" 
         "coefficients<-" "collapse" "compareFits" "comparePred" "corAR1" "corARMA" "corCAR1" 
         "corCompSymm" "corExp" "corFactor" "corGaus" "corIdent" "corLin" "corMatrix" "corNatural" 
         "corRatio" "corSpatial" "corSpher" "corSymm" "covariate<-" "Dim" "fdHess" "fixed.effects" 
         "fixef" "gapply" "getCovariate" "getCovariateFormula" "getData" "getGroups" 
         "getGroupsFormula" "getResponse" "getResponseFormula" "getVarCov" "gls" "glsControl" 
         "glsStruct" "gnls" "gnlsControl" "gnlsStruct" "groupedData" "gsummary" "Initialize" 
         "intervals" "isBalanced" "isInitialized" "LDEsysMat" "lme" "lmeControl" "lmeScale" 
         "lmeStruct" "lmList" "logDet" "matrix<-" "Names" "Names<-" "needUpdate" "nfGroupedData" "nlme" 
         "nlmeControl" "nlmeStruct" "nlsList" "nmGroupedData" "pdBlocked" "pdCompSymm" "pdConstruct" 
         "pdDiag" "pdFactor" "pdIdent" "pdLogChol" "pdMat" "pdMatrix" "pdNatural" "pdSymm" 
         "phenoModel" "pooledSD" "quinModel" "random.effects" "ranef" "recalc" "reStruct" 
         "simulate.lme" "splitFormula" "varComb" "varConstPower" "VarCorr" "varExp" "varFixed" 
         "varFunc" "varIdent" "Variogram" "varPower" "varWeights" "anova.lme" "nlsList.formula" 
         "plot.lme" "lme.formula" "lme.lmList" "lmList.formula" "nlme.formula" "nlme.nlsList" 
         "anova.gam" "coef.pdIdnot" "coef.pdTens" "corMatrix.pdIdnot" "cSplineDes" "Dim.pdIdnot" 
         "exclude.too.far" "extract.lme.cov" "extract.lme.cov2" "formXtViX" "full.score" 
         "formula.gam" "fixDependence" "fix.family.link" "fix.family.var" "gam" "gam2derivative" 
         "gam2objective" "gamm" "gam.check" "gam.control" "gam.fit3" "gam.fit" "gam.outer" 
         "gamm.setup" "gamSim" "influence.gam" "interpret.gam" "gam.setup" "gam.side" "gam.method" 
         "get.var" "logDet.pdIdnot" "initial.sp" "logLik.gam" "magic" "magic.post.proc" "mgcv" 
         "mgcv.control" "model.matrix.gam" "mono.con" "mroot" "negbin" "new.name" "notExp" 
         "notExp2" "notLog" "notLog2" "pcls" "null.space.dimension" "pdConstruct.pdIdnot" 
         "pdFactor.pdIdnot" "pdMatrix.pdIdnot" "pdIdnot" "pdConstruct.pdTens" "pdFactor.pdTens" 
         "pdMatrix.pdTens" "pdTens" "place.knots" "plot.gam" "print.anova.gam" "print.gam" 
         "print.summary.gam" "predict.gam" "PredictMat" "Predict.matrix" "Predict.matrix2" 
         "Predict.matrix.cr.smooth" "Predict.matrix.cs.smooth" "Predict.matrix.cyclic.smooth" 
         "Predict.matrix.tensor.smooth" "Predict.matrix.tprs.smooth" "Predict.matrix.ts.smooth" 
         "Predict.matrix.pspline.smooth" "residuals.gam" "s" "smoothCon" "smooth.construct" 
         "smooth.construct2" "smooth.construct.cc.smooth.spec" "smooth.construct.cr.smooth.spec" 
         "smooth.construct.cs.smooth.spec" "smooth.construct.tensor.smooth.spec" 
         "smooth.construct.tp.smooth.spec" "smooth.construct.ts.smooth.spec" 
         "smooth.construct.ps.smooth.spec" "smooth.construct.ad.smooth.spec" "summary.gam" 
         "solve.pdIdnot" "summary.pdIdnot" "summary.pdTens" "te" "tensor.prod.model.matrix" 
         "tensor.prod.penalties" "uniquecombs" "vcov.gam" "vis.gam" "Surv" "as.date" "attrassign" 
         "basehaz" "cch" "clogit" "cluster" "cox.zph" "coxph" "coxph.control" "coxph.detail" 
         "coxph.fit" "date.ddmmmyy" "date.mdy" "date.mmddyy" "date.mmddyyyy" "format.Surv" 
         "frailty" "frailty.gamma" "frailty.gammacon" "frailty.gaussian" "frailty.t" "is.Surv" 
         "is.date" "is.na.Surv" "is.na.coxph.penalty" "is.na.date" "is.na.ratetable" 
         "is.ratetable" "labels.survreg" "match.ratetable" "mdy.date" "pspline" 
         "pyears" "ratetable" "ridge" "strata" "survSplit" "survdiff" "survexp" "survfit" 
         "survfit.km" "survobrien" "survreg" "survreg.control" "survreg.fit" 
         "survreg.distributions" "survreg.old" "tcut" "untangle.specials" "walkCode" 
         "makeCodeWalker" "showTree" "makeConstantFolder" "constantFold" "isConstantValue" 
         "getAssignedVar" "makeLocalsCollector" "collectLocals" "findLocals" "findLocalsList" 
         "findFuncLocals" "flattenAssignment" "makeUsageCollector" "collectUsage" "findGlobals" 
         "checkUsage" "checkUsageEnv" "checkUsagePackage" "as.polySpline" "asVector" "backSpline" 
         "bs" "interpSpline" "ns" "periodicSpline" "polySpline" "spline.des" "splineDesign" 
         "splineKnots" "splineOrder" "xyVector" "mle" "Hershey" "as.graphicsAnnot" 
         "boxplot.stats" "check.options" "chull" "CIDFont" "col2rgb" "colors" "colours" "cm" 
         "colorRamp" "colorRampPalette" "contourLines" "convertColor" "colorConverter" 
         "colorspaces" "cm.colors" "devAskNewPage" "dev.control" "dev.copy" "dev.copy2eps" 
         "dev.copy2pdf" "dev.cur" "dev.interactive" "dev.list" "dev.new" "dev.next" "dev.off" 
         "dev.prev" "dev.print" "dev.set" "dev.size" "dev2bitmap" "deviceIsInteractive" "embedFonts" 
         "extendrange" "getGraphicsEvent" "graphics.off" "gray" "grey" "gray.colors" "grey.colors" 
         "heat.colors" "hsv" "hcl" "make.rgb" "n2mfrow" "nclass.Sturges" "nclass.FD" "nclass.scott" 
         "palette" "pdf" "pdf.options" "pdfFonts" "pictex" "postscript" "postscriptFont" 
         "postscriptFonts" "ps.options" "rainbow" "recordGraphics" "recordPlot" "replayPlot" 
         "rgb" "rgb2hsv" "savePlot" "setEPS" "setPS" "terrain.colors" "topo.colors" "trans3d" 
         "Type1Font" "xfig" "xyTable" "xy.coords" "xyz.coords" "X11" "x11" "bitmap" "bmp" "jpeg" 
         "png" "tiff" "clusplot" "pltree" "silhouette" "volume" "agnes" "clara" "daisy" "diana" 
         "fanny" "mona" "pam" "bannerplot" "ellipsoidhull" "ellipsoidPoints" 
         "lower.to.upper.tri.inds" "upper.to.lower.tri.inds" "meanabsdev" "sizeDiss" 
         "sortSilhouette" "predict.ellipsoid" "coef.hclust" "addterm" "area" "as.fractions" 
         "bandwidth.nrd" "bcv" "boxcox" "con2tr" "contr.sdif" "corresp" "cov.trob" "denumerate" 
         "dose.p" "dropterm" "enlist" "eqscplot" "fbeta" "fitdistr" "fractions" "frequency.polygon" 
         "gamma.dispersion" "gamma.shape" "ginv" "glm.convert" "glm.nb" "glmmPQL" "hist.FD" 
         "hist.scott" "huber" "hubers" "is.fractions" "isoMDS" "kde2d" "lda" "ldahist" 
         "lm.gls" "lm.ridge" "lmwork" "loglm" "loglm1" "logtrans" "mca" "mvrnorm" "nclass.freq" 
         "neg.bin" "negative.binomial" "negexp.SSival" "Null" "parcoord" "polr" "psi.bisquare" 
         "psi.hampel" "psi.huber" "qda" "rational" "renumerate" "rlm" "rms.curv" "rnegbin" "sammon" 
         "select" "Shepard" "stdres" "stepAIC" "studres" "theta.md" "theta.ml" "theta.mm" 
         "truehist" "ucv" "width.SJ" "write.matrix" "cov.mcd" "cov.mve" "cov.rob" "lmsreg" 
         "lqs" "lqs.formula" "ltsreg" "denumerate.formula" "renumerate.formula" 
         ".checkMFClasses" ".getXlevels" ".MFclass" "add.scope" "add1" "addmargins" "aggregate" 
         "aggregate.data.frame" "aggregate.default" "aggregate.ts" "AIC" "alias" "anova" 
         "anova.glm" "anova.glmlist" "anova.lm" "anova.lmlist" "anova.mlm" "aov" "approx" 
         "approxfun" "as.formula" "as.ts" "ave" "binomial" "bw.bcv" "bw.nrd" "bw.nrd0" 
         "bw.SJ" "bw.ucv" "C" "case.names" "coef" "coefficients" "complete.cases" "confint" 
         "confint.default" "constrOptim" "contr.SAS" "contr.helmert" "contr.poly" "contr.sum" 
         "contr.treatment" "contrasts" "contrasts<-" "convolve" "cooks.distance" "cor" "cov" 
         "cov.wt" "cov2cor" "covratio" "cycle" "D" "dbeta" "dbinom" "dcauchy" "dchisq" 
         "delete.response" "deltat" "density" "density.default" "deriv" "deriv.default" 
         "deriv.formula" "deriv3" "deriv3.default" "deriv3.formula" "deviance" "dexp" "df" 
         "df.residual" "dfbeta" "dfbetas" "dffits" "dgamma" "dgeom" "dhyper" "diff.ts" 
         "dlnorm" "dlogis" "dmultinom" "dnbinom" "dnorm" "dpois" "drop.scope" "drop.terms" 
         "drop1" "dsignrank" "dt" "dummy.coef" "dunif" "dweibull" "dwilcox" "eff.aovlist" 
         "effects" "estVar" "end" "expand.model.frame" "extractAIC" "factor.scope" "family" 
         "fft" "fitted" "fitted.values" "fivenum" "formula" "frequency" "ftable" "Gamma" 
         "gaussian" "get_all_vars" "glm" "glm.control" "glm.fit" "hasTsp" "hat" "hatvalues" 
         "hatvalues.lm" "influence" "influence.measures" "integrate" "interaction.plot" 
         "inverse.gaussian" "IQR" "is.empty.model" "is.mts" "is.ts" "lines.ts" "lm" 
         "lm.fit" "lm.influence" "lm.wfit" "logLik" "loglin" "lowess" "ls.diag" "ls.print" 
         "lsfit" "mad" "mahalanobis" "make.link" "makepredictcall" "manova" "mauchly.test" 
         "median" "model.extract" "model.frame" "model.frame.aovlist" "model.frame.default" 
         "model.frame.glm" "model.frame.lm" "model.matrix" "model.matrix.default" 
         "model.matrix.lm" "model.offset" "model.response" "model.tables" "model.weights" 
         "mvfft" "na.action" "na.exclude" "na.fail" "na.omit" "na.pass" "napredict" 
         "naprint" "naresid" "nextn" "nlm" "nlminb" "offset" "optim" "optimise" 
         "optimize" "p.adjust" "p.adjust.methods" "pbeta" "pbinom" "pcauchy" "pchisq" 
         "pexp" "pf" "pgamma" "pgeom" "phyper" "plnorm" "plogis" "plot.density" 
         "plot.lm" "plot.mlm" "plot.ts" "plot.TukeyHSD" "pnbinom" "pnorm" "poisson" 
         "poly" "polym" "power" "ppoints" "ppois" "predict" "predict.glm" "predict.lm" 
         "predict.mlm" "predict.poly" "preplot" "printCoefmat" "print.anova" "print.density" 
         "print.family" "print.formula" "print.ftable" "print.glm" "print.infl" 
         "print.integrate" "print.lm" "print.logLik" "print.terms" "print.ts" "profile" 
         "proj" "psignrank" "pt" "ptukey" "punif" "pweibull" "pwilcox" "qbeta" "qbinom" 
         "qcauchy" "qchisq" "qexp" "qf" "qgamma" "qgeom" "qhyper" "qlnorm" "qlogis" "qnbinom" 
         "qnorm" "qpois" "qqline" "qqnorm" "qqnorm.default" "qqplot" "qsignrank" "qt" "qtukey" 
         "quantile" "quantile.default" "quasi" "quasibinomial" "quasipoisson" "qunif" "qweibull" 
         "qwilcox" "r2dtable" "rbeta" "rbinom" "rcauchy" "rchisq" "read.ftable" 
         "reformulate" "relevel" "replications" "reshape" "resid" "residuals" 
         "residuals.default" "residuals.glm" "residuals.lm" "rexp" "rf" "rgamma" "rgeom" 
         "rhyper" "rlnorm" "rlogis" "rmultinom" "rnbinom" "rnorm" "rpois" "rsignrank" 
         "rstandard" "rstandard.glm" "rstandard.lm" "rstudent" "rstudent.glm" "rstudent.lm" 
         "rt" "runif" "rweibull" "rwilcox" "sd" "se.contrast" "simulate" "spline" 
         "splinefun" "splinefunH" "SSD" "start" "stat.anova" "step" "summary.aov" 
         "summary.aovlist" "summary.glm" "summary.infl" "summary.lm" "summary.manova" 
         "summary.mlm" "symnum" "termplot" "terms" "terms.aovlist" "terms.default" 
         "terms.formula" "terms.terms" "time" "ts" "tsp" "tsp<-" "TukeyHSD" "TukeyHSD.aov" 
         "uniroot" "update" "update.default" "update.formula" "var" "variable.names" "vcov" 
         "weighted.mean" "weighted.residuals" "weights" "window" "window<-" "write.ftable" 
         "xtabs" "pbirthday" "qbirthday" "median.default" "mauchley.test" "reshapeWide" 
         "reshapeLong" "print.coefmat" "anovalist.lm" "lm.fit.null" "lm.wfit.null" 
         "glm.fit.null" "pairwise.table" "line" "medpolish" "smooth" "isoreg" "ksmooth" 
         "loess" "loess.control" "loess.smooth" "ppr" "runmed" "scatter.smooth" "smooth.spline" 
         "smoothEnds" "supsmu" "as.dist" "as.hclust" "biplot" "cancor" "cmdscale" 
         "cophenetic" "cutree" "dist" "factanal" "hclust" "kmeans" "loadings" "plclust" 
         "prcomp" "princomp" "promax" "rect.hclust" "screeplot" "varimax" "as.dendrogram" 
         "dendrapply" "heatmap" "is.leaf" "order.dendrogram" "reorder" "NLSstAsymptotic" 
         "NLSstClosestX" "NLSstLfAsymptote" "NLSstRtAsymptote" "SSasymp" 
         "SSasympOff" "SSasympOrig" "SSbiexp" "SSfol" "SSfpl" "SSgompertz" "SSlogis" 
         "SSmicmen" "SSweibull" "asOneSidedFormula" "clearNames" "getInitial" "nls" 
         "nls.control" "numericDeriv" "selfStart" "setNames" "sortedXyData" "acf" 
         "acf2AR" "ar" "ar.burg" "ar.mle" "ar.ols" "ar.yw" "arima" "arima.sim" 
         "arima0" "arima0.diag" "ARMAacf" "ARMAtoMA" "bandwidth.kernel" "ccf" 
         "cpgram" "decompose" "df.kernel" "diffinv" "embed" "filter" "HoltWinters" 
         "is.tskernel" "KalmanForecast" "KalmanLike" "KalmanRun" "KalmanSmooth" 
         "kernapply" "kernel" "lag" "lag.plot" "makeARIMA" "monthplot" "na.contiguous" 
         "pacf" "plot.spec" "plot.spec.coherency" "plot.spec.phase" "spec.ar" 
         "spec.pgram" "spec.taper" "spectrum" "stl" "StructTS" "toeplitz" 
         "ts.intersect" "ts.plot" "ts.union" "tsdiag" "tsSmooth" "as.stepfun" "ecdf" 
         "is.stepfun" "knots" "plot.ecdf" "plot.stepfun" "stepfun" "summary.stepfun" 
         "arcCurvature" "arrow" "convertUnit" "convertX" "convertY" "convertWidth" 
         "convertHeight" "convertNative" "grid.convertWidth" "grid.convertX" "grid.convertY" 
         "grid.convert" "grid.convertHeight" "current.viewport" "current.vpPath" 
         "current.vpTree" "current.transform" "engine.display.list" "gpar" "get.gpar" 
         "grid.collection" "grid.copy" "grid.display.list" "grid.draw" "grid.record" 
         "grid.grill" "grid.layout" "grid.locator" "grid.newpage" "grid.plot.and.legend" 
         "grid.pretty" "grid.prompt" "grid.refresh" "grid.show.layout" "grid.show.viewport" 
         "unit" "is.unit" "unit.c" "unit.length" "unit.pmax" "unit.pmin" "unit.rep" 
         "stringWidth" "stringHeight" "grobX" "grobY" "grobWidth" "grobHeight" 
         "absolute.size" "viewport" "vpList" "vpStack" "vpTree" "vpPath" "pop.viewport" 
         "push.viewport" "popViewport" "pushViewport" "dataViewport" "plotViewport" 
         "downViewport" "upViewport" "seekViewport" "drawDetails" "preDrawDetails" 
         "postDrawDetails" "editDetails" "validDetails" "heightDetails" "widthDetails" 
         "xDetails" "yDetails" "grid.grob" "grob" "gList" "gTree" "gPath" "grobTree" 
         "grobName" "grid.get" "grid.set" "grid.add" "grid.remove" "grid.edit" "grid.gedit" 
         "grid.gremove" "grid.gget" "getGrob" "setGrob" "addGrob" "removeGrob" "editGrob" 
         "gEdit" "gEditList" "applyEdit" "applyEdits" "grid.grab" "grid.grabExpr" 
         "setChildren" "grid.ls" "nestedListing" "pathListing" "grobPathListing" "getNames" 
         "childNames" "grid.arrows" "grid.circle" "grid.clip" "grid.curve" "grid.line.to" 
         "grid.lines" "grid.move.to" "grid.null" "grid.points" "grid.polygon" 
         "grid.polyline" "grid.rect" "grid.segments" "grid.text" "grid.xspline" 
         "arrowsGrob" "circleGrob" "clipGrob" "curveGrob" "linesGrob" "lineToGrob" 
         "moveToGrob" "nullGrob" "pointsGrob" "polygonGrob" "polylineGrob" "rectGrob" 
         "segmentsGrob" "textGrob" "xsplineGrob" "grid.xaxis" "grid.yaxis" "xaxisGrob" 
         "yaxisGrob" "grid.frame" "grid.pack" "grid.place" "frameGrob" "packGrob" "placeGrob" 
         "draw.details" "grid.legend" "grid.multipanel" "grid.panel" "grid.strip" "layoutRegion" 
         "layout.widths" "layout.heights" "viewport.layout" "viewport.transform" "layout.torture" 
         "addTclPath" "as.tclObj" "is.tclObj" "is.tkwin" "tclfile.dir" "tclfile.tail" 
         "getTkProgressBar" "setTkProgressBar" "batchSOM" "condense" "knn" "knn.cv" 
         "knn1" "lvq1" "lvq2" "lvq3" "lvqinit" "lvqtest" "multiedit" "olvq1" "reduce.nn" 
         "SOM" "somgrid" "abline" "arrows" "assocplot" "axTicks" "Axis" "axis" "axis.Date" 
         "axis.POSIXct" "barplot" "barplot" "box" "boxplot" "bxp" "cdplot" "clip" 
         "close.screen" "co.intervals" "contour" "coplot" "curve" "dotchart" "erase.screen" 
         "filled.contour" "fourfoldplot" "frame" "grconvertX" "grconvertY" "grid" "hist" 
         "hist.default" "identify" "image" "layout" "layout.show" "lcm" "legend" "lines" 
         "lines.default" "locator" "matlines" "matplot" "matpoints" "mosaicplot" "mtext" 
         "pairs" "panel.smooth" "par" "persp" "pie" "plot" "plot.design" "plot.new" 
         "plot.window" "plot.xy" "points" "polygon" "rect" "rug" "screen" "segments" 
         "spineplot" "split.screen" "stars" "stem" "strheight" "stripchart" "strwidth" 
         "sunflowerplot" "symbols" "text" "title" "xinch" "xspline" "xyinch" "yinch" 
         "barplot.default" "boxplot.default" "contour.default" "image.default" "pairs.default" 
         "plot.default" "points.default" "text.default" "piechart" "Rdindex" "checkDocFiles" 
         "checkDocStyle" "checkFF" "checkMD5sums" "checkReplaceFuns" "checkS3methods" 
         "checkTnF" "checkVignettes" "codoc" "codocClasses" "codocData" "md5sum" "undoc" 
         "file_path_as_absolute" "file_path_sans_ext" "list_files_with_exts" 
         "list_files_with_type" "showNonASCII" "delimMatch" "texi2dvi" "buildVignettes" 
         "pkgVignettes" "pkgDepends" "getDepList" "installFoundDepends" "vignetteDepends" 
         "write_PACKAGES" "xgettext" "xgettext2pot" "xngettext" "encoded_text_to_latex" 
         "charset_to_Unicode" "Adobe_glyphs" "checkNEWS" "readNEWS" "Rd_db" "Rd_parse" 
         "package.dependencies" "read.00Index" "bkde" "bkde2D" "bkfe" "dpih" "dpik" "dpill" 
         "locpoly" ".Last.lib" ".TraceWithMethods" ".untracedFunction" ".doTracePrint" 
         ".valueClassTest" "@<-" "Arith" "Compare" "Complex" "Logic" "Math" "Math2" 
         "MethodAddCoerce" "MethodsList" "MethodsListSelect" "Ops" "Quote" "SignatureMethod" 
         "S3Class" "S3Class<-" "S3Part" "S3Part<-" "Summary" "addNextMethod" "allGenerics" 
         "allNames" "as" "as<-" "asMethodDefinition" "assignClassDef" "assignMethodsMetaData" 
         "balanceMethodsList" "body<-" "cacheGenericsMetaData" "cacheMetaData" "cacheMethod" 
         "callGeneric" "callNextMethod" "canCoerce" "checkSlotAssignment" "classMetaName" 
         "coerce" "coerce<-" "completeClassDefinition" "completeExtends" "completeSubclasses" 
         "conformMethod" "defaultDumpName" "defaultPrototype" "doPrimitiveMethod" "dumpMethod" 
         "dumpMethods" "el" "el<-" "elNamed" "elNamed<-" "empty.dump" "emptyMethodsList" 
         "existsFunction" "existsMethod" "extends" "finalDefaultMethod" "findClass" 
         "findFunction" "findMethod" "findMethods" "findMethodSignatures" "findUnique" 
         "fixPre1.8" "formalArgs" "functionBody" "functionBody<-" "generic.skeleton" 
         "getAccess" "getAllMethods" "getAllSuperClasses" "getClass" "getClassDef" 
         "getClassName" "getClassPackage" "getClasses" "getDataPart" "getExtends" 
         "getFunction" "getGeneric" "getGenerics" "getGroup" "getGroupMembers" 
         "getMethod" "getMethods" "getMethodsForDispatch" "getMethodsMetaData" 
         "getPackageName" "getProperties" "getPrototype" "getSlots" "getSubclasses" 
         "getValidity" "getVirtual" "hasArg" "hasMethod" "hasMethods" "initialize" 
         "insertMethod" "is" "isClass" "isClassDef" "isClassUnion" "isGeneric" "isGrammarSymbol" 
         "isGroup" "isSealedClass" "isSealedMethod" "isVirtualClass" "isXS3Class" 
         "languageEl" "languageEl<-" "linearizeMlist" "listFromMethods" "listFromMlist" 
         "loadMethod" "makeClassRepresentation" "makeExtends" "makeGeneric" "makeMethodsList" 
         "makePrototypeFromClassDef" "makeStandardGeneric" "matchSignature" "mergeMethods" 
         "metaNameUndo" "methodSignatureMatrix" "methodsPackageMetaName" "method.skeleton" 
         "missingArg" "mlistMetaName" "new" "newBasic" "newClassRepresentation" "newEmptyObject" 
         "packageSlot" "packageSlot<-" "possibleExtends" "promptClass" "promptMethods" 
         "prototype" "reconcilePropertiesAndPrototype" "rematchDefinition" "removeClass" 
         "removeGeneric" "removeMethod" "removeMethods" "removeMethodsObject" "representation" 
         "requireMethods" "resetClass" "resetGeneric" "sealClass" "selectMethod" "seemsS4Object" 
         "sessionData" "setAs" "setClass" "setClassUnion" "setDataPart" "setGeneric" 
         "setGroupGeneric" "setIs" "setMethod" "setOldClass" "setPackageName" "setPrimitiveMethods" 
         "setReplaceMethod" "setValidity" "show" "showClass" "showDefault" "showExtends" 
         "showMethods" "showMlist" "sigToEnv" "signature" "slot" "slot<-" "slotNames" ".slotNames" 
         "slotsFromS3" "substituteDirect" "substituteFunctionArgs" "superClassDepth" "testVirtual" 
         "traceOff" "traceOn" "tryNew" "trySilent" "unRematchDefinition" "validObject" "validSlotNames" 
         ".ShortPrimitiveSkeletons" ".EmptyPrimitiveSkeletons" "cbind2" "rbind2" "implicitGeneric" 
         "setGenericImplicit" "prohibitGeneric" "registerImplicitGenerics" "?" "CRAN.packages" 
         "Rprof" "Rprofmem" "RShowDoc" "RSiteSearch" "URLdecode" "URLencode" "View" "alarm" "apropos" 
         "argsAnywhere" "assignInNamespace" "as.roman" "as.person" "as.personList" "as.relistable" 
         "available.packages" "browseEnv" "browseURL" "browseVignettes" "bug.report" "capture.output" 
         "checkCRAN" "chooseCRANmirror" "citation" "citEntry" "citHeader" "citFooter" "close.socket" 
         "combn" "compareVersion" "contrib.url" "count.fields" "data" "data.entry" "dataentry" "de" 
         "de.ncols" "de.restore" "de.setup" "debugger" "demo" "download.file" "download.packages" 
         "dump.frames" "edit" "emacs" "example" "file_test" "file.edit" "find" "fix" "fixInNamespace" 
         "flush.console" "formatOL" "formatUL" "getAnywhere" "getCRANmirrors" "getFromNamespace" 
         "getS3method" "glob2rx" "head" "head.matrix" "help" "help.request" "help.search" "help.start" 
         "history" "index.search" "install.packages" "installed.packages" "is.relistable" 
         "limitedLabels" "loadhistory" "localeToCharset" "ls.str" "lsf.str" "make.packages.html" 
         "make.socket" "memory.limit" "memory.size" "menu" "methods" "mirror2html" "modifyList" 
         "new.packages" "normalizePath" "object.size" "old.packages" "package.contents" 
         "package.skeleton" "packageDescription" "packageStatus" "page" "person" "personList" 
         "pico" "prompt" "promptData" "promptPackage" "rc.getOption" "rc.options" "rc.settings" 
         "rc.status" "readCitationFile" "read.DIF" "read.csv" "read.csv2" "read.delim" "read.delim2" 
         "read.fwf" "read.fortran" "read.socket" "read.table" "recover" "relist" "remove.packages" 
         "savehistory" "select.list" "sessionInfo" "setRepositories" "stack" "str" "strOptions" 
         "summaryRprof" "tail" "tail.matrix" "timestamp" "toBibtex" "toLatex" "type.convert" 
         "unstack" "update.packageStatus" "update.packages" "upgrade" "url.show" "vi" "vignette" 
         "withVisible" "write.csv" "write.csv2" "write.socket" "write.table" "wsbrowser" "xedit" 
         "xemacs" "zip.file.extract" "txtProgressBar" "getTxtProgressBar" "setTxtProgressBar" 
         "Rtangle" "RtangleSetup" "RweaveLatex" "RweaveLatexSetup" "Stangle" "Sweave" "SweaveSyntConv" 
         "SweaveSyntaxLatex" "SweaveSyntaxNoweb" "RtangleWritedoc" "RweaveLatexOptions" 
         "RweaveChunkPrefix" "RweaveEvalWithOpt" "RweaveTryStop" "SweaveHooks" "RweaveLatexWritedoc" 
         "RweaveLatexOptions" "RweaveLatexFinish" "makeRweaveLatexCodeRunner" "nsl" "DLL.version" 
         "Filters" "choose.dir" "choose.files" "fixup.libraries.URLs" "fixup.package.URLs" 
         "getClipboardFormats" "getIdentification" "getWindowsHandle" "getWindowTitle" 
         "link.html.help" "loadRconsole" "make.search.html" "readClipboard" "readRegistry" 
         "setStatusBar" "setWindowTitle" "shortPathName" "win.version" "winDialog" "winDialogString" 
         "winMenuAdd" "winMenuAddItem" "winMenuDel" "winMenuDelItem" "winMenuNames" "winMenuItems" 
         "writeClipboard" "zip.unpack" "winProgressBar" "getWinProgressBar" "setWinProgressBar"
         ) #'(lambda (a b) (< (length a) (length b)))))

(defvar ac-source-acr
  '((candidates
     . (lambda ()
         (all-completions ac-target acr-keywords))))
  "Source for r keywords.")

(defvar ac-source-essacr
  '((init
     . (lambda ()
         (setq acr-completions-cache ess-local-process-name)))
    (candidates . (lambda ()
                    (condition-case nil
                        (all-completions ac-target (ess-get-object-list acr-completions-cache))
                      (error nil))))))

(defun ac-internal-complete-object-name (&optional listcomp)

  "This function base on \\[ess-internal-complete-object-name].
In detail, See `ess-inf.el'
Use \\[ess-resynch] to re-read the names of the attached directories.
This is done automatically (and transparently) if a directory is
modified (S only!), so the most up-to-date list of object names is always
available.  However attached dataframes are *not* updated, so this
command may be necessary if you modify an attached dataframe."
  (interactive "P");; FIXME : the `listcomp' argument is NOT used
  (ess-make-buffer-current)
  (if (memq (char-syntax (preceding-char)) '(?w ?_))
      (let* ((comint-completion-addsuffix nil)
             (end (point))
             (buffer-syntax (syntax-table))
             (beg (unwind-protect
                      (save-excursion
                        (set-syntax-table ess-mode-syntax-table)
                        (backward-sexp 1)
                        (point))
                    (set-syntax-table buffer-syntax)))
             (full-prefix (buffer-substring beg end))
             (pattern full-prefix)
             ;; See if we're indexing a list with `$'
             (listname
              (if (string-match "\\(.+\\)\\$\\(\\(\\sw\\|\\s_\\)*\\)$"
                                full-prefix)
                  (progn
                    (setq pattern
                          (if (not (match-beginning 2)) ""
                            (substring full-prefix
                                       (match-beginning 2)
                                       (match-end 2))))
                    (substring full-prefix (match-beginning 1)
                               (match-end 1)))))
             ;; are we trying to get a slot via `@' ?
             (classname
              (if (string-match "\\(.+\\)@\\(\\(\\sw\\|\\s_\\)*\\)$"
                                full-prefix)
                  (progn
                    (setq pattern
                          (if (not (match-beginning 2)) ""
                            (substring full-prefix
                                       (match-beginning 2)
                                       (match-end 2))))
                    (substring full-prefix (match-beginning 1)
                               (match-end 1)))))
             (components 
              (if listname
                  (ess-object-names listname)
                (if classname
                    (ess-slot-names classname)
                  ;; Default case: It hangs here when
                  ;;    options(error=recoves) :
                  (ess-get-object-list ess-current-process-name)))))
        ;; always return a non-nil value to prevent history expansions
        ;;(append classname components)
        components
        )))

(defvar ac-source-omni-list-essacr
  '(
    (prefix . "\\$\\(.*\\)")
    (init . ac-internal-complete-object-name)
    (requires . 0)
    (candidates . ac-internal-complete-object-name)
    (cache)))

(defvar ac-source-omni-class-essacr
  '(
    (prefix . "\\@\\(.*\\)")
    (init . ac-internal-complete-object-name)
    (requires . 0)
    (candidates . ac-internal-complete-object-name)
    (cache)))

(add-hook 'ess-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (setq ac-sources 
                  '(ac-source-omni-list-essacr
                    ac-source-omni-class-essacr
                    ac-source-filename 
                    ac-source-yasnippet 
                    ac-source-essacr 
                    ac-source-acr 
                    ac-source-words-in-same-mode-buffers))))

(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (setq ac-sources 
                  '(ac-source-omni-list-essacr
                    ac-source-omni-class-essacr
                    ac-source-filename
                    ac-source-yasnippet 
                    ac-source-essacr
                    ac-source-acr
                    ac-source-words-in-same-mode-buffers))))

(provide 'auto-complete-acr)