;;; auto-complete-acr.el --- auto-complete-mode extension for GNU R

;; Filename: auto-complete-acr.el
;; Description: auto-complete-mode extension for GNU R
;; Author: myuhe
;; Maintainer: myuhe
;; Copyright (C)  2009, myuhe  , all rights reserved.
;; Created: 2009-04-13 
;; Version: 0.1
;; Last-Updated: 2009-03-01 14:25:33
;;           By: Yen-Chin,Lee
;; URL: http://www.emacswiki.org/emacs/download/auto-complete-octave.el
;; Keywords: auto-complete
;; Compatibility: GNU Emacs 23.0.91
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
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:


;;; Change log:
;;
;; 2009/04/13
;;      First released.
;;

;;; Acknowledgements:
;; I refered and hacked auto-complete-acr.el(http://www.emacswiki.org/emacs/auto-complete-octave.el)
;; thanks Yen-Chin,Lee!!
;;

;;; TODO
;; add many NAMESPACE files
;; and more...
;;


;; Require
(require 'auto-complete)


;;; Code:
(defconst acr-keywords
   (sort (list
	  "add.scope" "add1" "addmargins" "aggregate" "aggregate.data.frame" "testaggregate.default"
	  "aggregate.ts" "AIC" "alias" "anova" "anova.glm" "testanova.glmlist" "anova.lm" "anova.lmlist"
	  "anova.mlm" "aov" "approx" "testapproxfun" "as.formula" "as.ts" "ave"
	  "binomial" "bw.bcv" "bw.nrd" "testbw.nrd0" "bw.SJ" "bw.ucv"
	  "C" "case.names" "coef" "coefficients" "testcomplete.cases" "confint" "confint.default"
	  "constrOptim" "testcontr.SAS" "contr.helmert" "contr.poly" "contr.sum" "testcontr.treatment"
	  "contrasts" "convolve" "testcooks.distance" "cor" "cov" "cov.wt" "cov2cor"
	  "covratio" "cycle"
	  "D" "testdbeta" "dbinom" "dcauchy" "dchisq" "delete.response" "deltat" "testdensity"
	  "density.default" "deriv" "deriv.default" "deriv.formula"
	  "testderiv3" "deriv3.default" "deriv3.formula" "deviance"
	  "dexp" "df" "testdf.residual" "dfbeta" "dfbetas" "dffits" "dgamma" "dgeom" "dhyper"
	  "testdiff.ts" "dlnorm" "dlogis" "dmultinom" "dnbinom" "dnorm" "dpois" "testdrop.scope"
	  "drop.terms" "drop1" "dsignrank" "dt" "dummy.coef" "testdunif" "dweibull" "dwilcox"
	  "eff.aovlist" "effects" "estVar" "end" "testexpand.model.frame" "extractAIC"
	  "factor.scope" "family" "fft" "testfitted" "fitted.values" "fivenum" "formula" "frequency" "ftable"
	  "testGamma" "gaussian" "get_all_vars" "glm" "glm.control" "glm.fit" "testhasTsp" "hat"
	  "hatvalues" "hatvalues.lm" "influence" "testinfluence.measures" "integrate"
	  "interaction.plot" "testinverse.gaussian"
	  "IQR" "is.empty.model" "is.mts" "is.ts"
	  "lines.ts" "testlm" "lm.fit" "lm.influence" "lm.wfit" "logLik" "loglin" "lowess" "testls.diag"
	  "ls.print" "lsfit"
	  "mad" "mahalanobis" "make.link" "testmakepredictcall" "manova" "mauchly.test" "median"
	  "model.extract" "testmodel.frame" "model.frame.aovlist" "model.frame.default"
	  "testmodel.frame.glm" "model.frame.lm" "model.matrix" "testmodel.matrix.default"
	  "model.matrix.lm" "model.offset" "testmodel.response" "model.tables" "model.weights" "mvfft"
	  "na.action" "testna.exclude" "na.fail" "na.omit" "na.pass" "napredict" "naprint" "testnaresid"
	  "nextn" "nlm" "nlminb"
	  "offset" "optim" "optimise" "optimize"
	  "testp.adjust" "p.adjust.methods" "pbeta" "pbinom" "pcauchy" "pchisq" "testpexp" "pf" "pgamma"
	  "pgeom" "phyper" "plnorm" "plogis" "plot.density" "testplot.lm" "plot.mlm" "plot.ts"
	  "plot.TukeyHSD" "pnbinom" "pnorm" "testpoisson" "poly" "polym" "power" "ppoints" "ppois" "predict"
	  "testpredict.glm" "predict.lm" "predict.mlm" "predict.poly" "preplot" "testprintCoefmat"
	  "print.anova" "print.density" "print.family" "testprint.formula" "print.ftable" "print.glm"
	  "print.infl" "testprint.integrate" "print.lm" "print.logLik" "print.terms" "print.ts"
	  "testprofile" "proj" "psignrank" "pt" "ptukey" "punif" "pweibull" "pwilcox"
	  "testqbeta" "qbinom" "qcauchy" "qchisq" "qexp" "qf" "qgamma" "qgeom" "testqhyper" "qlnorm"
	  "qlogis" "qnbinom" "qnorm" "qpois" "qqline" "qqnorm" "testqqnorm.default" "qqplot" "qsignrank"
	  "qt" "qtukey" "quantile" "testquantile.default" "quasi" "quasibinomial" "quasipoisson"
	  "qunif" "testqweibull" "qwilcox"
	  "r2dtable" "rbeta" "rbinom" "rcauchy" "rchisq" "testread.ftable" "reformulate" "relevel"
	  "replications" "reshape" "testresid" "residuals" "residuals.default" "residuals.glm"
	  "testresiduals.lm" "rexp" "rf" "rgamma" "rgeom" "rhyper" "rlnorm" "rlogis" "testrmultinom"
	  "rnbinom" "rnorm" "rpois" "rsignrank" "rstandard" "testrstandard.glm" "rstandard.lm"
	  "rstudent" "rstudent.glm" "testrstudent.lm" "rt" "runif" "rweibull" "rwilcox"
	  "sd" "se.contrast" "testsimulate" "spline" "splinefun" "splinefunH" "SSD" "start"
	  "stat.anova" "step" "testsummary.aov" "summary.aovlist" "summary.glm" "summary.infl"
	  "testsummary.lm" "summary.manova" "summary.mlm" "symnum"
	  "termplot" "testterms" "terms.aovlist" "terms.default" "terms.formula" "testterms.terms"
	  "time" "ts" "tsp" "tsp<-" "TukeyHSD" "TukeyHSD.aov" "testuniroot"
	  "update" "update.default" "update.formula"
	  "var" "testvariable.names" "vcov"
	  "weighted.mean" "weighted.residuals" "testweights" "window" "window<-" "write.ftable"
	  "xtabs" "pbirthday" "testqbirthdaytest"
	  ) #'(lambda (a b) (< (length a) (length b)))))

(defvar ac-source-acr
      '((candidates
         . (lambda ()
             (all-completions ac-target acr-keywords))))
      "Source for r keywords.")

(add-hook 'ess-mode-hook
    (lambda ()
     (make-local-variable 'ac-sources)
      (setq ac-sources '(
            ac-source-words-in-buffer ac-source-acr
))))

(add-hook 'inferior-ess-mode-hook
    (lambda ()
     (make-local-variable 'ac-sources)
      (setq ac-sources '(
            ac-source-words-in-buffer ac-source-acr
))))
;; ---------------------------------------------------------
(provide 'auto-complete-acr)

