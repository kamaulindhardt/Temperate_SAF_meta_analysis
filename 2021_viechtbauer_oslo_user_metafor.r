############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

# look at the BCG dataset
dat.bcg

# data are in the form of 2x2 tables:
#
#           TB+   TB-
# treated   tpos  tneg
# control   cpos  cneg

# calculate log risk ratios and corresponding sampling variances
# note: slab for adding 'study labels' to the dataset
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)
dat

# random-effects model (using log risk ratios and variances as input)
res <- rma(yi, vi, data=dat)
res

# predicted pooled risk ratio and corresponding CI/PI
predict(res, transf=exp, digits=2)

# forest plot
forest(res)
forest(res, addpred=TRUE, header=TRUE)
print(forest(res, addpred=TRUE, header=TRUE))
forest(res, addpred=TRUE, header=TRUE, xlim=c(-8,6))
forest(res, addpred=TRUE, header=TRUE, xlim=c(-8,6), atransf=exp)
forest(res, addpred=TRUE, header=TRUE, xlim=c(-8,5), atransf=exp, at=log(c(.05, .25, 1, 4)))

# funnel plot
funnel(res)
funnel(res, ylim=c(0,.8), las=1)
funnel(res, ylim=c(0,.8), las=1, digits=list(1L,1))

# calculate log odds ratios and corresponding sampling variances for
# the meta-analysis on the effectiveness of vaccines against cholera
dat <- escalc(measure="OR", ai=ai, n1i=n1i,
                            ci=ci, n2i=n2i, data=dat.graves2010)
dat

# random-effects model
res <- rma(yi, vi, data=dat)
predict(res, transf=exp, digits=2)

# contour-enhanced funnel plot
funnel(dat$yi, dat$vi, yaxis="seinv",
       xlim=c(-3,2), ylim=c(.00001,8), xaxs="i", yaxs="i", las=1,
       level=c(.10, .05, .01), shade=c("white", "gray55", "gray75"),
       legend=TRUE, back="gray90", hlines=NULL, ylab="Precision (1/se)")

# meta-analysis on the risk of lung cancer in women exposed to environmental
# tobacco smoke (ETS) from their smoking spouse (yi are log odds ratios)
res <- rma(yi, vi, data=dat.hackshaw1998)
res
predict(res, transf=exp, digits=2)
funnel(res, ylim=c(0,.8), las=1, digits=list(1L,1))

# trim and fill method
funnel(trimfill(res), las=1, ylim=c(0,.8), digits=list(1L,1), legend=TRUE)

# meta-analysis on the correlation between employment interview assessments
# and job performance (using r-to-z transformed correlation for the analysis)
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat.mcdaniel1994)
res <- rma(yi, vi, data=dat)
res
predict(res, transf=transf.ztor)

# outlier/influence diagnostics
par(mar=c(5,6,4,2))
plot(influence(res), cex=0.8, las=1)

# Baujat plot (back to the BCG meta-analysis)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)
res <- rma(yi, vi, data=dat)
baujat(res, bty="l", xlim=c(0,2), ylim=c(0,.25))

# cumulative meta-analyis
sav <- cumul(res, order=dat$year)
sav
forest(sav, xlim=c(-5,2.5), header=TRUE)

# back to the meta-analysis on the risk of lung cancer due to ETS
res <- rma(yi, vi, data=dat.hackshaw1998)

# radial plot
radial(res)

# meta-analysis on the effectiveness of wrist acupuncture point P6 stimulation
# for preventing postoperative nausea
res <- rma(measure="RR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat.lee2004)
res
predict(res, transf=exp, digits=2)

# L'Abbé plot
labbe(res, transf=exp, bty="l")

# meta-analysis based on 20 hypothetical trials to examine the effectiveness
# of a particular treatment/medication
dat <- escalc(measure="OR", ai=xTi, n1i=nTi, ci=xCi, n2i=nCi,
              add=1/2, to="all", data=dat.viechtbauer2021)
dat
res <- rma(yi, vi, data=dat, method="FE")
res

# GOSH plot
sav <- gosh(res, subset=20000)
plot(sav, out=6, xlim=c(-0.25,1.25), breaks=100, hh=0.2)

# GOSH plot based on all possible subsets (takes a long time to create)
#sav <- gosh(res, subsets=10^7)
#plot(sav, cex=0.1, out=6, xlim=c(-0.25,1.25), breaks=c(200,100), hh=0.2, lwd=1)

# mixed-effects meta-regression model with absolute latitude and type of
# allocation as moderators/predictors (back to BCG dataset)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)
res <- rma(yi, vi, mods = ~ ablat + alloc, data=dat)
res

############################################################################

# forest plots with subgroups

# fit random-effects model
res <- rma(yi, vi, data=dat)

# a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, res) {
   list(bquote(paste(.(text),
      " (Q = ", .(formatC(res$QE, digits=2, format="f")),
      ", df = ", .(res$k - res$p),
      ", p ", .(metafor:::.pval(res$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
      I^2, " = ", .(formatC(res$I2, digits=1, format="f")), "%, ",
      tau^2, " = ", .(formatC(res$tau2, digits=2, format="f")), ")")))}

# set up forest plot (with 2x2 table counts added; the 'rows' argument is
# used to specify in which rows the outcomes will be plotted)
forest(res, xlim=c(-16, 4.6), at=log(c(0.05, 0.25, 1, 4)), atransf=exp,
       ilab=cbind(dat$tpos, dat$tneg, dat$cpos, dat$cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.90, ylim=c(-1, 27),
       order=dat$alloc, rows=c(3:4,9:15,20:23),
       mlab=mlabfun("RE Model for All Studies", res),
       psize=1, header="Author(s) and Year")

# set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.90, font=2)

# add additional column headings to the plot
text(c(-9.5,-8,-6,-4.5), 26, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))

# switch to bold italic font
par(font=4)

# add text for the subgroups
text(-16, c(24,16,5), pos=4, c("Systematic Allocation",
                               "Random Allocation",
                               "Alternate Allocation"))

# set par back to the original settings
par(op)

# fit random-effects model in the three subgroups
res.s <- rma(yi, vi, subset=(alloc=="systematic"), data=dat)
res.r <- rma(yi, vi, subset=(alloc=="random"),     data=dat)
res.a <- rma(yi, vi, subset=(alloc=="alternate"),  data=dat)

# add summary polygons for the three subgroups
addpoly(res.s, row=18.5, cex=0.90, atransf=exp, mlab=mlabfun("RE Model for Subgroup", res.s))
addpoly(res.r, row= 7.5, cex=0.90, atransf=exp, mlab=mlabfun("RE Model for Subgroup", res.r))
addpoly(res.a, row= 1.5, cex=0.90, atransf=exp, mlab=mlabfun("RE Model for Subgroup", res.a))

# fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ alloc, data=dat)

# add text for the test of subgroup differences
text(-16, -1.8, pos=4, cex=0.90, bquote(paste("Test for Subgroup Differences: ",
     Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
     ", p = ", .(formatC(res$QMp, digits=2, format="f")))))

############################################################################

# mixed-effects meta-regression model with absolute latitude and type of
# allocation as moderators/predictors (back to BCG dataset)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
res <- rma(yi, vi, mods = ~ ablat, data=dat)

# draw plot
regplot(res, xlim=c(10,60), predlim=c(0,60), xlab="Absolute Latitude", refline=0,
        atransf=exp, at=log(seq(0.2,2,by=0.2)), digits=1, las=1, bty="l",
        label=c(4,7,12,13), offset=c(1.6,1), labsize=0.9)

############################################################################

# selection models

cols <- palette.colors(8, palette = "R4")

res <- rma(yi, vi, data=dat.hackshaw1998)

types <- c("beta", "halfnorm", "negexp", "logistic", "power", "negexppow", "stepfun")
sel <- list()

for (j in 1:length(types)) {
   sel[[j]] <- selmodel(res, type=types[j], steps = if (types[j] == "stepfun") c(.025, .1, .2, .5, 1) else NA,
                        control=list(pval.min = if (types[j] %in% c("beta","negexppow")) 1e-4 else 0))
   plot(sel[[j]], ylim=c(0,1), scale=j==1, lwd=3, col=cols[j], add=j>1, las=1, bty="l")
}

types <- c("beta function", "half normal function", "negative exponential function", "logistic function", "power function", "negative exponential power function", "step function")
ord <- c(2,5,4,3,6,7,1)
legend("topright", inset=0, legend=types[ord], lwd=3, col=cols[ord], bg="white", seg.len=3, box.col="white")

############################################################################

# reporter() function

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              slab=paste(author, ", ", year, sep=""), data=dat.bcg)
res <- rma(yi, vi, data=dat)
res

reporter(res)
reporter(res, format="pdf")
reporter(res, format="word")

# add an outlier
dat$yi[6] <- 2.5
res <- rma(yi, vi, data=dat)
reporter(res)

############################################################################
