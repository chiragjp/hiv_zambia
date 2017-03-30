### Chirag J Patel
### utility functions for HIV xwas analyses
### edited 9/28/2015
library(survey)
library(methods)

addToBase <- function(base_formula, adjustingVariables) {
		form <- base_formula
		if(length(adjustingVariables)) {
			addStr <- as.formula(sprintf('~ . + %s', paste(adjustingVariables, collapse='+')))
			form <- update.formula(base_formula, addStr)
		}
		return(form)
}

categorize_varname <- function(mainTab, varname) {
	catTab <- table(mainTab[, varname])
	if(length(catTab) <= 10 & length(catTab) > 2) {
		return(sprintf('factor(%s)', varname))
	}
	return(varname)
}

qvalue <- function(pvals, randData,numIter=100) {
	qvalueLow = function(pval) {
		numer = sum(randData <= (pval)) / numIter
		#denom = sum(pvals <= pval) / length(pvals)
		denom <- sum(pvals <= pval)
		q = numer/denom
		if(q > 1) {
			q=1
		}
		if(q == Inf) {
			q=NA
		}
		q
	}
	qvals <- sapply(pvals, qvalueLow)
	rankedPvals = rank(pvals)
	qsort = sort(qvals)
	qv = c()
	for(ii in 1:length(rankedPvals)) {
		currRank = rankedPvals[ii]
		qv = c(qv,qsort[currRank])
	}
	return(data.frame(qvalue=qv, pvalue=pvals))
}

svyDesign <- function(dat, wt) {
  ## to do
  return(NULL)
}

logistic_glm <- function(formula, dat) {
  N <- nrow(dat)
  #dsn <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weight=~WTMEC2YR, data=subset(dat, WTMEC2YR>0), nest=TRUE)
  mod <- NULL
  summaryFrame <- NULL
  mod <- tryCatch(glm(formula, dat, family=binomial()), error = function(e) {
    print(e)
    return(NULL);
  })
  
  if(!is.null(mod)) {
    summaryFrame <- as.data.frame(coef(summary(mod)))
    summaryFrame$N <- N
  }
  
  return(summaryFrame)
}


nagelkerke <- function(fit, null=NULL) {
  # function http://rcompanion.org/r_script/nagelkerke.r
    TOGGLE = (class(fit)[1]=="lm"
              | class(fit)[1]=="gls"
              | class(fit)[1]=="lme"
              | class(fit)[1]=="glm"
              | class(fit)[1]=="negbin"
              | class(fit)[1]=="zeroinfl"
              | class(fit)[1]=="clm"
              | class(fit)[1]=="vglm")
    BOGGLE = (class(fit)[1]=="nls"
              | class(fit)[1]=="lmerMod"
              | class(fit)[1]=="glmerMod"
              | class(fit)[1]=="merModLmerTest"
              | class(fit)[1]=="clmm")
    SMOGGLE = (class(fit)[1]=="lmerMod"
               | class(fit)[1]=="glmerMod"
               | class(fit)[1]=="merModLmerTest"
               | class(fit)[1]=="vglm")
    ZOGGLE = (class(fit)[1]=="zeroinfl")
    NOGGLE = is.null(null)
    ERROR = "Note: For models fit with REML, these statistics are based on refitting with ML"
    
    if(NOGGLE & TOGGLE){null = update(fit, ~ 1)}
    if(NOGGLE & BOGGLE)
    {ERROR = "You need to supply a null model for nls, lmer, glmer, or clmm"}
    if((!TOGGLE) & (!BOGGLE))
    {ERROR = "This function will work with lm, gls, lme, lmer, glmer, glm, negbin, zeroinfl, nls, clm, clmm, and vglm"}
    
    SMOGGLE2 = (class(null)[1]=="lmerMod"
                | class(null)[1]=="glmerMod"
                | class(null)[1]=="merModLmerTest"
                | class(null)[1]=="vglm")   
    
    Y = matrix(rep(NA,2),
               ncol=1)
    colnames(Y) = ""
    rownames(Y) = c("Model:", "Null:")
    
    Z = matrix(rep(NA, 3),
               ncol=1)
    colnames(Z) = c("Pseudo.R.squared")
    rownames(Z) = c("McFadden", "Cox and Snell (ML)", 
                    "Nagelkerke (Cragg and Uhler)") 
    X = matrix(rep(NA,4),
               ncol=4)
    colnames(X) = c("Df.diff","LogLik.diff","Chisq","p.value")
    rownames(X) = ""
    
    if(TOGGLE | BOGGLE){
      if (!SMOGGLE){Y[1]= toString(fit$call)}
      if (SMOGGLE){Y[1]= toString(fit@call)}
    }
    
    if(TOGGLE | (BOGGLE & !NOGGLE)){
      
      if (!SMOGGLE2){Y[2]= toString(null$call)}
      if (SMOGGLE2){Y[2]= toString(null@call)}
      
      if(!ZOGGLE){N = nobs(fit)}
      if(ZOGGLE){N = fit$n}  
      m = suppressWarnings(logLik(fit, REML=FALSE))[1]
      n = suppressWarnings(logLik(null, REML=FALSE))[1]
      mf = 1 - m/n
      Z[1,] = signif(mf, digits=6)
      cs = 1 - exp(-2/N * (m - n))
      Z[2,] = signif(cs, digits=6)
      nk = cs/(1 - exp(2/N * n))
      Z[3,] = signif(nk, digits=6)
      
      o = n - m
      dfm = attr(logLik(fit),"df")
      dfn = attr(logLik(null),"df")
      if(class(fit)[1]=="vglm"){dfm=df.residual(fit)}
      if(class(fit)[1]=="vglm"){dfn=df.residual(null)}
      dff = dfn - dfm
      CHI = 2 * (m - n)
      P = pchisq(CHI, abs(dff), lower.tail = FALSE)
      
      X [1,1] = dff
      X [1,2] = signif(o, digits=5)             
      X [1,3] = signif(CHI, digits=5)
      X [1,4] = signif(P, digits=5)     
    }
    
    W=ERROR
    
    V = list(Y, Z, X, W) 
    names(V) = c("Models", "Pseudo.R.squared.for.model.vs.null", "Likelihood.ratio.test",
                 "Messages")
    return(V)            
}

nagelkerke_r2 <- function(formula, dat) {
  mod <- glm(formula,dat, family=binomial()) 
  n <- nagelkerke(mod)
  return(n$Pseudo.R.squared.for.model.vs.null[3,1])
}

logistic_svyglm <- function(formula, dsn) {
  N <- nrow(dsn$variables)
  mod <- NULL
  summaryFrame <- NULL
  mod <- tryCatch(svyglm(formula, dsn, family=quasibinomial()), error = function(e) {
    print(e)
    return(NULL);
  })
  
  if(!is.null(mod)) {
    summaryFrame <- as.data.frame(coef(summary(mod)))
    summaryFrame$N <- N
  }
  
  return(summaryFrame)
}




introspect_variables_easy <-function(dat, ignoreThese) {
  allVars <- setdiff(colnames(dat), ignoreThese)
  lens <- apply(dat[, allVars], 2, function(arr) { length(table(arr))})
  return(list(binaryVariables=names(lens)[lens == 2], continuousVariables=names(lens)[lens > 2]))
}

introspect_variables <- function(dat, ignoreThese=c(dependentVariable)) {
  allVars <- setdiff(colnames(dat), ignoreThese)
  categoricalVariables <- colnames(dat)[grep('\\_', colnames(dat))]
  theRest <- setdiff(allVars, categoricalVariables)
  yesNo <- c()
  continuousVariables <- c()
  factorVariables <- c()
  for(col in theRest) {
    values <- table(dat[,col])
    variableNames <- names(values)
    if('yes' %in% variableNames) {
      yesNo <- c(yesNo, col)
      next;
    }
    
    if(sum(variableNames %in% c("0","1")) == 2 & length(variableNames) <= 2) {
      yesNo <- c(yesNo, col)
      next;
    }
    
    classes <- is(dat[, col])
    if('factor' %in% classes) {
      factorVariables <- c(factorVariables, col)
      next;
    }
    
    if('numeric' %in% classes) {
      continuousVariables <- c(continuousVariables, col) 
      next;
    }
    
  }
  return(list(categoricalVariables=categoricalVariables, yesNoVariables=yesNo, continuousVariables=continuousVariables, factorVariables=factorVariables))
}

