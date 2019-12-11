#' Get predictions from compositional ilr multiple linear regression model
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Provided the data (containing outcome, composiitional compoents and covariates), fit a ilr multiple linear regression model and provide predictions from reallocating compositional values pairwise amunsnst the components model.
#' @param dataf A \code{data.frame} containing data
#' @param y Name (as string) of outcome in \code{dataf}
#' @param comps Character vector of names of compositions in \code{dataf}
#' @param covars Optional. Character vector of covariates names  (non-comp variables) in \code{dataf}. Defaults to NULL.
#' @param deltas Optional. Changes in compositions to be computed pairwise. Defaults to 0, 10 and 20 minutes as a proportion of minutes in a day.
#' @param comparisons Currently three choices: "one-v-one" (default), "one-v-all" or "prop-realloc". Currently proportional re-allocaiton isn't properly implemented.
#' @param alpha Optional. Level of significance. Defaults to 0.05.
#' @param verbose Optional. Whether the function provides extra information upon return. Defaults to FALSE.
#' @export
#' @examples
#' get_plus_minus_changes(
#'   dataf=fat_data,
#'   y="fat",
#'   comps=c("sl","sb","lpa","mvpa"),
#'   covars=c("sibs","parents","ed"),
#'   deltas=seq(-60,60,by=5)/(24*60),
#'   comparisons="one-v-one",
#'   alpha=0.05,
#'   verbose=FALSE
#' )
#'

get_plus_minus_changes<-function(
  dataf, # data.frame of data
  y, #name of outcome in dataf
  comps, # vector of names of compositions in dataf
  covars=NULL, # vector of names of covariates (non-comp variables) in dataf
  deltas=c(0,10,20)/(24*60), # changes in compositions to be computed pairwise
  comparisons=c("one-v-one","one-v-all","prop-realloc")[1],
  alpha=0.05,
  verbose=FALSE
){

  n<-nrow(dataf)
  n_comp<-length(comps)
  n_covar<-ifelse(is.null(covars),0,length(covars))
  n_delta<-length(deltas)

  m_cov<-vector(mode="list",length=n_covar)
  names(m_cov)<-paste0("m_",covars)
  for(j in 1:n_covar){ #j<-1
    this_covar<-dataf[,covars[j]]
    if(is.factor(this_covar)){
      m_temp=median(as.integer(this_covar))
      m_cov[[j]]=this_covar[this_covar==m_temp][1]
    }else if(is.numeric(this_covar)){
      m_cov[[j]]<-mean(this_covar)
    }else{
      cat("please have all covariates specified as either factors or numeric variables\n")
      break;
    }
    cat("The 'average' case to be used for prediction of covariate"
        ,covars[j],"is",as.character(m_cov[[j]]),"\n")
  }


  # standardise comps
  old_comps<-dataf[,comps]
  comp_totals<-rowSums(old_comps)
  cat("These are the quartiles of the summed compositions (ideally all equal)\n")
  print(quantile(comp_totals,seq(0,1,by=0.25)))
  dataf[,comps]<-old_comps/matrix(comp_totals,ncol=n_comp,nrow=n)



  # sequential binary partition for 3 comp vars can be
  # (1,-1,-1), (0,1,-1)
  # sequential binary partition for 4 comp vars can be
  # (1,-1,-1,-1), (0,1,-1,-1), (0,0,1,-1)
  # etc
  base_zeros<-rep(0,n_comp)
  sbp<-matrix(0,nrow=n_comp,ncol=n_comp-1)
  for(j in 1:(n_comp-1)){
    this_col<-base_zeros
    this_col[j]<-1
    this_col[(j+1):n_comp]<- -1
    sbp[,j]<-this_col
  }
  # can also do ilrBase(D=n_comp) etc

  psi <- suppressWarnings(compositions::gsi.buildilrBase(sbp)) ## The orthonormal transformation matrix


  ilr_comps <- suppressWarnings(compositions::ilr(dataf[,comps], V=psi))
  ilr_comps<-as.data.frame(ilr_comps[1:n,])
  ilr_names<-paste0("ilr",1:(n_comp-1))
  colnames(ilr_comps)<-ilr_names
  head(ilr_comps)

  dataf<-cbind(ilr_comps,dataf)
  head(dataf)


  # X<-dataf[,!(colnames(dataf) %in% comps)] # this only removes original comp vars - INCORRECT!
  X<-dataf[, colnames(dataf) %in% c(y, ilr_names, covars)] # Better now - keep all required cols in df
  head(X)
  class(X)
  lm_formula <- as.formula(paste(y,"~."))
  lm_X <- lm(lm_formula,data=X)
  cat("---\nSummary of the linear model:\n---\n")
  print(summary(lm_X))
  # get the design matrix from the LM
  head(dmX<-model.matrix(lm_X))
  # (X^T X)^{-1}
  XtX_inv<-solve(t(dmX) %*% dmX)
  # the resid standard error
  s_e<-sqrt(sum(residuals(lm_X)^2)/df.residual(lm_X))
  # crit val with 95% conf of relevant t-dist
  crit_val<-qt(1-alpha/2,df.residual(lm_X))
  # beta estimates
  (beta_hat<-matrix(coefficients(lm_X),ncol=1))

  ########################## mean values #########################


  mean_comps<-colMeans(dataf[,comps]) #all
  mean_comps
  # sum(mean_comps) == 1?
  #reallocation 0f 15 minutes


  ################################# all predict changes  #################################
  K<-poss_comps0<-NULL
  comp_type<-which(c("one-v-one","one-v-all","prop-realloc") %in% comparisons)

  if(!(length(comp_type)==1)) {
    cat("only specify one comparison method at a time\n")
    stop()
  }

  if(comparisons %in% c("one-v-all","prop-realloc")) {
    cat("Comparison type being used: '"
        ,ifelse(comparisons=="prop-realloc","prop-realloc","one-v-all")
        ,"'\n")

    # number of combinations is:
    # K = n_comps as only one combination per composition
    (K<-n_comp)
    poss_comps0<-matrix(0,nrow=K,ncol=n_comp,dimnames=list(NULL,names(mean_comps)))

    for(k in 1:K) {
      poss_comps0[k,k]<-1
      poss_comps0[k,-k]<- -1/(K-1)
    }

  } else if (comparisons=="prop-realloc") {
    cat("Comparison type being used: 'prop-realloc'\n")
    # number of combinations is:
    # K = number of positions the 1 can be in
    #     times
    #     number of positions left for the -1 can be in
    (K<-n_comp*(n_comp-1))
    poss_comps0<-matrix(0,nrow=K,ncol=n_comp,dimnames=list(NULL,names(mean_comps)))

    k<-0
    for(i in 1:n_comp) for(j in 1:n_comp) if(i!=j) {
      k<-k+1
      poss_comps0[k,c(i,j)]<-c(1,-1)
    }

  } else { # comparisons="one-v-one"
    cat("Comparison type being used: 'one-v-one'\n")
    if(comparisons!="one-v-one") cat("(DEFAULT) as 'comparisons' not exact match\n")

    # number of combinations is:
    # K = number of positions the 1 can be in
    #     times
    #     number of positions left for the -1 can be in
    (K<-n_comp*(n_comp-1))
    poss_comps0<-matrix(0,nrow=K,ncol=n_comp,dimnames=list(NULL,names(mean_comps)))

    k<-0
    for(i in 1:n_comp) for(j in 1:n_comp) if(i!=j) {
      k<-k+1
      poss_comps0[k,c(i,j)]<-c(1,-1)
    }

  }

  delta_mat<-poss_comps<-NULL
  for(d in 1:n_delta) {
    delta_mat<-rbind(delta_mat,deltas[d]*poss_comps0)
    poss_comps<-rbind(poss_comps,poss_comps0)
  }


  delta_mat
  n_preds<-K*n_delta


  m_comps<-matrix(rep(mean_comps,n_preds),nrow=n_preds,byrow=TRUE)
  m_delta<-m_comps+delta_mat
  m_delta
  ilr_means<-suppressWarnings(
    compositions::ilr(m_comps,V=psi)[1:n_preds,1:(n_comp-1)] # get rid of "rmult" class
  )
  ilr_delta<-suppressWarnings(
    compositions::ilr(m_delta,V=psi)[1:n_preds,1:(n_comp-1)] # get rid of "rmult" class
  )
  class(ilr_delta)

  (x0_star<-matrix(dmX[1,]
                   ,ncol=ncol(dmX),nrow=n_preds,byrow=TRUE,dimnames=list(NULL,colnames(dmX))))
  x0_star[,ilr_names]<-ilr_delta-ilr_means
  x0_star[,!(colnames(x0_star) %in% ilr_names)]<-0
  x0_star


  y0_star<-x0_star %*% beta_hat
  se_y0_star<-matrix(0,ncol=1,nrow=n_preds)
  for(i in 1:n_preds) {
    x0_star_i<-x0_star[i,,drop=FALSE]
    class(x0_star_i)
    se_y0_star[i,]<-s_e *sqrt(x0_star_i %*% XtX_inv %*% t(x0_star_i))
  }
  se_y0_star

  re_alloc_nms<-matrix("",nrow=n_preds,ncol=2) #make labels for reallocations
  if(comparisons=="one-v-all") {
    for(i in 1:n_preds)
      re_alloc_nms[i,]<-c(comps[poss_comps[i,]==1],"all")
  } else {
    for(i in 1:n_preds)
      re_alloc_nms[i,]<-c(comps[poss_comps[i,]==1],comps[poss_comps[i,]==-1])
  }
  re_alloc_nms
  bound_fac<-c(0,-1,1)
  preds0<-crit_val * matrix(bound_fac,nrow=n_preds,ncol=3,byrow=TRUE)
  for(j in 1:3) preds0[,j]<-y0_star+preds0[,j,drop=FALSE]*se_y0_star
  preds0<-as.data.frame(
    cbind(
      as.data.frame(re_alloc_nms)
      ,rep(deltas,each=K)
      ,alpha
      ,as.data.frame(preds0)
    )
  )

  colnames(preds0)<-c("comp+","comp-","delta","alpha","delta_pred"
                      ,paste0("ci_",c("lo","up")))
  preds0

  preds0$sig<-ifelse(preds0$ci_lo<=0 & preds0$ci_up>=0,"","*")


  ret_obj<-preds0
  if(verbose) ret_obj<-list(
    predictions=preds0
    ,sbp=sbp
    ,psi=psi
    ,data=X
    ,linmod=lm_X
    ,n=c(n=n,n_comp=n_comp,n_covar=n_covar,n_delta=n_delta)
  )

  return(ret_obj)

}
