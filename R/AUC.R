AUC_generic <- function(a,b){
  s = 0.0
  for(i in 1 : (length(a)-1)){
    s = 1/2 * (a[i] - a[i+1]) * (b[i] + b[i+1]) + s
  }
  return(s)
}

#' return AUC score for JointNets method
#' @author Zhaoyang Wang (Author), Zhaoyang Wang (maintainer) \email{zw4dn@virginia.edu}
#' @param simulationresult output from the function simulation()
#' @param gm_method method name from any one of the JointNets methods
#' @param lambdas a vector of lambda values for the JointNets method to run with
#' @param ... extra parameters passed to the JointNets method such as lambda, epislon and etc, refer to each method for details (eg, ?simule)
#' @return AUC score, a list of precisions and recalls
#' @export
#' @examples
#' simulationresult = simulation(n=c(100,100,100))
#' AUC_result = AUC(simulationresult,lambdas = seq(0.1,2,0.5),epsilon = 2)
#' AUC_result
#' graphics.off()
#' par(ask = FALSE)
#' par(mfrow = c(1, 1))
#' plot(AUC_result$fPM,AUC_result$tPM)
AUC <- function(simulationresult, gm_method = "simule", lambdas, ...){
  range = length(lambdas)+1
  tP = rep(0,range)
  fN = rep(0,range)
  fP = rep(0,range)
  tN = rep(0,range)
  fPM = rep(1, range)
  tPM = rep(1, range)
  out = list() ## output initialized
  X = simulationresult$simulatedsamples ## samples to run algorithms

  if (gm_method == "simule"){
    for(i in 2:range){ ## run simule against multiple lambdas
       lambda = lambdas[i-1] ## lambda for the run
       result = simule(X, lambda, ...) ## run simule

       for (j in 1:length(result$graphs)){ ## obtain predition stats sum over all contexts
         test = abs(result$graphs[[j]]) > 0
         real = abs(simulationresult$simulatedgraphs$graphs[[j]]) > 0
         tP[i] = tP[i] + sum(test & real)
         tN[i] = tN[i] + sum((test == 0) & (real == 0))
         fP[i] = fP[i] + sum((test == 1) & (real == 0))
         fN[i] = fN[i] + sum((test == 0) & (real == 1))
       }

       ## calculate stat for lambda slot i
       tPM[i] = tP[i] / (tP[i] + fN[i])
       fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC_generic(c(fPM,0),c(tPM,0)) ## compute AUC score
  }

  if (gm_method == "wsimule"){
    for(i in 2:range){ ## run wsimule against multiple lambdas
      lambda = lambdas[i-1] ## lambda for the run
      result = wsimule(X, lambda = lambda, ...) ## run wsimule

      for (j in 1:length(result$graphs)){ ## obtain predition stats sum over all contexts
        test = abs(result$graphs[[j]]) > 0
        real = abs(simulationresult$simulatedgraphs$graphs[[j]]) > 0
        tP[i] = tP[i] + sum(test & real)
        tN[i] = tN[i] + sum((test == 0) & (real == 0))
        fP[i] = fP[i] + sum((test == 1) & (real == 0))
        fN[i] = fN[i] + sum((test == 0) & (real == 1))
      }

      ## calculate stat for lambda slot i
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC_generic(c(fPM,0),c(tPM,0)) ## compute AUC score
  }


  if (gm_method == "jeek"){
    for(i in 2:range){ ## run jeek against multiple lambdas
      lambda = lambdas[i-1] ## lambda for the run
      result = jeek(X, lambda = lambda, ...) ## run jeek

      for (j in 1:length(result$graphs)){ ## obtain predition stats sum over all contexts
        test = abs(result$graphs[[j]]) > 0
        real = abs(simulationresult$simulatedgraphs$graphs[[j]]) > 0
        tP[i] = tP[i] + sum(test & real)
        tN[i] = tN[i] + sum((test == 0) & (real == 0))
        fP[i] = fP[i] + sum((test == 1) & (real == 0))
        fN[i] = fN[i] + sum((test == 0) & (real == 1))
      }

      ## calculate stat for lambda slot i
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC_generic(c(fPM,0),c(tPM,0)) ## compute AUC score
  }

  ## fix lambda2 to 1
  if (gm_method == "jgl"){
    for(i in 2:range){ ## run jeek against multiple lambdas
      lambda = lambdas[i-1] ## lambda for the run
      result = jgl(X, lambda1 = lambda, lambda2 = 1, ...) ## run jeek
      for (j in 1:length(result$graphs)){ ## obtain predition stats sum over all contexts
        test = abs(result$graphs[[j]]) > 0
        real = abs(simulationresult$simulatedgraphs$graphs[[j]]) > 0
        tP[i] = tP[i] + sum(test & real)
        tN[i] = tN[i] + sum((test == 0) & (real == 0))
        fP[i] = fP[i] + sum((test == 1) & (real == 0))
        fN[i] = fN[i] + sum((test == 0) & (real == 1))
      }

      ## calculate stat for lambda slot i
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC_generic(c(fPM,0),c(tPM,0)) ## compute AUC score
  }

  if (gm_method == "diffee"){
    for(i in 2:range){ ## run diffee against multiple lambdas
      lambda = lambdas[i-1] ## lambda for the run
      result = diffee(X[[1]],X[[2]], lambda = lambda, ...) ## run diffee

      real = abs(simulationresult$simulatedgraphs$graphs[[1]] -
                   simulationresult$simulatedgraphs$graphs[[2]] ) > 0
      test = abs(result$graphs[[1]]) > 0
        tP[i] = sum(test & real)
        tN[i] = sum((test == 0) & (real == 0))
        fP[i] = sum((test == 1) & (real == 0))
        fN[i] = sum((test == 0) & (real == 1))

      ## calculate stat for lambda slot i
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC_generic(c(fPM,0),c(tPM,0)) ## compute AUC score
  }

  if (gm_method == "kdiffnet"){
    for(i in 2:range){ ## run diffee against multiple lambdas
      lambda = lambdas[i-1] ## lambda for the run
      result = kdiffnet(X[[1]],X[[2]], lambda = lambda, ...) ## run kdiffnet
      real = abs(simulationresult$simulatedgraphs$graphs[[1]] -
                   simulationresult$simulatedgraphs$graphs[[2]] ) > 0
      test = abs(result$graphs[[1]]) > 0
      tP[i] = sum(test & real)
      tN[i] = sum((test == 0) & (real == 0))
      fP[i] = sum((test == 1) & (real == 0))
      fN[i] = sum((test == 0) & (real == 1))

      ## calculate stat for lambda slot i
      tPM[i] = tP[i] / (tP[i] + fN[i])
      fPM[i] = fP[i] / (fP[i] + tN[i])
    }
    auc = AUC_generic(c(fPM,0),c(tPM,0)) ## compute AUC score

  }

  out$auc = auc
  out$fPM = c(fPM,0)
  out$tPM = c(tPM,0)
  return(out)
}
