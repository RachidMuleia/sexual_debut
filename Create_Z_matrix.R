

create.Z.matrix <- function(data, dist.data, dist.knots, model, phi){
  if (model=="exponential"){
    Z = exp(-dist.data/phi)
    K = exp(-dist.knots/phi)
    svd.K = svd(K)
    sqrt.K = t(svd.K$v %*% (t(svd.K$u)*sqrt(svd.K$d)))
    Z.tilde = t(solve(sqrt.K,t(Z)))
  }
  if (model=="gaussian"){
    Z = exp(-(dist.data/phi)^2)
    K = exp(-(dist.knots/phi)^2)
    svd.K = svd(K)
    sqrt.K = t(svd.K$v %*% (t(svd.K$u)*sqrt(svd.K$d)))
    Z.tilde = t(solve(sqrt.K,t(Z)))
  }
  if (model=="spherical"){
    Z = ( 1 - 3/2*(dist.data/phi) + 1/2*(dist.data/phi)^3 ) * (dist.data < phi)
    K = ( 1 - 3/2*(dist.knots/phi) + 1/2*(dist.knots/phi)^3 ) * (dist.knots < phi)
    svd.K = svd(K)
    sqrt.K = t(svd.K$v %*% (t(svd.K$u)*sqrt(svd.K$d)))
    Z.tilde = t(solve(sqrt.K,t(Z)))
  }
  if (model=="circular"){
    theta1v = unlist( lapply(dist.data, function(t){min(1,t/phi)}) )
    theta2v = unlist( lapply(dist.knots, function(t){min(1,t/phi)}) )
    theta1 = matrix(as.vector(theta1v), nrow=nrow(dist.data), ncol=ncol(dist.data))
    theta2 = matrix(as.vector(theta2v), nrow=nrow(dist.knots), ncol=ncol(dist.knots))
    g1 = 2*( theta1*sqrt(1-theta1^2) +  asin(theta1)) / pi
    g2 = 2*( theta2*sqrt(1-theta2^2) +  asin(theta2)) / pi
    Z = 1-g1
    K = 1-g2
    svd.K = svd(K)
    sqrt.K = t(svd.K$v %*% (t(svd.K$u)*sqrt(svd.K$d)))
    Z.tilde = t(solve(sqrt.K,t(Z)))
  }
  if (model=="matern"){
    Z = exp(-dist.data/phi)*(1 + dist.data/phi)
    K = exp(-dist.knots/phi)*(1 + dist.knots/phi)
    svd.K = svd(K)
    sqrt.K = t(svd.K$v %*% (t(svd.K$u)*sqrt(svd.K$d)))
    Z.tilde = t(solve(sqrt.K,t(Z)))
  }
  if (model=="inverse.multiquadratic"){
    Z = 1/sqrt(1 + dist.data^2/phi)
    K = 1/sqrt(1 + dist.knots^2/phi)
    svd.K = svd(K)
    sqrt.K = t(svd.K$v %*% (t(svd.K$u)*sqrt(svd.K$d)))
    Z.tilde = t(solve(sqrt.K,t(Z)))
  }
  if (model=="thin.plate"){
    Z = matrix(0, nrow(dist.data), ncol(dist.data))
    K = matrix(0, nrow(dist.knots), ncol(dist.knots))
    Z[dist.data==0] = (dist.data^2)[dist.data==0]
    Z[dist.data!=0] = (dist.data^2*log(dist.data))[dist.data!=0]
    K[dist.knots==0] = (dist.knots^2)[dist.knots==0]
    K[dist.knots!=0] = (dist.knots^2*log(dist.knots))[dist.knots!=0]
    svd.K = svd(K)
    sqrt.K = t(svd.K$v %*% (t(svd.K$u)*sqrt(svd.K$d)))
    Z.tilde = t(solve(sqrt.K,t(Z)))
  }
  return(Z.tilde)
}