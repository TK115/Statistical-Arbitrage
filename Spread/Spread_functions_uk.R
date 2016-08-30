Matlab2Rdate <- function(val) as.Date(val - 1, origin = '0000-01-01') 

#find the index locations of N largest weights
indexN=function(N,mat){
	m=matrix(nrow=nrow(mat),ncol=N)
	for(i in 1:nrow(mat)){
		ndx <- order(mat[i,], decreasing = T)[1:N]
		m[i,]=ndx
	}
	return(m)
}

index.retN=function(N,weights,returns){
	index.retN=vector("numeric"); cumul.index.retN=vector("numeric")
	cumul.index.retN[1]=100
	index.matrix=indexN(N,weights)
	for(i in 1:nrow(returns)){
		index.retN[i]=sum(returns[i,][index.matrix[i,]]*weights[i,][index.matrix[i,]])
		cumul.index.retN[i+1]=cumul.index.retN[i]*(1+index.retN[i])
	}
	return(list(index.retN,cumul.index.retN))
}