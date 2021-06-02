# criando dataset generico
n = 100
df <- data.frame(
  x = c(rnorm(n/2,1,1),rnorm(n/2,10,1)),
  y = c(rnorm(n/2,1,1),rnorm(n/2,10,1)),
  z = c(rnorm(n/2,1,1),rnorm(n/2,10,1)),
  w = c(rnorm(n/2,1,1),rnorm(n/2,10,1)),
  #label = rbinom(n = n, size = 1, prob = 0.5)
  label = c(rep(0,n/2),rep(1,n/2))
)


# knn na unha -----------------------------------------------------------------

#definindo o numero de vizinhos
k = 9

#definindo matriz de distancias
distancia <- matrix(
  nrow = dim(df)[1],
  ncol = dim(df)[1]
)

#calculando as distancia
for(i in 1:nrow(df)){
  for(j in 1:nrow(df)){
    distancia[i,j] <- sqrt(sum((df[i,] - df[j,])^2))
  }  
}

#espiando as distancias
distancia

#selecionando os vizinhos mais proximos
vizinhos <-NULL
for(i in 1:nrow(df)){
d_i <- data.frame(
  id = 1:nrow(df),
  d = distancia[i,]
)
d_i = d_i[-i,]
vizinhos[[i]] <- d_i[order(d_i$d),][1:k,'id']
}
#espiando os id dos k-vizinhos
vizinhos

#predicao
for (i in 1:nrow(df)) {
  tab_label <- table(df[vizinhos[[i]],'label'])
  df[i,'predito'] <- names(tab_label[tab_label==max(tab_label)])
}


#Validação
print(table(df[,'label'],df[,'predito']))
print(prop.table(table(df[,'label'],df[,'predito']),))

