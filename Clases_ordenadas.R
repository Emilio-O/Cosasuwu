#### Practica uno: Conceptos basicos de redes con igraph
# ----

# libreria igraph
library(igraph)

# hacer una red vaciacon cinco esferas como vertices
red_vacia <- make_empty_graph(n = 5, directed = F)
V(red_vacia)$color = "yellow"
V(red_vacia)$shape = "sphere"
# con el signo de pesos asignamos la caracteristica sobre los nodos

# plot nos sirve para visualizar
plot(red_vacia)


# Asi añadimos las conexiones
red_vacia <- add.edges(red_vacia, c(1,2, 1,3, 2,4, 3,4, 4,5))
# Une 1 a 2, une 1 a 3, une 2 a 4
# Si es dirigida entonces va a poner la direccin de las flechas
# Si es no dirigida (directed = F) entonces no hay direccion de las conexiones
plot(red_vacia)


red_vacia <- add.vertices(red_vacia, 5, color = "red", shape = "sphere")
plot(red_vacia)
# ya me aparecen los demas vertices y falta añadir las conexiones


red_vacia <- add.edges(red_vacia, c(5,6, 5,7, 5,8, 5,9, 5,10))
plot(red_vacia)


red_vacia <- delete.edges(red_vacia, c(1))
plot(red_vacia)
# si solamente ponemos uno, eliminara la conexion con el siguiente nodo
# en ese caso el dos, si volvemos a correr la misma linea de codigo sera el siguiente nodo

red_vacia <- add.edges(red_vacia, c(1,3, 1,2))
plot(red_vacia)
# con add.edges podemos volver a reestablecer las conexiones


V(red_vacia)$name <- LETTERS[1:10]


# Cuando es una red dirigida hay dos clases de vecinos
# distinguimos entre objetos que entren y objetos que salen

# degree
degree(red_vacia)

# Hacer un plot de del degree distribution
plot(degree_distribution(red_vacia), main = "Degree distribution", xlab = "Degree", ylab = "Frequency")





# ----

#### Ejercicios practica uno
# ----
# Calcular el degree
g1 <- barabasi.game(26,directed = FALSE)
deg <- degree(g1)


# degree distribution
plot(degree.distribution(g1))

# Saber el nombre del nodo que tiene el degree mas alto
V(g1)$name <- LETTERS[1:26]
V(g1)$name[degree(g1)==max(degree(g1))]

# Con esto obtienes el degree y el nombre(en caso de que tenca uno el nodo)
sort(deg, decreasing = T)[1]


# Encontrar los diez nodos mas conectados
sort(deg, decreasing = T)[1:10]
# ----

#### Practica dos: Distancias y coeficiente de clusterizacion
# ----

# Importar el csv del archivo de amitades
matriz <- read.csv('RedDeAmistades.csv')

row.names(matriz) <- matriz[ ,1]
matriz <- matriz[ ,-1]
matriz <- as.matrix(matriz)
diag(matriz) <- rep(0,19)



# Hacer la red
red <- graph_from_adjacency_matrix(matriz, mode = 'directed')
# Hace la matriz de adyacencia y de ahi hace la red
plot(red)

# Asi se hace solamente la matriz de adyacencias
adj <- as.matrix(get.adjacency(red))



# Hacer la red pero con las caracteristicas que se me peguen la gana
plot(red, vertex.shape = "none", vertex.label.font = .8 ,edge.color = "darkred", 
     edge.curved = .2, edge.arrow.size = .4)

# Diametro de la red
diameter(red)

# Distancias mas cortas entre todos los nodos
shortest.paths(red)

# El promedio de la kingitud de las distancias en un grafi
mean_distance(red)


# Hacer una red que no sea free scale
g2 <- random.graph.game(100, 0.5)
plot(g2)


# Hacer una red free scale
# Con base en un mecanismo de conexion preferente
g3 <- barabasi.game(1000, power = 1)
layout <- layout.fruchterman.reingold(g3)
plot(g3, layout = layout, vertex.size = 2,
     vertex.label = NA, edge.arrow.size = .1)


# Hacer un heatmap a partir de la matriz de distancias de los shortest.paths
heatmap(shortest.paths(red))


# Aqui se calcula la distancia mas corta que hay entre dos nodos que yo escoja de la red
road <- shortest.paths(red, "EMILIO", "GERARDO")
road


# Calcula de un nodo todas las distancias mas cortas respecto a otros nodos
shortest.paths(red, "EMILIO")


# Distancias entre todos los nodos
# Por debajo de la diagonal son lo out
# Por arriba de la diagonal son los in
distances(red, mode = "out")


# Cluster de diferenciacion
# ¿Mis amigos son amigos entre si?
transitivity(red)


# Hacer una red de mundo pequeño
g4 <- sample_smallworld(1, 100, p = 0.2, nei = 3)
plot(g4)
# ----

#### Ejercicios practica dos
# ----

# Redes
g5 <- barabasi.game(100,directed = FALSE)
g6 <- random.graph.game(100,0.20)
g7 <- sample_smallworld(1,100,p=0.2,nei=3)

# distancias (solo hare los de uno)
shortest.paths(g5)

# promedio de las distancias
mean_distance(g5)

# diametro
diameter(g5)

# C.C
transitivity(g7)
# ----

#### Practica tres: Propiedad free scale
# ----
# Hacer una red free scale
g8 <- barabasi.game(1000)
plot(hist(degree(g8, mode = "in"))) 


# Fit a power law
deg <- degree(g8, mode = "in")
fit1 <- fit_power_law (deg + 1, 10)
fit1$alpha
# ----

#### Practica cuatro: Robustes de las redes
# ----

# Eliminar un nodo de la red
g9 <- make_ring(10) %>%
  set_vertex_attr("name", value = LETTERS[1:10])

plot(g9)

g10 <- delete_vertices(g9, c(1,5)) %>%
  delete_vertices("B")

plot(g10)
# ----

#### Ejercicios practica cuatro
# ----
# Eliminar un nodo al azar por 10 veces y calcular el promedio de las distancias
# Tambien calcular el diametro
g11 <- barabasi.game(100, directed = F)

for (i in 0:10) {
  g11 <- delete.vertices(g11, sample(1:(100 - i), 1))
  print(mean_distance(g11))
  print(diameter(g11))
}


# ----

#### Ejercicios extra de clase
# ----

# Quedarme con el numero mas grande de un vector
empanada <- sample(50)
max(empanada)


as.vector(empanada)

empanada1 <- empanada[1]
for (i in 1:length(empanada)) {
  if ( empanada1 < empanada[i]){
    empanada1 <- empanada[i]
  }
}
empanada1


# Calcular las tres personas que tienen un degree mayor 
# de entrada y de saida
# El que tiene mas amigos y al que consideran mas su amigo
uwu <- sort(degree(red, mode = "in"), decreasing = T)[1:3]
uwu2 <- sort(degree(red, mode = "out"), decreasing = T)[1:3]
uwu
uwu2

# Promedio de amigos que tienen mis amigos
mean(uwu2[c("GERARDO", "JAVIER", "FRANCISCO", "LIZBETH", "CINDY", "JANETZY", "PEDRO")])

# A continuacion una forma mas eficiente de hacer lo anterior
mis_amigos <- matriz["Emilio", ] # Elijo mi renglon de los que yo considero mis amigos
amigos_amigos <- 0 # En este vector se almacenara el promedio

for (i in 1:length(matriz[1, ])) {if (mis_amigos[i] > 0) {amigos_amigos <- amigos_amigos + sum(matriz[i, ])}}

amigos_amigos/sum(mis_amigos)




# Importancia de nodos
# Identificar cuales son los mas importantes

eccentricity(red)[1]

betweenness(red)[1]

closeness(red)[1]


#Usar la red de amistades y clusterizarlo con cuatro metodos distintows y plotearlos
eb <- edge.betweenness.community(red) # viene como argumento default que la red es dirigida
lb <- label.propagation.community(red)
cs <- cluster_spinglass(red)

plot(eb, red)
plot(lb, red)
plot(cs, red)





# Hacer la lista de adyasencia y exportar
ff<-as.matrix(as_edgelist(red, names = T))
write.csv(ff, "prueba.csv")

# ----
