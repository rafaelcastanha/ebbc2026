{

  library(rvest)
  library(dplyr)
  library(igraph)
  library(visNetwork)
  library(stringi)
  library(stringr)

  #Base de autores
  
  base<-read_html("https://ebbc.inf.br/ojs/index.php/ebbc")%>%
  html_nodes(".authors") %>%
  html_text2()
  
  #Acessa links dos artigos
  
  links_art <- read_html("https://ebbc.inf.br/ojs/index.php/ebbc") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_subset("^https://ebbc\\.inf\\.br/ojs/index\\.php/ebbc/article/view/[0-9]+$")
  
  kw<-character()
  
  for (i in links_art){
  
    extract<-read_html(i)%>%
    html_nodes(".keywords") %>%
    html_text2()
      
    kw<-append(extract, kw)
    }  
  
  kw <- gsub("^Palavras-chave:\\n", "", kw)  
  
  

#ajuste base de autores

  base<-iconv(base, from = "UTF-8", to = "ASCII//TRANSLIT")
  
  base <- gsub("\\s*\\((Autor|Tradutor)\\)\\s*", "", base)
 
  base<-gsub(";", ",", base)
  
  base<-gsub("-", " ", base)
  
  base <- gsub("Washington R\\. de Carvalho Segundo|Washington Luis Ribeiro de Carvalho|Washington Luis Ribeiro de Carvalho Segundo",
               "Washington L. R. Carvalho Segundo",
               base)
  
  #Produtividade - artigos
  
  autores_prod <- (unlist(strsplit(base, ", ")))   #Autores
  
  autores_prod2 <- gsub("\\s*\\((Autor|Tradutor)\\)\\s*", "", autores_prod)
  
  prod<-as.data.frame(table(autores_prod2)) #Producao por autor
  
  prod_total<-prod[order(prod$Freq,decreasing=TRUE),]
  
  prod_top10<-na.omit(prod_total[1:14,]) #autores mais produtivos
  
  colnames(prod_top10)[1]<-"Autores"
  colnames(prod_top10)[2]<-"Frequência"
  
  
  #ajuste base de KW
  
  kw<-iconv(kw, from = "UTF-8", to = "ASCII//TRANSLIT")
  
  kw<-tolower(kw)
  
  kw<-gsub(";", ",", kw)
  
  kw<-gsub(".", ",")
  
  kw<-gsub("-", "", kw)
  
  #Frequências - Palavras-chave
  
  kw_prod <- (unlist(strsplit(kw, ", ")))   #Autores
  
  kw_prod<-as.data.frame(table(kw_prod)) #Producao por autor
  
  prod_total_kw<-kw_prod[order(kw_prod$Freq,decreasing=TRUE),]
  
  kw_prod_top10<-na.omit(prod_total_kw[1:10,]) #autores mais produtivos
  
  colnames(kw_prod_top10)[1]<-"Autores"
  colnames(kw_prod_top10)[2]<-"Frequência"
  
  #Estruturação rede KW
  
  b44<-strsplit(as.character(kw), split = ", " , fixed = FALSE)
  
  b55<-as.data.frame(do.call(cbind, b44))
  
  #Matriz de coocorrencia KW
  
  mtx_kw<-table(stack(b55))
  
  mtx_kw[mtx_kw>1]<-1
  
  mtx_kw_c<-mtx_kw%*%t(mtx_kw) 
  
  diag(mtx_kw_c)<-0
  
  #Rede igraph KW
  
  rede_kw<-graph_from_adjacency_matrix(mtx_kw_c, weighted = T, mode = "undirected")
  
  #rede visNetwork KW
  
  vis_kw<-toVisNetworkData(rede_kw)
  
  node_kw<-data.frame("id"=vis_kw$nodes$id, "label"=vis_kw$nodes$label)
  links_kw<-as.data.frame(vis_kw$edges) 
  colnames(links_kw)[3]<-'width'
  links_kw<-filter(links_kw, width>=1)
  
  links_kw$label <- as.character(links_kw$width)
  
  #Grau da Rede - KW
  
  grau_df_kw <- data.frame(
    Palavra = names(degree(rede_kw)),
    Grau = degree(rede_kw),
    row.names = NULL)
  
  grau_df_kw <- grau_df[order(grau_df_kw$Grau, decreasing = TRUE), ]  
  
  colnames(grau_df_kw)[1]<-"Palavra-chave"
  colnames(grau_df_kw)[2]<-"Grau (Coocorrências)"
  
  
  #Intensidade de pares de KW
  
  peso_df_kw <- data.frame(as_edgelist(rede_kw), row.names = NULL)
  colnames(peso_df_kw) <- c("Palavra-Chave 1", "Palavra-Chave 2")
  peso_df_kw$Peso <- E(rede_kw)$weight
  peso_df_kw<- peso_df_kw[order(peso_df_kw$Peso, decreasing = TRUE), ]
  
  
  ### Estruturação dos dados para construção da rede de Coautorias
  

  b4<-strsplit(as.character(base), split = ", " , fixed = FALSE)
  
  b5<-as.data.frame(do.call(cbind, b4))
  
  #Matriz de coautoria
  
  mtx<-table(stack(b5))
  
  mtx[mtx>1]<-1
  
  mtx_coaut<-mtx%*%t(mtx) 
  
  diag(mtx_coaut)<-0
  
  #Rede igraph
  
  rede_coaut<-graph_from_adjacency_matrix(mtx_coaut, weighted = T, mode = "undirected")
  
  #rede visNetwork
  
  vis_coaut<-toVisNetworkData(rede_coaut)
  
  #condicional e erro para frequencia mínima
  
  node_coaut<-data.frame("id"=vis_coaut$nodes$id, "label"=vis_coaut$nodes$label)
  links_coaut<-as.data.frame(vis_coaut$edges) 
  colnames(links_coaut)[3]<-'width'
  links_coaut<-filter(links_coaut, width>=1)
  
  links_coaut$label <- as.character(links_coaut$width)
  
  
 #Grau da Rede
  
  grau_df <- data.frame(
      Autor = names(degree(rede_coaut)),
      Grau = degree(rede_coaut),
      row.names = NULL)
    
  grau_df <- grau_df[order(grau_df$Grau, decreasing = TRUE), ]  
  
  colnames(grau_df)[2]<-"Grau (Coautorias)"
  
  
  #Intensidade de Colaboração
  
  peso_df <- data.frame(as_edgelist(rede_coaut), row.names = NULL)
  colnames(peso_df) <- c("Autor 1", "Autor 2")
  peso_df$Peso <- E(rede_coaut)$weight
  peso_df <- peso_df[order(peso_df$Peso, decreasing = TRUE), ]
 
}
  
  #REDE DE COAUTORIAS
  
  #Construção da rede de coautorias
  
  vis<-visNetwork(node_coaut, links_coaut) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visEdges(font = list(size = 14, align = "middle"))
  
 vis
 
 #REDE DE COOCORRÊNCIA KW
 
 #Construção da rede de coocorrencia KW
 
 vis_kw<-visNetwork(node_kw, links_kw) %>%
   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
   visIgraphLayout(layout = "layout_with_fr") %>%
   visEdges(font = list(size = 14, align = "middle"))
 
 vis_kw

