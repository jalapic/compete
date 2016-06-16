#' Signficance testing of Triangle Transitivity of Sociomatrix
#'
#' @param m A frequency or binary win-loss matrix
#' @param Nperms Number of randomizations in signficance test
#' @return Pt, triangle transitivity and pvalue
#' @examples
#' ttri_test(mouse)
#' @importFrom igraph "graph.adjacency"
#' @importFrom igraph "triad.census"
#' @importFrom igraph "dyad.census"
#' @importFrom sna "rguman"
#' @section Further details:
#' Algorithm described in D. Shizuka and D. B. McDonald, 2012,
#' A social network perspective on transitivity and linearity
#' in dominance hierarchies.  Animal Behaviour.
#' DOI:10.1016/j.anbehav.2012.01.011
#' Code adapted from original code by Dai Shikua - see
#' http://www.shizukalab.com/toolkits/sna/triangle-transitivity
#' @export

ttri_test=function(m, Nperms=1000){

  mat <- get_di_matrix(as.matrix(m))
  diag(mat)=0
  g = igraph::graph.adjacency(mat, mode="directed", diag=FALSE)
  tri=igraph::triad.census(g)
  Ntri=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,1)))
  sumtri = sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,0,0,1,1,0.5,0.75,0.75)) )
  Pt=sumtri/Ntri
  ttri.val = 4*(Pt-0.75)

  dyads=igraph::dyad.census(g)
  r.p.t=vector(length=Nperms)
  j=1
  Nperms1 = Nperms+1

    while(j<Nperms1){
      r=sna::rguman(1,nv=nrow(m),mut=dyads[[1]],asym=dyads[[2]],null=dyads[[3]])
      gr=igraph::graph.adjacency(as.matrix(r))
      r.triad=igraph::triad.census(gr)

      r.p.t[j]=r.triad[9]/(r.triad[10]+r.triad[9])
      if (is.na(r.p.t[j])) next else j=j+1
                    }

   pval=length(r.p.t[r.p.t>=Pt])/Nperms

   return(list("Pt"=Pt, "ttri"=ttri.val, "pval"=pval))

 }

