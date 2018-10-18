install.packages('DiagrammeR')

library(DiagrammeR)



grViz("
      
      digraph voces_and_circles{
      ### Add node statements


node [shape = box
      fontname = Helvetica]


'Regression based approaches' 

### Add edge statements

'Regression based approaches'  -> 'Global functional forms' ; 
'Regression based approaches'  -> 'Segmented functional form' ; 

  # graph, node, and edge definitions
  graph [compound = true, nodesep = .5, ranksep = .25,
         color = crimson]


node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 1,
        color = darkslategray]

edge [color = grey, arrowhead = none, arrowtail = none]
  # subgraph for Global functional forms
  subgraph cluster0 {
node [fixedsize = true, width = 3]
'Global functional forms' -> 'Categorised variable';
  }

# graph, node, and edge definitions
graph [compound = true, nodesep = .5, ranksep = .25, color = pink]
  subgraph cluster {
node [fixedsize = true, width = 3]
'Categorised variable' -> 'Linear models';
'Linear models' -> 'Polynomial models';
'Polynomial models' -> 'Fractional Polynomial models';
  }

# graph, node, and edge definitions
graph [compound = true, nodesep = .5, ranksep = .25,
         color = purple]


node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, fixedsize = true, width = 1,
      color = darkslategray]
      
edge [color = grey, arrowhead = none, arrowtail = none]

  # subgraph for Segmented functional form
subgraph cluster1 {
node [fixedsize = true, width = 3]
'Segmented functional form' -> 'Piecewise Polynomial models';
}


# graph, node, and edge definitions
graph [compound = true, nodesep = .5, ranksep = .25, color = blue]
subgraph cluster2 {
node [fixedsize = true, width = 3]
'Piecewise Polynomial models' -> 'Regression splines';
'Regression splines' -> 'Natural splines';
'Natural splines' -> 'Smoothing splines';
}
      }
      ")


library(itsadug)
library(mgcv)
data(simdat)
# add missing values to simdat:
simdat[sample(nrow(simdat), 15),]$Y <- NA


simdat$Condition <- as.factor(simdat$Condition)
# Note: this is really not the best fitting model for the data:
m1 <- bam(Y ~ Group * Condition + s(Time) + s(Trial) + 
            ti(Time, Trial) + s(Time, Subject, bs='fs', m=1, k=5), data=simdat, discrete=TRUE)

summary(m1)

table(simdat$Condition)
table(simdat$Group)
table(simdat$Condition)
table(simdat$Condition)


par(mfrow=c(1,2), cex=1.1)
# including random effects:
plot_smooth(m1, view="Time", cond=list(Group="Adults"))
# excluding random effects:
plot_smooth(m1, view="Time", cond=list(Group="Adults"), rm.ranef=TRUE)


par(mfrow=c(1,3), cex=1.1)
# first smooth of time:
plot(m1, select=1, shade=TRUE, scale=0, ylim=c(-15,15))
abline(h=0)
# second smooth, of trial:
plot(m1, select=2, shade=TRUE, scale=0, ylim=c(-15,15))
abline(h=0)
# third smooth, nonlienar interaction between Time and Trial:
plot(m1, select=3, scale=0, rug=FALSE)



par(mfrow=c(1,2), cex=1.1)
plot(m1, select=4)
abline(h=0)
inspect_random(m1, select=4)

