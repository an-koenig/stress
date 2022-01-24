library(dagitty)

dag1_raw <-
  'dag{
a [pos = "1,0"]
g [pos = "2,0"]
p [pos = "1,-1"]
s [pos = "2,-1"]
c [outcome, pos = "1.5,-2"]
a -> p
g -> p
p -> s
g -> s
a -> s
p -> c
s -> c
a -> c
g -> c
}
'

dag1 <- dagitty(dag1_raw)

coordinates(dag1) <-
  list(
    x = c(a = 1,
          g = 2,
          s = 2,
          p = 1,
          c = 1.5),
    y = c(a = 0,
          g = 0,
          s = -1,
          p = -1,
          c = -2))


plot(dag1)


impliedConditionalIndependencies(dag1)


