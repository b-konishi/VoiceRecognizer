data = data.frame(a = c(776, 1705),
                   i = c(265, 2392),
                   u = c(285, 988),
                   e = c(489, 1932),
                   o = c(504, 882))

print(data)
data2 <- data
m <- c(0,0)
ds <- c(0,0)
for (i in 1:ncol(data2)) {
  m <- m + data2[i]
}
m <- m/ncol(data2)
for (i in 1:ncol(data2)) {
  ds <- ds + (m-data2[i])^2
}
ds <- sqrt(ds/ncol(data2))
diag(A) <- c(ds[1], ds[2])
for (i in 1:ncol(data2)) {
  X <- c(data2[1,i],data2[2,i]) %*% A
  dim(X) <- c(2, 1)
  data2[i] <- X
}

cols <- c("red", "purple", "green", "black", "lightblue")

plot(0, 0, xlim=c(30000,160000), ylim=c(400000,1500000),
     xlab="1st Normalized Formant", ylab="2nd Normalized Formant"
     )
labels <- colnames(data)
legend("topleft", legend=labels, col=cols, pch=16)

for (i in 1:ncol(data)) {
  points(data2[1, i], data2[2, i], col=cols[i], pch=16, cex=2)
}
while (TRUE) {
  input <- readline()
  input <- as.numeric(unlist(strsplit(input, " ")))

  for (i in 1:ncol(data)) {
    x <- abs(data[1, i]-input[1])
    y <- abs(data[2, i]-input[2])
    d <- x^2 + y^2
    if (i == 1) {
      min <- c(i, d)
    } else {
      if (d < min[2]) {
        min <- c(i, d)
      }
    }
  }

  X <- input %*% A
  dim(X) <- c(2,1)
  input2 <- X

  points(input2[1], input2[2], col=cols[min[1]], pch=16, cex=2)
  msg = readline("How to fix, or not? : ")
  for (i in 1:ncol(data)) {
    m <- (match(labels[i], msg))
    if (!is.na(m)) {
      points(input2[1], input2[2], col=cols[i], pch=16, cex=2)
      # Updated data
      data[1, i] <- (data[1, i]+input[1]) /2
      data[2, i] <- (data[2, i]+input[2]) /2
      judge <- 1
      break
    }
  }
  if (judge == 1) {
    # Updated data
    data[1, min[1]] <- (data[1, min[1]]+input[1]) /2
    data[2, min[1]] <- (data[2, min[1]]+input[2]) /2
    judge <- 0
  }
}


