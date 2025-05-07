read int n
real sum := 0.0
int count := 0
do
    check count < n
    read real num
    sum := sum + num
    count := count + 1
od
real average := sum / float (n)
write average
