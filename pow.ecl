read real k
read int n
real nk := 1.
int count := 0
do
    check count < n
    nk := nk * k
    count := count + 1
od
write nk
