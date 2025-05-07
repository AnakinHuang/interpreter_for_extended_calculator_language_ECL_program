read int n
int count := 1
read real max
real min := max
do
    check count < n
    read real next
    if next > max
        max := next
    fi
    if next < min
        min := next
    fi
    count := count + 1
od
write min
write max
